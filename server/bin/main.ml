open Linol_lwt
open Creusot_lsp
open Rust_syntax

module Log = (val Logs.src_log Logs.Src.(create "creusotlsp"))

let loc_to_range loc =
  let _, l1, c1, l2, c2 = Why3.Loc.get loc in
  Lsp.Types.Range.create (* why is this necessary?       ----------v *)
    ~start:(Lsp.Types.Position.create ~line:(l1 - 1) ~character:c1)
    ~end_:(Lsp.Types.Position.create ~line:(l2 - 1) ~character:c2)

let warn loc message =
  Lsp.Types.Diagnostic.create () ~range:(loc_to_range loc) ~severity:Warning ~source:"Creusot IDE" ~message

let error loc message =
  Lsp.Types.Diagnostic.create () ~range:(loc_to_range loc) ~severity:Error ~source:"Creusot IDE" ~message

let log_info (n : Jsonrpc2.notify_back) msg = n#send_log_msg ~type_:MessageType.Info msg

type state_after_processing = unit

let process_some_input_file (_file_contents : string) : state_after_processing =
  ()

let diagnostics (_state : state_after_processing) : Lsp.Types.Diagnostic.t list
    =
  []

type doc = {
    package: string;
    module_: string;
    defns: (def_path * Lsp.Types.Range.t) list;
  }

(* Lsp server class

   This is the main point of interaction beetween the code checking documents
   (parsing, typing, etc...), and the code of linol.

   The [Linol_lwt.Jsonrpc2.server] class defines a method for each of the action
   that the lsp server receives, such as opening of a document, when a document
   changes, etc.. By default, the method predefined does nothing (or errors out ?),
   so that users only need to override methods that they want the server to
   actually meaningfully interpret and respond to.
*)
class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super

    (* one env per document *)
    val buffers : (Lsp.Types.DocumentUri.t, state_after_processing) Hashtbl.t =
      Hashtbl.create 32

    val rootUri : DocumentUri.t option ref = ref None
    method private mk_relative_path p =
      match !rootUri with
      | None -> Stdlib.failwith "No rootUri found"
      | Some root -> DocumentUri.of_path (Filename.concat (DocumentUri.to_path root) p)

    method private spawn_query_handler f = Linol_lwt.spawn f

    method! on_req_initialize ~notify_back params =
      let open Lsp.Types in
      rootUri := params.rootUri;
      let* () = match params.InitializeParams.workspaceFolders with
        | Some (Some folders) -> folders |> Lwt_list.iter_s @@ fun folder ->
          let root = DocumentUri.to_path folder.WorkspaceFolder.uri in
          let open Creusot_lsp in
          let () =
          Creusot_manager.read_cargo ~root;
            let crate = Creusot_manager.get_package_name () in
            (* Why3session.collect_sessions_for ~root ~crate; *)
            let (/) = Filename.concat in
            Creusot_manager.coma_file (root / "target" / (crate ^ "-lib.coma"));
            Why3find.read_proof_json ~fname:(root / "target" / (crate ^ "-lib") / "proof.json")
              |> List.iter (fun (name, thy) -> Why3session.add_thy name thy)
          in
          log_info notify_back @@ Creusot_lsp.Why3session.debug_theories ()
        | _ -> Lwt.return ()
      in
      super#on_req_initialize ~notify_back params

    method! on_notification_unhandled ~notify_back = function
      | Initialized ->
        (* We can start registering capabilities after receiving the Initialized notification. *)
        let file_watcher_registration = Registration.create
          ~id:"fileWatcher"
          ~method_:"workspace/didChangeWatchedFiles"
          ~registerOptions:(`Assoc [
              "watchers", `List [
                `Assoc [
                  "globPattern", `String "**/{why3session.xml,proof.json,*.coma}";
                ]
              ]
            ])
          () in
        let reg_params = RegistrationParams.create ~registrations:[file_watcher_registration] in
        let* _req_id = notify_back#send_request (ClientRegisterCapability reg_params) (fun _result -> Lwt.return ()) in
        Lwt.return ()
      | DidChangeWatchedFiles { changes } -> Lwt_list.iter_s (self#refresh_proof ~notify_back) changes
      | notif -> super#on_notification_unhandled ~notify_back notif

    method private refresh_proof ~notify_back change =
      match change.type_ with
      | Created | Changed ->
          let path = DocumentUri.to_path change.uri in
          let base = Filename.basename path in
          if base = "why3session.xml" then
            Creusot_lsp.Why3session.process_why3session_path path
          else if base = "proof.json" then
            Creusot_lsp.Why3find.read_proof_json ~fname:path |> List.iter (fun (name, thy) ->
              Creusot_lsp.Why3session.add_thy name thy)
            else if Filename.check_suffix base ".coma" then
              Creusot_manager.coma_file path;
          self#refresh_all ~notify_back
      | _ -> Lwt.return ()

    val files_with_diags : (DocumentUri.t, unit) Hashtbl.t = Hashtbl.create 32

    method private refresh_all ~notify_back : _ t =
      let* _ = notify_back#send_request Lsp.Server_request.CodeLensRefresh (fun _result -> Lwt.return ()) in
      Hashtbl.to_seq files_with_diags |> List.of_seq |>
        Lwt_list.iter_s (fun (uri, _) ->
          notify_back#set_uri uri; (* This is disgusting *)
          self#update_diagnostics ~notify_back uri)

    method private update_diagnostics ~notify_back uri =
      let to_related_information (goal_name, location) =
        DiagnosticRelatedInformation.create ~location ~message:goal_name
      in
      let to_lsp_diagnostics d =
        let open Creusot_manager.RustDiagnostic in
        let range = d.range in
        let source = "creusot" in
        let message, severity, relatedInformation = match d.status with
          | Qed -> "QED", DiagnosticSeverity.Information, []
          | ToProve goals ->
              Printf.sprintf "%d unproved goals" (Array.length goals),
              DiagnosticSeverity.Warning,
              Array.(to_list (map to_related_information goals))
        in
        Lsp.Types.Diagnostic.create ~message ~range ~severity ~source ~relatedInformation ()
      in
      let diags = Creusot_manager.get_rust_diagnostics ~path:(DocumentUri.to_path uri)
          |> List.map to_lsp_diagnostics in
      notify_back#send_diagnostic diags

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        ?languageId
        (uri : Lsp.Types.DocumentUri.t) (content : string) =
        self#refresh_file ~languageId uri ~content;
        self#update_diagnostics ~notify_back uri

    method private refresh_file ?languageId (uri : DocumentUri.t) ~content =
      let package = Creusot_manager.get_package_name () in
      let path = DocumentUri.to_path uri in
      let rusty = match languageId with
        | Some (Some "rust") -> true
        | Some _ -> false
        | None -> Filename.check_suffix path ".rs"
      in
      if rusty then Creusot_manager.rust_file_as_string ~package ~path content

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      Hashtbl.add files_with_diags d.uri ();
      self#_on_doc ~notify_back ~languageId:d.languageId d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_lwt.t =
      Hashtbl.remove buffers d.uri;
      Linol_lwt.return ()

    method! config_code_lens_options : CodeLensOptions.t option = Some {
        resolveProvider = Some false;
        workDoneProgress = Some false;
      }

    method! on_req_code_lens ~notify_back:_ ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_ _doc_state =
      let to_lsp_lenses d =
        let open Creusot_manager.RustDiagnostic in
        let range = d.range in
        let command = match d.status with
          | Qed -> Command.create ~title:"QED" ~command:"" ()
          | ToProve goals -> Command.create
              ~title:(Printf.sprintf "%d unproved goals" (Array.length goals))
              ~command:"creusot.peekLocations"
              ~arguments:[
                DocumentUri.yojson_of_t uri;
                Position.(yojson_of_t range.Range.start);
                `List (Array.(to_list (map (fun (_name, location) -> Location.yojson_of_t location) goals)));
                `String "gotoAndPeek"]
                ()
        in
        CodeLens.create ~command ~range ()
      in
      let lenses = Creusot_manager.get_rust_diagnostics ~path:(DocumentUri.to_path uri)
          |> List.map to_lsp_lenses in
      Lwt.return lenses
  end

let run () =
  Printexc.record_backtrace true;
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio ~env:() s in
  let task =
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_lwt.Jsonrpc2.run ~shutdown server
  in
  match Linol_lwt.run task with
  | () -> ()
  | exception e ->
    let e = Printexc.to_string e in
    Printf.eprintf "error: %s\n%!" e;
    exit 1

let () = run ()
(*
let () =
  let version = ref false in
  let arg = Lsp.Cli.Arg.create () in
  let spec =
    [ "--version", Arg.Set version, "print version"
    ] @ Lsp.Cli.Arg.spec arg
  in
  let usage =
    "creusotlsp [ --stdio | --socket PORT | --port PORT | --pipe PIPE ] [ --clientProcessId pid ]"
  in
  Arg.parse spec (fun _ -> raise @@ Arg.Bad "anonymous arguments aren't allowed") usage;
  let channel =
    match Lsp.Cli.Arg.channel arg with
    | Ok c -> c
    | Error s ->
      Format.eprintf "%s@." s;
      Arg.usage spec usage;
      exit 1
  in
  if !version then
    print_endline (Creusot_lsp.Version.get ())
  else
    try
      Creusot_lsp.Server.run channel
    with
    | exn ->
      let exn = Printexc.to_string exn in
      let backtrace = Printexc.get_raw_backtrace () |> Printexc.raw_backtrace_to_string in
      Format.eprintf "%s@.%s@." exn backtrace;
      exit 1
*)
