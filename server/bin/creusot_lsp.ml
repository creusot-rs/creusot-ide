open Linol_lwt
open Creusot_lsp

type revdeps =
  | AllRsFiles  (* The coma and proofs in target/ touch every rs file *)
  | OneFile of DocumentUri.t

let revdeps : (DocumentUri.t, revdeps) Hashtbl.t = Hashtbl.create 16
let rs_files : (DocumentUri.t, unit) Hashtbl.t = Hashtbl.create 16

(* Given a .coma or proof.json, what .rs files have diagnostics that depend on it *)
let add_revdeps uri d = Hashtbl.replace revdeps uri d
let add_rs_file uri = Hashtbl.replace rs_files uri ()

let is_rs path = Filename.check_suffix path ".rs"

type file_type = Rs | Coma | Proof | Other

let file_type ?language_id path =
  match language_id with
  | Some (Some "rust") -> Rs
  | Some (Some "coma") -> Coma
  | Some (Some "why3proof") -> Proof
  | _ when Filename.check_suffix path ".rs" -> Rs
  | _ when Filename.check_suffix path ".coma" -> Coma
  | _ when Filename.basename path = "proof.json" -> Proof
  | _ -> Other

let iter_revdeps uri process =
  match Hashtbl.find_opt revdeps uri with
  | Some AllRsFiles ->
    Hashtbl.iter (fun name () -> if is_rs (DocumentUri.to_path name) then process name) rs_files
  | Some (OneFile f) -> process f
  | None -> ()

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
          Creusot_manager.read_cargo ~root;
          (match Creusot_manager.get_package_name () with
          | None -> ()
          | Some crate ->
            (* Why3session.collect_sessions_for ~root ~crate; *)
            let (/) = Filename.concat in
            let global_coma = root / "target" / (crate ^ "-lib.coma") in
            let global_proof = root / "target" / (crate ^ "-lib") / "proof.json" in
            if Creusot_manager.coma_file ~uri:(DocumentUri.of_path global_coma) global_coma then (
              Creusot_manager.add_proof_json (File global_proof);
              add_revdeps (DocumentUri.of_path global_coma) AllRsFiles;
              add_revdeps (DocumentUri.of_path global_proof) AllRsFiles)
          );
          Lwt.return ()
        | _ -> Lwt.return ()
      in
      super#on_req_initialize ~notify_back params

    method! config_modify_capabilities capabilities =
      { capabilities with
        documentLinkProvider = Some (DocumentLinkOptions.create ~resolveProvider:false ());
      }

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
      | DidChangeWatchedFiles { changes } -> Lwt_list.iter_s (self#changed_watched_file ~notify_back) changes
      | notif -> super#on_notification_unhandled ~notify_back notif

    method private changed_watched_file ~notify_back change =
      match change.type_ with
      | Created | Changed ->
          let path = DocumentUri.to_path change.uri in
          let base = Filename.basename path in
          if base = "why3session.xml" then
            () (* Creusot_lsp.Why3session.process_why3session_path path *)
          else if base = "proof.json" then
            Creusot_manager.add_proof_json (File path)
          else if Filename.check_suffix base ".coma" then
            ignore (Creusot_manager.coma_file ~uri:change.uri path);
          self#refresh_all ~notify_back change.uri
      | _ -> Lwt.return ()

    method private refresh_all ~notify_back uri : _ t =
      let* _ = notify_back#send_request Lsp.Server_request.CodeLensRefresh (fun _result -> Lwt.return ()) in
      let open Util.Async in
      async_handler @@ fun () ->
        iter_revdeps uri @@ fun uri ->
          notify_back#set_uri uri; (* This is disgusting *)
          let ft = file_type (DocumentUri.to_path uri) in
          async (self#update_diagnostics ~notify_back ~ft uri)

    method private update_diagnostics ~notify_back ~ft uri =
      let diags = match ft with
        | Rs -> Creusot_manager.get_rust_diagnostics uri
        | Proof -> Creusot_manager.get_proof_json_diagnostics uri
        | _ -> [] in
      notify_back#send_diagnostic diags

    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        ?languageId
        (uri : Lsp.Types.DocumentUri.t) (content : string) =
        let ft = file_type ~language_id:languageId (DocumentUri.to_path uri) in
        let () = self#refresh_file ~notify_back ~ft uri ~content in
        self#update_diagnostics ~notify_back ~ft uri

    method private refresh_file ~notify_back:_ ~ft (uri : DocumentUri.t) ~content =
      let path = DocumentUri.to_path uri in
      match ft with
      | Rs ->
        add_rs_file uri;
        let base = Filename.chop_suffix path ".rs" in
        let coma = base ^ ".coma" in
        let proof = Filename.concat base "proof.json" in
        (* Hack for the creusot repository: tests are standalone rust files and the coma and proofs are next to them. *)
        if Creusot_manager.coma_file ~uri:(DocumentUri.of_path coma) coma then (
          Creusot_manager.declare_orphan path;
          Creusot_manager.add_proof_json (File proof);
          add_revdeps (DocumentUri.of_path coma) (OneFile uri);
          add_revdeps (DocumentUri.of_path proof) (OneFile uri);
        );
        Creusot_manager.rust_file_as_string ~path content
      | Coma -> ignore (Creusot_manager.coma_file ~uri path)
      | Proof -> Creusot_manager.add_proof_json (String (path, content))
      | Other -> ()

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      self#_on_doc ~notify_back ~languageId:d.languageId d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ _ : unit Linol_lwt.t =
      Linol_lwt.return ()

    method! config_code_lens_options : CodeLensOptions.t option = Some {
        resolveProvider = Some false;
        workDoneProgress = Some false;
      }

    method! on_req_code_lens ~notify_back:_ ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_ _doc_state =
      let path = DocumentUri.to_path uri in
      let lenses = if Filename.check_suffix path ".rs" then
          Creusot_manager.get_rust_lenses uri
        else if Filename.check_suffix path ".coma" then
            Creusot_manager.get_coma_lenses uri
        else []
      in return lenses

    method! config_inlay_hints = Some (`InlayHintOptions (InlayHintOptions.create ()))

    method! on_req_inlay_hint ~notify_back:_ ~id:_ ~uri ~range:_ () =
      let path = DocumentUri.to_path uri in
      if Filename.basename path = "proof.json" then
        let hints = Creusot_manager.get_proof_json_inlay_hints path in
        return (Some hints)
      else return None

    method! on_request_unhandled (type a) ~notify_back ~id (r : a Lsp.Client_request.t) : a t =
      match r with
      | TextDocumentLink (DocumentLinkParams.{ textDocument = { uri }; _ }) ->
        let path = DocumentUri.to_path uri in
        Lwt.return @@ if Filename.check_suffix path ".coma" then
            Some (Creusot_manager.get_coma_links uri)
          else None
      | _ -> super#on_request_unhandled ~notify_back ~id r

    method! on_unknown_request ~notify_back ~server_request ~id name req : Yojson.Safe.t t =
      match name with
      | "creusot/show" -> (
        match req with
        | None -> Lwt.return `Null (* error *)
        | Some req ->
          let arg = Jsonrpc.Structured.yojson_of_t req in
          match Why3findUtil.ProofPath.qualified_goal_of_json arg with
          | None -> Lwt.return (`String "Error: invalid proof path")
          | Some path ->
            let goal = Why3findUtil.get_goal path in
            let msg = match goal with
            | None -> "No goal found"
            | Some goal -> goal in
            Lwt.return (`String msg)
      )
      | _ -> super#on_unknown_request ~notify_back ~server_request ~id name req
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
