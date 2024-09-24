open Linol_lwt

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
    defns: (string list * Lsp.Types.Range.t) list;
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

    val funhooks : (Lsp.Types.DocumentUri.t, doc) Hashtbl.t =
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
          Creusot_lsp.Why3session.collect_sessions
            ~root:(DocumentUri.to_path folder.WorkspaceFolder.uri);
          log_info notify_back @@ Creusot_lsp.Why3session.debug_theories ()
        | _ -> Lwt.return ()
      in
      super#on_req_initialize ~notify_back params

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      let new_state = process_some_input_file contents in
      Hashtbl.replace buffers uri new_state;
      let diags = diagnostics new_state in
      notify_back#send_diagnostic diags

    method private refresh_lenses ?languageId (uri : DocumentUri.t) ~content =
      let rusty = match languageId with
        | Some "rust" -> true
        | Some _ -> false
        | None -> match Hashtbl.find_opt funhooks uri with (* if we did refresh the lenses once it must have been rust *)
          | Some _ -> true
          | None -> false in
      if rusty then (
        let package = Creusot_lsp.Why3session.get_package_name () in
        let module_ = uri |> DocumentUri.to_path |> Filename.basename |> Filename.remove_extension in
        let names = Creusot_lsp.Hacky_rs_parser.list_names (Lexing.from_string content) in
        let defns = names |> List.map (fun (qname, span) ->
              let span_to_range (start, stop) =
                Lsp.Types.Range.create
                  ~start:(Lsp.Types.Position.create ~line:(start.Lexing.pos_lnum - 1) ~character:(start.Lexing.pos_cnum - start.Lexing.pos_bol))
                  ~end_:(Lsp.Types.Position.create ~line:(stop.Lexing.pos_lnum - 1) ~character:(stop.Lexing.pos_cnum - stop.Lexing.pos_bol)) in
              (qname, span_to_range span)) in
        Hashtbl.add funhooks uri { package; module_; defns }
      )

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      self#refresh_lenses ~languageId:d.languageId d.uri ~content;
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#refresh_lenses d.uri ~content:new_content;
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

    method! on_req_code_lens ~notify_back ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_ _doc_state =
      match Hashtbl.find_opt funhooks uri with
      | None -> Lwt.return []
      | Some doc ->
        let* lenses = doc.defns |> Lwt_list.filter_map_s (fun (qname, range) ->
            let th_name = Creusot_lsp.Why3session.theory_of_path (doc.package :: doc.module_ :: qname) in
            let* _ = log_info notify_back (Printf.sprintf "%s" th_name) in
            let th_opt = Creusot_lsp.Why3session.get_theory th_name in
            let lwt_option_map f = function
              | None -> Lwt.return None
              | Some x -> Lwt.map (fun y -> Some y) (f x) in
            th_opt |> lwt_option_map @@ fun th ->
              let n_goals = Array.length th.Creusot_lsp.Types.unproved_goals in
              let why3session_path = DocumentUri.of_path th.Creusot_lsp.Types.path in
              let dummy_pos = Position.create ~line:1 ~character:1 in
              let dummy_pos' = Position.create ~line:1 ~character:2 in
              let tgt_range = Range.create ~start:dummy_pos ~end_:dummy_pos' in
              let command = if n_goals = 0 then
                  Lsp.Types.Command.create
                    ~title:"QED"
                    ~command:""
                    ()
                else Lsp.Types.Command.create
                  ~title:(Printf.sprintf "%d unproved goals" n_goals)
                  ~command:"creusot.peekLocations"
                  ~arguments:[
                    DocumentUri.yojson_of_t uri;
                    Position.(yojson_of_t range.Range.start);
                    `List [(Array.map (fun _ -> Location.(yojson_of_t @@ create ~range:tgt_range ~uri:why3session_path)) th.Creusot_lsp.Types.unproved_goals).(0)];
                    `String "gotoAndPeek"]
                    ()
              in
              Lwt.return @@ Lsp.Types.CodeLens.create ~command ~range ()) in
        Lwt.return lenses

    method! config_inlay_hints = None (* Some (`InlayHintOptions (InlayHintOptions.create ())) *)

    method! on_req_inlay_hint ~notify_back ~id:_ ~uri ~range:_ () =
      if false then (
      let* _ = log_info notify_back "req inlay hint" in
      match Hashtbl.find_opt funhooks uri with
      | None -> Lwt.return None
      | Some doc ->
        let* hints = doc.defns |> Lwt_list.filter_map_s (fun (qname, range) ->
            let th_name = Creusot_lsp.Why3session.theory_of_path (doc.package :: doc.module_ :: qname) in
            let th_opt = Creusot_lsp.Why3session.get_theory th_name in
            let lwt_option_map f = function
              | None -> Lwt.return None
              | Some x -> Lwt.map (fun y -> Some y) (f x) in
            th_opt |> lwt_option_map @@ fun th ->
              let position = range.Range.start in
              let command = Lsp.Types.Command.create
                ~title:"" (* unused *)
                ~command:"creusot.openFile"
                ~arguments:[`String th.Creusot_lsp.Types.path] () in
              let to_hint_label goal = Lsp.Types.InlayHintLabelPart.create
                  ~tooltip:(`String "Go to goal")
                  ~command
                  ~value:goal.Creusot_lsp.Types.goal_name () in
              let label = `List (Array.to_list @@ Array.map to_hint_label th.Creusot_lsp.Types.unproved_goals) in
              Lwt.return @@ Lsp.Types.InlayHint.create ~label ~position ()) in
        Lwt.return (Some hints))
      else Lwt.return None
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
