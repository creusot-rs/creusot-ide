open Linol_lwt
open Creusot_lsp

let target_lsp = "target/.creusot-lsp"

let proof_info_path ~proof_file =
  let cwd = Sys.getcwd () in
  if String.sub proof_file 0 (String.length cwd) <> cwd || proof_file.[String.length cwd] <> '/' then
    Stdlib.failwith (Printf.sprintf "Expected the proof file %s to be in the current working directory %s" proof_file cwd);
  let proof_file = String.sub proof_file (String.length cwd + 1) (String.length proof_file - String.length cwd - 1) in
  Filename.concat target_lsp proof_file

let rec mkdir_parents dir =
  if Sys.file_exists dir then ()
  else (
    mkdir_parents (Filename.dirname dir);
    Sys.mkdir dir 0o755)

let exec_self ~cmd file =
  let self = Sys.argv.(0) in
  let ih, oh = Unix.open_process_args self [|self; cmd; file|] in
  let msg =
    let buf = Buffer.create 1024 in
    try while true do
      Buffer.add_channel buf ih 1024;
    done with End_of_file -> Buffer.contents buf
  in
  (Unix.close_process (ih, oh), msg)

let get_current_proof_info ~info_file =
  try
    let info = Why3findUtil.ProofInfo.of_file info_file in
    if Why3findUtil.ProofInfo.up_to_date info then
      Some info
    else
      None
  with _ -> None

let get_proof_info ~proof_file =
  try
    let info_file = proof_info_path ~proof_file in
    (* If the existing ProofInfo matches the current file, reuse it *)
    match get_current_proof_info ~info_file with
    | Some info -> Some info
    | None ->
      let status, msg = exec_self ~cmd:"why3" proof_file in
      match status with
      | Unix.WEXITED 0 -> Some (Why3findUtil.ProofInfo.of_file info_file)
      | Unix.WEXITED _ -> Stdlib.failwith msg
      | _ -> Stdlib.failwith "Interrupted"
  with
  | e -> Log.log Error "get_proof_info: %s" (Printexc.to_string e); None

let exec_show_task goal =
  match exec_self ~cmd:"showtask" goal with
  | _, msg -> msg
  | exception e -> Printexc.to_string e

let run_why3 proof_file =
  let coma_file = Filename.(dirname proof_file ^ ".coma") in
  let output = proof_info_path ~proof_file in
  mkdir_parents (Filename.dirname output);
  let info = Why3findUtil.get_proof_info (Why3findUtil.get_env ()) ~proof_file ~coma_file in
  Why3findUtil.ProofInfo.to_file output info

let show_task req =
  let output = match Why3findUtil.decode_subgoal req with
    | exception Failure _ -> "Invalid goal (please report to https://github.com/creusot-rs/creusot-ide/issues)\n" ^ req
    | goal ->
      match Why3findUtil.get_goal (Why3findUtil.get_env ()) goal with
      | None -> "No goal found (perhaps the proofs are out of date)"
      | Some goal -> goal
  in
  print_endline output

let pp_changed_files fmt =
  Format.pp_print_string fmt "Changed: ";
  let pp_file fmt change = Format.pp_print_string fmt (DocumentUri.to_path change.FileEvent.uri) in
  Format.pp_print_list ~pp_sep:Format.pp_print_space pp_file fmt

let server_info = InitializeResult.create_serverInfo ~name:"Creusot" ~version:"0.1" ()

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
          Creusot_manager.initialize root;
          Lwt.return ()
        | _ -> Lwt.return ()
      in
      let* result = super#on_req_initialize ~notify_back params in
      return InitializeResult.{ result with serverInfo = Some server_info }

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
                  "globPattern", `String "**/{why3session.xml,proof.json,*.coma,*.rs}";
                ]
              ]
            ])
          () in
        let reg_params = RegistrationParams.create ~registrations:[file_watcher_registration] in
        let* _req_id = notify_back#send_request (ClientRegisterCapability reg_params) (fun _result -> Lwt.return ()) in
        Lwt.return ()
      | DidChangeWatchedFiles { changes } ->
        Log.debug "%a" pp_changed_files changes;
        Lwt_list.iter_s (self#changed_watched_file ~notify_back) changes
      | notif -> super#on_notification_unhandled ~notify_back notif

    method private changed_watched_file ~notify_back change =
      match change.type_ with
      | Created | Changed ->
        Creusot_manager.add_file ~get_proof_info (File (DocumentUri.to_path change.uri));
        let* _ = notify_back#send_request Lsp.Server_request.CodeLensRefresh (fun _ -> Lwt.return ()) in
        Lwt.return ()
      | _ -> Lwt.return ()

    method private _on_doc ~notify_back:(_ : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (content : string) =
        let path = DocumentUri.to_path uri in
        Creusot_manager.add_file ~get_proof_info (String (path, content));
        Lwt.return ()

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back:_ _d _c ~old_content:_ ~new_content:_ =
      Lwt.return ()

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ _ : unit Linol_lwt.t =
      Linol_lwt.return ()

    method! config_code_lens_options : CodeLensOptions.t option = Some {
        resolveProvider = Some false;
        workDoneProgress = Some false;
      }

    method! on_req_code_lens ~notify_back ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_ _doc_state =
      match Creusot_manager.uri_to_file uri with
      | None -> return []
      | Some file ->
        let send_test_items items =
          let items = List.map Test_api.yojson_of_test_item items in
              notify_back#send_notification (Lsp.Server_notification.UnknownNotification
                (Jsonrpc.Notification.create
                  ~method_:"creusot/testitems"
                  ~params:(Jsonrpc.Structured.t_of_yojson (`List [`String (DocumentUri.to_string uri); `List items]))
                  ())) in
        let option_iter f = function
          | None -> return ()
          | Some x -> f x in
        Creusot_manager.refresh ~get_proof_info file;
        let* _ = option_iter send_test_items (Creusot_manager.get_test_items file) in
        let* _ = option_iter notify_back#send_diagnostic (Creusot_manager.get_diagnostics file) in
        return (Creusot_manager.get_code_lenses file)

    method! config_inlay_hints = None (* Some (`InlayHintOptions (InlayHintOptions.create ())) *)

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
      | "creusot/showTask" -> (
        match req with
        | None -> Lwt.return `Null (* error *)
        | Some req ->
          let req = Jsonrpc.Structured.yojson_of_t req in
          let msg =
            match req with
            | `List [`String req] -> exec_show_task req
            | _ -> "Bad request (please report to https://github.com/creusot-rs/creusot-ide/issues)\n" ^ Yojson.Safe.to_string req
          in Lwt.return (`String msg)
      )
      | _ -> super#on_unknown_request ~notify_back ~server_request ~id name req
  end

let run_server () =
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

type args
  = Server
  | Why3 of string
  | ShowTask of string

let args () =
  if Array.length Sys.argv = 1 then Server
  else if Sys.argv.(1) = "why3" then Why3 Sys.argv.(2)
  else if Sys.argv.(1) = "showtask" then ShowTask Sys.argv.(2)
  else (
    if Array.mem "--debug" Sys.argv then Log.set_debug ();
    Server
  )

let () =
  match args () with
  | Server -> run_server ()
  | Why3 arg -> run_why3 arg
  | ShowTask arg -> show_task arg

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
