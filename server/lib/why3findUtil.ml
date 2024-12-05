open Why3find
open Lsp.Types
open Util
open Log

module ProofPath = struct
  type lazy_tactic_path = (string option ref * int) list  (* tactic names might not have been resolved yet *)
  type tactic_path = (string * int) list  (* tactic * goalindex *)
  type 'a _goal = {
    vc: string;
    tactics: 'a;
    }
  type goal = tactic_path _goal
  type lazy_goal = lazy_tactic_path _goal
  type 'a with_theory = {
    file: string;  (* The source coma file *)
    theory: string;
    goal_info: 'a;
  }
  type 'a goal_info = {
    goal: 'a;
    goal_range: Range.t;
    is_null: bool;
  }
  type info_goal = goal goal_info
  type theory = info_goal list with_theory
  type qualified_goal = goal with_theory

  let force_tactic_path : lazy_tactic_path -> tactic_path =
    List.map @@ fun (tac, i) -> (match !tac with None -> "_" | Some tac -> tac), i

  let pp_tactic_path =
    Format.pp_print_list ~pp_sep:Format.pp_print_nothing
      (fun h (tac, i) -> Format.fprintf h ".%s.%d" tac i)

  let pp_short_tactic =
    Format.pp_print_list ~pp_sep:Format.pp_print_nothing
    (fun h (_, i) -> Format.fprintf h ".%d" i)

  let pp_goal_ pp_tactic h t =
    Format.fprintf h "%s%a" t.vc pp_tactic t.tactics

  let pp_goal = pp_goal_ pp_tactic_path
  let pp_short_goal = pp_goal_ pp_short_tactic

  let pp_info_goals h = Format.pp_print_list (fun h goal -> pp_goal h goal.goal) h

  let pp_theory h (t : theory) = Format.fprintf h "%s:%s:@[%a@]" t.file t.theory pp_info_goals t.goal_info

  let pp_qualified_goal h (t : qualified_goal) = Format.fprintf h "%s:%s:%a" t.file t.theory pp_goal t.goal_info

  let string_of_goal = Format.asprintf "%a" pp_goal
  let short_string_of_goal = Format.asprintf "%a" pp_short_goal

  let string_of_qualified_goal = Format.asprintf "%a" pp_qualified_goal

  let tactic_path_to_json (p : tactic_path) : Yojson.Safe.t =
    `List (List.map (fun (tactic, i) -> `List [`String tactic; `Int i]) p)

  let qualified_goal_to_json (p : qualified_goal) : Yojson.Safe.t =
    `Assoc [
      "file", `String p.file;
      "theory", `String p.theory;
      "vc", `String p.goal_info.vc;
      "tactics", tactic_path_to_json p.goal_info.tactics;
    ]

  let rec map_option f = function
    | [] -> Some []
    | x :: xs -> match f x with
      | Some y -> map_option f xs |> Option.map (fun ys -> y :: ys)
      | None -> None

  let tactic_path_of_json (j : Yojson.Safe.t) : tactic_path option =
    match j with
    | `List l -> map_option (function
      | `List [`String tactic; `Int i] -> Some (tactic, i)
      | _ -> None) l
    | _ -> None

  let qualified_goal_of_json (j : Yojson.Safe.t) : qualified_goal option =
    let (let+) = Option.bind in
    let open Yojson.Safe.Util in
    match j with
    | `Assoc _l -> (
      let+ file = to_string_option @@ member "file" j in
      let+ theory = to_string_option @@ member "theory" j in
      let+ vc = to_string_option @@ member "vc" j in
      let+ tactics = member "tactics" j |> tactic_path_of_json in
      Some { file; theory; goal_info = { vc; tactics } }
    )
    | _ -> None
end

open ProofPath

exception BadJson of string

let throw e decoder =
  let (lnum, cnum), _ = Jsonm.decoded_range decoder in
  let msg f x = Format.asprintf "JSON error at %d:%d, %a" lnum cnum f x in
  match e with
  | `Error e -> raise (BadJson (msg Jsonm.pp_error e))
  | `Lexeme x -> raise (BadJson (msg (fun h -> Format.fprintf h "unexpected %a" Jsonm.pp_lexeme) x))
  | _ -> raise (BadJson (msg (fun h () -> Format.fprintf h "unexpected EOF") ()))

let eat_Os decoder = match Jsonm.decode decoder with
  | `Lexeme `Os -> ()
  | e -> throw e decoder

let eat_As decoder = match Jsonm.decode decoder with
  | `Lexeme `As -> ()
  | e -> throw e decoder

let eat_string decoder = match Jsonm.decode decoder with
  | `Lexeme (`String str) -> str
  | e -> throw e decoder

type trailing_ae = TrailingAe | NoAe

let from_jsonm_range ((start_l, start_c), end_) =
  let position_of (line, character) = Position.create ~line:(line - 1) ~character in
  Range.create ~start:(position_of (start_l, start_c - 1)) ~end_:(position_of end_)

let rec parse_proofs_body ~coma vc visit decoder tactic_path =
  let tactic = ref None in
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name "tactic") ->
      let name = eat_string decoder in
      tactic := Some name;
      loop ()
    | `Lexeme (`Name "children") ->
      eat_As decoder;
      let rec loop_children i = match parse_proofs ~coma vc visit decoder ((tactic, i) :: tactic_path) with
        | TrailingAe -> ()
        | NoAe -> loop_children (i + 1)
      in loop_children 0; loop ()
    | `Lexeme (`Name ("prover" | "time" | "size")) ->
      (* eat strings and numbers *)
      (match Jsonm.decode decoder with
      | `Lexeme (`String _ | `Float _) -> loop ()
      | e -> throw e decoder)
    | `Lexeme `Oe -> ()
    | e -> throw e decoder
  in loop ()

and parse_proofs ~coma vc (visit : lazy_goal goal_info -> unit) decoder tactic_path =
  match Jsonm.decode decoder with
  | `Lexeme `Os ->
    (* For non-null goals (non-leaf or proved), we remember the location of the opening brace *)
    let goal_range = from_jsonm_range (Jsonm.decoded_range decoder) in
    let tactics = List.rev tactic_path in
    visit { goal = { vc; tactics }; goal_range; is_null = false};
    parse_proofs_body ~coma vc visit decoder tactic_path; NoAe
  | `Lexeme `Null ->
    let goal_range = from_jsonm_range (Jsonm.decoded_range decoder) in
    let tactics = List.rev tactic_path in
    visit { goal = { vc; tactics }; goal_range; is_null = true }; NoAe
  | `Lexeme `Ae -> TrailingAe
  | e -> throw e decoder

let parse_theory ~coma visit decoder =
  eat_Os decoder;
  let goals = ref [] in
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name vc) ->
      let visit_goal = fun goal -> goals := goal :: !goals in
      (match parse_proofs ~coma vc visit_goal decoder [] with
      | TrailingAe -> throw (`Lexeme `Ae) decoder
      | NoAe -> loop ())
    | `Lexeme `Oe -> ()
    | e -> throw e decoder
  in loop ();
  (* After traversing the vc the tactic names should be known *)
  let force_goal info_goal =
    let goal = { info_goal.goal with tactics = force_tactic_path info_goal.goal.tactics } in
    { info_goal with goal } in
  visit (List.map force_goal (List.rev !goals))

let parse_theories ~coma (visit : theory -> unit) decoder =
  eat_Os decoder;
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name theory) ->
      let visit_goals = fun goal_info -> visit { file = coma; theory; goal_info } in
      parse_theory ~coma visit_goals decoder;
      loop ()
    | `Lexeme `Oe -> ()
    | e -> throw e decoder
  in loop ()

let rec skip_json_ decoder : [`Ae] option = match Jsonm.decode decoder with
    | `Lexeme `As ->
      let rec skip_items () = match skip_json_ decoder with
        | Some `Ae -> None
        | None -> skip_items ()
      in skip_items ()
    | `Lexeme `Os ->
      let rec skip_members () =
        match Jsonm.decode decoder with
        | `Lexeme `Oe -> ()
        | `Lexeme (`Name _) -> skip_json decoder; skip_members ()
        | e -> throw e decoder
      in skip_members (); None
    | `Lexeme (`Null | `String _ | `Float _ | `Bool _) -> None
    | `Lexeme `Ae -> Some `Ae
    | e -> throw e decoder

and skip_json decoder =
  match skip_json_ decoder with
  | None -> ()
  | Some e -> throw (`Lexeme e) decoder

let parse_json ~coma (visit : theory -> unit) decoder =
  eat_Os decoder;
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name "proofs") -> parse_theories ~coma visit decoder
    | `Lexeme (`Name _) -> skip_json decoder; loop ()
    | `Lexeme `Oe -> () (* no proofs found *)
    | e -> throw e decoder in
  loop ()

let parse_json_list ~coma decoder =
  let theories = ref [] in
  let visit theory = theories := theory :: !theories in
  parse_json ~coma visit decoder;
  List.rev !theories

let read_proof_json ~coma source : theory list =
  let source = match source with
    | File file -> `Channel (open_in file)
    | String (_, contents) -> `String contents
  in
  let decoder = Jsonm.decoder source in
  parse_json_list ~coma decoder

(* Why3.Whyconf.load_plugins can only be called once.
   Hope that it doesn't depend on the loadpath which is going to vary. *)
let load_plugins =
  let once = ref true in
  fun env ->
    if !once then (
      once := false;
      Why3.Whyconf.load_plugins @@ Why3.Whyconf.get_main env.Config.wenv.config
    )

let load_config ?(root = ".") ?(warn = true) =
  let lazy_config = lazy (
  let config = Config.load_config root in
  if warn then (
    (match config.packages with
    | [] -> log Warning "No package found in config \"why3find.json\", at least prelude is needed for creusot proofs"
    | _ -> ());
    let check_warning w =
      if not (List.mem w config.warnoff) then
        log Warning "Disable warning \"%s\" in why3config.json, otherwise Why3 may produce lots of warnings on Creusot-generated code" w
    in
    check_warning "unused_variable";
    check_warning "axiom_abstract";
  );
  List.iter (fun unwarn ->
    Why3.Loc.disable_warning @@ Why3.Loc.register_warning unwarn Why3.Pp.empty_formatted)
    ("unused_variable" :: "axiom_abstract" :: config.warnoff);
  config) in
  fun () -> Lazy.force lazy_config

let get_env () =
  let config = load_config () in
  let env = Config.create_env ~config () in
  load_plugins env;
  env

(* Hack for the test suite. get_env must not have been called. *)
let get_test_env () =
  let root = "../testdata" in
  let config = load_config ~root ~warn:false () in
  let env = Config.create_env ~root ~config () in
  load_plugins env;
  env

let rec path_goal_ theory (e : Why3.Env.env) (g : Session.goal) (q : ProofPath.tactic_path) breadcrumbs : Session.goal option =
  let (let+) = Option.bind in
  match q with
  | [] -> Some g
  | (tactic, i) :: q ->
    let display_breadcrumbs () = Format.asprintf "%s:%a" theory ProofPath.pp_tactic_path (List.rev breadcrumbs) in
    let+ tactic' = Tactic.lookup e tactic in
    match Session.apply e tactic' g with
    | None -> log Error "%s: Could not apply tactic %s" (display_breadcrumbs ()) tactic; None
    | Some gs ->
      match List.nth_opt gs i with
      | None -> log Error "%s: Index %d out of bounds, tactic %s gave only %d goals" (display_breadcrumbs ()) i tactic (List.length gs); None
      | Some g -> path_goal_ theory e g q ((tactic, i) :: breadcrumbs)

let path_goal ~theory (e : Why3.Env.env) (g : Session.goal) (q : ProofPath.tactic_path) : Session.goal option =
  path_goal_ theory e g q []

let warn_if_none msg x = (match x with
  | None -> log Warning "%s" msg
  | Some _ -> ()); x

let for_goal (env : _) (q : qualified_goal) (f : Session.goal -> 'a option) : 'a option =
  try
    let session = true in
    let file = q.file in
    let (let+) = Option.bind in
    let theories, format = Wutil.load_theories env.Config.wenv.env file in
    let dir, _lib = Wutil.filepath file in
    let s = Why3find.Session.create ~session ~dir ~file ~format theories in
    let+ theory = List.find_opt (fun t -> Session.name t = q.theory) (Session.theories s) |> warn_if_none (Printf.sprintf "theory %s not found" q.theory) in
    let+ goal = List.find_opt (fun g -> Session.goal_name g = q.goal_info.vc) (Session.split theory) |> warn_if_none (Printf.sprintf "vc %s not found" q.goal_info.vc) in
    let+ goal = path_goal ~theory:(Session.name theory) env.wenv.env goal q.goal_info.tactics in
    f goal
  with e -> log Error "get_goal: Failed to load why3: %s" (Printexc.to_string e); None

let get_goal env q = for_goal env q (fun goal ->
  let task = Session.goal_task goal in
  Some (Format.asprintf "%a" Why3.Pretty.print_sequent task))

let loc_to_range loc =
  let _, l1, c1, l2, c2 = Why3.Loc.get loc in
  Lsp.Types.Range.create
    ~start:(Lsp.Types.Position.create ~line:(l1 - 1) ~character:c1)
    ~end_:(Lsp.Types.Position.create ~line:(l2 - 1) ~character:c2)

let goal_term_loc (g : Session.goal) =
  let open Why3 in
  (Task.task_goal_fmla (Session.goal_task g)).Term.t_loc

let get_goal_loc env q = for_goal env q goal_term_loc

(**)

type action_tree = { action: (Session.goal -> unit) option; tactic: (string * action_tree list) option }

type vc_tree = (string * action_tree) list
type theory_tree = (string * vc_tree) list

let rec walk_goal (env : Why3.Env.env) th_name vc_name tactic_path (t : action_tree) (g : Session.goal) : unit =
  Option.iter (fun action -> action g) t.action;
  match t.tactic with
  | None -> ()
  | Some (tactic, children) ->
    match Tactic.lookup env tactic with
    | None -> log Error "Could not find tactic %s" tactic
    | Some tactic' ->
      match Session.apply env tactic' g with
      | None -> log Error "Could not apply tactic %s" tactic
      | Some gs -> List.iter (fun (t, g) -> walk_goal env th_name vc_name tactic_path t g) (List.combine children gs)

let walk_theory (env : Why3.Env.env) (t : vc_tree) (th : Session.theory) : unit =
  let tbl = Hashtbl.create 10 in
  Session.split th |> List.iter (fun g -> Hashtbl.replace tbl (Session.goal_name g) g);
  t |> List.iter (fun (vc_name, t) ->
    match Hashtbl.find_opt tbl vc_name with
    | None -> log Error "Could not find vc %s in theory %s" vc_name (Session.name th)
    | Some g -> walk_goal env (Session.name th) vc_name [] t g)

let walk_session (env : Why3.Env.env) (t : theory_tree) (s : Session.session) : unit =
  let tbl = Hashtbl.create 10 in
  Session.theories s |> List.iter (fun th -> Hashtbl.replace tbl (Session.name th) th);
  t |> List.iter (fun (th_name, t) ->
    match Hashtbl.find_opt tbl th_name with
    | None -> log Error "Could not find theory %s" th_name
    | Some th -> walk_theory env t th)

let walk_file (env : _) (file : string) (t : theory_tree) : unit =
    try
    let session = true in
    let theories, format = Wutil.load_theories env.Config.wenv.env file in
    let dir, _lib = Wutil.filepath file in
    let s = Why3find.Session.create ~session ~dir ~file ~format theories in
    walk_session env.wenv.env t s
  with e -> log Error "walk_tree error: %s" (Printexc.to_string e)

(**)

module ProofInfo = struct
  type t = {
    coma_file: string;
    proof_file: string;
    rust_file: string;

    (* Typically, location of the function declaration *)
    entity_position: Range.t;

    unproved_located: Range.t list;
    unproved_unlocated: int;
  }
end

let get_src_regex = Str.regexp "(\\* #\"\\([^\"]*\\)\" \\([0-9]*\\) \\([0-9]*\\) \\([0-9]*\\) \\([0-9]*\\) \\*)"

let get_src (coma_file : string) : string * Range.t =
  let file = open_in coma_file in
  let first_line = input_line file in
  close_in file;
  if not (Str.string_match get_src_regex first_line 0) then failwith (Printf.sprintf "%s: bad header \"%s\"" coma_file first_line);
  let rust_file = Str.matched_group 1 first_line in
  let l1 = int_of_string (Str.matched_group 2 first_line) in
  let c1 = int_of_string (Str.matched_group 3 first_line) in
  let l2 = int_of_string (Str.matched_group 4 first_line) in
  let c2 = int_of_string (Str.matched_group 5 first_line) in
  (rust_file, Range.create ~start:(Position.create ~line:l1 ~character:c1) ~end_:(Position.create ~line:l2 ~character:c2))

(* needle must not be empty *)
let rec string_contains needle haystack =
  String.length needle <= String.length haystack &&
  (String.starts_with ~prefix:needle haystack || string_contains needle (String.sub haystack 1 (String.length haystack - 1)))

let get_proof_info (env : _) (proof_file : string) (coma_file : string) : ProofInfo.t =
  let rust_file, entity_position = get_src coma_file in
  let unproved_located = ref [] in
  let unproved_unlocated = ref 0 in
  let json = Yojson.Basic.from_file proof_file in
  let tree =
    let open Yojson.Basic.Util in
    let json = json |> member "proofs" in
    to_assoc json |> List.map (fun (theory, json) ->
      let vc_tree =
        to_assoc json |> List.map (fun (vc, json) ->
          let goal_tree = match json with
            | `Null -> { action = None; tactic = None } (* TODO: all subgoals failed; do a split_vc to inspect subgoals *)
            | `Assoc _ ->
              (match member "tactic" json with
              | `Null -> { action = None; tactic = None } (* completed goal *)
              | `String tactic ->
                let children =
                  match member "children" json with
                  | `List children -> children |> List.map (fun json ->
                      let goal_type : [`Unknown | `Known] ref = ref `Unknown in
                      let check_goal_type g =
                        let expl = Session.goal_expl g in
                        if String.starts_with ~prefix:"loop invariant" expl ||
                          string_contains "ensures" expl then
                          goal_type := `Known
                      in
                      let remember_unproved g =
                        match !goal_type, goal_term_loc g with
                        | `Known, Some loc -> unproved_located := loc_to_range loc :: !unproved_located
                        | `Unknown, _ | _, None -> incr unproved_unlocated
                      in
                      let rec subgoals json =
                        match json with
                        | `Null -> { action = Some remember_unproved; tactic = None }
                        | _ ->
                          match member "tactic" json with
                          | `Null -> { action = None; tactic = None } (* completed goal *)
                          | `String tactic ->
                            let children = match member "children" json with
                              | `List children -> List.map subgoals children
                              | _ -> failwith "bad \"children\" field (must be a list)" in
                            { action = None; tactic = Some (tactic, children) }
                          | _ -> failwith "bad \"tactic\" field (must be a string)"
                      in
                      let insert_action action tree = { tree with action = Some (fun g -> action g; Option.iter (fun f -> f g) tree.action) } in
                      insert_action check_goal_type (subgoals json)
                    )
                  | _ -> failwith "bad \"children\" field (must be a list)"
                in
                { action = None; tactic = Some (tactic, children) }
              | _ -> failwith "bad \"tactic\" field (must be a string)"
              )
            | _ -> failwith "bad proof.json (must be an object or null)"
          in
          (vc, goal_tree))
      in
      (theory, vc_tree))
  in
  walk_file env coma_file tree;
  let unproved_located = !unproved_located in
  let unproved_unlocated = !unproved_unlocated in
  ProofInfo.{ coma_file; proof_file; rust_file; entity_position; unproved_located; unproved_unlocated }

let proof_info : (string, ProofInfo.t) Hashtbl.t = Hashtbl.create 10

(* Mapping from rust file names to relevant proof.json file names *)
let proof_info_deps : (string, string list) Hashtbl.t = Hashtbl.create 10

let create_proof_info (env : Config.env) ~proof_file ~coma_file =
  match get_proof_info env proof_file coma_file with
  | info ->
      Hashtbl.replace proof_info coma_file info;
      Hashtbl.replace proof_info_deps info.rust_file
        (match Hashtbl.find_opt proof_info_deps info.rust_file with
        | None -> [proof_file]
        | Some files -> if List.mem proof_file files then files else proof_file :: files)
  | exception e -> log Error "create_proof_info: %s" (Printexc.to_string e)

let diagnostics_of_info info : Diagnostic.t list =
  let open Lsp.Types in
  let open ProofInfo in
  let unproved_located = info.unproved_located |> List.map (fun range ->
    Diagnostic.create ~range ~severity:DiagnosticSeverity.Error
      ~message:"Unproved subgoal" ()) in
  let unproved_unlocated =
    if info.unproved_unlocated = 0 then []
    else
      [Diagnostic.create ~range:info.entity_position
        ~severity:DiagnosticSeverity.Error ~message:(Printf.sprintf "%d unproved subgoal%s" info.unproved_unlocated (if info.unproved_unlocated > 1 then "s" else "")) ()] in
  unproved_unlocated @ unproved_located

let get_diagnostics ~rust_file : Diagnostic.t list =
  match Hashtbl.find_opt proof_info_deps rust_file with
  | None -> []
  | Some proof_files -> proof_files |> List.concat_map (fun proof_file ->
    match Hashtbl.find_opt proof_info proof_file with
    | None -> []
    | Some info -> diagnostics_of_info info)