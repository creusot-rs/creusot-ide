open Why3find
open Linol.Lsp.Types
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
      Why3.Whyconf.load_plugins @@ Why3.Whyconf.get_main env.Why3find.Project.why3.config
    )

let load_config ?(root = ".") ?(warn = true) =
  let lazy_config = lazy (
  let config = Config.load_config root in
  if warn then (
    (match config.packages with
    | [] -> log Warning "No package found in config \"why3find.json\", at least \"creusot\" is needed"
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
  let env = Why3find.Project.create ~config () in
  load_plugins env;
  env

(* Hack for the test suite. get_env must not have been called. *)
let get_test_env () =
  let root = "../testdata" in
  let config = load_config ~root ~warn:false () in
  let env = Why3find.Project.create ~root ~config () in
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

let for_goal (env : Why3find.Project.env) (q : qualified_goal) (f : Session.goal -> 'a option) : 'a option =
  try
    let file = q.file in
    let (let+) = Option.bind in
    let theories, format = Wutil.load_theories env.why3.env file in
    let dir, _lib = Wutil.filepath file in
    let s = Why3find.Session.create ~dir ~file ~format theories in
    let+ theory = List.find_opt (fun t -> Session.name t = q.theory) (Session.theories s) |> warn_if_none (Printf.sprintf "theory %s not found" q.theory) in
    let+ goal = List.find_opt (fun g -> Session.goal_name g = q.goal_info.vc) (Session.split theory) |> warn_if_none (Printf.sprintf "vc %s not found" q.goal_info.vc) in
    let+ goal = path_goal ~theory:(Session.name theory) env.why3.env goal q.goal_info.tactics in
    f goal
  with e -> log Error "get_goal: Failed to load why3: %s" (Printexc.to_string e); None

let get_goal env q = for_goal env q (fun goal ->
  let task = Session.goal_task goal in
  Some (Format.asprintf "%a" Why3.Pretty.print_sequent task))

let rawloc_to_range (_, l1, c1, l2, c2) =
  Linol.Lsp.Types.Range.create
    ~start:(Linol.Lsp.Types.Position.create ~line:(l1 - 1) ~character:c1)
    ~end_:(Linol.Lsp.Types.Position.create ~line:(l2 - 1) ~character:c2)

let loc_to_range loc = rawloc_to_range (Why3.Loc.get loc)

let goal_term_loc (g : Session.goal) =
  let open Why3 in
  Term.t_loc (Task.task_goal_fmla (Session.goal_task g))

let get_goal_loc env q = for_goal env q goal_term_loc

(**)

module Why3Session = struct
  type goal = { goal : Session.goal; children : string -> goal list }
  type theory = { theory : Session.theory; theory_children : unit -> goal list }
end

exception Bad_tactic

let rec get_goal_ (env : Why3.Env.env) (goal : Session.goal) : Why3Session.goal =
  let open Why3Session in
  { goal; children = fun tactic ->
    match Tactic.lookup env tactic with
    | None -> raise Not_found
    | Some tactic' ->
      match Session.apply env tactic' goal with
      | None -> raise Bad_tactic
      | Some goals -> List.map (get_goal_ env) goals
  }

let get_session (env : Why3find.Project.env) (coma_file : string) : Why3Session.theory list =
  let open Why3Session in
  let theories, format = Wutil.load_theories env.why3.env coma_file in
  let dir, _lib = Wutil.filepath coma_file in
  let s = Why3find.Session.create ~dir ~file:coma_file ~format theories in
  Session.theories s |> List.map (fun theory -> { theory; theory_children = fun () ->
    Session.split theory |> List.map (fun g -> get_goal_ env.why3.env g) })

(**)

module ProofInfo = struct
  type goal = {
    range: Range.t option;
    expl: string;
    unproved_subgoals: ProofPath.qualified_goal list;
  }
  type t = {
    coma_file: string;
    proof_file: string;
    rust_file: string;

    coma_file_hash: string;
    proof_file_hash: string;

    (* Typically, location of the function declaration *)
    entity_range: Range.t;

    unproved_goals: goal list;
  }

  let pp_info h info =
    Format.fprintf h "%s: %d goals" info.coma_file (List.length info.unproved_goals)

  let append_opt field f x acc = match x with
    | None -> acc
    | Some y -> (field, f y) :: acc

  let to_json info =
    `Assoc [
      "coma_file", `String info.coma_file;
      "proof_file", `String info.proof_file;
      "rust_file", `String info.rust_file;
      "coma_file_hash", `String info.coma_file_hash;
      "proof_file_hash", `String info.proof_file_hash;
      "entity_range", Range.yojson_of_t info.entity_range;
      "unproved_goals", `List (List.map (fun goal ->
        `Assoc ([
          "expl", `String goal.expl;
          "unproved_subgoals", `List (List.map ProofPath.qualified_goal_to_json goal.unproved_subgoals);
        ] |> append_opt "range" Range.yojson_of_t goal.range)) info.unproved_goals);
    ]

  let of_json json =
    let open Yojson.Safe.Util in
    let coma_file = json |> member "coma_file" |> to_string in
    let proof_file = json |> member "proof_file" |> to_string in
    let coma_file_hash = json |> member "coma_file_hash" |> to_string in
    let proof_file_hash = json |> member "proof_file_hash" |> to_string in
    let rust_file = json |> member "rust_file" |> to_string in
    let entity_range = json |> member "entity_range" |> Range.t_of_yojson in
    let unproved_goals = json |> member "unproved_goals" |> to_list |> List.map (fun goal ->
      let expl = goal |> member "expl" |> to_string in
      let unproved_subgoals = goal |> member "unproved_subgoals" |> to_list |> List.filter_map ProofPath.qualified_goal_of_json in
      let range = try Some (goal |> member "range" |> Range.t_of_yojson) with _ -> None in
      { range; expl; unproved_subgoals }) in
    { coma_file; coma_file_hash; proof_file; proof_file_hash; rust_file; entity_range; unproved_goals }

  let to_file file info =
    try
      let json = to_json info in
      let oc = open_out file in
      Yojson.Safe.pretty_to_channel oc json;
      close_out oc
    with e -> log Error "to_file: Failed to write proof info to %s: %s" file (Printexc.to_string e)

  let of_file file = of_json (Yojson.Safe.from_file file)

  let up_to_date info = Digest.BLAKE256.(file info.coma_file = of_hex info.coma_file_hash &&
    file info.proof_file = of_hex info.proof_file_hash)
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
  let range = rawloc_to_range (rust_file, l1, c1, l2, c2) in
  (rust_file, range)

type 'a hashset = ('a, unit) Hashtbl.t
(* Mapping from rust file names to relevant proof.json file names *)
let proof_info_deps : (string, string hashset) Hashtbl.t = Hashtbl.create 10

let get_deps rust_file =
  match Hashtbl.find_opt proof_info_deps rust_file with
  | None -> let files = Hashtbl.create 10 in Hashtbl.replace proof_info_deps rust_file files; files
  | Some files -> files

let add_coma_dep ~coma_file ~rust_file =
  let deps = get_deps rust_file in
  Hashtbl.replace deps coma_file ()

let add_coma2 coma_file : unit =
  try
    let rust_file, _ = get_src coma_file in
    add_coma_dep ~coma_file ~rust_file
  with e -> log Error "Bad match %s: %s" coma_file (Printexc.to_string e); ()

let get_rust_source ~coma_file =
  try
    let (rust_file, _) = get_src coma_file in
    Some rust_file
  with _ -> None

(* needle must not be empty *)
let rec string_contains needle haystack =
  String.length needle <= String.length haystack &&
  (String.starts_with ~prefix:needle haystack || string_contains needle (String.sub haystack 1 (String.length haystack - 1)))

let find_remove (f : 'a -> bool) (l : 'a list) : ('a * 'a list) option =
  let rec loop acc = function
    | [] -> raise Not_found
    | x :: xs -> if f x then Some (x, List.rev_append acc xs) else loop (x :: acc) xs
  in try loop [] l with
  | Not_found -> None

let known_goal_type expl =
  String.starts_with ~prefix:"loop invariant" expl ||
  String.starts_with ~prefix:"for invariant" expl ||
  "assertion" = expl ||
  string_contains "ensures" expl

let get_proof_info (env : _) ~proof_file ~coma_file : ProofInfo.t =
  let rust_file, entity_range = get_src coma_file in
  let collect_goals theories =
    let goals = ref [] in
    begin match Yojson.Basic.from_file proof_file with
    | exception Sys_error _ ->
      theories |> List.iter (fun th ->
        th.Why3Session.theory_children () |> List.iter (fun g ->
          g.Why3Session.children "split_vc" |> List.iteri (fun i g ->
            let subgoal = ProofPath.{
              file = coma_file;
              theory = Session.name th.Why3Session.theory;
              goal_info = { vc = Session.goal_name g.Why3Session.goal; tactics = [("split_vc", i)] } } in
            let expl = Session.goal_expl g.Why3Session.goal in
            let range = if known_goal_type expl then Option.map loc_to_range (goal_term_loc g.Why3Session.goal) else None in
            goals := ProofInfo.{ range ; expl ; unproved_subgoals = [subgoal] } :: !goals
          );
        )
      )
    | json ->
      let open Yojson.Basic.Util in
      let json = json |> member "proofs" in
      let theories_left = ref theories in
      to_assoc json |> List.iter (fun (theory, json) ->
        match find_remove (fun th -> Session.name th.Why3Session.theory = theory) !theories_left with
        | None -> failwith (Printf.sprintf "theory %s not found" theory)
        | Some (th, theories') ->
          theories_left := theories';
          let goals_left = ref (th.Why3Session.theory_children ()) in
          to_assoc json |> List.iter (fun (vc, json) ->
            match find_remove (fun g -> Session.goal_name g.Why3Session.goal = vc) !goals_left with
            | None -> failwith (Printf.sprintf "vc %s.%s not found" theory vc)
            | Some (g, goals') ->
              goals_left := goals';
              let tactic, children =
                match json with
                | `Null -> let split_vc = "split_vc" in
                  split_vc, List.map (fun child -> (child, `Null)) (g.Why3Session.children split_vc)
                | `Assoc _ ->
                  (match member "tactic" json with
                  | `Null -> "", [] (* completed goal *)
                  | `String tactic ->
                    let children' = match member "children" json with
                      | `List children' -> children'
                      | _ -> failwith "Bad \"children\" field" in
                    (* Generate empty proofs if there are more Why3 subgoals than proofs from the proof.json
                       (we can't use List.combine because it throws an exception when lists don't have the same length) *)
                    let rec combine goals proofs = match goals, proofs with
                      | [], _ -> []
                      | goals, [] -> List.map (fun g -> g, `Null) goals
                      | goal :: goals, proof :: proofs -> (goal, proof) :: combine goals proofs in
                    tactic, combine (g.Why3Session.children tactic) children'
                  | _ -> failwith "Bad \"tactic\" field")
                | _ -> failwith "Bad goal object"
              in
              children |> List.iteri (fun i (child, json) ->
                let subgoals = ref [] in
                let rec collect_subgoals tactics json =
                  match json with
                  | `Null ->
                    let subgoal = ProofPath.{ file = coma_file; theory; goal_info = { vc; tactics } } in (*  *)
                    subgoals := subgoal :: !subgoals
                  | _ ->
                    match member "tactic" json with
                    | `Null -> () (* completed goal *)
                    | `String tactic ->
                      (match member "children" json with
                      | `List children -> List.iteri (fun i -> collect_subgoals ((tactic, i) :: tactics)) children
                      | _ -> failwith "bad \"children\" field (must be a list)")
                    | _ -> failwith "bad \"tactic\" field (must be a string)"
                in
                collect_subgoals [(tactic, i)] json;
                if not (List.is_empty !subgoals) then (
                  let expl = Session.goal_expl child.Why3Session.goal in
                  let range = if known_goal_type expl then Option.map loc_to_range (goal_term_loc child.Why3Session.goal) else None in
                  goals := ProofInfo.{ range ; expl ; unproved_subgoals = List.rev !subgoals } :: !goals
                ))
              ))
    end;
    List.rev !goals
  in
  let unproved_goals = collect_goals (get_session env coma_file) in
  let coma_file_hash = Digest.BLAKE256.(file coma_file |> to_hex) in
  let proof_file_hash = Digest.BLAKE256.(file proof_file |> to_hex) in
  ProofInfo.{ coma_file; coma_file_hash; proof_file; proof_file_hash; rust_file; entity_range; unproved_goals }

let proof_info : (string, ProofInfo.t) Hashtbl.t = Hashtbl.create 10

let register_proof_info ~coma_file = function
  | Some info ->
    Hashtbl.replace proof_info coma_file info;
    add_coma_dep ~coma_file ~rust_file:info.rust_file
  | None ->
    Hashtbl.remove proof_info coma_file

let create_proof_info (env : Why3find.Project.env) ~proof_file ~coma_file =
  log Debug "create_proof_info %s" coma_file;
  register_proof_info ~coma_file
    (match get_proof_info env ~proof_file ~coma_file with
    | info -> Some info
    | exception _ -> None)

(* Hack: the ":why3" suffix is a custom file type to enable syntax highlighting in VSCode
   (ideally we would be able to tell VSCode to set a language for all virtual documents in the
   "why3:" scheme) *)
let encode_subgoal subgoal = Format.asprintf "why3:%s/%s/%a:why3" subgoal.file subgoal.theory ProofPath.pp_goal subgoal.goal_info

let decode_subgoal str =
  try
    let str = Util.drop_suffix ~suffix:"%3Awhy3" str in
    let str = Util.drop_prefix ~prefix:"why3:" str in
    let str = Uri.pct_decode str in
    let str, subgoal = split_last '/' str in
    let file, theory = split_last '/' str in
    match split_first '.' subgoal with
    | exception Not_found ->
      ProofPath.{ file; theory; goal_info = { vc = subgoal; tactics = [] } }
    | vc, tactics ->
      let tactics =
        let[@ocaml.tail_mod_cons] rec loop acc str =
          let tactic, str = split_first '.' str in
          match split_first '.' str with
          | exception Not_found -> List.rev ((tactic, int_of_string str) :: acc)
          | index, str -> loop ((tactic, int_of_string index) :: acc) str
        in loop [] tactics in
      ProofPath.{ file; theory; goal_info = { vc; tactics } }
  with _ -> failwith "decode_subgoal: expected syntax .*/[^/]*/[^/]*(\\.[^/]*\\.[0-9]*)*"

let dummy_range =
  let zero = Position.create ~line:0 ~character:0 in
  let one = Position.create ~line:0 ~character:1 in
  Range.create ~start:zero ~end_:one

let diagnostics_of_info info : Diagnostic.t list =
  let open ProofInfo in
  let mk_diagnostic goal =
    let range = Option.value ~default:info.entity_range goal.range in
    let expl = if goal.expl = "" then "(no description)" else goal.expl in
    let message = `String (Printf.sprintf "Unproved goal: %s" expl) in
    let relatedInformation = goal.unproved_subgoals |> List.map (fun subgoal ->
      DiagnosticRelatedInformation.create ~message:"Show task" ~location:(
        Location.create ~uri:(DocumentUri.t_of_yojson (`String (encode_subgoal subgoal))) ~range:dummy_range
      )) in
    Diagnostic.create ~source:"Creusot" ~range ~severity:DiagnosticSeverity.Error ~message ~relatedInformation ()
  in
  List.map mk_diagnostic info.unproved_goals

let code_lenses_of_info info : CodeLens.t list =
  let open ProofInfo in
  let range = info.entity_range in
  let show_tasks =
    if List.is_empty info.unproved_goals then
      Command.create ~title:"QED" ~command:"" ()
    else
      let title = Printf.sprintf "%d unproved goals" (List.length info.unproved_goals) in
      let locations = info.unproved_goals |> List.concat_map (fun goal ->
        goal.unproved_subgoals |> List.map (fun subgoal ->
          Location.create ~uri:(DocumentUri.t_of_yojson (`String (encode_subgoal subgoal))) ~range:dummy_range
            |> Location.yojson_of_t
        )) in
      Command.create ~title ~command:"creusot.peekLocations"
        ~arguments:[
          info.rust_file |> DocumentUri.of_path |> DocumentUri.yojson_of_t;
          Position.(yojson_of_t range.Range.start);
          `List locations]
        ()
  in
  let zero = Position.create ~line:0 ~character:0 in
  let zero_range = Range.create ~start:zero ~end_:zero in
  let coma_location = Location.create ~uri:(DocumentUri.of_path info.coma_file) ~range:zero_range in
  let goto_coma = Command.create
      ~title:"Inspect output Coma"
      ~command:"creusot.peekLocations"
      ~arguments:[
        DocumentUri.(yojson_of_t (DocumentUri.of_path info.rust_file));
        Position.(yojson_of_t range.start);
        `List [Location.yojson_of_t coma_location];
        `String "gotoAndPeek"]
      ()
  in
  [ CodeLens.create ~range ~command:show_tasks ();
    CodeLens.create ~range ~command:goto_coma ();
  ]

let test_item_of_info info =
  let open ProofInfo in
  let id = info.coma_file in
  let label = drop_prefix ~prefix:"M_" Filename.(chop_suffix (basename info.coma_file) ".coma") in
  let range = info.entity_range in
  Test_api.{ id; label; range }

let concat_map_info ~rust_file (f : ProofInfo.t -> 'a list) : 'a list =
  match Hashtbl.find_opt proof_info_deps rust_file with
  | None -> log Debug "No proofs found for %s" rust_file; []
  | Some proof_files ->
    let result = ref [] in
    proof_files |> Hashtbl.iter (fun proof_file () ->
      match Hashtbl.find_opt proof_info proof_file with
        (* Skip if the dependency is outdated *)
      | Some info when info.rust_file = rust_file -> result := f info :: !result
      | Some info -> log Debug "Outdated dependency %s for %s" info.coma_file rust_file
      | None -> ());
    List.(concat (rev !result))

let get_diagnostics ~rust_file : Diagnostic.t list =
  concat_map_info ~rust_file diagnostics_of_info

let get_lenses ~rust_file : CodeLens.t list =
  concat_map_info ~rust_file code_lenses_of_info

let get_test_items ~rust_file : Test_api.test_item list =
  concat_map_info ~rust_file (fun info -> [test_item_of_info info])

let refresh_info ~get_proof_info ~rust_file : unit =
  match Hashtbl.find_opt proof_info_deps rust_file with
  | None -> ()
  | Some coma_files ->
    let bad_coma_files = ref [] in
    coma_files |> Hashtbl.iter (fun coma_file () ->
      let (/) = Filename.concat in
      let proof_file = Filename.chop_extension coma_file / "proof.json" in
      if Sys.file_exists coma_file then
        register_proof_info ~coma_file (get_proof_info ~proof_file)
      else
        bad_coma_files := coma_file :: !bad_coma_files);
    List.iter (Hashtbl.remove coma_files) !bad_coma_files
