open Why3find
open Lsp.Types
open Util

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
  type theory = (goal * Range.t) list with_theory
  type full_goal = goal with_theory

  let force_tactic_path : lazy_tactic_path -> tactic_path =
    List.map @@ fun (tac, i) -> (match !tac with None -> "_" | Some tac -> tac), i

  let pp_tactic_path h =
    Format.pp_print_list ~pp_sep:Format.pp_print_nothing
      (fun h (tac, i) -> Format.fprintf h ".%s.%d" tac i) h

  let pp_goal_ pp_tactic h t =
    Format.fprintf h "%s%a" t.vc pp_tactic t.tactics

  let pp_lazy_goal = pp_goal_ (fun h t -> pp_tactic_path h (force_tactic_path t))
  let pp_goal = pp_goal_ pp_tactic_path

  let pp_goals h = Format.pp_print_list (fun h (goal, _pos) -> pp_goal h goal) h

  let pp_theory h t = Format.fprintf h "%s:%s:@[%a@]" t.file t.theory pp_goals t.goal_info

  let pp_full_goal h t = Format.fprintf h "%s:%s:%a" t.file t.theory pp_goal t.goal_info

  let string_of_goal = Format.asprintf "%a" pp_goal

  let string_of_full_goal = Format.asprintf "%a" pp_full_goal

  let tactic_path_to_json (p : tactic_path) : Yojson.Safe.t =
    `List (List.map (fun (tactic, i) -> `List [`String tactic; `Int i]) p)

  let full_goal_to_json (p : full_goal) : Yojson.Safe.t =
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

  let full_goal_of_json (j : Yojson.Safe.t) : full_goal option =
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

let rec parse_proofs_body ~coma visit decoder tactic_path =
  let tactic = ref None in
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name "tactic") ->
      let name = eat_string decoder in
      tactic := Some name;
      loop ()
    | `Lexeme (`Name "children") ->
      eat_As decoder;
      let rec loop_children i = match parse_proofs ~coma visit decoder ((tactic, i) :: tactic_path) with
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

and parse_proofs ~coma visit decoder tactic_path =
  match Jsonm.decode decoder with
  | `Lexeme `Os -> parse_proofs_body ~coma visit decoder tactic_path; NoAe
  | `Lexeme `Null ->
    let range = from_jsonm_range (Jsonm.decoded_range decoder) in
    let tactic_path = List.rev tactic_path in
    visit range tactic_path; NoAe
  | `Lexeme `Ae -> TrailingAe
  | e -> throw e decoder

let parse_theory ~coma visit decoder =
  eat_Os decoder;
  let goals = ref [] in
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name vc) ->
      let visit_goal = fun range tactics -> goals := ({ vc; tactics }, range) :: !goals in
      (match parse_proofs ~coma visit_goal decoder [] with
      | TrailingAe -> throw (`Lexeme `Ae) decoder
      | NoAe -> loop ())
    | `Lexeme `Oe -> ()
    | e -> throw e decoder
  in loop ();
  (* After traversing the vc the tactic names should be known *)
  let force_goal ({ vc; tactics }, range) =
    { vc; tactics = force_tactic_path tactics }, range in
  visit (List.map force_goal !goals)

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
  !theories

let read_proof_json ~coma source : theory list =
  let source = match source with
    | File file -> `Channel (open_in file)
    | String (_, contents) -> `String contents
  in
  let decoder = Jsonm.decoder source in
  parse_json_list ~coma decoder

let env_ = lazy (
  let config = Config.load_config "." in
  (match config.packages with
  | [] -> Printf.eprintf "Warning: no package found in config \"why3find.json\", at least prelude is needed for creusot proofs\n"
  | _ -> ());
  let env = Config.create_env ~config () in
  Why3.Whyconf.load_plugins @@ Why3.Whyconf.get_main env.wconfig;
  List.iter (fun unwarn ->
    Why3.Loc.disable_warning @@ Why3.Loc.register_warning unwarn Why3.Pp.empty_formatted)
    ("unused_variable" :: "axiom_abstract" :: config.warnoff);
  env)

let get_env () = Lazy.force env_

let rec path_goal_ theory (e : Why3.Env.env) (g : Session.goal) (q : ProofPath.tactic_path) breadcrumbs : Session.goal option =
  match q with
  | [] -> Some g
  | (tactic, i) :: q ->
    let display_breadcrumbs () = Format.asprintf "%s:%a" theory ProofPath.pp_tactic_path (List.rev breadcrumbs) in
    match Session.apply e tactic g with
    | None -> Printf.eprintf "%s: Could not apply tactic %s%!" (display_breadcrumbs ()) tactic; None
    | Some gs ->
      match List.nth_opt gs i with
      | None -> Printf.eprintf "%s: Index %d out of bounds, tactic %s gave only %d goals%!" (display_breadcrumbs ()) i tactic (List.length gs); None
      | Some g -> path_goal_ theory e g q ((tactic, i) :: breadcrumbs)

let path_goal ~theory (e : Why3.Env.env) (g : Session.goal) (q : ProofPath.tactic_path) : Session.goal option =
  path_goal_ theory e g q []

module Wutil = struct
include Wutil
open Why3
    (* modified from why3find without exit *)
    let load_theories (env : Why3.Env.env) file =
      let byloc a b =
        match a.Theory.th_name.id_loc , b.Theory.th_name.id_loc with
        | None,None -> 0
        | Some _,None -> (-1)
        | None,Some _ -> (+1)
        | Some la, Some lb -> Why3.Loc.compare la lb
      in
      try
        let tmap,format = Why3.Env.(read_file base_language env file) in
        Some (Wstdlib.Mstr.bindings tmap |> List.map snd |> List.sort byloc , format)
      with error ->
        Printf.eprintf "%s\n%!" (Printexc.to_string error) ;
        None
      end

let get_goal (q : full_goal) : string option =
  try
    let session = true in
    let env = get_env () in
    let file = q.file in
    let dir, _lib = Wutil.filepath file in
    let (let+) = Option.bind in
    let+ theories, format = Wutil.load_theories env.Config.wenv file in
    let s = Why3find.Session.create ~session ~dir ~file ~format theories in
    let+ theory = List.find_opt (fun t -> Session.name t = q.theory) (Session.theories s) in
    let+ goal = List.find_opt (fun g -> Session.goal_name g = q.goal_info.vc) (Session.split theory) in
    let+ goal = path_goal ~theory:(Session.name theory) env.wenv goal q.goal_info.tactics in
    let task = Session.goal_task goal in
    Some (Format.asprintf "%a" Why3.Pretty.print_sequent task)
  with e -> Debug.debug ("Failed to load why3: " ^ Printexc.to_string e); None
