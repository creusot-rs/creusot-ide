open Why3find
open Lsp.Types
open Types

let has_key key json =
  let open Yojson.Safe.Util in
  match member key json with
  | `Null -> false
  | _ -> true
  | exception Type_error(_, _) -> false

let rec find_unproved_goals_in jpath json =
  let open Yojson.Safe.Util in
  match json with
  | `Null -> [{ goal_name = String.concat "." (List.rev jpath) }]
  | `Assoc _ ->
    if has_key "prover" json then []
    else member "children" json |> to_list
      |> List.mapi (fun i child ->
        find_unproved_goals_in (string_of_int i :: jpath) child)
      |> List.concat
  | _ -> []

let find_unproved_goals json =
  let open Yojson.Safe.Util in
  to_assoc json |> List.concat_map @@ fun (name, gjson) ->
    find_unproved_goals_in [name] gjson

let from_proof_json (fpath: string) json =
  let open Yojson.Safe.Util in
  member "proofs" json
    |> to_assoc
    |> List.map @@ fun (name, thjson) ->
      let unproved_goals = Unproved (Array.of_list (find_unproved_goals thjson)) in
      (name, { path = fpath; name; unproved_goals })

let parse_proof_json ~fname contents =
  match Yojson.Safe.from_string ~fname contents with
  | json -> from_proof_json fname json
  | exception Yojson.Json_error e ->
    Debug.debug ("Could not parse JSON " ^ fname ^ ": " ^ e);
    []

let read_proof_json ~fname =
  match Yojson.Safe.from_file fname with
  | json -> from_proof_json fname json
  | exception Yojson.Json_error e ->
    Debug.debug ("Could not parse JSON " ^ fname ^ ": " ^ e);
    []

module ProofPath = struct
  type tactic_path = (string * int) list  (* tactic * goalindex *)
  type t = {
    file: string;
    theory: string;
    vc: string;
    tactics: tactic_path;
    }

  let tactic_path_to_json (p : tactic_path) : Yojson.Safe.t =
    `List (List.map (fun (tactic, i) -> `List [`String tactic; `Int i]) p)

  let to_json (p : t) : Yojson.Safe.t =
    `Assoc [
      "file", `String p.file;
      "theory", `String p.theory;
      "vc", `String p.vc;
      "path", tactic_path_to_json p.tactics;
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

  let of_json (j : Yojson.Safe.t) : t option =
    let (let+) = Option.bind in
    let open Yojson.Safe.Util in
    match j with
    | `Assoc l -> (
      let+ file = to_string_option @@ member "file" j in
      let+ theory = to_string_option @@ member "theory" j in
      let+ vc = to_string_option @@ member "vc" j in
      let+ tactics = member "path" j |> tactic_path_of_json in
      Some { file; theory; vc; tactics }
    )
    | _ -> None
end

let get_env () =
  let config = Config.load_config "." in
  (match config.packages with
  | [] -> Printf.eprintf "no package found in config \"why3find.json\", at least prelude is needed for creusot proofs\n"
  | _ -> ());
  let env = Config.create_env ~config () in
  Why3.Whyconf.load_plugins @@ Why3.Whyconf.get_main env.wconfig;
  Why3.Loc.disable_warning @@ Why3.Loc.register_warning "axiom_abstract" Why3.Pp.empty_formatted;
  env

let rec path_goal (e : Why3.Env.env) (g : Session.goal) (q : ProofPath.tactic_path) : Session.goal option =
  match q with
  | [] -> Some g
  | (tactic, i) :: q ->
    let (let+) = Option.bind in
    let+ gs = Session.apply e tactic g in
    let+ g = List.nth_opt gs i in
    path_goal e g q

let get_goal (q : ProofPath.t) : string option =
  try
    let session = true in
    let env = get_env () in
    let file = q.file in
    let dir, lib = Wutil.filepath file in
    let theories, format = Wutil.load_theories env.Config.wenv file in
    let s = Why3find.Session.create ~session ~dir ~file ~format theories in
    let (let+) = Option.bind in
    let+ theory = List.find_opt (fun t -> Session.name t = q.theory) (Session.theories s) in
    let+ goal = List.find_opt (fun g -> Session.goal_name g = q.vc) (Session.split theory) in
    let+ goal = path_goal env.wenv goal q.tactics in
    let task = Session.goal_task goal in
    Some (Format.asprintf "%a" Why3.Pretty.print_sequent task)
  with e -> Debug.debug ("Failed to load why3: " ^ Printexc.to_string e); None
