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
  type lazy_tactic_path = (string option ref * int) list  (* tactic names might not have been resolved yet *)
  type tactic_path = (string * int) list  (* tactic * goalindex *)
  type 'a t0 = {
    file: string;
    theory: string;
    vc: string;
    tactics: 'a;
    }
  type t = tactic_path t0
  type lazy_t = lazy_tactic_path t0

  let force_tactic_path = List.map @@ fun (tac, i) -> (match !tac with None -> "_" | Some tac -> tac), i

  let rec pp_tactic_path h = function
    | [] -> ()
    | (tac, i) :: t -> Format.fprintf h ".%s.%d" tac i; pp_tactic_path h t

  let pp_ pp_tactic h t =
    Format.fprintf h "%s:%s:%s%a" t.file t.theory t.vc pp_tactic t.tactics

  let pp_lazy = pp_ (fun h t -> pp_tactic_path h (force_tactic_path t))
  let pp = pp_ pp_tactic_path

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

let rec parse_proofs_body ~file visit th_name vc_name decoder tactic_path =
  let tactic = ref None in
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name "tactic") ->
      let name = eat_string decoder in
      tactic := Some name;
      loop ()
    | `Lexeme (`Name "children") ->
      eat_As decoder;
      let rec loop_children i = match parse_proofs ~file visit th_name vc_name decoder ((tactic, i) :: tactic_path) with
        | TrailingAe -> ()
        | NoAe -> loop_children (i + 1)
        in loop_children 0
    | `Lexeme (`Name ("prover" | "time" | "size")) ->
      (* eat strings and numbers *)
      (match Jsonm.decode decoder with
      | `Lexeme (`String _ | `Float _) -> loop ()
      | e -> throw e decoder)
    | `Lexeme `Oe -> ()
    | e -> throw e decoder
  in loop ()

and parse_proofs ~file visit th_name vc_name decoder tactic_path =
  match Jsonm.decode decoder with
  | `Lexeme `Os -> parse_proofs_body ~file visit th_name vc_name decoder tactic_path; NoAe
  | `Lexeme `Null -> visit ProofPath.{ file; theory = th_name; vc = vc_name; tactics = tactic_path }; NoAe
  | `Lexeme `Ae -> TrailingAe
  | e -> throw e decoder

let parse_theory ~file visit th_name decoder =
  eat_Os decoder;
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name vc_name) ->
      (match parse_proofs ~file visit th_name vc_name decoder [] with
      | TrailingAe -> throw (`Lexeme `Ae) decoder
      | NoAe -> loop ())
    | `Lexeme `Oe -> ()
    | e -> throw e decoder
    in
  loop ()

let parse_theories ~file visit decoder =
  eat_Os decoder;
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name th_name) ->
      parse_theory ~file visit th_name decoder;
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
        | `Lexeme (`Name _) -> skip_json decoder
        | e -> throw e decoder
      in skip_members (); None
    | `Lexeme (`Null | `String _ | `Float _ | `Bool _) -> None
    | `Lexeme `Ae -> Some `Ae
    | e -> throw e decoder

and skip_json decoder =
  match skip_json_ decoder with
  | None -> ()
  | Some e -> throw (`Lexeme e) decoder

let parse_json ~file visit decoder =
  eat_Os decoder;
  let rec loop () = match Jsonm.decode decoder with
    | `Lexeme (`Name "proofs") -> parse_theories ~file visit decoder
    | `Lexeme (`Name _) -> skip_json decoder; loop ()
    | `Lexeme `Oe -> () (* no proofs found *)
    | e -> throw e decoder in
  loop ()

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
