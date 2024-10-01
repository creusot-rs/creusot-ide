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
  from_proof_json fname (Yojson.Safe.from_string ~fname contents)

let read_proof_json ~fname =
  from_proof_json fname (Yojson.Safe.from_file fname)
