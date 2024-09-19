open Types

let theories : theories_map = Hashtbl.create 32

type tree
  = [
    `Skip
  | `Proof
  | `Goal of string * tree list
  | `Transf of string * tree list
  | `Theory of string * tree list
  | `Session of tree list
  ]

let rec process_goal : tree -> goal list = function
  | `Goal (name, children) -> [{
      name;
      unproved_subgoals =
        match children with
          (* only look at the first attempt *)
        | `Proof :: _ -> 0
        | `Transf (_, children) :: _ ->
          let sum_of f = List.fold_left (fun acc x -> acc + f x) 0 in
          children |> sum_of (fun goal ->
                process_goal goal |> sum_of (fun g -> g.unproved_subgoals))
        | _ -> 1 (* unproved goal *) }]
  | _ -> []

let process_theory path : tree -> unit = function
  | `Theory (name, children) ->
    let goals = List.concat_map process_goal children in
    Hashtbl.add theories name { path; name; goals }
  | _ -> ()

let process_why3session path : tree -> unit = function
  | `Session children -> List.iter (process_theory path) children
  | _ -> Printf.eprintf "Invalid why3session\n"

let parse_why3session path : tree option =
  try
    let file = open_in path in
    let input = Xmlm.make_input (`Channel file) in
    let not_skip = function `Skip -> false | _ -> true in
    let filter_skip = List.filter not_skip in
    let el ((_, tag), attr : Xmlm.tag) children : tree = match tag with
      | "why3session" ->
        begin match List.find_opt not_skip children with
        | None -> `Session []
        | Some s -> s
        end
      | "file" -> `Session (filter_skip children)
      | "path" | "prover" | "result" | "undone" | "unedited" | "internalfailure" | "label" -> `Skip
      | "theory" -> `Theory (List.assoc ("", "name") attr, filter_skip children)
      | "goal" -> `Goal (List.assoc ("", "name") attr, filter_skip children)
      | "transf" -> `Transf (List.assoc ("", "name") attr, filter_skip children)
      | "proof" -> `Proof
      | tag -> failwith ("Unexpected tag: " ^ tag) in
    let data _ = `Skip in
    Some (snd (Xmlm.input_doc_tree ~el ~data input))
  with
  | e -> raise e (* TODO: log error *)

let collect_sessions_for ~root ~crate =
  let (/) = Filename.concat in
  let why3session_path = root / "target" / (crate ^ "-lib") / "why3session.xml" in
  match parse_why3session why3session_path with
  | Some why3session -> process_why3session why3session_path why3session
  | None -> ()

let find_rust_crate root =
  let (/) = Filename.concat in
  try
    match Toml.Parser.from_filename (root / "Cargo.toml") with
    | `Error _ -> None
    | `Ok file -> Toml.Lenses.(get file (field "package" |-- key "name" |-- string))
  with Sys_error _ -> None

let collect_sessions ~root =
  match find_rust_crate root with
  | None -> ()
  | Some crate -> collect_sessions_for ~root ~crate

let debug_theories () =
  let r = ref [] in
  Hashtbl.iter (fun name info ->
    let msg = Printf.sprintf "Theory %s (%s) has %d goals\n" name info.path (List.length info.goals) in
    r := msg :: !r) theories;
  String.concat "" !r
