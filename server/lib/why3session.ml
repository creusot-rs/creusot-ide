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
  | `Goal (goal_name, children) ->
        begin match children with
          (* only look at the first attempt *)
        | `Proof :: _ -> []
        | `Transf (_, children) :: _ -> List.concat_map process_goal children
        | _ (* should be [] *) -> [{ goal_name }]
        end
  | _ -> []

let process_theory path : tree -> unit = function
  | `Theory (name, children) ->
    let unproved_goals = Unproved (Array.of_list @@ List.concat_map process_goal children) in
    Hashtbl.add theories name { path; name; unproved_goals }
  | _ -> ()

let process_why3session path : tree -> unit = function
  | `Session children -> List.iter (process_theory path) children
  | _ -> Printf.eprintf "Invalid why3session\n"

let parse_why3session path : tree option =
  match open_in path with
  | exception Sys_error _ -> None
  | file ->
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

let process_why3session_path path =
  match parse_why3session path with
  | Some why3session -> process_why3session path why3session
  | None -> ()

let collect_sessions_for ~root ~crate =
  let (/) = Filename.concat in
  let path = root / "target" / (crate ^ "-lib") / "why3session.xml" in
  process_why3session_path path

let collect_sessions ~root =
  match Cargo.find_rust_crate root with
  | None -> ()
  | Some crate -> collect_sessions_for ~root ~crate

let debug_theories () =
  let r = ref [] in
  Hashtbl.iter (fun name info ->
    let msg =
      match info.unproved_goals with
      | Unknown -> Printf.sprintf "Theory %s, status unknown" name
      | Unproved unproved_goals -> Printf.sprintf "Theory %s (%s) has %d unproved goals\n" name info.path (Array.length unproved_goals) in
    r := msg :: !r) theories;
  String.concat "" !r

let get_theory (name : string) =
  let r = Hashtbl.find_opt theories name in
  if Option.is_none r then Debug.debug ("No proof found for " ^ name);
  r

let encode_segment (s : string) =
  let rec encode i : char Seq.t = fun () ->
    if i = String.length s then Seq.Nil
    else if i + 1 = String.length s && s.[i] = '_' then String.to_seq "qy95z" ()
    else if s.[i] = '_' && s.[i+1] = '_' then Seq.append (String.to_seq "qy95z") (encode (i+1)) ()
    else Seq.Cons (s.[i], encode (i+1))
  in
  String.of_seq (if s.[0] = '_'
    then Seq.append (String.to_seq "qy95z") (encode 1)
    else encode 0)

let encode_path (s : string list) =
  s |> List.map encode_segment
    |> String.concat "__"

let theory_of_path (s : string list) = "M_" ^ encode_path s

let add_thy name thy = Hashtbl.add theories name thy
