open Types

let theories : theories_map = Hashtbl.create 32
let package_name = ref ""
let get_package_name () = !package_name

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
  | `Goal (name, children) ->
        begin match children with
          (* only look at the first attempt *)
        | `Proof :: _ -> []
        | `Transf (_, children) :: _ -> List.concat_map process_goal children
        | _ (* should be [] *) -> [{ name }]
        end
  | _ -> []

let process_theory path : tree -> unit = function
  | `Theory (name, children) ->
    let unproved_goals = Array.of_list @@ List.concat_map process_goal children in
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
    | `Ok file ->
      let name_opt = Toml.Lenses.(get file (field "package" |-- key "name" |-- string)) in
      Option.iter (fun name -> package_name := name) name_opt;
      name_opt
  with Sys_error _ -> None

let collect_sessions ~root =
  match find_rust_crate root with
  | None -> ()
  | Some crate -> collect_sessions_for ~root ~crate

let debug_theories () =
  let r = ref [] in
  Hashtbl.iter (fun name info ->
    let msg = Printf.sprintf "Theory %s (%s) has %d unproved goals\n" name info.path (Array.length info.unproved_goals) in
    r := msg :: !r) theories;
  String.concat "" !r

let get_theory (name : string) =
  Hashtbl.find_opt theories name

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
