open Lsp.Types
open Hacky_coma_parser

let orphans : (string, unit) Hashtbl.t = Hashtbl.create 32
let declare_orphan path = Hashtbl.replace orphans path ()
let is_orphan path = Hashtbl.mem orphans path

type trie =
  { ident_map: (string, trie) Hashtbl.t;
    mutable coma_loc_ident: loc_ident option; (* Location in the coma file *)
    mutable impl_map: (Rust_syntax.impl_subject * trie) list }

let new_trie () = {
  ident_map = Hashtbl.create 16;
  coma_loc_ident = None;
  impl_map = [] }

let global_trie = new_trie ()

let demangler : (string, Rust_syntax.def_path) Hashtbl.t = Hashtbl.create 16

let insert_demangle (name : string) (path : Rust_syntax.def_path) : unit =
  Hashtbl.replace demangler name path

let lookup_demangle (name : string) : Rust_syntax.def_path option =
  Hashtbl.find_opt demangler name

let rec unify_qualid t u : bool =
  let open Rust_syntax in
  let rec is_prefix t u =
    match t, u with
    | [], _ -> true
    | s :: t, s' :: u -> s = s' && is_prefix t u
    | _ :: _, [] -> false in
  t.unqual = u.unqual && is_prefix t.qualifier u.qualifier
and unify_ty (t : Rust_syntax.ty) (u : Rust_syntax.ty) : bool =
  let open Rust_syntax in
  (match t, u with
  | Unit, Unit -> true
  | App (t1, t2), App (u1, u2) -> unify_qualid t1 u1 && unify_ty_list t2 u2
  | _, _ -> false
  )
and unify_ty_list t u =
  match t, u with
  | [], [] -> true
  | t1 :: t2, u1 :: u2 -> unify_ty t1 u1 && unify_ty_list t2 u2
  | _, _ -> false

let unify_impl_subject t u =
  let open Rust_syntax in
  match t, u with
  | Trait (t1, t2), Trait (u1, u2) -> unify_ty t1 u1 && unify_ty t2 u2
  | Inherent t, Inherent u -> unify_ty t u
  | _, _ -> false

let rec insert_trie (trie : trie) (path : Rust_syntax.def_path) (p : loc_ident) : unit =
  match path with
  | [] -> trie.coma_loc_ident <- Some p
  | Other ident :: rest ->
      let t = match Hashtbl.find_opt trie.ident_map ident with
      | Some t -> t
      | None -> let t = new_trie () in Hashtbl.add trie.ident_map ident t;
                t in
      insert_trie t rest p
  | Impl impl :: rest ->
      let t = match List.assoc_opt impl trie.impl_map with
      | Some t -> t
      | None -> let t = new_trie () in trie.impl_map <- (impl, t) :: trie.impl_map;
                t in
      insert_trie t rest p

let rec lookup_trie (trie : trie) (path : Rust_syntax.def_path) : loc_ident option =
  match path with
  | [] -> trie.coma_loc_ident
  | Other ident :: rest ->
      Option.bind (Hashtbl.find_opt trie.ident_map ident)
        (fun t -> lookup_trie t rest)
  | Impl impl :: rest ->
      Option.bind (List.find_opt (fun (i, _) -> unify_impl_subject impl i) trie.impl_map)
        (fun (_, t) -> lookup_trie t rest)

let m = Mutex.create ()

let insert_def_path d =
  Mutex.protect m (fun () -> insert_trie global_trie d)
let lookup_def_path d =
  let r = Mutex.protect m (fun () -> lookup_trie global_trie d) in
  if Option.is_none r then Debug.debug ("No coma module found for " ^ Rust_syntax.string_of_def_path d);
  r

let subtrie =
  let rec subtrie_ t = function
    | [] -> Some t
    | ident :: idents ->
      match Hashtbl.find_opt t.ident_map ident with
      | None -> None
      | Some t -> subtrie_ t idents
  in
  subtrie_ global_trie

let walk_trie t f =
  let rec walk_ t =
    Option.iter (fun i -> f i) t.coma_loc_ident;
    Hashtbl.iter (fun _ t -> walk_ t) t.ident_map;
    List.iter (fun (_, t) -> walk_ t) t.impl_map
  in
  walk_ t

let coma_lexbuf lexbuf =
  let open Lexing in
  let open Hacky_coma_parser in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = 0 };
  match coma [] lexbuf with
  | acc ->
    List.iter (fun (name, path) -> insert_demangle name.ident path; insert_def_path path name) acc
  | exception e -> Debug.debug ("Failed to parse coma file " ^ lexbuf.Lexing.lex_curr_p.Lexing.pos_fname ^ ": " ^ Printexc.to_string e)

let coma_file (path : string) : bool =
  try
    let h = open_in path in
    let lexbuf = Lexing.from_channel h in
    Lexing.set_filename lexbuf path;
    coma_lexbuf lexbuf;
    close_in h;
    true
  with
  | Sys_error _ -> false
  | e -> Debug.debug ("Unexpected exception: " ^ Printexc.to_string e); false

let coma_file_as_string ~path (s : string) : unit =
  let lexbuf = Lexing.from_string s in
  Lexing.set_filename lexbuf path;
  coma_lexbuf lexbuf

type rust_doc = {
  module_: string;
  defns: (Rust_syntax.def_path * Lsp.Types.Range.t) list;
}

let rust_files : (string, rust_doc) Hashtbl.t = Hashtbl.create 16

let span_to_range (start, stop) =
  Lsp.Types.Range.create
    ~start:(Lsp.Types.Position.create ~line:(start.Lexing.pos_lnum - 1) ~character:(start.Lexing.pos_cnum - start.Lexing.pos_bol))
    ~end_:(Lsp.Types.Position.create ~line:(stop.Lexing.pos_lnum - 1) ~character:(stop.Lexing.pos_cnum - stop.Lexing.pos_bol))

let rust_lexbuf ~path lexbuf =
  let open Hacky_rs_parser in
  match list_names lexbuf with
  | names ->
    let module_ = path |> Filename.basename |> Filename.remove_extension in
    let defns = names |> List.map (fun (def_path, span) -> (def_path, span_to_range span)) in
    Hashtbl.add rust_files path { module_; defns }
  | exception _ -> ()

let rust_file (path : string) : unit =
  let h = open_in path in
  let lexbuf = Lexing.from_channel h in
  Lexing.set_filename lexbuf path;
  rust_lexbuf ~path lexbuf;
  close_in h

let rust_file_as_string ~path (s : string) : unit =
  let lexbuf = Lexing.from_string s in
  Lexing.set_filename lexbuf path;
  rust_lexbuf ~path lexbuf

let package_name = ref ""

let read_cargo ~root =
  match Cargo.find_rust_crate root with
  | None -> ()
  | Some crate -> package_name := crate

let get_package_name () = Some !package_name
let crate_of path =
  if is_orphan path then None else get_package_name ()

module RustInfo = struct
  type status =
    | Unknown
    | Qed
    | ToProve of (string * Location.t) array
  type item = {
      range: Range.t;
      to_coma: Location.t;
      status: status;
  }
  type orphan_item = {
    orphan_name: string;
    orphan_coma_loc: Location.t;
    orphan_status: status;
  }
  type t = {
    inline_items: item list;
    orphans: orphan_item list;
  }
  let empty = { inline_items = []; orphans = [] }
end

let status_of_thy thy =
  let open Types in
  let open RustInfo in
  match thy.unproved_goals with
  | Types.Unknown -> Unknown
  | Types.Unproved goals when Array.length goals = 0 -> Qed
  | Types.Unproved goals ->
    let dummy_position = Position.create ~line:0 ~character:0 in
    let dummy_range = Range.create ~start:dummy_position ~end_:dummy_position in
    let from_goal goal = (goal.goal_name, Location.create ~uri:(DocumentUri.of_path thy.path) ~range:dummy_range) in
    ToProve (Array.map from_goal goals)

let get_status ident = match Why3session.get_theory ident with
  | None -> RustInfo.Unknown
  | Some thy -> status_of_thy thy

let find_orphan_goals ~package modname visited =
  match subtrie (match package with None -> [modname] | Some package -> [package; modname]) with
  | None -> []
  | Some t ->
    let r = ref [] in
    let _ = walk_trie t @@ fun i -> if not (visited i.ident) then (
      let item = RustInfo.{
        orphan_name = i.ident;
        orphan_coma_loc = i.loc;
        orphan_status = get_status i.ident;
      } in
      r := item :: !r
    ) in
    !r

let get_rust_info ~package ~path : RustInfo.t =
  let open Rust_syntax in
  match Hashtbl.find_opt rust_files path with
  | None -> RustInfo.empty
  | Some doc ->
    let visited = Hashtbl.create 16 in
    let inline_items =
      doc.defns |> List.filter_map @@ fun (def_path, range) ->
        let (let+) = Option.bind in
        let dpath = match package with
          | None -> Other doc.module_ :: def_path
          | Some package -> Other package :: Other doc.module_ :: def_path
        in
        let+ loc_ident = lookup_def_path dpath in
        let status = get_status loc_ident.ident in
        let to_coma = loc_ident.loc in
        Hashtbl.replace visited loc_ident.ident ();
        Some RustInfo.{ range; to_coma; status }
    in
    let orphans = find_orphan_goals ~package doc.module_ (Hashtbl.mem visited) in
    RustInfo.{
      inline_items;
      orphans;
    }

let get_rust_lenses uri =
  let to_lsp_lenses d =
    let open RustInfo in
    let create_lens ~title ~command ~arguments = CodeLens.create ~range:d.range ~command:(Command.create ~title ~command ~arguments ()) () in
    let proof_lens =
      match d.status with
      | Unknown -> []
      | Qed -> [create_lens ~title:"QED" ~command:"" ~arguments:[]]
      | ToProve goals -> [create_lens
          ~title:(Printf.sprintf "%d unproved goals" (Array.length goals))
          ~command:"creusot.peekLocations"
          ~arguments:[
            DocumentUri.yojson_of_t uri;
            Position.(yojson_of_t d.range.Range.start);
            `List (Array.(to_list (map (fun (_name, location) -> Location.yojson_of_t location) goals)));
            `String "gotoAndPeek"]]
    in
    let coma_lens = create_lens
        ~title:"Inspect output Coma"
        ~command:"creusot.peekLocations"
        ~arguments:[
          DocumentUri.yojson_of_t uri;
          Position.(yojson_of_t d.range.Range.start);
          `List [Location.yojson_of_t d.to_coma];
          `String "gotoAndPeek"]
    in
    proof_lens @ [coma_lens]
  in
  let path = DocumentUri.to_path uri in
  let info = get_rust_info ~package:(crate_of path) ~path in
  let inline_items = info.inline_items |> List.concat_map to_lsp_lenses in
  let orphan_lens_opt =
    match info.orphans with
    | [] -> []
    | _ :: _ ->
      let open RustInfo in
      let zero = Position.create ~line:0 ~character:0 in
      let range = Range.create ~start:zero ~end_:zero in
      let n_orphan_unproved = List.fold_left (fun acc orphan -> acc + match orphan.orphan_status with
        | Unknown -> 0
        | Qed -> 0
        | ToProve goals -> Array.length goals) 0 info.orphans in
      let title_end = if n_orphan_unproved = 0 then "" else " with %d unproved goals" in
      let title = Printf.sprintf "%d orphan theories%s" (List.length info.orphans) title_end in
      let orphans_locs = info.orphans |> List.map @@ fun orphan ->
        Location.(yojson_of_t orphan.orphan_coma_loc) in
      let command = Command.create
        ~title
        ~command:"creusot.peekLocations"
        ~arguments: [
          DocumentUri.yojson_of_t uri;
          Position.(yojson_of_t zero);
          `List orphans_locs
        ] () in
      [CodeLens.create ~range ~command ()]
  in
  orphan_lens_opt @ inline_items

let get_rust_diagnostics uri =
  let to_related_information (goal_name, location) =
    DiagnosticRelatedInformation.create ~location ~message:goal_name
  in
  let to_lsp_diagnostics d =
    let open RustInfo in
    let range = d.range in
    let source = "Creusot" in
    let create_diagnostic message severity relatedInformation =
      Diagnostic.create ~range ~source ~message ~severity ~relatedInformation () in
    match d.status with
    | Unknown -> []
    | Qed -> [create_diagnostic "QED" DiagnosticSeverity.Information []]
    | ToProve goals -> [create_diagnostic
          (Printf.sprintf "%d unproved goals" (Array.length goals))
          DiagnosticSeverity.Warning
          Array.(to_list (map to_related_information goals))]
  in
  let path = DocumentUri.to_path uri in
  let info = get_rust_info ~package:(crate_of path) ~path in
  info.inline_items |> List.concat_map to_lsp_diagnostics

let proof_json path =
  try
    Why3find.read_proof_json ~fname:path
      |> List.iter (fun (name, thy) -> Why3session.add_thy name thy)
  with
  | Sys_error _ -> ()
  | e -> Debug.debug (Printexc.to_string e)