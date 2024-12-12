open Lsp.Types
open Hacky_coma_parser
open Why3findUtil
open Util
open Log

(* Map from theory name to its proof.json path and the theory info *)
let theory_map : (string, string * ProofPath.theory) Hashtbl.t = Hashtbl.create 32
let proof_json_map : (string, ProofPath.theory list) Hashtbl.t = Hashtbl.create 16

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
  | Const x, Const y -> unify_qualid x y
  | Tup ts, Tup us -> List.length ts = List.length us && List.for_all2 unify_ty ts us
  | App (t1, t2), App (u1, u2) -> unify_qualid t1 u1 && unify_generic_arg_list t2 u2
  | Ref (_, t), Ref (_, u) -> unify_ty t u
  | _, _ -> false
  )
and unify_generic_arg_list t u =
  match t, u with
  | [], [] -> true
  | t1 :: t2, u1 :: u2 -> unify_generic_arg t1 u1 && unify_generic_arg_list t2 u2
  | _, _ -> false
and unify_generic_arg t u =
  match t, u with
  | LifetimeArg _, LifetimeArg _ -> true  (* alpha renaming of lifetimes is ignored *)
  | TypeArg t, TypeArg u -> unify_ty t u
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
  | Rust_syntax.Closure _ :: _rest
  | Rust_syntax.Unknown _ :: _rest -> ()

let rec lookup_trie (trie : trie) (path : Rust_syntax.def_path) : loc_ident option =
  match path with
  | [] -> trie.coma_loc_ident
  | Other ident :: rest ->
      Option.bind (Hashtbl.find_opt trie.ident_map ident)
        (fun t -> lookup_trie t rest)
  | Impl impl :: rest ->
      Option.bind (List.find_opt (fun (i, _) ->
        (* log Debug "Unify %a %a" Rust_syntax.fprint_impl_subject i Rust_syntax.fprint_impl_subject impl; *)
        unify_impl_subject impl i) trie.impl_map)
        (fun (_, t) -> lookup_trie t rest)
  | Rust_syntax.Closure _ :: _rest
  | Rust_syntax.Unknown _ :: _rest -> None

let m = Mutex.create ()

let insert_def_path d =
  Mutex.protect m (fun () -> insert_trie global_trie d)
let lookup_def_path d =
  let r = Mutex.protect m (fun () -> lookup_trie global_trie d) in
  if Option.is_none r then log Debug "No coma module found for %s" (Rust_syntax.string_of_def_path d);
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

module ComaInfo = struct
  open Hacky_coma_parser
  type coma_file = {
      modules: mod_info list;
      locations: (Range.t * Location.t) list;
  }
  let coma_files : (DocumentUri.t, coma_file) Hashtbl.t = Hashtbl.create 16
  let add_file = Hashtbl.replace coma_files
  let lookup_file = Hashtbl.find_opt coma_files
end

let coma_lexbuf ~uri lexbuf =
  let open Lexing in
  let open Hacky_coma_parser in
  let open ComaInfo in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = 0 };
  let state = new_state () in
  match coma state lexbuf with
  | () ->
    state.modules |> List.iter (fun mod_info ->
      insert_demangle mod_info.name.ident mod_info.demangled;
      insert_def_path mod_info.demangled mod_info.name);
    ComaInfo.add_file uri { modules = state.State.modules; locations = state.State.locations }
  | exception e ->
    let p = lexbuf.lex_curr_p in
    log Error "Failed to parse coma file %s:%d:%d: %s"
      p.pos_fname
      (p.pos_lnum + 1)
      (p.pos_cnum - p.pos_bol + 1)
      (Printexc.to_string e)

let coma_file ~uri (path : string) =
  try
    let h = open_in path in
    let lexbuf = Lexing.from_channel h in
    Lexing.set_filename lexbuf path;
    coma_lexbuf ~uri lexbuf;
    close_in h
  with
  | Sys_error _ -> ()
  | e -> log Error "coma_file: unexpected exception: %s" (Printexc.to_string e)

let coma_file_as_string ~uri ~path (s : string) : unit =
  let lexbuf = Lexing.from_string s in
  Lexing.set_filename lexbuf path;
  coma_lexbuf ~uri lexbuf

let add_orphan_coma_file src =
  match src with
  | File path -> coma_file ~uri:(DocumentUri.of_path path) path
  | String (path, s) -> coma_file_as_string ~uri:(DocumentUri.of_path path) ~path s

let get_coma_info uri = ComaInfo.lookup_file uri

let get_coma_lenses uri =
  match get_coma_info uri with
  | None -> log Error "get_coma_lenses: not found %s" (DocumentUri.to_path uri); []
  | Some info ->
    info.modules |> List.map @@ fun info ->
      let range = info.name.loc.range in
      let loc = match info.loc with
        | None -> []
        | Some loc -> [Location.yojson_of_t loc] in
      let command = Command.create
        ~title:(Rust_syntax.string_of_def_path info.demangled)
        ~command:"creusot.peekLocations"
        ~arguments:[
          DocumentUri.yojson_of_t uri;
          Position.(yojson_of_t range.Range.start);
          `List loc]
        () in
      CodeLens.create
        ~command
        ~range ()

let get_coma_links uri =
  match get_coma_info uri with
  | None -> []
  | Some info ->
    info.locations |> List.map @@ fun (range, loc) ->
      let target = loc.Location.uri in
      (* TODO: append fragment "#LINE:COL" *)
      DocumentLink.create ~range ~target ()

type rust_doc = {
  module_: string;
  defns: (Rust_syntax.def_path * string list * Lsp.Types.Range.t) list;
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
    let defns = names |> List.map (fun (def_path, attrs, span) -> (def_path, attrs, span_to_range span)) in
    Hashtbl.replace rust_files path { module_; defns }
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

let add_rust_file src =
  match src with
  | File path -> rust_file path
  | String (path, s) -> rust_file_as_string ~path s

let package_name = ref ""

let read_cargo ~root =
  match Cargo.find_rust_crate root with
  | None -> ()
  | Some crate -> package_name := crate

let get_package_name () = Some !package_name

module RustInfo = struct
  type status =
    | Unknown
    | Qed
    | ToProve of (string * Location.t) array
  type item = {
    name: string;
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

let status_of_thy json_file (theory : ProofPath.theory) =
  let open ProofPath in
  let open RustInfo in
  let goals = theory.goal_info in
  let from_goal { goal; goal_range = range; is_null }=
    if is_null then
      Some (string_of_goal goal, Location.create ~uri:(DocumentUri.of_path json_file) ~range)
    else None in
  let goals = Array.of_list (List.filter_map from_goal goals) in
  if Array.length goals = 0 then
    Qed
  else
    ToProve goals

let get_status ident = match Hashtbl.find_opt theory_map ident with
  | None -> log Debug "No proofs found for %s" ident; RustInfo.Unknown
  | Some (json_file, thy) -> status_of_thy json_file thy

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
      doc.defns |> List.filter_map @@ fun (def_path, _attrs, range) ->
        let (let+) = Option.bind in
        let dpath = match package with
          | None -> Other doc.module_ :: def_path
          | Some package -> Other package :: Other doc.module_ :: def_path
        in
        let+ loc_ident = lookup_def_path dpath in
        let name = string_of_def_path dpath in
        let status = get_status loc_ident.ident in
        let to_coma = loc_ident.loc in
        Hashtbl.replace visited loc_ident.ident ();
        Some RustInfo.{ name; range; to_coma; status }
    in
    let orphans = find_orphan_goals ~package doc.module_ (Hashtbl.mem visited) in
    RustInfo.{
      inline_items;
      orphans;
    }

let get_rust_lenses rust_file =
  Why3findUtil.refresh_info ~rust_file;
  Why3findUtil.get_lenses ~rust_file

let get_rust_diagnostics rust_file =
  Why3findUtil.get_diagnostics ~rust_file

let get_rust_test_items rust_file =
  Why3findUtil.get_test_items ~rust_file

let guess_crate_dir (file : string) : (string * string * string) option =
  let rec guess acc file =
    if file = "." || file = "/" then None
    else
      let parent = Filename.dirname file in
      if Filename.basename parent = "verif" then
        Some (parent, Filename.basename file, String.concat "/" acc)
      else
        guess (Filename.basename file :: acc) parent
  in guess [] file

let add_orphan_proof_json src =
  let path = file_name_of_source src in
  let coma = Filename.dirname path ^ ".coma" in
  try
    let theories = read_proof_json ~coma src in
    theories |> List.iter (fun theory ->
      Hashtbl.replace theory_map theory.ProofPath.theory (path, theory));
    Hashtbl.replace proof_json_map path theories
  with
  | e ->
    log Error "add_proof_json: failed to read %s: %s" path (Printexc.to_string e)

let add_top_proof_json src =
  let path = file_name_of_source src in
  let coma = Filename.dirname path ^ ".coma" in
  try
    let theories = read_proof_json ~coma src in
    theories |> List.iter (fun theory ->
      if theory.ProofPath.theory <> "Coma" then log Warning "add_top_proof_json: unexpected theory %s, should be \"Coma\"" theory.ProofPath.theory;
      Hashtbl.replace theory_map coma (path, theory));
    Hashtbl.replace proof_json_map path theories
  with
  | e ->
    log Error "add_top_proof_json: failed to read %s: %s" path (Printexc.to_string e)

let get_proof_json_inlay_hints file : InlayHint.t list = match Hashtbl.find_opt proof_json_map file with
  | None -> log Error "hints: %s not found" file; []
  | Some theories ->
    let open ProofPath in
    let hints = ref [] in
    let add_hint l = hints := l :: !hints in
    theories |> List.iter (fun theory ->
      theory.goal_info |> List.iter (fun { goal; goal_range = range; _ } ->
        let qualified_goal = { theory with goal_info = goal } in
        let command = Command.create
          ~title:"Show proof context"
          ~command:"creusot.showTask"
          ~arguments:[qualified_goal_to_json qualified_goal]
          () in
        let value = short_string_of_goal goal in
        let label = InlayHintLabelPart.create ~command ~value () in
        let sep = InlayHintLabelPart.create ~value:":" () in
        let label = `List [label; sep] in
        let position = range.Range.start in
        add_hint (InlayHint.create ~label ~position ())
        )
      );
    !hints

let get_proof_json_diagnostics file : Diagnostic.t list =
  match Hashtbl.find_opt proof_json_map file with
  | None -> log Error "diagnostics: %s not found" file; []
  | Some theories ->
    let open ProofPath in
    let diagnostics = ref [] in
    let add_diagnostic d = diagnostics := d :: !diagnostics in
    theories |> List.iter (fun theory ->
      theory.goal_info |> List.iter (fun { goal; goal_range = range; is_null } ->
        if is_null then
          add_diagnostic (Diagnostic.create
            ~range
            ~source:"Creusot"
            ~message:(short_string_of_goal goal)
            ~severity:DiagnosticSeverity.Error
            ())
        )
      );
      !diagnostics

let revdeps : (string, DocumentUri.t) Hashtbl.t = Hashtbl.create 16

let add_revdeps = Hashtbl.replace revdeps
let lookup_revdeps = Hashtbl.find_opt revdeps

let get_revdep uri =
  let path = DocumentUri.to_path uri in
  let open Filename in
  if basename path = "proof.json" then
    lookup_revdeps (dirname path ^ ".coma")
  else
    lookup_revdeps path

let top_coma_lexbuf ~primary ~uri file lexbuf =
  let open Lexing in
  let open Hacky_coma_parser in
  let open ComaInfo in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = 0 };
  let state = new_state () in
  match top_coma uri file None state lexbuf with
  | () ->
    if primary then (
      match state.locations with
      | (_, loc) :: _ -> add_revdeps (DocumentUri.to_path uri) loc.uri
      | [] -> ()
    );
    (match coma state lexbuf with
    | () ->
      if primary then (
        state.modules |> List.iter (fun mod_info ->
          insert_demangle mod_info.name.ident mod_info.demangled;
          insert_def_path mod_info.demangled mod_info.name);
      );
      ComaInfo.add_file uri { modules = state.State.modules; locations = state.State.locations }
    | exception e ->
      let p = lexbuf.lex_curr_p in
      log Error "Failed to parse coma file %s:%d:%d: %s"
        p.pos_fname
        (p.pos_lnum + 1)
        (p.pos_cnum - p.pos_bol + 1)
        (Printexc.to_string e))
  | exception e ->
    let p = lexbuf.lex_curr_p in
    log Error "Failed to parse coma file %s:%d:%d: %s"
      p.pos_fname
      (p.pos_lnum + 1)
      (p.pos_cnum - p.pos_bol + 1)
      (Printexc.to_string e)

let add_coma_file' ~primary uri file =
  try
    let path = DocumentUri.to_path uri in
    let h = open_in path in
    let lexbuf = Lexing.from_channel h in
    Lexing.set_filename lexbuf path;
    top_coma_lexbuf ~primary ~uri file lexbuf;
    close_in h
  with
  | Sys_error _ -> ()
  | e -> log Error "add_coma_file': unexpected exception: %s" (Printexc.to_string e)

(* If we have both a lib and a binary crate with the same name,
   we want to link to the lib. *)
let is_primary ~target_dir root_crate crate_type file =
  let only_crate crate =
    let topdirs = Sys.readdir target_dir in
    let is_homonym dir =
      try
        let crate', crate_type' = split_last '_' dir in
        crate = crate' && crate_type <> crate_type'
      with Not_found -> false in
    List.exists is_homonym (Array.to_list topdirs)
  in
  try
    let crate, _ = split_first '/' file in
    crate = root_crate && (crate_type = "rlib" || (crate_type = "bin" && only_crate crate))
  with Not_found -> false

let add_coma_file src =
  try
    let path = file_name_of_source src in
    let uri = DocumentUri.of_path path in
    match guess_crate_dir path with
    | Some (target_dir, crate_dir, file) ->
      let root_crate, crate_type = split_last '_' crate_dir in
      let primary = is_primary ~target_dir root_crate crate_type file in
      add_coma_file' ~primary uri file
    | None ->
      add_orphan_coma_file src
  with
  | e -> log Error "add_coma_file: unexpected exception: %s" (Printexc.to_string e)

let initialize root =
  let (/) = Filename.concat in
  let target_dir = root / "verif" in
  let crates = try Sys.readdir target_dir with _ -> [||] in
  crates |> Array.iter @@ fun crate_dir ->
    try
      let crate, crate_type = split_last '_' crate_dir in
      let target_crate_dir = target_dir / crate_dir in
      let files = Util.walk_dir target_crate_dir in
      let read file =
        let path = target_crate_dir / file in
        if Filename.check_suffix file ".coma" then (
          Why3findUtil.add_coma2 path;
          let primary = is_primary ~target_dir crate crate_type file in
          add_coma_file' ~primary (DocumentUri.of_path path) file
        ) else if Filename.basename file = "proof.json" then
          add_top_proof_json (File path)
        else ()
      in
      List.iter read files
    with Not_found -> ()

type language = Rust | Coma | ProofJson | Unknown

let guess_language path =
  let open Filename in
  if check_suffix path ".rs" then Rust
  else if check_suffix path ".coma" then Coma
  else if basename path = "proof.json" then ProofJson
  else Unknown

let add_file src =
  let path = file_name_of_source src in
  match guess_language path with
  | Rust ->
    let base = Filename.chop_suffix path ".rs" in
    let coma = base ^ ".coma" in
    (* Hack for the creusot repository: tests are standalone rust files and the coma and proofs are next to them. *)
    if Sys.file_exists coma then (
      declare_orphan path;
      let proof = Filename.concat base "proof.json" in
      add_coma_file (File coma);
      add_orphan_proof_json (File proof);
      add_rust_file src
    ) else (
      Why3findUtil.refresh_info ~rust_file:path;
      add_rust_file src
    )
  | Coma -> add_coma_file src; add_coma2 (file_name_of_source src)
  | ProofJson ->
    let rs = Filename.dirname path ^ ".rs" in
    (* Also hack for creusot *)
    if Sys.file_exists rs then (
      declare_orphan rs;
      add_orphan_proof_json src
    ) else
      add_top_proof_json src
  | Unknown -> ()

(* Path tagged with file type *)
type file_id
  = Rust of string
  | Coma of string
  | ProofJson of string

let uri_to_file uri =
  let path = DocumentUri.to_path uri in
  if Filename.check_suffix path ".rs" then Some (Rust path)
  else if Filename.check_suffix path ".coma" then Some (Coma path)
  else if Filename.basename path = "proof.json" then Some (ProofJson path)
  else None

let get_code_lenses = function
  | Rust rust_file -> get_rust_lenses rust_file
  | Coma coma_file -> get_coma_lenses (DocumentUri.of_path coma_file)
  | ProofJson _ -> []

let get_diagnostics = function
  | Rust rust_file -> Some (get_rust_diagnostics rust_file)
  | Coma _ -> None
  | ProofJson proof_json_file -> Some (get_proof_json_diagnostics proof_json_file)

let get_test_items = function
  | Rust rust_file -> Some (get_rust_test_items rust_file)
  | Coma _ | ProofJson _ -> None