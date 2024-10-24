open Lsp.Types
open Util

val lookup_demangle : string -> Rust_syntax.def_path option

val coma_file : uri:DocumentUri.t -> string -> bool
(** [true] if the file was found *)

val coma_file_as_string : uri:DocumentUri.t -> path:string -> string -> unit
(** Like [coma_file] except we already got the contents as a string (what we get in the DidOpen notification) *)

val lookup_def_path : Rust_syntax.def_path -> Hacky_coma_parser.loc_ident option

val get_coma_lenses : DocumentUri.t -> CodeLens.t list
val get_coma_links : DocumentUri.t -> DocumentLink.t list

type rust_doc = {
    module_: string;
    defns: (Rust_syntax.def_path * string list * Range.t) list;
  }

val rust_file : string -> unit

val rust_file_as_string : path:string -> string -> unit

val read_cargo : root:string -> unit
(** Given DIR, read DIR/Cargo.toml *)

val get_package_name : unit -> string option

module RustInfo : sig
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
end

val get_rust_info : package:string option -> path:string -> RustInfo.t
val get_rust_lenses : DocumentUri.t -> CodeLens.t list
val get_rust_diagnostics : DocumentUri.t -> Diagnostic.t list
val get_rust_test_items : string -> Test_api.test_item list

val add_proof_json : source -> unit
val get_proof_json_inlay_hints : string -> InlayHint.t list
val get_proof_json_diagnostics : DocumentUri.t -> Diagnostic.t list

val declare_orphan : string -> unit
(** Rust files that don't have a crate *)

val add_coma_file : DocumentUri.t -> unit
val initialize : string -> unit

val get_revdep : DocumentUri.t -> DocumentUri.t option
