open Lsp.Types
open Util

val lookup_demangle : string -> Rust_syntax.def_path option

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

val get_proof_json_inlay_hints : string -> InlayHint.t list

val declare_orphan : string -> unit
val is_orphan : string -> bool
(** Special layout for the creusot test suite, with .coma next to .rs sources *)

val initialize : string -> unit

val get_revdep : DocumentUri.t -> DocumentUri.t option
val add_file : source -> unit

type file_id

val uri_to_file : DocumentUri.t -> file_id option
val get_code_lenses : file_id -> CodeLens.t list
val get_diagnostics : file_id -> Diagnostic.t list option
val get_test_items : file_id -> Test_api.test_item list option
val get_notify_changes : file_id -> DocumentUri.t list