open Lsp.Types

val lookup_demangle : string -> Rust_syntax.def_path option

val coma_file : string -> bool
(** [true] if the file was found *)

val coma_file_as_string : path:string -> string -> unit
(** Like [coma_file] except we already got the contents as a string (what we get in the DidOpen notification) *)

val lookup_def_path : Rust_syntax.def_path -> Hacky_coma_parser.loc_ident option

type rust_doc = {
    module_: string;
    defns: (Rust_syntax.def_path * Range.t) list;
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

val proof_json : string -> unit

val declare_orphan : string -> unit
(** Rust files that don't have a crate *)
