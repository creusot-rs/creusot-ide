open Lsp.Types

val lookup_demangle : string -> Rust_syntax.def_path option

val coma_file : string -> unit

val coma_file_as_string : path:string -> string -> unit
(** Like [coma_file] except we already got the contents as a string (what we get in the DidOpen notification) *)

val lookup_def_path : Rust_syntax.def_path -> Hacky_coma_parser.loc_ident option

type rust_doc = {
    package: string;
    module_: string;
    defns: (Rust_syntax.def_path * Range.t) list;
  }

val rust_file : package:string -> string -> unit

val rust_file_as_string : package:string -> path:string -> string -> unit

val read_cargo : root:string -> unit
(** Given DIR, read DIR/Cargo.toml *)

val get_package_name : unit -> string

module RustInfo : sig
    type status =
        | Unknown
        | Qed
        | ToProve of (string * Location.t) array
    type t = {
        range: Range.t;
        to_coma: Location.t;
        status: status;
    }
end

val get_rust_info : path:string -> RustInfo.t list
val get_rust_lenses : DocumentUri.t -> CodeLens.t list
val get_rust_diagnostics : DocumentUri.t -> Diagnostic.t list

val proof_json : string -> unit
