open Lsp.Types

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

module RustDiagnostic : sig
    type status = Qed | ToProve of (string * Location.t) array
    type t = {
        range: Range.t;
        status: status;
    }
end

val get_rust_diagnostics : path:string -> RustDiagnostic.t list

val proof_json : string -> unit