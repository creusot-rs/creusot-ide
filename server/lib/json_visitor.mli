open Lsp.Types

type json_pathlet =
  | ArrayIndex of int
  | ObjectKey of string
type json_path = json_pathlet list

val string_of_pathlet : json_pathlet -> string
val string_of_path : json_path -> string

type visitor = {
  visit_null : json_path -> Range.t -> unit;
  visit_string : json_path -> Range.t -> string -> unit;
}

val empty_visitor : visitor

val visit_lexbuf : visitor -> Lexing.lexbuf -> unit
val visit_file : visitor -> string -> unit
val visit_string : visitor -> string -> unit
