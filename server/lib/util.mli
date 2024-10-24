open Lsp.Types

type source =
  | File of (* file name *) string
  | String of (* file name *) string * string

val file_of_source : source -> string

module Lex : sig
  val new_line : Lexing.lexbuf -> unit
  val line_incs : Lexing.lexbuf -> unit
  val range : Lexing.lexbuf -> Range.t
end

module Async : sig
  val async : 'a Lwt.t -> 'a
  val async_handler : (unit -> 'a) -> 'a Lwt.t
end

(* List files in a directory recursively, excluding some toplevel directories.
   Return paths relative to the input directory. *)
val walk_dir : ?exclude:string list -> string -> string list
val split_first : char -> string -> string * string
val split_last : char -> string -> string * string