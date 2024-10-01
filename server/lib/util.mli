module Lex : sig
  val new_line : Lexing.lexbuf -> unit
  val line_incs : Lexing.lexbuf -> unit
end