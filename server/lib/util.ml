module Lex = struct
  (* When we just started a new line *)
  let new_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {
        pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum
    }

  (* Call this when [lexeme lexbuf] may contain newlines. *)
  let line_incs lexbuf =
    let splits = String.split_on_char '\n' (Lexing.lexeme lexbuf) in
    let n_lines = List.length splits - 1 in
    let len_last_line = String.length (List.hd (List.rev splits)) in
    if n_lines > 0 then () else
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- {
            pos with
            Lexing.pos_lnum = pos.Lexing.pos_lnum + n_lines;
            Lexing.pos_bol = pos.Lexing.pos_cnum - len_last_line
        }
end