{
(* Function to increase line count in lexbuf *)
let line_incs s lexbuf =
(*  Printf.printf "Read: %s\n" s; *)
  let splits = String.split_on_char '\n' s in
  let pos = lexbuf.Lexing.lex_curr_p in
(* Printf.printf "Was in line %d, position %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); *)
  lexbuf.Lexing.lex_curr_p <- {
    pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + (List.length splits - 1);
      Lexing.pos_bol = if List.length splits > 1 then pos.Lexing.pos_cnum - (String.length (List.hd (List.rev splits))) else pos.Lexing.pos_bol
  }

let print_position p =
    Printf.eprintf "%s:%d:%d\n" p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

let minus_position p n =
    { p with Lexing.pos_cnum = p.Lexing.pos_cnum - n }
}

let white = [' ' '\t' '\n' '\r']
let nonwhite = [^' ' '\t' '\n' '\r']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule rust = parse
    | (white+ as s) { line_incs s lexbuf; rust lexbuf }
    | ("pub" white+ "fn" white+ (ident as name) as s) {
        line_incs s lexbuf;
        prerr_endline name;
        print_position (minus_position lexbuf.Lexing.lex_curr_p (String.length name));
        print_position lexbuf.Lexing.lex_curr_p;
        rust lexbuf }
    | _ { rust lexbuf }
    | eof { () }
