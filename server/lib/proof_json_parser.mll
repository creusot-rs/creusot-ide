{
type position = { line: int; column: int }

let get_position lexbuf =
  let open Lexing in
  let position_of lexpos = { line = lexpos.pos_lnum; column = lexpos.pos_cnum - lexpos.pos_bol } in
  position_of lexbuf.lex_curr_p

type range = position * position

type symbol
  = Brace of position    (* position right after the brace (for folding) *)
  | Bracket of position  (* position right after the bracket *)

type path_item
  = Index of int
  | Key of string

type ctx = {
    stack: symbol list;
    path: path_item list;
  }

let push_symbol s ctx = { ctx with stack = s :: ctx.stack }
let push_path p ctx = { ctx with path = p :: ctx.path }

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
}

let white = [' ' '\t' '\n' '\r']
let nonwhite = [^' ' '\t' '\n' '\r']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule json ctx = parse
  | white+ as s { line_incs s lexbuf; json ctx lexbuf }
  | '{' { let start_pos = get_position lexbuf in
          let end_pos, fold_ranges = json_object ctx lexbuf in
          (start_pos, end_pos) :: fold_ranges
        }

and json_object ctx = parse
  | white+ as s { line_incs s lexbuf; json_object ctx lexbuf }
  | '}' { let end_pos = get_position lexbuf in end_pos, [] }
