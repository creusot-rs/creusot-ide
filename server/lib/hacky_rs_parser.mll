{
open Rust_parser
open Rust_syntax

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

type span = Lexing.position * Lexing.position
let funnames : (def_path * span) list ref = ref []
let get_funnames () = !funnames
type stack_symbol
    = Curly
    | Mod of string
    | Impl of impl_subject option
let stack = ref []
let push_stack s = stack := s :: !stack
let pop_stack () =
    match !stack with
    | [] -> ()
    | _ :: tl -> stack := tl

let mk_name name =
    let rec unstack qname = function
        | [] -> qname
        | Curly :: s -> unstack qname s
        | Mod m :: s -> unstack (Rust_syntax.Other m :: qname) s
        | Impl None :: s -> unstack qname s (* TODO: do something sensible *)
        | Impl (Some impl) :: s -> unstack (Rust_syntax.Impl impl :: qname) s
    in
    unstack [Other name] !stack

let string_of_qname = String.concat "."

let push_impl impl = stack := Impl impl :: !stack
}

let white = [' ' '\t' '\n' '\r']
let nonwhite = [^' ' '\t' '\n' '\r']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule rust = parse
    | (white+ as s) { line_incs s lexbuf; rust lexbuf }
    | ("mod" white+ (ident as modname) white* '{' as s) {
        line_incs s lexbuf;
        push_stack (Mod modname);
        rust lexbuf
    }
    | '{' { push_stack Curly; rust lexbuf }
    | ("fn" white+ (ident as name) as s) {
        line_incs s lexbuf;
        funnames := (mk_name name, (minus_position lexbuf.Lexing.lex_curr_p (String.length name), lexbuf.Lexing.lex_curr_p)) :: !funnames;
        rust lexbuf }
    | '}' { pop_stack (); rust lexbuf }
    | "impl" white* {
        line_incs (Lexing.lexeme lexbuf) lexbuf;
        let end_flag = ref false in
        match Rust_parser.impl_subject1 (rust_lexer end_flag) lexbuf with
        | impl -> push_impl (Some impl); rust lexbuf
        | exception _ -> push_impl None;
            if !end_flag then rust lexbuf
            else rust_impl_start lexbuf
    }
    | _ { rust lexbuf }
    | eof { () }

and rust_impl_start = parse
    | "{" { rust lexbuf }
    | _ { rust_impl_start lexbuf }

and rust_lexer end_flag = parse
    | ident { IDENT (Lexing.lexeme lexbuf) }
    | '<' { LANGLE }
    | '>' { RANGLE }
    | ',' { COMMA }
    | "()" { UNIT }
    | "::" { COLONCOLON }
    | "for" { FOR }
    | "{" { end_flag := true; EOF }
    | white+ { line_incs (Lexing.lexeme lexbuf) lexbuf; rust_lexer end_flag lexbuf }


{
let list_names lexbuf =
    funnames := [];
    rust lexbuf;
    get_funnames ()
}
