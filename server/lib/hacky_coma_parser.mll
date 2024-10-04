{
open Lsp.Types
open Rust_syntax
open Rust_parser
open Util.Lex

type loc_ident = { ident: string; loc: Location.t }

let new_module ?impl modname loc acc =
    let open Creusot_demangler in
    (match demangle_def_id ?impl modname.ident with
    | Some path -> (modname, loc, path) :: acc
    | None -> acc)
}

let white = [' ' '\t' '\n' '\r']
let nonwhite = [^' ' '\t' '\n' '\r']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let string = '"' ([^ '"']* as string) '"'
let num = ['0'-'9']+

rule coma acc = parse
    | white+ { line_incs lexbuf; coma acc lexbuf }
    | "module" ' '+ (ident as modname) {
        let open Lexing in
        let end_col = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        let begin_col = end_col - String.length modname in
        let modident = {
            ident = modname;
            loc = Location.create
                ~range:(Range.create
                    ~start:(Position.create ~line:lexbuf.lex_curr_p.pos_lnum ~character:begin_col)
                     ~end_:(Position.create ~line:lexbuf.lex_curr_p.pos_lnum ~character:end_col))
                ~uri:(DocumentUri.of_path lexbuf.lex_curr_p.pos_fname)
        } in
        coma_module_meta acc modident None lexbuf
    }
    | _ { coma acc lexbuf }
    | eof { List.rev acc }

and coma_module_meta acc modident loc = parse
    | ' '+ { coma_module_meta acc modident loc lexbuf }
    | "[#" string ' '+ (num as start_line) ' '+ (num as start_col) ' '+ (num as end_line) ' '+ (num as end_col) ' '* ']' {
        let start_line = int_of_string start_line - 1 in
        let start_col = int_of_string start_col in
        let end_line = int_of_string end_line - 1 in
        let end_col = int_of_string end_col in
        let range = Range.create
            ~start:(Position.create ~line:start_line ~character:start_col)
            ~end_:(Position.create ~line:end_line ~character:end_col) in
        let loc = (string, range) in
        coma_module_meta acc modident (Some loc) lexbuf
    }
    | "[#" [^ '\n' ']']* ']' {
        coma_module_meta acc modident loc lexbuf } (* skip attributes *)
    | "(*" ' '* {
        let end_flag = ref false in
        match Rust_parser.impl_subject0 (rust_lexer end_flag) lexbuf with
        | impl ->
            let acc = new_module ~impl modident loc acc in
            coma acc lexbuf
        | exception _ ->
            let acc = new_module modident loc acc in
            if !end_flag then coma acc lexbuf
            else coma_module_meta_end acc lexbuf }
    | '\n' {
        (* Nothing right after the module name, go back to main loop *)
        new_line lexbuf;
        let acc = new_module modident loc acc in
        coma acc lexbuf }
    | eof {
        List.rev (new_module modident loc acc)
    }

and coma_module_meta_end acc = parse
    | "*)" { coma acc lexbuf }

and rust_lexer end_flag = parse
    | "as" { AS } (* reminder: overlaps with ident! *)
    | ident { IDENT (Lexing.lexeme lexbuf) } 
    | '<' { LANGLE }
    | '>' { RANGLE }
    | ',' { COMMA }
    | "()" { UNIT }
    | "::" { COLONCOLON }
    | ":" { COLON }
    | "+" { PLUS }
    | "(" { LPAR }
    | ")" { RPAR }
    | "[" { LBRA }
    | "]" { RBRA }
    | "*)" { end_flag := true; EOF }
    | ' '+ { rust_lexer end_flag lexbuf }

{
let parse_coma_string str = try coma [] (Lexing.from_string str) with _ -> []
}
