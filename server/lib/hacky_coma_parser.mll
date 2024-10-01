{
open Lsp.Types
open Rust_syntax
open Rust_parser
open Util.Lex

type loc_ident = { ident: string; loc: Location.t }

let new_module ?impl modname acc =
    let open Creusot_demangler in
    (match demangle_def_id ?impl modname.ident with
    | Some path -> (modname, path) :: acc
    | None -> acc)
}

let white = [' ' '\t' '\n' '\r']
let nonwhite = [^' ' '\t' '\n' '\r']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule coma acc = parse
    | white+ { line_incs lexbuf; coma acc lexbuf }
    | "module" white+ (ident as modname) (' '* as spaces) {
        line_incs lexbuf;
        let end_col = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - String.length spaces in
        let begin_col = end_col - String.length modname in
        let modident = {
            ident = modname;
            loc = Location.create
                ~range:(Range.create
                    ~start:(Position.create ~line:lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum ~character:begin_col)
                     ~end_:(Position.create ~line:lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum ~character:end_col))
                ~uri:(DocumentUri.of_path lexbuf.Lexing.lex_curr_p.Lexing.pos_fname)
        } in
        coma_module_meta acc modident lexbuf
    }
    | _ { coma acc lexbuf }
    | eof { List.rev acc }

and coma_module_meta acc modident = parse
    | "(*" ' '* {
        let end_flag = ref false in
        match Rust_parser.impl_subject0 (rust_lexer end_flag) lexbuf with
        | impl ->
            let acc = new_module ~impl modident acc in
            coma acc lexbuf
        | exception _ ->
            let acc = new_module modident acc in
            if !end_flag then coma acc lexbuf
            else coma_module_meta_end acc lexbuf }
    | '\n' {
        new_line lexbuf;
        let acc = new_module modident acc in
        coma acc lexbuf }
    | eof {
        List.rev (new_module modident acc)
    }

and coma_module_meta_end acc = parse
    | "*)" { coma acc lexbuf }

and rust_lexer end_flag = parse
    | ident { IDENT (Lexing.lexeme lexbuf) } 
    | '<' { LANGLE }
    | '>' { RANGLE }
    | ',' { COMMA }
    | "()" { UNIT }
    | "::" { COLONCOLON }
    | "as" { AS }
    | "*)" { end_flag := true; EOF }
    | ' '+ { rust_lexer end_flag lexbuf }

{
    let parse_coma_string str = try coma [] (Lexing.from_string str) with _ -> []
    let%expect_test _ =
        let modules = parse_coma_string "module M_qyi1 (* T *)\nmodule M_z__y\n" in
        List.iter (fun (name, path) -> Format.printf "Module %s: %a\n" name.ident fprint_def_path path) modules;
        [%expect {|
            Module M_qyi1: impl{T}
            Module M_z__y: z::y
        |} ]
}
