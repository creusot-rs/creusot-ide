{
open Lsp.Types
open Rust_syntax
open Rust_parser
open Util.Lex

type loc_ident = { ident: string; loc: Location.t }

(* Info based on file alone *)
type mod_info = {
    name: loc_ident;
    demangled: def_path;
    loc: Location.t option;
}
module State = struct
    type t = {
        mutable modules: mod_info list;
        mutable locations: (Range.t * Location.t) list;
    }
end

(* the string is a relative path (relative to the why3session/proof.json) *)
let relative_loc string start_line start_col end_line end_col lexbuf =
    let start_line = int_of_string start_line - 1 in
    let start_col = int_of_string start_col in
    let end_line = int_of_string end_line - 1 in
    let end_col = int_of_string end_col in
    let range = Range.create
        ~start:(Position.create ~line:start_line ~character:start_col)
        ~end_:(Position.create ~line:end_line ~character:end_col) in
    let proofdir = Filename.chop_suffix lexbuf.Lexing.lex_curr_p.pos_fname ".coma" in
    let uri = DocumentUri.of_path (Filename.concat proofdir string) in
    Location.create ~uri ~range

let new_state () = State.{ modules = []; locations = [] }

let new_module ?impl name loc state =
    let open Creusot_demangler in
    let open State in
    match demangle_def_id ?impl name.ident with
    | Some demangled -> state.modules <- { name; demangled; loc } :: state.modules
    | None -> ()

let new_location range loc state =
    let open State in
    state.locations <- (range, loc) :: state.locations
}

let white = [' ' '\t' '\n' '\r']
let nonwhite = [^' ' '\t' '\n' '\r']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let string = '"' ([^ '"']* as string) '"'
let num = ['0'-'9']+

rule coma state = parse
    | white+ { line_incs lexbuf; coma state lexbuf }
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
        coma_module_meta state modident None lexbuf
    }
    | string ' '+ (num as start_line) ' '+ (num as start_col) ' '+ (num as end_line) ' '+ (num as end_col) {
        if String.length string > 0 then (
            let loc = relative_loc string start_line start_col end_line end_col lexbuf in
            new_location (range lexbuf) loc state;
        );
        coma state lexbuf
    }
    | _ { coma state lexbuf }
    | eof { () }

and coma_module_meta state modident loc = parse
    | ' '+ { coma_module_meta state modident loc lexbuf }
    | "[#" string ' '+ (num as start_line) ' '+ (num as start_col) ' '+ (num as end_line) ' '+ (num as end_col) ' '* ']' {
        let loc = relative_loc string start_line start_col end_line end_col lexbuf in
        coma_module_meta state modident (Some loc) lexbuf
    }
    | "[#" [^ '\n' ']']* ']' {
        coma_module_meta state modident loc lexbuf } (* skip attributes *)
    | "(*" ' '* {
        let end_flag = ref false in
        match Rust_parser.impl_subject0 (rust_lexer end_flag) lexbuf with
        | impl ->
            new_module ~impl modident loc state;
            coma state lexbuf
        | exception _ ->
            new_module modident loc state;
            if !end_flag then coma state lexbuf
            else coma_module_meta_end state lexbuf }
    | '\n' {
        (* Nothing right after the module name, go back to main loop *)
        new_line lexbuf;
        new_module modident loc state;
        coma state lexbuf }
    | eof {
        new_module modident loc state
    }

and coma_module_meta_end state = parse
    | "*)" { coma state lexbuf }

and rust_lexer end_flag = parse
    | "as" { AS } (* reminder: overlaps with ident! *)
    | '\'' ident { LIFETIME_OR_LABEL (Lexing.lexeme lexbuf) }
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
let parse_coma_string str =
    let state = new_state () in
    let reverse () = state.modules <- List.rev state.modules; state.locations <- List.rev state.locations in
    try
        coma state (Lexing.from_string str);
        reverse ();
        state
    with _ -> reverse (); state
}
