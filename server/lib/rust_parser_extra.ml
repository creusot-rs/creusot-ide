open Rust_parser

let string_of_token = function
    | IDENT s -> Printf.sprintf "IDENT %s" s
    | LANGLE -> "LANGLE"
    | RANGLE -> "RANGLE"
    | LPAR -> "LPAR"
    | RPAR -> "RPAR"
    | LBRA -> "LBRA"
    | RBRA -> "RBRA"
    | COMMA -> "COMMA"
    | UNIT -> "UNIT"
    | COLONCOLON -> "COLONCOLON"
    | FOR -> "FOR"
    | WHERE -> "WHERE"
    | EOF -> "EOF"
    | AS -> "AS"
    | COLON -> "COLON"
    | PLUS -> "PLUS"
    | QUESTION -> "QUESTION"
    | OCTOTHORPE -> "OCTOTHORPE"
    | EQUALS -> "EQUALS"
    | CONST -> "CONST"
    | AMP -> "AMP"
    | MUT -> "MUT"
    | ARROW -> "ARROW"
    | LIFETIME_OR_LABEL s -> Printf.sprintf "LIFETIME_OR_LABEL %s" s