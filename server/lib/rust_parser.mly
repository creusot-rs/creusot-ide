%{
    open Rust_syntax
%}

%token <string> IDENT
%token LANGLE
%token RANGLE
%token LPAR
%token RPAR
%token LBRA
%token RBRA
%token COMMA
%token UNIT
%token COLONCOLON
%token COLON
%token PLUS
%token AS
%token FOR
%token EOF

%start ty0 impl_subject0 impl_subject1

%type <impl_subject> impl_subject0 impl_subject1;
%type <ty> ty0 ty
%type <ty list> tys
%type <qualid> qualid

%%

impl_subject0:
| LANGLE ty AS ty RANGLE EOF { Trait ($4, $2) }
| ty EOF { Inherent $1 }

impl_subject1:
| ty_binders trait=ty FOR t=ty EOF { Trait (trait, t) }
| ty_binders t=ty EOF { Inherent t }

ty_binders:
| { () }
| LANGLE separated_list(COMMA, ty_binder) RANGLE { () }

ty_binder:
| IDENT { () }
| IDENT COLON separated_list(PLUS, ty) { () }

ty0:
| ty EOF { $1 }

ty:
| qualid { App ($1, []) }
| UNIT { Unit }
| qualid LANGLE tys RANGLE { App ($1, $3) }
| LPAR ts=separated_list(COMMA, ty) RPAR { Tup ts }

tys:
| ty { [$1] }
| ty COMMA tys { $1 :: $3 }
| { [] }

qualid:
| IDENT { { unqual = $1; qualifier = [] } }
| qualid COLONCOLON IDENT { let { unqual = i; qualifier = j } = $1 in { unqual = $3; qualifier = i :: j } }
%%

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
    | EOF -> "EOF"
    | AS -> "AS"
    | COLON -> "COLON"
    | PLUS -> "PLUS"