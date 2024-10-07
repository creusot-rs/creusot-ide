%{
    open Rust_syntax
%}

%token <string> IDENT
%token <string> LIFETIME_OR_LABEL
%token CONST
%token OCTOTHORPE
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
%token EQUALS
%token QUESTION
%token AS
%token FOR
%token WHERE
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
| ty_binders trait=ty FOR t=ty where_clause EOF { Trait (trait, t) }
| ty_binders t=ty where_clause EOF { Inherent t }

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

(* https://doc.rust-lang.org/stable/reference/items/generics.html#where-clauses *)
where_clause:
| { () }
| WHERE separated_trailing(COMMA, where_clause_item) { () }

where_clause_item:
| lifetime COLON lifetime_bounds { () }
| for_lifetimes? ty COLON type_param_bounds? { () }

(* https://doc.rust-lang.org/stable/reference/trait-bounds.html *)
type_param_bounds:
| nonempty_separated_trailing(PLUS, type_param_bound) { () }

type_param_bound:
| lifetime { () }
| trait_bound { () }

trait_bound:
| optional_bracket(LPAR, trait_bound_, RPAR) { () }

trait_bound_:
| QUESTION? for_lifetimes? type_path { () }

lifetime_bounds:
| separated_trailing(PLUS, lifetime) { () }

lifetime:
| LIFETIME_OR_LABEL { () }

%public optional_bracket(l, m, r):
| m { $1 }
| l m r { $2 }

for_lifetimes:
| FOR generic_params { () }

(* https://doc.rust-lang.org/stable/reference/items/generics.html *)
generic_params:
| LANGLE separated_trailing(COMMA, generic_param) RANGLE { () }

generic_param:
| outer_attribute param { () }

param:
| lifetime_param | type_param | const_param { () }

lifetime_param:
| LIFETIME_OR_LABEL preceded(COLON, lifetime_bounds)? { () }

type_param:
| IDENT preceded(COLON, type_param_bounds?)? preceded(EQUALS, ty)? { () }

const_param:
| CONST IDENT COLON ty { () }

(* https://doc.rust-lang.org/stable/reference/attributes.html *)
outer_attribute:
| OCTOTHORPE LBRA attr RBRA { () }

attr:
| todo { () }

todo:
| error { assert false }

type_path:
| qualid { () }

qualid:
| IDENT { { unqual = $1; qualifier = [] } }
| COLONCOLON IDENT { { unqual = $2; qualifier = [""] } }
| qualid COLONCOLON IDENT { let { unqual = i; qualifier = j } = $1 in { unqual = $3; qualifier = i :: j } }

(* Separated list with optional trailing separator *)
separated_trailing(sep, item):
| { [] }
| x=item { [x] }
| x=item sep y=separated_trailing(sep, item) { x :: y }

nonempty_separated_trailing(sep, item):
| x=item { [x] }
| x=item sep y=separated_trailing(sep, item) { x :: y }
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
    | WHERE -> "WHERE"
    | EOF -> "EOF"
    | AS -> "AS"
    | COLON -> "COLON"
    | PLUS -> "PLUS"
    | QUESTION -> "QUESTION"
    | OCTOTHORPE -> "OCTOTHORPE"
    | EQUALS -> "EQUALS"
    | CONST -> "CONST"
    | LIFETIME_OR_LABEL s -> Printf.sprintf "LIFETIME_OR_LABEL %s" s