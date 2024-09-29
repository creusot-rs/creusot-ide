%{
    open Rust_syntax
%}

%token <string> IDENT
%token LANGLE
%token RANGLE
%token COMMA
%token UNIT
%token COLONCOLON
%token AS
%token EOF

%start ty0 impl_subject0

%type <impl_subject> impl_subject0
%type <ty> ty0 ty
%type <ty list> tys
%type <qualid> qualid

%%

impl_subject0:
| LANGLE ty AS ty RANGLE EOF { Trait ($4, $2) }
| ty EOF { Inherent $1 }

ty0:
| ty EOF { $1 }

ty:
| qualid { App ($1, []) }
| UNIT { Unit }
| qualid LANGLE tys RANGLE { App ($1, $3) }

tys:
| ty { [$1] }
| ty COMMA tys { $1 :: $3 }
| { [] }

qualid:
| IDENT { { unqual = $1; qualifier = [] } }
| qualid COLONCOLON IDENT { let { unqual = i; qualifier = j } = $1 in { unqual = $3; qualifier = i :: j } }
