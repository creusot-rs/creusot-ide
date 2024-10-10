type qualid =
    { unqual: string
    ; qualifier: string list (* In reverse order (inner modules first) *)
    }

type lifetime = string

type delim_token_tree =
  | Paren of token_tree list
  | Bracket of token_tree list
  | Curly of token_tree list
and token_tree =
  | Token of string
  | Delim of delim_token_tree

type attribute = Attr of qualid * delim_token_tree option

type ty =
  | Const of qualid
  | App of qualid * generic_arg list
  | Tup of ty list
  | Unit
  | Ref of lifetime option * ty
  | Fn of qualid * ty list * ty (* FnMut(t1,t2) -> r *)

and fn_arg =
  | FnArg of string option * ty

and generic_arg =
  | LifetimeArg of lifetime
  | TypeArg of ty

type impl_subject =
  | Trait of ty * ty  (* (trait, type) *)
  | Inherent of ty

type def_path_item =
  | Impl of impl_subject
  | Other of string
  | Unknown of string  (* stuff we don't know how to handle yet *)

type def_path = def_path_item list
