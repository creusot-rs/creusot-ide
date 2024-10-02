type qualid =
    { unqual: string
    ; qualifier: string list (* In reverse order (inner modules first) *)
    }

type ty
  = App of qualid * ty list
  | Tup of ty list
  | Unit

type impl_subject
  = Trait of ty * ty
  | Inherent of ty

type def_path_item
  = Impl of impl_subject
  | Other of string
  | Unknown of string  (* stuff we don't know how to handle yet *)

type def_path = def_path_item list
