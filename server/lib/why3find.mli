open Types
val read_proof_json : fname:string -> (string * theory_info) list
(** Read from a file *)

val parse_proof_json : fname:string -> string -> (string * theory_info) list
(** Read from a string (use fname for error reporting) *)
