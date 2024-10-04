open Lsp.Types
open Types

val read_proof_json : fname:string -> (string * theory_info) list
(** Read from a file *)

val parse_proof_json : fname:string -> string -> (string * theory_info) list
(** Read from a string (use fname for error reporting) *)

module ProofPath : sig
  type tactic_path = (string * int) list  (* tactic * goalindex *)
  type t = {
    file: string;
    theory: string;
    vc: string;
    tactics: tactic_path;
  }
  val to_json : t -> Yojson.Safe.t
  val of_json : Yojson.Safe.t -> t option
end

val get_goal : ProofPath.t -> string option
