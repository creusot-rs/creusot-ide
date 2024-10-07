open Lsp.Types
open Types

val read_proof_json : fname:string -> (string * theory_info) list
(** Read from a file *)

val parse_proof_json : fname:string -> string -> (string * theory_info) list
(** Read from a string (use fname for error reporting) *)

module ProofPath : sig
  type lazy_tactic_path = (string option ref * int) list  (* tactic names might not have been resolved yet *)
  type tactic_path = (string * int) list  (* tactic * goalindex *)
  type 'a t0 = {
    file: string;
    theory: string;
    vc: string;
    tactics: 'a;
  }
  type t = tactic_path t0
  type lazy_t = lazy_tactic_path t0
  val pp : Format.formatter -> t -> unit
  val pp_lazy : Format.formatter -> lazy_t -> unit
  val to_json : t -> Yojson.Safe.t
  val of_json : Yojson.Safe.t -> t option
end

val parse_json : file:string -> (ProofPath.lazy_t -> unit) -> Jsonm.decoder -> unit

val get_goal : ProofPath.t -> string option
