open Lsp.Types
open Types
open Util

module ProofPath : sig
  type lazy_tactic_path = (string option ref * int) list  (* tactic names might not have been resolved yet *)
  type tactic_path = (string * int) list  (* tactic * goalindex *)
  type 'a _goal = {
    vc: string;
    tactics: 'a;
    }
  type goal = tactic_path _goal
  type lazy_goal = lazy_tactic_path _goal
  type 'a with_theory = {
    file: string;  (* The source coma file *)
    theory: string;
    goal_info: 'a;
  }
  type 'a goal_info = {
    goal: 'a;
    goal_range: Range.t;
    is_null: bool;
  }
  type info_goal = goal goal_info
  type theory = info_goal list with_theory
  type qualified_goal = goal with_theory

  val pp_theory : Format.formatter -> theory -> unit
  val pp_goal : Format.formatter -> goal -> unit
  val string_of_goal : goal -> string
  val string_of_qualified_goal : qualified_goal -> string
  val qualified_goal_to_json : qualified_goal -> Yojson.Safe.t
  val qualified_goal_of_json : Yojson.Safe.t -> qualified_goal option
end

open ProofPath

val parse_json : coma:string -> (theory -> unit) -> Jsonm.decoder -> unit
val read_proof_json : coma:string -> source -> theory list

val get_goal : ProofPath.qualified_goal -> string option
