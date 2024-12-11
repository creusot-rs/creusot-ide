open Lsp.Types
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
  val short_string_of_goal : goal -> string  (* Only subgoal numbers, no tactic names *)
  val string_of_qualified_goal : qualified_goal -> string
  val qualified_goal_to_json : qualified_goal -> Yojson.Safe.t
  val qualified_goal_of_json : Yojson.Safe.t -> qualified_goal option
end

open ProofPath

val parse_json : coma:string -> (theory -> unit) -> Jsonm.decoder -> unit
val read_proof_json : coma:string -> source -> theory list

val get_env : unit -> Why3find.Config.env
val get_test_env : unit -> Why3find.Config.env
val get_goal : Why3find.Config.env -> ProofPath.qualified_goal -> string option
val get_goal_loc : Why3find.Config.env -> ProofPath.qualified_goal -> Why3.Loc.position option

val loc_to_range : Why3.Loc.position -> Range.t

(* Exposed for testing *)
module ProofInfo : sig
    type goal = {
      range: Range.t option;
      expl: string;
      unproved_subgoals: ProofPath.goal ProofPath.with_theory list;
    }
    type t = {
      coma_file: string;
      proof_file: string;
      rust_file: string;
  
      (* Typically, location of the function declaration *)
      entity_range: Range.t;
  
      unproved_goals: goal list;
    }
  end
  
val get_proof_info : Why3find.Config.env -> proof_file:string -> coma_file:string -> ProofInfo.t
val create_proof_info : Why3find.Config.env -> proof_file:string -> coma_file:string -> unit
val get_diagnostics : rust_file:string -> Diagnostic.t list
val get_lenses : rust_file:string -> CodeLens.t list
val get_test_items : rust_file:string -> Test_api.test_item list
val refresh_info : rust_file:string -> unit
val add_coma2 : string -> unit
