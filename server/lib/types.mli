(* Type definitions in their own module to not duplicate them in a .ml file. *)

type goal = { goal_name: string }

type unproved_goals =
  | Unknown
  | Unproved of goal array

type theory_ident = string
type theory_info =
    {
        path: string;
        name: string;
        unproved_goals: unproved_goals;
    }
type theories_map = (theory_ident, theory_info) Hashtbl.t
