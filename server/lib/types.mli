(* Type definitions in their own module to not duplicate them in a .ml file. *)

type goal = { name: string; unproved_subgoals: int }

type theory_ident = string
type theory_info =
    {
        path: string;
        name: string;
        goals: goal list;
    }
type theories_map = (theory_ident, theory_info) Hashtbl.t
