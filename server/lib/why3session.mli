val collect_sessions_for : root:string -> crate:string -> unit
val collect_sessions : root:string -> unit
val process_why3session_path : string -> unit
val get_theory : string -> Types.theory_info option
val debug_theories : unit -> string
val theory_of_path : string list -> string

val add_thy : string -> Types.theory_info -> unit
