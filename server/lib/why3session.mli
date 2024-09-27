val collect_sessions : root:string -> unit
val process_why3session_path : string -> unit
val get_theory : string -> Types.theory_info option
val debug_theories : unit -> string
val get_package_name : unit -> string
val theory_of_path : string list -> string

val add_thy : string -> Types.theory_info -> unit
