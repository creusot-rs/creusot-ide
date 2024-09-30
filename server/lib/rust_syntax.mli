include module type of Rust_syntax_types

val fprint_def_path : Format.formatter -> def_path -> unit
val print_def_path : def_path -> unit
val string_of_def_path : def_path -> string