type log_level =
  | Debug
  | Info
  | Warning
  | Error

let log_level = ref Info

let set_log_level level = log_level := level

let set_debug () = set_log_level Debug

let string_of_log_level = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warning -> "WARNING"
  | Error -> "ERROR"

let log (type a) (level : log_level) (fmt : (a, Format.formatter, unit) format) : a =
  if level >= !log_level then
    Format.eprintf ("[%s] " ^^ fmt ^^ "@.") (string_of_log_level level)
  else
    Format.ifprintf Format.err_formatter fmt

let debug (type a) (fmt : (a, Format.formatter, unit) format) : a =
  log Debug fmt

let info (type a) (fmt : (a, Format.formatter, unit) format) : a =
  log Info fmt

let warn (type a) (fmt : (a, Format.formatter, unit) format) : a =
  log Warning fmt

let error (type a) (fmt : (a, Format.formatter, unit) format) : a =
  log Error fmt
