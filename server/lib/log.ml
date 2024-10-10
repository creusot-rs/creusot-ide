type log_level =
  | Debug
  | Info
  | Warning
  | Error

let log_level = ref Debug

let set_log_level level = log_level := level

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