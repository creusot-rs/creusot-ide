{
open Lexing
open Util.Lex
open Lsp.Types

type json_pathlet =
  | ArrayIndex of int
  | ObjectKey of string
type json_path = json_pathlet list

type visitor = {
  visit_null : json_path -> Range.t -> unit;
  visit_string : json_path -> Range.t -> string -> unit;
}

let empty_visitor = {
    visit_null = (fun _ _ -> ());
    visit_string = (fun _ _ _ -> ());
}

let string_of_pathlet = function
  | ArrayIndex i -> Printf.sprintf "[%d]" i
  | ObjectKey k -> Printf.sprintf ".%s" k

let string_of_path path =
    String.concat "" ("$" :: List.map string_of_pathlet path)

let to_position p =
  Position.create ~line:p.pos_lnum ~character:(p.pos_cnum - p.pos_bol)

let get_range lexbuf =
  let start = to_position lexbuf.lex_start_p in
  let end_ = to_position lexbuf.lex_curr_p in
  Range.create ~start ~end_
}

let white = [' ' '\t' '\n' '\r']
let nonwhite = [^' ' '\t' '\n' '\r']
let string_contents = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule json visitor path = parse
| white+ { line_incs lexbuf; json visitor path lexbuf }
| '"' (string_contents as raw_s) '"' { visitor.visit_string path (get_range lexbuf) raw_s }
| "null" { visitor.visit_null path (get_range lexbuf) }
| '[' { let rec go i =
          try closing_bracket lexbuf
          with Failure _ ->
            json visitor (ArrayIndex i :: path) lexbuf;
            let found_comma = comma_or_closing_bracket lexbuf in
            if found_comma then go (i + 1)
        in go 0 }
| '{' { json_object visitor path lexbuf }

and closing_bracket = parse
| white+ { line_incs lexbuf; closing_bracket lexbuf }
| ']' { () }

and comma_or_closing_bracket = parse
| white+ { line_incs lexbuf; comma_or_closing_bracket lexbuf }
| ',' { true }
| ']' { false }

and json_object visitor path = parse
| white+ { line_incs lexbuf; json_object visitor path lexbuf }
| '"' (string_contents as raw_key) '"' white* ':' {
    line_incs lexbuf;
    json visitor (ObjectKey raw_key :: path) lexbuf;
    json_object_comma visitor path lexbuf
}
| '}' { () }

and json_object_comma visitor path = parse
| white+ { line_incs lexbuf; json_object_comma visitor path lexbuf }
| ',' { json_object visitor path lexbuf } (* allows trailing commas *)
| '}' { () }

{
let visit_lexbuf visitor lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = 0 };
  json visitor [] lexbuf

let visit_file visitor path =
  let h = open_in path in
  let lexbuf = Lexing.from_channel h in
  visit_lexbuf visitor lexbuf;
  close_in h

let visit_string visitor s =
  let lexbuf = Lexing.from_string s in
  visit_lexbuf visitor lexbuf
}