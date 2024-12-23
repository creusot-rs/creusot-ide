open Lsp.Types

type source =
  | File of (* file name *) string
  | String of (* file name *) string * string

let file_name_of_source = function
  | File file | String (file, _) -> file

module Lex = struct
  (* When we just started a new line *)
  let new_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {
        pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum
    }

  (* Call this when [lexeme lexbuf] may contain newlines. *)
  let line_incs lexbuf =
    let splits = String.split_on_char '\n' (Lexing.lexeme lexbuf) in
    let n_lines = List.length splits - 1 in
    let len_last_line = String.length (List.hd (List.rev splits)) in
    if n_lines > 0 then
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- {
            pos with
            Lexing.pos_lnum = pos.Lexing.pos_lnum + n_lines;
            Lexing.pos_bol = pos.Lexing.pos_cnum - len_last_line
        }

  let range lexbuf =
    let start = lexbuf.Lexing.lex_start_p in
    let stop = lexbuf.Lexing.lex_curr_p in
    Range.create
      ~start:(Position.create ~line:start.Lexing.pos_lnum ~character:(start.Lexing.pos_cnum - start.Lexing.pos_bol))
      ~end_:(Position.create ~line:stop.Lexing.pos_lnum ~character:(stop.Lexing.pos_cnum - stop.Lexing.pos_bol))
end

module Async = struct
  type _ Effect.t += Async : 'a Lwt.t -> 'a Effect.t

  let async (t : 'a Lwt.t) : 'a = Effect.perform (Async t)

  let async_handler (type a) (f : unit -> a) : a Lwt.t =
    let open Effect.Deep in
    match_with f () {
      retc = (fun x -> Lwt.return x);
      exnc = raise;
      effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Async t -> Some (fun (k : (a, _) continuation) -> Lwt.bind t (continue k))
        | _ -> None)
    }
end

let walk_dir ?(exclude = []) dir =
  let (/) = Filename.concat in
  let rec walk acc path =
    try
      let dir = dir / path in
      if Sys.is_directory dir then
        let files = Sys.readdir dir in
        Array.fold_left (fun acc file -> walk acc (path / file)) acc files
      else path :: acc
    with Sys_error _ -> acc
  in
  match Sys.readdir dir with
  | top -> Array.fold_left (fun acc file ->
    if List.mem file exclude then acc
    else walk acc file) [] top
  | exception Sys_error _ -> []

let split_first c s =
  let i = String.index s c in
  (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let split_last c s =
  let i = String.rindex s c in
  (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let drop_prefix ~prefix str =
  if String.starts_with ~prefix str then String.sub str (String.length prefix) (String.length str - String.length prefix)
  else str

let drop_suffix ~suffix str =
  if String.ends_with ~suffix str then String.sub str 0 (String.length str - String.length suffix)
  else str
