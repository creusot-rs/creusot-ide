let _test_hacky_rs_parser () =
  let names = Creusot_lsp.Hacky_rs_parser.list_names (Lexing.from_string "pub fn main() {}\n /* z */ pub fn foo() {}") in
  List.iter (fun (name, (start, stop)) ->
    Printf.eprintf "%s: %d:%d-%d:%d\n" name start.Lexing.pos_lnum (start.Lexing.pos_cnum - start.Lexing.pos_bol) stop.Lexing.pos_lnum (stop.Lexing.pos_cnum - stop.Lexing.pos_bol)) names

let _test_why3session () =
  let root = Sys.argv.(1) in
  Creusot_lsp.Why3session.collect_sessions ~root;
  Printf.eprintf "%s" @@ Creusot_lsp.Why3session.debug_theories ()

let () = _test_hacky_rs_parser ();;
