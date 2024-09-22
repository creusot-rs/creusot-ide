let _test_hacky_rs_parser () =
  Creusot_lsp.Hacky_rs_parser.rust (Lexing.from_string "pub fn main() {}\n /* z */ pub fn foo() {}")

let _test_why3session () =
  let root = Sys.argv.(1) in
  Creusot_lsp.Why3session.collect_sessions ~root;
  Printf.eprintf "%s" @@ Creusot_lsp.Why3session.debug_theories ()

let () = _test_hacky_rs_parser ();;
