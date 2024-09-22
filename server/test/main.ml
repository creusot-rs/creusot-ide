let () =
  let root = Sys.argv.(1) in
  Creusot_lsp.Why3session.collect_sessions ~root;
  Printf.eprintf "%s" @@ Creusot_lsp.Why3session.debug_theories ()
