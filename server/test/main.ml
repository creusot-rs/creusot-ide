let () =
  let root = Sys.argv.(1) in
  Creusot_lsp.collect_sessions ~root;
  Printf.eprintf "%s" @@ Creusot_lsp.debug_theories ()
