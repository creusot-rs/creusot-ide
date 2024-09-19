let () =
  let root = Sys.argv.(1) in
  let package = Sys.argv.(2) in
  Creusot_lsp.collect_sessions ~root ~package;
  Creusot_lsp.debug_theories ()
