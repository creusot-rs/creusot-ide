(lang dune 2.7)
(name creusot-lsp)
(version dev)
(using menhir 2.1)

(package
 (name "creusot-lsp")
 (synopsis "Language server for Creusot")
 (depends
  "why3"
  "why3find"
  "ocaml-lsp-server"
  "lwt"
  "linol"
  "linol-lwt"
  "toml"
  "xmlm"
  "uri")
 (authors "Li-yao Xia"))
(generate_opam_files true)
