(lang dune 3.13)
(name creusot-lsp)
(version dev)
(using menhir 3.0)

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
