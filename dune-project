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
  "uri"
  "yojson"
  "jsonm")
 (authors "Li-yao Xia")
 (maintainers "Li-yao Xia <lysxia@gmail.com>")
 (homepage "https://github.com/creusot-rs/creusot-ide"))
(generate_opam_files true)
