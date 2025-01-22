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
  ("lsp" (>= 1.20))
  "lwt"
  "linol"
  "linol-lwt"
  "toml"
  "xmlm"
  "uri"
  "yojson"
  "ppx_deriving"
  "ppx_expect"
  "ppx_yojson_conv"
  "ppx_yojson_conv_lib"
  "jsonm")
 (authors "Li-yao Xia")
 (maintainers "Li-yao Xia <lysxia@gmail.com>")
 (homepage "https://github.com/creusot-rs/creusot-ide"))
(generate_opam_files true)
