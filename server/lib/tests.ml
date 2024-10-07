let%expect_test _ =
  let open Json_visitor in
  let visitor = { empty_visitor with
    visit_null = fun path _ -> print_endline (string_of_path path)
  } in
  visit_string visitor "[[], {\"k\": null}, null]";
  [%expect {|
    $.k[1]
    $[2]|}]

let%expect_test _ =
  let open Hacky_coma_parser in
  let open Rust_syntax in
  let coma = parse_coma_string "module M_qyi1 (* T *)\nmodule M_z__y\n" in
  List.iter (fun { name; demangled; _ } -> Format.printf "Module %s: %a\n" name.ident fprint_def_path demangled) coma.State.modules;
  [%expect {|
      Module M_qyi1: impl{T}
      Module M_z__y: z::y
  |} ]