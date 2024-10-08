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

let%expect_test _ =
  let open Creusot_demangler in
  Option.iter print_segments (demangle "M_qyi3__qy65z");
  [%expect {|
    ::impl{3}::A
  |}]

let%expect_test _ =
  let open Hacky_rs_parser in
  Debug.silence_debug (fun () ->
    let lexbuf = Lexing.from_string "{rust|
    impl Node<K> where K::DeepModelTy : OrdLogic, {
      pub fn f() {}
    }" in
    let names = list_names lexbuf in
    List.iter (fun (def, _) -> Rust_syntax.print_def_path def) names);
    [%expect {| impl{Node<K>}::f |}]

let%expect_test _ =
  let open Why3findUtil in
  let open ProofPath in
  let json = {json|{"proofs": {"theory1": {"vc1": {"tactic": "split_vc", "children":[null]}}}}|json} in
  let theories = read_proof_json (String ("a.coma", json)) in
  theories |> List.iter (fun theory ->
    Printf.printf "%s:%s\n" theory.file theory.theory;
    theory.goal_info |> List.iter (fun (goal, _) -> Format.printf "  %a\n" pp_goal goal));
  [%expect {|
    a.coma:theory1
      vc1.split_vc.0 |}]