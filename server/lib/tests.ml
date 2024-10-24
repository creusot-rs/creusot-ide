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
  Option.iter print_segments (demangle_ident "M_qyi3__qy65z");
  [%expect {|
    ::impl{3}::A
  |}]

let%expect_test _ =
  let open Hacky_rs_parser in
  let lexbuf = Lexing.from_string "{rust|
  impl Node<K> where K::DeepModelTy : OrdLogic, {
    pub fn f() {}
  }" in
  let names = list_names lexbuf in
  List.iter (fun (def, _, _) -> Rust_syntax.print_def_path def) names;
  [%expect {| impl{Node<K>}::f |}]

let%expect_test _ =
  let open Why3findUtil in
  let open ProofPath in
  let json = {json|
    { "profile": {"prover": "z3", "size": 33, "time": 0.5},
      "proofs": {
        "theory1": {"vc1": {"tactic": "split_vc", "children":[null]}},
        "theory2": {"vc2": {"tactic": "split_vc", "children":[
          {"tactic": "split_vc", "children": [
            {"prover": "z3", "time": 0.5, "size": 33},
            null]},
          null]}}
      }
    }|json} in
  let theories = read_proof_json ~coma:"a.coma" (String ("proof.json", json)) in
  theories |> List.iter (fun theory ->
    Format.printf "%s:%s\n" theory.file theory.theory;
    theory.goal_info |> List.iter (fun { goal; _ } -> Format.printf "  %a\n" pp_goal goal));
  [%expect {|
    a.coma:theory1
      vc1
      vc1.split_vc.0
    a.coma:theory2
      vc2
      vc2.split_vc.0
      vc2.split_vc.0.split_vc.0
      vc2.split_vc.0.split_vc.1
      vc2.split_vc.1
    |}]
