let%expect_test _ =
  let open Hacky_coma_parser in
  let open Rust_syntax in
  let coma = parse_coma_string "module M_qyi1 (* T *)\nmodule M_z__y\n" in
  List.iter (fun { name; demangled; _ } -> Format.printf "Module %s: %a\n" name.ident fprint_def_path demangled) coma.State.modules;
  [%expect {|
    Module M_qyi1: impl{T}::M
    Module M_z__y: z::y::M
    |} ]

let%expect_test _ =
  let open Creusot_demangler in
  Option.iter print_segments (demangle_ident "M_qyi3__qy65z");
  [%expect {|
    ::impl{3}::A::M
  |}]

let%expect_test _ =
  let open Hacky_rs_parser in
  let lexbuf = Lexing.from_string "{rust|
  impl Node<K> where K::DeepModelTy : OrdLogic, {
    pub fn f() {}
  }" in
  let names = list_names lexbuf in
  List.iter (fun (def, _, _) -> Rust_syntax.print_def_path def) names;
  [%expect {| impl{Node<K>}::f::M |}]

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

let pp_range h loc =
  let (file, bl, bc, el, ec) = Why3.Loc.get loc in
  Format.fprintf h "%s#%d:%d-%d:%d" file bl bc el ec

let%test_unit _ =
  let open Why3findUtil in
  let open ProofPath in
  let env = get_test_env () in
  let goal i = {
    file = "../testdata/sum.coma";
    theory = "M_sum__sum_first_n";
    goal_info = {
      vc = "vc_sum_first_n'0";
      tactics = [("split_vc", i)];
    }
  } in
  let p = Why3.Loc.user_position in
  let expected = [|
    Some (p "../testdata/sum.coma" 61 118 61 129); (* TODO: locate type invariants *)
    Some (p "../testdata/sum.coma" 62 38 62 48);
    Some (p "../testdata/sum.coma" 77 126 77 136);
    Some (p "../../../creusot-contracts/src/std/iter.rs" 101 0 213 1);
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 8 16 8 65);
    Some (p "../testdata/sum.coma" 154 119 154 129);
    Some (p "sum.rs" 5 10 5 38);
    Some (p "../testdata/prelude/prelude.coma" 49 59 49 76); (* TODO: locate overflows *)
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 8 16 8 65);
    None;
  |] in
  for i = 0 to 15 do
    let loc = get_goal_loc env (goal i) in
    match expected.(i), loc with
    | None, None -> ()
    | None, Some actual -> failwith (Format.asprintf "Expected: None\n Actual: Some(%a)" pp_range actual)
    | Some expected, None -> failwith (Format.asprintf "Expected: Some(%a)\nActual: None" pp_range expected)
    | Some expected, Some actual ->
      if not (expected = actual) then
        failwith (Format.asprintf "Expected: %a\nActual: %a" pp_range expected pp_range actual)
  done