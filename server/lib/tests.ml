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

let assert_equal pp x y =
  if not (x = y) then (
    Format.printf "Expected: %a\nActual: %a" pp x pp y;
    failwith "assert_equal failed")

let%test_unit "get_goal_loc" =
  let open Why3findUtil in
  let open ProofPath in
  let env = get_test_env () in
  let goal i = {
    file = "../testdata/sum.coma";
    theory = "Coma";
    goal_info = {
      vc = "vc_sum_first_n'0";
      tactics = [("split_vc", i)];
    }
  } in
  let p = Why3.Loc.user_position in
  let expected = [|
    Some (p "../testdata/sum.coma" 61 116 61 127); (* TODO: locate type invariants *)
    Some (p "../testdata/sum.coma" 62 36 62 46);
    Some (p "../testdata/sum.coma" 77 124 77 134);
    Some (p "../../../creusot-contracts/src/std/iter.rs" 101 0 213 1);
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 9 4 9 7);
    Some (p "sum.rs" 8 16 8 65);
    Some (p "../testdata/sum.coma" 154 117 154 127);
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
    assert_equal (Format.pp_print_option pp_range) expected.(i) loc
  done

let%test_unit "proof_info" =
  let open Why3findUtil in
  let env = get_test_env () in
  let info = get_proof_info env ~proof_file:"../testdata/sum/proof.json" ~coma_file:"../testdata/sum.coma" in
  let open ProofInfo in
  let expected = [
    "new 'start' type invariant";
    "new 'end' type invariant";
    "into_iter 'self' type invariant";
    "into_iter requires";
    "next 'self' type invariant";
    "integer overflow";
    "";
  ] in
  let actual =
    List.filter_map (fun goal -> if Option.is_some goal.range then None else Some goal.expl)
    info.unproved_goals in
  let pp_list h xs =
    let open Format in
    fprintf h "\n%a\n" (pp_print_list ~pp_sep:pp_print_newline pp_print_string) xs in
  assert_equal pp_list expected actual
