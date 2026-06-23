open Why3find

let () =
  let config = Config.load_config ~root:"." in
  (match config.packages with
  | [] -> Printf.eprintf "no package found in config \"why3find.json\", at least prelude is needed for creusot proofs\n"
  | _ -> ());
  let env = Why3find.Project.create ~config () in
  Why3.Whyconf.load_plugins @@ Why3.Whyconf.get_main env.why3.config;
  Why3.Loc.disable_warning @@ Why3.Loc.register_warning "axiom_abstract" Why3.Pp.empty_formatted;
  let file = Sys.argv.(1) in
  let dir, _lib = Wutil.filepath file in
  let theories, format = Wutil.load_theories env.why3.env file in
  let s = Why3find.Session.create
    ~dir
    ~file
    ~format
    theories
  in
  Session.theories s |> List.iter @@ fun theory ->
    Printf.printf "THEORY %s\n" (Session.thy_name theory);
    Session.split theory |> List.iter @@ fun goal ->
      Printf.printf "- GOAL %s\n" (Session.name goal);
      Printf.printf "  %s\n" (Session.hashname goal);
      let task = Session.task goal in
      Format.printf "    task:\n%a@." Why3.Pretty.print_sequent task
