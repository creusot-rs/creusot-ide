let find_rust_crate root =
  let (/) = Filename.concat in
  try
    match Toml.Parser.from_filename (root / "Cargo.toml") with
    | `Error _ -> None
    | `Ok file ->
      let name_opt = Toml.Lenses.(get file (field "package" |-- key "name" |-- string)) in
      name_opt
  with Sys_error _ -> None
