
let package_name = ref ""
let get_package_name () = !package_name

let find_rust_crate root =
  let (/) = Filename.concat in
  try
    match Toml.Parser.from_filename (root / "Cargo.toml") with
    | `Error _ -> None
    | `Ok file ->
      let name_opt = Toml.Lenses.(get file (field "package" |-- key "name" |-- string)) in
      Option.iter (fun name -> package_name := name) name_opt;
      name_opt
  with Sys_error _ -> None
