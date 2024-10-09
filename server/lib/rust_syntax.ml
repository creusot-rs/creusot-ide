include Rust_syntax_types
open Format

let fprint_qualid h q =
  List.rev (q.unqual :: q.qualifier) |> String.concat "::" |> fprintf h "%s"

let rec fprint_list fprint sep h = function
  | [] -> ()
  | [x] -> fprint h x
  | x :: xs -> fprintf h "%a%s%a" fprint x sep (fprint_list fprint sep) xs

let rec fprint_ty h = function
  | App (q, []) -> fprint_qualid h q
  | App (q, ts) -> fprintf h "%a<%a>" fprint_qualid q (fprint_list fprint_generic_arg ", ") ts
  | Unit -> fprintf h "()"
  | Tup ts -> fprintf h "(%a)" (fprint_list fprint_ty ",") ts
and fprint_generic_arg h = function
  | LifetimeArg l -> fprintf h "%s" l
  | TypeArg t -> fprint_ty h t

let fprint_impl_subject h = function
  | Trait (t1, t2) -> fprintf h "<%a as %a>" fprint_ty t1 fprint_ty t2
  | Inherent t -> fprint_ty h t

let fprint_def_path_item h = function
  | Impl i -> fprintf h "impl{%a}" fprint_impl_subject i
  | Unknown s -> fprintf h "unknown{%s}" s
  | Other s -> fprintf h "%s" s

let fprint_def_path h =
  fprint_list fprint_def_path_item "::" h

let fprint_stdout f x = f Format.std_formatter x; Format.print_flush ()
let fprint_string f x =
  let buf = Buffer.create 16 in
  let h = Format.formatter_of_buffer buf in
  f h x; Format.pp_print_flush h ();
  Buffer.contents buf

let print_def_path = fprint_stdout fprint_def_path
let string_of_def_path = fprint_string fprint_def_path