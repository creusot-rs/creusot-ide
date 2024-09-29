include Rust_syntax_types

let fprint_qualid h q =
  List.rev (q.unqual :: q.qualifier) |> String.concat "::" |> Printf.fprintf h "%s"

let rec fprint_list fprint sep h = function
  | [] -> ()
  | [x] -> fprint h x
  | x :: xs -> Printf.fprintf h "%a%s%a" fprint x sep (fprint_list fprint sep) xs

let rec fprint_ty h = function
  | App (q, []) -> fprint_qualid h q
  | App (q, ts) -> Printf.fprintf h "%a<%a>" fprint_qualid q (fprint_list fprint_ty ", ") ts
  | Unit -> Printf.fprintf h "()"

let fprint_impl_subject h = function
  | Trait (t1, t2) -> Printf.fprintf h "<%a as %a>" fprint_ty t1 fprint_ty t2
  | Inherent t -> fprint_ty h t

let fprint_def_path_item h = function
  | Impl i -> Printf.fprintf h "impl{%a}" fprint_impl_subject i
  | Other s -> Printf.fprintf h "%s" s
  
let fprint_def_path h =
  fprint_list fprint_def_path_item "::" h

let print_def_path = fprint_def_path stdout
