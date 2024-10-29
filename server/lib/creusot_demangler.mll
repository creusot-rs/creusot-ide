{
type segment
    = Impl of string
    | Closure of string
    | Other of string

let fprint_segment h = function
    | Impl s -> Format.fprintf h "::impl{%s}" s
    | Closure s -> Format.fprintf h "::closure{%s}" s
    | Other s -> Format.fprintf h "::%s" s

let print_segment = Format.printf "%a" fprint_segment

let print_segments s = List.iter print_segment s; Printf.printf "\n"
}

rule demangle_segment_ buffer = parse
| "qyi" (['0' - '9']+ as h) (("__" | eof) as e) { (Impl h, String.length e = 0) }
| "qyClosure" (['0' - '9']+ as i) (("__" | eof) as e) { (Closure i, String.length e = 0) }
| "qy" (['0' - '9']+ as c) "z" {
    let c = Char.chr (int_of_string c) in (* TODO: overflow *)
    Buffer.add_char buffer c;
    demangle_segment_ buffer lexbuf
}
| "qy" { failwith "demangle: unrecognized \"qy\"" }
| ("__" | eof) as e { (Other (Buffer.contents buffer), String.length e = 0) }
| (_ as c) {
    Buffer.add_char buffer c;
    demangle_segment_ buffer lexbuf }

and demangle_namespace = parse
| "M_" { "M" }
| "T_" { "T" }

{
    let demangle_ident0 lexbuf =
        let ns = demangle_namespace lexbuf in
        let buffer = Buffer.create 16 in
        let rec loop acc =
            let (seg, eof) = demangle_segment_ buffer lexbuf in
            Buffer.clear buffer;
            if eof then List.rev (Other ns :: seg :: acc)
            else loop (seg :: acc) in
        loop []

    let demangle_ident s = try Some (demangle_ident0 (Lexing.from_string s)) with _ -> None

    let demangle_path path =
        let path = Filename.chop_extension path in
        let path = String.split_on_char '/' path in
        let demangle_segment s = fst (demangle_segment_ (Buffer.create 16) (Lexing.from_string s)) in
        try
            Some (match List.rev path with
            | last :: rest ->
                let namespace, last = Util.split_first '_' last in
                let segs = List.map demangle_segment (last :: rest) in
                List.rev (Other namespace :: segs)
            | [] -> [])
        with _ -> None

    let demangled_def_id ?impl d =
        let of_seg = function
            | Impl _ -> (match impl with Some i -> Rust_syntax.Impl i | None -> failwith "missing impl")
            | Closure s -> Rust_syntax.Closure s
            | Other s -> Other s in
        try Some (List.map of_seg d) with
        | _ -> None
}
