{
type segment
    = Impl of string
    | Other of string

let fprint_segment h = function
    | Impl s -> Printf.fprintf h "::impl{%s}" s
    | Other s -> Printf.fprintf h "::%s" s

let print_segment = fprint_segment stdout

let print_segments s = List.iter print_segment s; Printf.printf "\n"

type mode
    = NewSegment of char list
    | EndSegment

exception DemangleFail
}

rule demangle_ mode acc = parse
| "qyi" (['0' - '9']+ as h) {
    match mode with
    | NewSegment [] -> demangle_ EndSegment (Impl h :: acc) lexbuf
    | _ -> failwith "demangle: \"qyi\" in the middle of segment" }
| "__" {
    match mode with
    | NewSegment [] -> failwith "demangle: empty segment"
    | NewSegment seg -> demangle_ (NewSegment []) (Other (String.of_seq (List.to_seq (List.rev seg))) :: acc) lexbuf
    | EndSegment -> demangle_ (NewSegment []) acc lexbuf
}
| "qy" (['0' - '9']+ as c) "z" {
    match mode with
    | EndSegment -> failwith "demangle: \"qy\" after segment"
    | NewSegment seg ->
        let c = Char.chr (int_of_string c) in (* TODO: overflow *)
        demangle_ (NewSegment (c :: seg)) acc lexbuf
}
| "qy" { failwith "demangle: unrecognized \"qy\"" }
| (_ as c) {
    match mode with
    | EndSegment -> failwith "demangle: character after segment"
    | NewSegment seg -> demangle_ (NewSegment (c :: seg)) acc lexbuf
}
| eof {
    List.rev @@ match mode with
    | NewSegment [] -> failwith "demangle: empty segment at end of string"
    | EndSegment -> acc
    | NewSegment seg -> Other (String.of_seq (List.to_seq (List.rev seg))) :: acc
}

and demangle0 = parse
| "M_" { demangle_ (NewSegment []) [] lexbuf }
| "T_" { demangle_ (NewSegment []) [] lexbuf }

{
    let demangle s = try Some (demangle0 (Lexing.from_string s)) with _ -> None
    let demangle_def_id ?impl s =
        let of_seg = function
            | Impl s -> (match impl with Some i -> Rust_syntax.Impl i | None -> failwith "missing impl")
            | Other s -> Other s in
        try Option.map (List.map of_seg) (demangle s) with
        | _ -> None

    let%expect_test _ =
        Option.iter print_segments (demangle "M_qyi3__qy65z");
        [%expect {|
            ::impl{3}::A
        |}]
}
