type _ Effect.t += Debug : string -> unit Effect.t

let debug (s : string) : unit = Effect.perform (Debug s)

let debug_handler_ (type a) (log : string -> unit) (f : unit -> a) : a =
  let open Effect.Deep in
  match_with f () {
    retc = (fun x -> x);
    exnc = raise;
    effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Debug s -> Some (fun (k : (a, _) continuation) -> log s; continue k ())
      | _ -> None)
  }

let silence_debug f = debug_handler_ (fun _ -> ()) f
let debug_stderr f = debug_handler_ prerr_endline f

let debug_handler (type a) (log : string -> unit Lwt.t) (f : unit -> a) : a Lwt.t =
  let open Effect.Deep in
  match_with f () {
    retc = (fun x -> Lwt.return x);
    exnc = raise;
    effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Debug s -> Some (fun (k : (a, _) continuation) -> Lwt.bind (log s) (continue k))
      | _ -> None)
  }
