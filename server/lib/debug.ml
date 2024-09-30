type _ Effect.t += Debug : string -> unit Effect.t

let debug (s : string) : unit = Effect.perform (Debug s)

let debug_handler (type a) (f : unit -> a) (log : string -> unit Lwt.t) : a Lwt.t =
  let open Effect.Deep in
  match_with f () {
    retc = (fun x -> Lwt.return x);
    exnc = raise;
    effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | Debug s -> Some (fun (k : (a, _) continuation) -> Lwt.bind (log s) (continue k))
      | _ -> None)
  }
