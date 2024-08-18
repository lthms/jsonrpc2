module Throttler = Lwt_throttle.Make (struct
  include String

  let hash = Hashtbl.hash
end)

type t = Throttler.t

let create = Throttler.create
let throttler_field = Dream.new_field ~name:"throttler" ()

let middleware (get_client : Dream.request -> string option Lwt.t)
    (throttle : t) : Dream.middleware =
 fun handler request ->
  let open Lwt.Syntax in
  let* client = get_client request in
  (match client with
  | Some client -> Dream.set_field request throttler_field (throttle, client)
  | None -> ());
  handler request

let throttle ~rejection request k =
  let open Lwt.Syntax in
  match Dream.field request throttler_field with
  | Some (throttler, client) ->
      let* can_perform_request = Throttler.wait throttler client in
      if can_perform_request then k () else Lwt.return rejection
  | None -> k ()
