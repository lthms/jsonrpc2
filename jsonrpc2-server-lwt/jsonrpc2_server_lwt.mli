open Jsonrpc2_api

type ('state, 'input, 'output, 'error) handler =
  'state -> 'input option -> ('output, 'error Jsonrpc2_api.error) result Lwt.t

type 'state methods

val no_methods : 'state methods

val register :
  ('input, 'output, 'error) Jsonrpc2_api.t ->
  ('state, 'input, 'output, 'error) handler ->
  'state methods ->
  'state methods

val force_params :
  ?on_missing_params:'error ->
  ('state -> 'input -> ('output, 'error Jsonrpc2_api.error) result Lwt.t) ->
  ('state, 'input, 'output, 'error) handler

val handle :
  'state methods ->
  'state ->
  (Ezjsonm.value, [ id | `Notification ]) request_object batch ->
  (Ezjsonm.value, Ezjsonm.value) response_object batch option Lwt.t

val raw_handle : 'state methods -> 'state -> string -> string Lwt.t
