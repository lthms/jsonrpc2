type ('state, 'input, 'output, 'error) handler =
  'state -> 'input option -> ('output, 'error Jsonrpc2_api.error) result Lwt.t

type 'state methods

val no_methods : 'state methods

val register :
  ('input, 'output, 'error) Jsonrpc2_api.t ->
  ('state, 'input, 'output, 'error) handler ->
  'state methods ->
  'state methods

val route : 'state methods -> 'state -> string -> Dream.route

val force_params :
  ?on_missing_params:'error ->
  ('input -> ('output, 'error Jsonrpc2_api.error) result Lwt.t) ->
  'input option ->
  ('output, 'error Jsonrpc2_api.error) result Lwt.t
