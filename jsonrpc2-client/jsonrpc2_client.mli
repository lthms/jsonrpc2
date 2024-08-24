val rpc :
  ?params:'input ->
  string ->
  ('input, 'output, 'error) Jsonrpc2_api.t ->
  ('output, 'error Jsonrpc2_api.error) result Lwt.t

