let route methods state path =
  Dream.post path (fun request ->
      let open Lwt.Syntax in
      let* body = Dream.body request in
      let* response = Jsonrpc2_server_lwt.raw_handle methods state body in
      Lwt.return (Dream.response ~code:200 response))
