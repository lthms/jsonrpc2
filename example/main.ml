open Jsonrpc2_api

module Api = struct
  let echo =
    make_specs "echo"
      ~input:Ezjsonm_encoding.(tup1 string)
      ~output:Ezjsonm_encoding.string ~error:Ezjsonm_encoding.null
end

module Handler = struct
  let echo =
    Jsonrpc2_server_lwt.force_params @@ fun () msg -> Lwt.return (Ok msg)
end

let () =
  let open Lwt.Syntax in
  let methods =
    Jsonrpc2_server_lwt.(no_methods |> register Api.echo Handler.echo)
  in
  Lwt_main.run
  @@
  let* () =
    Dream.serve ~port:8080 @@ Dream.logger
    @@ Dream.router [ Jsonrpc2_dream.route methods () "/" ]
  and* _ =
    let* () = Lwt_unix.sleep 2. in
    let* result =
      Jsonrpc2_http_client_lwt.rpc ~params:"Hello, world!"
        "http://localhost:8080/" Api.echo
    in
    match result with
    | Ok result ->
        Dream.log "%s\n%!" result;
        Lwt.return ()
    | Error { error_message; _ } ->
        Dream.log "Something went wrong (%s)" error_message;
        Lwt.return ()
  in
  Lwt.return ()
