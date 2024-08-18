open Nightmare

module Api = struct
  let echo =
    make_specs "echo"
      ~input:Ezjsonm_encoding.(tup1 string)
      ~output:Ezjsonm_encoding.string ~error:Ezjsonm_encoding.null
end

module Handler = struct
  let echo () = function
    | Some msg -> Lwt.return (Ok msg)
    | None ->
        Lwt.return
          (Error
             {
               error_code = Invalid_params;
               error_message = "Missing params";
               error_data = Some ();
             })
end

let get_client request =
  let client = Dream.client request in
  let host =
    String.split_on_char ':' client
    |> List.rev |> List.tl |> List.rev |> String.concat ":"
  in
  Lwt.return (Some host)

let () =
  let throttler = Dream_throttle.create ~rate:50 ~max:20 ~n:10_000 in
  let methods = Server.(no_methods |> register Api.echo Handler.echo) in
  Lwt_main.run @@ Dream.serve ~port:8080 @@ Dream.logger
  @@ Dream_throttle.middleware get_client throttler
  @@ Dream.router [ Server.route methods () "/" ]
