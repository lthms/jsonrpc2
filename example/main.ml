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

let () =
  let methods = Server.no_methods |> Server.register Api.echo Handler.echo in
  Lwt_main.run @@ Dream.serve ~port:8080 @@ Dream.logger
  @@ Dream.router [ Server.route methods () "/" ]
