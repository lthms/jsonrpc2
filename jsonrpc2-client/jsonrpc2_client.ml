open Jsonrpc2_api

let rpc (type input output error) :
    ?params:input -> string -> (input, output, error) t -> _ =
 fun ?params uri method_ ->
  let open Lwt.Syntax in
  let request =
    {
      request_method = method_name method_;
      request_params = params;
      request_id = `Null;
    }
  in
  let request =
    Ezjsonm_encoding.to_string_exn ~minify:true
      (request_encoding (method_input_encoding method_))
      request
  in
  let* result =
    Http_lwt_client.request ~meth:`POST ~body:request uri
      (fun _response acc part -> Lwt.return (String.concat "" [ acc; part ]))
      ""
  in
  match result with
  | Ok (_, response) -> (
      let response_object =
        Ezjsonm_encoding.from_string_exn
          (response_encoding
             (method_output_encoding method_)
             (method_error_encoding method_))
          response
      in
      Lwt.return
      @@
      match response_object with
      | Failure { error; _ } -> Error error
      | Success { result; _ } -> Ok result)
  | Error _ -> failwith "Something went wrong"
