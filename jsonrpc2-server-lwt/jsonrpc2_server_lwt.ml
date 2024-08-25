open Jsonrpc2_api

type ('state, 'input, 'output, 'error) handler =
  'state -> 'input option -> ('output, 'error error) result Lwt.t

type ('state, 'input, 'output, 'error) method_ = {
  method_name : string;
  method_input_encoding : 'input Ezjsonm_encoding.t;
  method_output_encoding : 'output Ezjsonm_encoding.t;
  method_error_encoding : 'error Ezjsonm_encoding.t;
  method_handler : ('state, 'input, 'output, 'error) handler;
}

type 'state t = Method : ('state, 'input, 'output, 'error) method_ -> 'state t

module Methods = Map.Make (String)

type 'state methods = 'state t Methods.t

let no_methods = Methods.empty

let register specs method_handler table =
  let method_ =
    {
      method_name = method_name specs;
      method_input_encoding = method_input_encoding specs;
      method_output_encoding = method_output_encoding specs;
      method_error_encoding = method_error_encoding specs;
      method_handler;
    }
  in
  Methods.add method_.method_name (Method method_) table

let untype_response id m = function
  | Ok typed_result -> (
      match
        Ezjsonm_encoding.to_value_exn m.method_output_encoding typed_result
      with
      | exception _ -> internal_error id "Could not serialize method result"
      | result -> Success { result; id })
  | Error ({ error_data; _ } as typed_error) -> (
      match
        Option.map
          (Ezjsonm_encoding.to_value_exn m.method_error_encoding)
          error_data
      with
      | exception _ -> internal_error id "Could not serialize method error"
      | error -> Failure { error = { typed_error with error_data = error }; id }
      )

let cut_on_exn (type a r) ~on_exn:(f : exn -> r) (m : unit -> a Lwt.t) k =
  let open Lwt.Syntax in
  let* r =
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* x = m () in
        Lwt.return (Ok x))
      (fun exn -> Lwt.return (Error exn))
  in
  match r with Ok r -> k r | Error exn -> Lwt.return (f exn)

let request_id request k =
  let open Lwt.Syntax in
  match request.request_id with
  | `Notification ->
      Lwt.dont_wait
        (fun () ->
          let* _ = k { request with request_id = (`Null :> id) } in
          Lwt.return ())
        ignore;
      Lwt.return None
  | #id as i ->
      let* res = k { request with request_id = i } in
      Lwt.return_some res

let from_json_rpc_response response =
  Ezjsonm_encoding.(
    to_string_exn ~minify:true (response_encoding json json) response)

let from_json_rpc_batch l =
  Ezjsonm_encoding.(
    to_string_exn ~minify:true (list (response_encoding json json)) l)

let request_method methods request k =
  match Methods.find request.request_method methods with
  | m -> k m
  | exception Not_found ->
      Lwt.return
        (method_not_found_error request.request_id request.request_method)

let type_request m request k =
  match
    Option.map
      Ezjsonm_encoding.(from_value_exn m.method_input_encoding)
      request.request_params
  with
  | exception _ -> Lwt.return (invalid_params request.request_id)
  | v -> k { request with request_params = v }

let run_handler m state typed_request =
  cut_on_exn
    ~on_exn:(fun _exn ->
      internal_error typed_request.request_id
        "Unexpected error while processing request")
    (fun () -> m.method_handler state typed_request.request_params)

let handler_request_object methods state untyped_request =
  request_id untyped_request @@ fun untyped_request ->
  request_method methods untyped_request @@ fun (Method m) ->
  type_request m untyped_request @@ fun typed_request ->
  run_handler m state typed_request @@ fun typed_response ->
  Lwt.return (untype_response typed_request.request_id m typed_response)

let parse_body body =
  cut_on_exn ~on_exn:(fun _exn -> from_json_rpc_response parse_error)
  @@ fun () -> Lwt.return (Ezjsonm.from_string body)

let handle methods state =
  let open Lwt.Syntax in
  function
  | Singleton untyped_request ->
      let* res = handler_request_object methods state untyped_request in
      Lwt.return (Option.map (fun res -> Singleton res) res)
  | Batch l ->
      let* l = Lwt_list.filter_map_s (handler_request_object methods state) l in
      if l = [] then Lwt.return None else Lwt.return (Some (Batch l))

let raw_handle methods state body =
  let open Lwt.Syntax in
  parse_body body @@ fun value ->
  match
    Ezjsonm_encoding.(from_value (batch_encoding (request_encoding json)) value)
  with
  | Some request -> (
      let* untyped_response = handle methods state request in

      match untyped_response with
      | Some (Singleton untyped_response) ->
          Lwt.return (from_json_rpc_response untyped_response)
      | Some (Batch l) -> Lwt.return (from_json_rpc_batch l)
      | None -> Lwt.return "")
  | None -> Lwt.return (from_json_rpc_response invalid_request)

let force_params ?on_missing_params k state = function
  | Some params -> k state params
  | None ->
      Lwt.return
        (Error
           {
             error_code = Invalid_params;
             error_message = "Missing params";
             error_data = on_missing_params;
           })
