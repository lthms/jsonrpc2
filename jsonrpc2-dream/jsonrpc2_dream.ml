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

type ('a, 'r) m = ('a -> 'r Lwt.t) -> 'r Lwt.t

let ( let*! ) (type a b r) (x : a Lwt.t) (f : a -> (b, r) m) : (b, r) m =
 fun k -> Lwt.bind x (fun x -> f x k)

let ( let* ) (type a b r) (k : (a, r) m) (f : a -> (b, r) m) : (b, r) m =
 fun k' -> k (fun x -> f x k')

let return (type a r) (x : a) : (a, r) m = fun k -> k x
let return_some x = return (Some x)
let cut (type a r) (x : r) : (a, r) m = fun _k -> Lwt.return x
let cut_some x = cut (Some x)
let run (k : ('a, 'r) m) = k Lwt.return

let cut_on_exn (type a r) ~on_exn:(f : exn -> r) (m : unit -> a Lwt.t) :
    (a, r) m =
  let*! r =
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* x = m () in
        Lwt.return (Ok x))
      (fun exn -> Lwt.return (Error exn))
  in
  match r with Ok r -> return r | Error exn -> cut (f exn)

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
  | #id as i -> k { request with request_id = i }

let from_json_rpc_response response =
  Dream.response ~code:200
  @@ Ezjsonm_encoding.(
       to_string_exn ~minify:true (response_encoding json json) response)

let from_json_rpc_batch l =
  Dream.response ~code:200
  @@ Ezjsonm_encoding.(
       to_string_exn ~minify:true (list (response_encoding json json)) l)

let request_method methods request =
  match Methods.find request.request_method methods with
  | m -> return m
  | exception Not_found ->
      cut_some
        (method_not_found_error request.request_id request.request_method)

let type_request m request =
  match
    Option.map
      Ezjsonm_encoding.(from_value_exn m.method_input_encoding)
      request.request_params
  with
  | exception _ -> cut_some (invalid_params request.request_id)
  | v -> return { request with request_params = v }

let run_handler m state typed_request =
  cut_on_exn
    ~on_exn:(fun _exn ->
      Some
        (internal_error typed_request.request_id
           "Unexpected error while processing request"))
    (fun () -> m.method_handler state typed_request.request_params)

let handler_request_object methods state untyped_request =
  run
  @@
  let* untyped_request = request_id untyped_request in
  let* (Method m) = request_method methods untyped_request in
  let* typed_request = type_request m untyped_request in
  let* typed_response = run_handler m state typed_request in
  let response = untype_response typed_request.request_id m typed_response in
  return_some response

let parse_body body =
  cut_on_exn ~on_exn:(fun _exn -> from_json_rpc_response parse_error)
  @@ fun () -> Lwt.return (Ezjsonm.from_string body)

let handler methods state request =
  let open Lwt.Syntax in
  let* body = Dream.body request in
  parse_body body @@ fun value ->
  match
    Ezjsonm_encoding.(from_value (batch_encoding (request_encoding json)) value)
  with
  | Some (Singleton untyped_request) -> (
      let* untyped_response =
        handler_request_object methods state untyped_request
      in
      match untyped_response with
      | Some untyped_response ->
          Lwt.return (from_json_rpc_response untyped_response)
      | None -> Lwt.return (Dream.response ~code:200 ""))
  | Some (Batch l) ->
      let* l = Lwt_list.filter_map_s (handler_request_object methods state) l in
      if l = [] then Lwt.return (Dream.response ~code:200 "")
      else Lwt.return (from_json_rpc_batch l)
  | None -> Lwt.return (from_json_rpc_response invalid_request)

let route methods state path = Dream.post path (handler methods state)

let force_params ?on_missing_params k = function
  | Some params -> k params
  | None ->
      Lwt.return
        (Error
           {
             error_code = Invalid_params;
             error_message = "Missing params";
             error_data = on_missing_params;
           })
