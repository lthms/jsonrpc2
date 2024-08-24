type id = [ `Number of int | `String of string | `Null ]

let ranged_int m n =
  Ezjsonm_encoding.(satisfies (fun i -> m <= i && i <= n) int)

let id_encoding () =
  let open Ezjsonm_encoding in
  union
    [
      case (function `Number i -> Some i | _ -> None) (fun i -> `Number i) int;
      case
        (function `String s -> Some s | _ -> None)
        (fun s -> `String s)
        string;
      case (function `Null -> Some () | _ -> None) (fun () -> `Null) null;
    ]

type ('params, 'id) request_object = {
  request_method : string;
  request_params : 'params option;
  request_id : 'id;
}

let request_encoding params_encoding =
  let open Ezjsonm_encoding in
  conv
    (fun { request_method; request_params; request_id } ->
      ((), request_method, request_params, request_id))
    (fun ((), request_method, request_params, request_id) ->
      { request_method; request_params; request_id })
    (obj4
       (req "jsonrpc" (constant "2.0"))
       (req "method" string)
       (opt "params" params_encoding)
       (dft "id" (id_encoding ()) (`Notification :> _)))

type error_code =
  | Parse_error
  | Invalid_request
  | Method_not_found
  | Invalid_params
  | Internal_error
  | Server_error of int

let error_code_encoding =
  Ezjsonm_encoding.(
    union
      [
        case
          (function Server_error i -> Some i | _ -> None)
          (fun i -> Server_error i)
          (ranged_int (-32_099) (-32_000));
        enum int
          [
            (-32_700, Parse_error);
            (-32_600, Invalid_request);
            (-32_601, Method_not_found);
            (-32_602, Invalid_params);
            (-32_603, Internal_error);
          ];
      ])

type 'a error = {
  error_code : error_code;
  error_message : string;
  error_data : 'a option;
}

let error_encoding error_data_encoding =
  Ezjsonm_encoding.(
    conv
      (fun { error_code; error_message; error_data } ->
        (error_code, error_message, error_data))
      (fun (error_code, error_message, error_data) ->
        { error_code; error_message; error_data })
      (obj3
         (req "code" error_code_encoding)
         (req "message" string)
         (opt "data" error_data_encoding)))

type ('result, 'error) response_object =
  | Success of { result : 'result; id : id }
  | Failure of { error : 'error error; id : id }

let response_encoding result_encoding error_data_encoding =
  Ezjsonm_encoding.(
    union
      [
        case
          (function Success { result; id } -> Some (result, id) | _ -> None)
          (fun (result, id) -> Success { result; id })
          (obj2 (req "result" result_encoding) (req "id" (id_encoding ())));
        case
          (function Failure { error; id } -> Some (error, id) | _ -> None)
          (fun (error, id) -> Failure { error; id })
          (obj2
             (req "error" (error_encoding error_data_encoding))
             (req "id" (id_encoding ())));
      ])

let method_not_found_error id method_name =
  Failure
    {
      error =
        {
          error_code = Method_not_found;
          error_message = Format.sprintf "Method %s not found" method_name;
          error_data = None;
        };
      id;
    }

let invalid_request =
  Failure
    {
      error =
        {
          error_code = Invalid_request;
          error_message = "Request is not a valid Request object";
          error_data = None;
        };
      id = `Null;
    }

let parse_error =
  Failure
    {
      error =
        {
          error_code = Parse_error;
          error_message = "Parse error";
          error_data = None;
        };
      id = `Null;
    }

let invalid_params id =
  Failure
    {
      error =
        {
          error_code = Invalid_params;
          error_message = "Invalid params";
          error_data = None;
        };
      id;
    }

let internal_error id msg =
  Failure
    {
      error =
        {
          error_code = Internal_error;
          error_message = "Internal error";
          error_data = Some (Ezjsonm.string msg);
        };
      id;
    }

module type METHOD_SPECS = sig
  type input
  type output
  type error

  val name : string
  val input_encoding : input Ezjsonm_encoding.t
  val output_encoding : output Ezjsonm_encoding.t
  val error_encoding : error Ezjsonm_encoding.t
end

type ('input, 'output, 'error) method_specs =
  (module METHOD_SPECS
     with type input = 'input
      and type output = 'output
      and type error = 'error)

let make_specs (type input output error) name ~input ~output ~error :
    (input, output, error) method_specs =
  (module struct
    type nonrec input = input
    type nonrec output = output
    type nonrec error = error

    let name = name
    let input_encoding = input
    let output_encoding = output
    let error_encoding = error
  end)

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

type 'a batch = Singleton of 'a | Batch of 'a list

let batch_encoding data_encoding =
  Ezjsonm_encoding.(
    union
      [
        case
          (function Singleton x -> Some x | _ -> None)
          (fun x -> Singleton x)
          data_encoding;
        case
          (function Batch l -> Some l | _ -> None)
          (fun l -> Batch l)
          (satisfies (( <> ) []) (list data_encoding));
      ])

module Server = struct
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

  let register (type input output error)
      (module M : METHOD_SPECS
        with type input = input
         and type output = output
         and type error = error) method_handler table =
    let method_ =
      {
        method_name = M.name;
        method_input_encoding = M.input_encoding;
        method_output_encoding = M.output_encoding;
        method_error_encoding = M.error_encoding;
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
        | error ->
            Failure { error = { typed_error with error_data = error }; id })

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
      Ezjsonm_encoding.(
        from_value (batch_encoding (request_encoding json)) value)
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
        let* l =
          Lwt_list.filter_map_s (handler_request_object methods state) l
        in
        if l = [] then Lwt.return (Dream.response ~code:200 "")
        else Lwt.return (from_json_rpc_batch l)
    | None -> Lwt.return (from_json_rpc_response invalid_request)

  let route methods state path = Dream.post path (handler methods state)
end
