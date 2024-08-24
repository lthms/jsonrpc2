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

module type METHOD_SPECS = sig
  type input
  type output
  type error

  val name : string
  val input_encoding : input Ezjsonm_encoding.t
  val output_encoding : output Ezjsonm_encoding.t
  val error_encoding : error Ezjsonm_encoding.t
end

type ('input, 'output, 'error) t =
  (module METHOD_SPECS
     with type input = 'input
      and type output = 'output
      and type error = 'error)

let make_specs (type input output error) name ~input ~output ~error :
    (input, output, error) t =
  (module struct
    type nonrec input = input
    type nonrec output = output
    type nonrec error = error

    let name = name
    let input_encoding = input
    let output_encoding = output
    let error_encoding = error
  end)

let method_name (type input output error) : (input, output, error) t -> string =
 fun (module M) -> M.name

let method_input_encoding (type input output error) :
    (input, output, error) t -> input Ezjsonm_encoding.t =
 fun (module M) -> M.input_encoding

let method_output_encoding (type input output error) :
    (input, output, error) t -> output Ezjsonm_encoding.t =
 fun (module M) -> M.output_encoding

let method_error_encoding (type input output error) :
    (input, output, error) t -> error Ezjsonm_encoding.t =
 fun (module M) -> M.error_encoding
