type id = [ `Null | `Number of int | `String of string ]

type ('params, 'id) request_object = {
  request_method : string;
  request_params : 'params option;
  request_id : 'id;
}

type error_code =
  | Parse_error
  | Invalid_request
  | Method_not_found
  | Invalid_params
  | Internal_error
  | Server_error of int

type 'a error = {
  error_code : error_code;
  error_message : string;
  error_data : 'a option;
}

type ('result, 'error) response_object =
  | Success of { result : 'result; id : id }
  | Failure of { error : 'error error; id : id }

val method_not_found_error : id -> string -> ('a, 'b) response_object
val invalid_request : ('a, 'b) response_object
val parse_error : ('a, 'b) response_object
val invalid_params : id -> ('a, 'b) response_object
val internal_error : id -> string -> ('a, Ezjsonm.value) response_object

type 'a batch = Singleton of 'a | Batch of 'a list

val id_encoding : unit -> [> id ] Ezjsonm_encoding.t

val request_encoding :
  'a Ezjsonm_encoding.t ->
  ('a, [> `Notification | id ]) request_object Ezjsonm_encoding.t

val error_code_encoding : error_code Ezjsonm_encoding.t
val error_encoding : 'a Ezjsonm_encoding.t -> 'a error Ezjsonm_encoding.t

val response_encoding :
  'a Ezjsonm_encoding.t ->
  'b Ezjsonm_encoding.t ->
  ('a, 'b) response_object Ezjsonm_encoding.t

val batch_encoding : 'a Ezjsonm_encoding.t -> 'a batch Ezjsonm_encoding.t

type ('input, 'output, 'error) t

val make_specs :
  string ->
  input:'input Ezjsonm_encoding.t ->
  output:'output Ezjsonm_encoding.t ->
  error:'error Ezjsonm_encoding.t ->
  ('input, 'output, 'error) t

val method_name : ('input, 'output, 'error) t -> string

val method_input_encoding :
  ('input, 'output, 'error) t -> 'input Ezjsonm_encoding.t

val method_output_encoding :
  ('input, 'output, 'error) t -> 'output Ezjsonm_encoding.t

val method_error_encoding :
  ('input, 'output, 'error) t -> 'error Ezjsonm_encoding.t
