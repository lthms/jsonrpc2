type id = [ `Number of int | `String of string | `Null ]

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

type 'a batch = Singleton of 'a | Batch of 'a list

val batch_encoding : 'a Ezjsonm_encoding.t -> 'a batch Ezjsonm_encoding.t

type ('input, 'output, 'error) method_specs

val make_specs :
  string ->
  input:'input Ezjsonm_encoding.t ->
  output:'output Ezjsonm_encoding.t ->
  error:'error Ezjsonm_encoding.t ->
  ('input, 'output, 'error) method_specs

type ('state, 'input, 'output, 'error) handler =
  'state -> 'input option -> ('output, 'error error) result Lwt.t

module Server : sig
  type 'state methods

  val no_methods : 'state methods

  val register :
    ('input, 'output, 'error) method_specs ->
    ('state, 'input, 'output, 'error) handler ->
    'state methods ->
    'state methods

  val route : 'state methods -> 'state -> string -> Dream.route
end
