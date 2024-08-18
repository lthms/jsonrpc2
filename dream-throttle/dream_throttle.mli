type t

val create : rate:int -> max:int -> n:int -> t
val middleware : (Dream.request -> string option Lwt.t) -> t -> Dream.middleware
val throttle : rejection:'a -> Dream.request -> (unit -> 'a Lwt.t) -> 'a Lwt.t
