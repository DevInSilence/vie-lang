(** The result type representing either a success ([Ok]) or a failure ([Error]). *)
type ('a, 'b) t = Ok of 'a | Error of 'b

(** [result_ok r] returns [true] if [r] is [Ok], otherwise returns [false]. *)
val result_ok : ('a, 'b) t -> bool

(** [result_error r] returns [true] if [r] is [Error], otherwise returns [false]. *)
val result_error : ('a, 'b) t -> bool

(** [result_unwrap r] returns the value inside [Ok x], or raises [Failure] if [r] is [Error]. *)
val result_unwrap : ('a, 'b) t -> 'a

(** [result_unwrap_error r] returns the error inside [Error e], or raises [Failure] if [r] is [Ok]. *)
val result_unwrap_error : ('a, 'b) t -> 'b

(** [result_map f r] applies [f] to the value inside [Ok x], leaving [Error e] unchanged. *)
val result_map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
