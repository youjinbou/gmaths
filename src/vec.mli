module type SCALAR =
sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val opp : t -> t
  val rand : t -> t
  val modulo : t -> t -> t
  val epsilon : t
  val abs : t -> t
  val sqrt : t -> t
  val to_string : t -> string
end
module type T =
sig
  module Scalar : SCALAR
  type scalar = Scalar.t
  type t
  type tuple_t
  val size : int
  val null : unit -> t
  val one : unit -> t
  val unit : int -> t
  val of_tuple : tuple_t -> t
  val to_tuple : t -> tuple_t
  val get : t -> int -> scalar
  val set : t -> int -> scalar -> unit
  val map : (scalar -> scalar) -> t -> t
  val map2 : (scalar -> scalar -> scalar) -> t -> t -> t
  val map3 : (scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> t
  val map4 :
    (scalar -> scalar -> scalar -> scalar -> scalar) ->
    t -> t -> t -> t -> t
  val mapset  : (scalar -> scalar) -> t -> unit
  val map2set : (scalar -> scalar -> scalar) -> t -> t -> unit
  val map3set : (scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> unit
  val map4set : (scalar -> scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> t -> unit
  val opp : t -> t
  val add : t -> t -> t
  val add3 : t -> t -> t -> t
  val add4 : t -> t -> t -> t -> t
  val sub : t -> t -> t
  val sub3 : t -> t -> t -> t
  val sub4 : t -> t -> t -> t -> t
  val scale : t -> scalar -> t
  val muladd : t -> scalar -> t -> t
  val dot : t -> t -> scalar
  val cross : t -> t -> t
  val copy : t -> t -> unit
  val clone : t -> t
  val random : t -> t
  val modulo : t -> scalar -> t
  val below_epsilon : t -> bool
  val for_all : (scalar -> bool) -> t -> bool
  val fold_left : ('a -> scalar -> 'a) -> 'a -> t -> 'a
  val fold_right : (scalar -> 'a -> 'a) -> t -> 'a -> 'a
  val to_string : t -> string
end
