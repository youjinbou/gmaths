module type SCALAR =
sig
  type t
  val  zero      : t
  val  one       : t
  val  add       : t -> t -> t
  val  sub       : t -> t -> t
  val  mul       : t -> t -> t
  val  div       : t -> t -> t
  val  opp       : t -> t
  val  rand      : t -> t
  val  modulo    : t -> t -> t
  val  epsilon   : t
  val  abs       : t -> t
  val  sqrt      : t -> t
  val  to_string : t -> string
end

module type S =
sig

  module Scalar : SCALAR

  type scalar = Scalar.t

  type t
  type tuple_t

  (** vector dimension *)
  val size : int
    
  (** null vector *)
  val null : unit -> t

  (** < 1 ; 1 ... > *)
  val one  : unit -> t

  (** unit vector generator *)
  val unit : int -> t

  (** tuple conversion *)

  val of_tuple : tuple_t -> t
  val to_tuple : t -> tuple_t

  (** array conversion *)

  val of_array : scalar array -> t
  val to_array : t -> scalar array

  (** vector creation *)

  val init    : (int -> scalar) -> t
  val make    : scalar -> t

  (** accessors *)

  val get     : t -> int -> scalar
  val set     : t -> int -> scalar -> unit

  (** map operators *)

  val map     : (scalar -> scalar) -> t -> t
  val map2    : (scalar -> scalar -> scalar) -> t -> t -> t
  val map3    : (scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> t
  val map4    : (scalar -> scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> t -> t
  val mapset  : (scalar -> scalar) -> t -> unit
  val map2set : (scalar -> scalar -> scalar) -> t -> t -> unit
  val map3set : (scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> unit
  val map4set : (scalar -> scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> t -> unit

  (** opposit vector operator *)
  val opp     : t -> t

  (** additions *)

  val add     : t -> t -> t
  val add3    : t -> t -> t -> t
  val add4    : t -> t -> t -> t -> t
  val sub     : t -> t -> t
  val sub3    : t -> t -> t -> t
  val sub4    : t -> t -> t -> t -> t

  (** vector scaling *)
  val scale   : t -> scalar -> t

  (** this is actually scale and add *)
  val muladd  : t -> scalar -> t -> t

  (** product *)
  val mul     : t -> t -> t

  (** dot product *)
  val dot     : t -> t -> scalar

  (** cross product *)
  val cross   : t array -> t

  (** equivalent of map id *)
  val clone   : t -> t

  (** copy the first operand in the second *)
  val copy    : t -> t -> unit

  (** random vector generator *)
  val random : t -> t

  (** equivalent of map Scalar.mod *)
  val modulo : t -> scalar -> t

  (** check that the operand components are all below Scalar.epsilon in absolute value *)
  val below_epsilon : t -> bool

  (** fold operators *)

  val fold_left : ('a -> scalar -> 'a) -> 'a -> t -> 'a
  val fold_right : (scalar -> 'a -> 'a) -> t -> 'a -> 'a

  (** misc operators *)

  val for_all : (scalar -> bool) -> t -> bool
  val min     : t -> t -> t
  val max     : t -> t -> t

  (** string 'serialization' *)
  val to_string : t -> string
    
  (** vector lengths/norm, normalization *)

  val length : t -> scalar
  val normalize : t -> t

end
