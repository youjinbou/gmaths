(** module signature *)
module type S =
sig
  module Scalar : Vec.SCALAR

  type scalar = Scalar.t
  type t
  type tuple_t = scalar * scalar * scalar

  val size : int
  val init : (int -> scalar) -> t
  val fold_left : ('a -> scalar -> 'a) -> 'a -> t -> 'a
  val fold_right : ('a -> scalar -> 'a) -> t -> 'a -> 'a
  val get : t -> int -> scalar
  val set : t -> int -> scalar -> unit
  val map : (scalar -> 'a) -> t -> 'a array
  val map2 : (scalar -> scalar -> 'a) -> t -> t -> 'a array
  val map3 :
    (scalar -> scalar -> scalar -> 'a) -> t -> t -> t -> 'a array
  val map4 :
    (scalar -> scalar -> scalar -> scalar -> 'a) ->
    t -> t -> t -> t -> 'a array
  val mapset  : (scalar -> scalar) -> t -> unit
  val map2set : (scalar -> scalar -> scalar) -> t -> t -> unit
  val map3set : (scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> unit
  val map4set : (scalar -> scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> t -> unit
  val make : scalar -> t
  val null : unit -> t
  val one : unit -> t
  val unit : int -> t
  val opp : t -> t
  val neg : t -> t
  val add : t -> t -> t
  val add3 : t -> t -> t -> t
  val add4 : t -> t -> t -> t -> t
  val sub : t -> t -> t
  val sub3 : t -> t -> t -> t
  val sub4 : t -> t -> t -> t -> t
  val scale : t -> scalar -> t
  val mul : t -> t -> t
  val muladd : t -> scalar -> t -> t
  val dot : t -> t -> scalar
  val cross : t -> t -> t
  val clone : t -> t
  val copy : t -> t -> unit
  val blit : t -> t -> unit
  val to_tuple : t -> scalar * scalar * scalar
  val of_tuple : scalar * scalar * scalar -> t
  val random : t -> t
  val modulo : t -> scalar -> t
  val below_epsilon : t -> bool
  val for_all : (scalar -> bool) -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val length : t -> scalar
  val normalize : t -> t
  val to_string : t -> string
  val make_system : t -> t * t * t

end

module Make ( T: Vec.SCALAR) : S with module Scalar = T and type scalar = T.t

module PFloat : Vec.SCALAR with type t = float
module PInt : Vec.SCALAR with type t = int

module Float : S with module Scalar = PFloat and type scalar = float
module Int : S with module Scalar = PInt and type scalar = int
