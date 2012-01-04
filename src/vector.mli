module Scalar :
  sig
    type t = float
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

type scalar = float
type t = Vec4.Float.t

type tuple_t = scalar * scalar * scalar * scalar

val size : int
val init : (int -> scalar) -> t
val init3 : (int -> scalar) -> t
val fold_left : ('a -> scalar -> 'a) -> 'a -> t -> 'a
val fold_right : ('a -> scalar -> 'a) -> t -> 'a -> 'a
val get : t -> int -> scalar
val set : t -> int -> scalar -> unit
val map : (scalar -> 'a) -> t -> 'a array
val map2 : (scalar -> scalar -> 'a) -> t -> t -> 'a array
val map3 : (scalar -> scalar -> scalar -> 'a) -> t -> t -> t -> 'a array
val map4 :
  (scalar -> scalar -> scalar -> scalar -> 'a) ->
  t -> t -> t -> t -> 'a array
val make : scalar -> t
val one : unit -> t
val mul : t -> t -> t
val clone : t -> t
val copy : t -> t -> unit
val blit : t -> t -> unit
val to_tuple : t -> scalar * scalar * scalar * scalar
val of_tuple : scalar * scalar * scalar * scalar -> t
val random : t -> t
val modulo : t -> scalar -> t
val below_epsilon : t -> bool
val for_all : (scalar -> bool) -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
val to_string : t -> string
val make_system : t -> t * t * t
val vec3 : scalar * scalar * scalar -> t
val vec4 : scalar * scalar * scalar * scalar -> t
val alpha : t -> t -> float

val null : unit -> t
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
val muladd : t -> scalar -> t -> t
val dot : t -> t -> float
val cross : t -> t -> t
val length : t -> float
val normalize : t -> t
val distance : t -> t -> float
val unit_vector : t -> t -> t
