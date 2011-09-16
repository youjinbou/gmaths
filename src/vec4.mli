(** support for 4D vectors *)

module type S =
sig
  module Scalar : Vec.SCALAR
  type scalar = Scalar.t
  type t = scalar array
  type tuple_t = scalar * scalar * scalar * scalar

  (** vector dimensions *)
  val size : int

  (** vector creation *)
  val init : (int -> scalar) -> t

  (** homogeneous vector creation *)
  val init3 : (int -> scalar) -> t

  (** fold operators *)
  val fold_left : ('a -> scalar -> 'a) -> 'a -> t -> 'a
  val fold_right : ('a -> scalar -> 'a) -> t -> 'a -> 'a

  (** accessors *)

  val get : t -> int -> scalar
  val set : t -> int -> scalar -> unit

  (** map operators *)

  val map : (scalar -> 'a) -> t -> 'a array
  val map2 : (scalar -> scalar -> 'a) -> t -> t -> 'a array
  val map3 : (scalar -> scalar -> scalar -> 'a) -> t -> t -> t -> 'a array
  val map4 :
    (scalar -> scalar -> scalar -> scalar -> 'a) ->
    t -> t -> t -> t -> 'a array

  (** vector creation *)
  val make : scalar -> t

  (** null vector *)
  val null : unit -> t

  (** < 1 ; 1 ; 1 ; 1 > *)
  val one : unit -> t

  (** unit vector generator *)
  val unit : int -> t

  (** opposit vector operator *)
  val opp : t -> t

  (** alias of opp *)
  val neg : t -> t

  (** additions *)

  val add : t -> t -> t
  val add3 : t -> t -> t -> t
  val add4 : t -> t -> t -> t -> t
  val sub : t -> t -> t
  val sub3 : t -> t -> t -> t
  val sub4 : t -> t -> t -> t -> t

  (** vector scaling *)
  val scale : t -> scalar -> t

  (** equivalent of map (Scalar.mul) *)
  val mul : t -> t -> t

  (** this is actually scale and add *)
  val muladd : t -> scalar -> t -> t

  (** homogeneous dot products *)
  val dot3 : t -> t -> scalar

  (** 4D dot product *)
  val dot : t -> t -> scalar

  (** cross products *)

  val cross3d : t -> t -> t
  val cross : t -> t -> t

  (** equivalent of map id *)
  val clone : t -> t

  (** copy the first operand in the second *)
  val copy : t -> t -> unit

  (** alias of copy *)
  val blit : t -> t -> unit

  (** tuple conversion *)

  val to_tuple : t -> scalar * scalar * scalar * scalar
  val of_tuple : scalar * scalar * scalar * scalar -> t

  (** random vector generator *)
  val random : t -> t

  (** equivalent of map Scalar.mod *)
  val modulo : t -> scalar -> t

  (** check that the operand components are all below Scalar.epsilon in absolute value *)
  val below_epsilon : t -> bool

  (** misc operators *)

  val for_all : (scalar -> bool) -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
    
  (** vector lengths/norm, 4D and homogeneous, normalization *)

  val length : t -> scalar
  val length3 : t -> scalar

  val normalize : t -> t

  (** string 'serialization' *)
  val to_string : t -> string

  (** generate an orthonormal coord system using one vector *)
  val make_system : t -> t * t * t
end

module Make :
  functor (T : Vec.SCALAR) -> S with module Scalar = T and type scalar = T.t 

module PFloat : Vec.SCALAR with type t = float

module PInt : Vec.SCALAR with type t = int

module Float : S with module Scalar = PFloat and type scalar = float

module Int : S with module Scalar = PInt and type scalar = int
