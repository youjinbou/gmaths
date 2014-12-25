

module Period : sig
  val n : int
  val m : int
end

module State : sig
  type t
  val create : unit -> t
  val mt : t -> int64 array
  val idx : t -> int
  val incr_idx : t -> unit
  val reset_idx : t -> unit
  val init : t -> int64 -> t
  val make : int64 -> t
  val full_init : int array -> t
  val make_self_init : unit -> t
end

module Core : sig
  val rand : State.t -> int32
  val int32 : State.t -> int32 -> int32
end

module Rng : sig
  val int : State.t -> int -> int
  val int32 : State.t -> int32 -> int32
  val int64 : State.t -> int64 -> int64
  val nativeint : State.t -> nativeint -> nativeint
  val rawsingle : State.t -> float
  val rawdouble : State.t -> float
  val single : State.t -> float -> float
  val float : State.t -> float -> float
  val bool : State.t -> bool
end

(* state as a module-scoped value *)

module type DATA = sig
  val state : State.t
end

module Make : functor (D : DATA) -> sig
  val state : State.t
  val rand : unit -> int32
  val int32 : int32 -> int32
  val int64 : int64 -> int64
  val int : int -> int
  val nativeint : nativeint -> nativeint
  val float : float -> float
  val bool : bool
end
