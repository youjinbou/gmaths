(** support modules for primitive types *)

module Float :
  sig
    type t = float
    val zero : float
    val one : float
    val add : float -> float -> float
    val sub : float -> float -> float
    val mul : float -> float -> float
    val div : float -> float -> float
    val rand : float -> float
    val opp : float -> float
    val modulo : float -> float -> float
    val pi : float
    val epsilon : float
    val acos : float -> float
    val asin : float -> float
    val cos : float -> float
    val sin : float -> float
    val power : float -> float -> float
    val abs : float -> float
    val compare : float -> float -> int
    val sqrt : float -> float
    val to_string : float -> string
  end
module Int :
  sig
    type t = int
    val zero : int
    val one : int
    val add : int -> int -> int
    val sub : int -> int -> int
    val mul : int -> int -> int
    val div : int -> int -> int
    val rand : int -> int
    val opp : int -> int
    val modulo : int -> int -> int
    val epsilon : int
    val abs : int -> int
    val compare : int -> int -> int
    val sqrt : int -> int
    val to_string : int -> string
  end
