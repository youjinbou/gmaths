(** support modules for primitive types *)

module Float =
struct
  type t = float
  let  zero = 0.
  let  one  = 1.
  let  add  = ( +. )
  let  sub  = ( -. )
  let  mul  = ( *. )
  let  div  = ( /. )
  let  rand = Random.float
  let  opp x = -.x
  let  modulo x m = mod_float x m
  let  pi = acos(0.0) *. 2.0
  let  epsilon = epsilon_float
  let  acos    = acos
  let  asin    = asin
  let  cos     = cos
  let  sin     = sin
  let  power   = ( ** )
  let  abs     = abs_float
  let compare a b = let c = a -. b in if c < 0.0 then (-1) else if c > 0.0 then 1 else 0
  let  sqrt    = sqrt
  let  to_string = string_of_float
end

module Int   =
struct
  type t = int
  let  zero = 0
  let  one  = 1
  let  add  = ( + )
  let  sub  = ( - )
  let  mul  = ( * )
  let  div  = ( / )
  let  rand = Random.int
  let  opp x = -x
  let  modulo  = ( mod )
  let  epsilon = 0
  let  abs   = abs
  let  compare a b = a - b
  let  sqrt v  = int_of_float (sqrt ((float) v))
  let to_string = string_of_int
end
