module type T = 
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

module Make ( T: T) =
struct

  module Scalar = T
    
  type t = T.t array

  type tuple_t = T.t * T.t

  let ( + ) = T.add
  let ( - ) = T.sub
  let ( * ) = T.mul
  let ( / ) = T.div
  let abs   = T.abs

  let size = 2

  let init f =
    [| f 0 ; f 1 |]

  let fold_left f acc v =
    f (f acc v.(0)) v.(1)

  let fold_right f v acc =
    f (f acc v.(1)) v.(0)

  let get v i = v.(i)

  let set v i s = v.(i) <- s

  let map f v1 =
    [| f v1.(0) ; f v1.(1) |]

  let map2 f v1 v2 = 
    [| f v1.(0) v2.(0) ; f v1.(1) v2.(1) |]

  let map3 f v1 v2 v3 = 
    [| f v1.(0) v2.(0) v3.(0) ; f v1.(1) v2.(1) v3.(1) |]

  let map4 f v1 v2 v3 v4 = 
    [| f v1.(0) v2.(0) v3.(0) v4.(0) ; f v1.(1) v2.(1) v3.(1) v4.(1) |]

  let make v = 
    [| v ; v |]

  let null () = 
    make T.zero

  let one  () = make T.one

  let unit i  = let u = null () in u.(i) <- T.one ; u

  let opp v   = map (fun x -> T.opp x) v

  let neg = opp

  let add v1 v2 = 
    init (fun i -> v1.(i) + v2.(i))


  let add3 v1 v2 v3 = 
    map3 (fun x y z -> x + y + z) v1 v2 v3

  let add4 v1 v2 v3 v4 = 
    map4 (fun x y z w -> x + y + z + w) v1 v2 v3 v4


  let sub v1 v2 = 
    init (fun i -> v1.(i) - v2.(i))


  let sub3 v1 v2 v3 = 
    map3 (fun x y z -> x - y - z) v1 v2 v3

  let sub4 v1 v2 v3 v4 = 
    map4 (fun x y z w -> x - y - z - w) v1 v2 v3 v4


  let scale v1 s = 
    init (fun i -> v1.(i) * s)


  let mul v1 v2 =
    map2 (fun x y -> x * y) v1 v2


  let muladd v1 s v2 =
    map2 (fun x y -> x * s + y) v1 v2

  let dot v1 v2 = v1.(0) * v2.(0) + v1.(1) * v2.(1)

  let clone v = init (fun i -> v.(i))

  let copy v1 v2 = Array.blit v1 0 v2 0 size

  let to_tuple v = v.(0),v.(1)

  let of_tuple (x,y) = [| x; y |]

  let random v = map (fun x -> if x = T.zero then T.zero else T.rand x) v 

  let modulo v m = map (fun x -> T.modulo x m) v

  let below_epsilon v = Array.fold_left (fun acc x -> acc && (abs x < T.epsilon)) true v

  (*
  let fold_left  : ('a -> T.t -> 'a) -> 'a -> t -> 'a = Array.fold_left

  let fold_right : (T.t -> 'a -> 'a) -> t -> 'a -> 'a = Array.fold_right
  *)

  let for_all f v = fold_left (fun acc x -> acc && (f x)) true v

  let min v1 v2 = map2 min v1 v2 

  let max v1 v2 = map2 max v1 v2 

  let length v = T.sqrt (dot v v)

  let normalize v =
    let n = T.div T.one (length v)
    in 
      scale v n

  let to_string v =
    let to_string = T.to_string in
    "< "^(to_string v.(0))^" ; "^(to_string v.(1))^" >"

end


module Float = Make(Primitives.Float)
module Int   = Make(Primitives.Int)
