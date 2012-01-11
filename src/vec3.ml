(** a very simple 3D vector module *)

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
  val mapset : (scalar -> scalar) -> t -> unit
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

module Make ( T: Vec.SCALAR) : S with module Scalar = T and type scalar = T.t  =
struct

  module Scalar = T

  type scalar = Scalar.t
    
  type t = scalar array

  type tuple_t = scalar * scalar * scalar

  let ( + ) = T.add
  let ( - ) = T.sub
  let ( * ) = T.mul
  let ( / ) = T.div
  let abs   = T.abs

  let size = 3

  let init f : t =
    [| f 0 ; f 1 ; f 2 |]

  let fold_left f (acc : 'a) (v : t) =
    f (f (f acc v.(0)) v.(1)) v.(2)

  let fold_right f (v : t) (acc : 'a) =
    f (f (f acc v.(2)) v.(1)) v.(0)

  let get (v : t) i = v.(i)

  let set (v : t) i s = v.(i) <- s

  let map f (v : t) =
    [| f v.(0) ; f v.(1) ; f v.(2)  |]

  let map2 f (v1 : t) (v2 : t) = 
    [| f v1.(0) v2.(0) ; f v1.(1) v2.(1) ; f v1.(2) v2.(2) |]

  let map3 f (v1 : t) (v2 : t) (v3 : t) = 
    [| f v1.(0) v2.(0) v3.(0) ; f v1.(1) v2.(1) v3.(1) ; f v1.(2) v2.(2) v3.(2) |]

  let map4 f (v1 : t) (v2 : t) (v3 : t) (v4 : t) = 
    [| f v1.(0) v2.(0) v3.(0) v4.(0) ; f v1.(1) v2.(1) v3.(1) v4.(1) ; f v1.(2) v2.(2) v3.(2) v4.(2) |]

  let mapset f (v : t) =
    v.(0) <- f v.(0);
    v.(1) <- f v.(1);
    v.(2) <- f v.(2)

  let map2set f (v1 : t) (v2 : t) =
    v1.(0) <- f v1.(0) v2.(0);
    v1.(1) <- f v1.(1) v2.(1);
    v1.(2) <- f v1.(2) v2.(2)


  let map3set f (v1 : t) (v2 : t) (v3 : t) =
    v1.(0) <- f v1.(0) v2.(0) v3.(0);
    v1.(1) <- f v1.(1) v2.(1) v3.(1);
    v1.(2) <- f v1.(2) v2.(2) v3.(2)

  let map4set f (v1 : t) (v2 : t) (v3 : t) (v4 : t) =
    v1.(0) <- f v1.(0) v2.(0) v3.(0) v4.(0);
    v1.(1) <- f v1.(1) v2.(1) v3.(1) v4.(1);
    v1.(2) <- f v1.(2) v2.(2) v3.(2) v4.(2)

  let make (v : scalar) : t = 
    [| v ; v ; v |]

  let null () = 
    make T.zero

  let one  () = make T.one

  let unit i  = let u = null () in u.(i) <- T.one ; u

  let opp v   = map (fun x -> T.opp x) v

  let neg = opp

  let add (v1 : t) (v2 : t) : t = 
    init (fun i -> v1.(i) + v2.(i))

  let add3 v1 v2 v3 : t = 
    map3 (fun x y z -> x + y + z) v1 v2 v3

  let add4 v1 v2 v3 v4 : t = 
    map4 (fun x y z w -> x + y + z + w) v1 v2 v3 v4

  let sub (v1 : t) (v2 : t) : t = 
    init (fun i -> v1.(i) - v2.(i))

  let sub3 v1 v2 v3 : t = 
    map3 (fun x y z -> x - y - z) v1 v2 v3

  let sub4 v1 v2 v3 v4 : t = 
    map4 (fun x y z w -> x - y - z - w) v1 v2 v3 v4

  let scale (v1 : t) s : t = 
    init (fun i -> v1.(i) * s)

  let mul v1 v2 : t =
    map2 (fun x y -> x * y) v1 v2

  let muladd v1 s v2 : t =
    map2 (fun x y -> x * s + y) v1 v2

  let dot (v1 : t) (v2 : t) : scalar = v1.(0) * v2.(0) + v1.(1) * v2.(1) + v1.(2) * v2.(2) 

  (* 3D cross product *)
  let cross (v1 : t) (v2 : t) : t = [|
      (v1.(1) * v2.(2)) - (v1.(2) * v2.(1)) ;
      (v1.(2) * v2.(0)) - (v1.(0) * v2.(2)) ;
      (v1.(0) * v2.(1)) - (v1.(1) * v2.(0))
  |]

  let clone (v : t) = init (fun i -> v.(i))

  let copy (v1 : t) (v2 : t) = Array.blit v1 0 v2 0 size

  let blit = copy

  let to_tuple (v : t)  = (v.(0),v.(1),v.(2))

  let of_tuple (x,y,z) : t = [| x; y; z |]

  let random (v : t) : t = map (fun x -> if x = T.zero then T.zero else T.rand x) v 

  let modulo (v : t) m : t = map (fun x -> T.modulo x m) v

  let below_epsilon (v : t) = Array.fold_left (fun acc x -> acc && (abs x < T.epsilon)) true v

  let for_all f (v : t) = Array.fold_left (fun acc x -> acc && (f x)) true v

  let min (v1 : t) (v2 : t) = map2 min v1 v2 

  let max (v1 : t) (v2 : t) = map2 max v1 v2 

  let length (v : t) = T.sqrt (dot v v)

  let normalize (v : t) =
    let n = T.div T.one (length v)
    in 
      scale v n

  let to_string (v : t) =
    let to_string = T.to_string in
    "< "^(to_string v.(0))^" ; "^(to_string v.(1))^" ; "^(to_string v.(2))^" >"

  let make_system (v : t) : t * t * t =
    let zero = T.zero in
    let v' =
      let x,y,z = to_tuple v in
      if T.abs x > zero || T.abs y > zero
      then of_tuple (T.opp y, x, zero)
      else
	if T.abs z > zero
	then of_tuple (zero, z, T.opp y)
	else
	  invalid_arg "make_system : null vector"
    in
    let v'' = cross v' v in
    let v'  = cross v'' v in
    v', v, v''
      
end

module PFloat : Vec.SCALAR with type t = float = Primitives.Float
module PInt : Vec.SCALAR with type t = int = Primitives.Int

module Float : S with module Scalar = PFloat and type scalar = float = Make(PFloat)
module Int : S with module Scalar = PInt and type scalar = int = Make(PInt)
