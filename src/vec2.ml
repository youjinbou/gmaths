(** a very simple éD vector module *)

(** module signature *)
module type S =
sig
  module Scalar : Vec.SCALAR

  type scalar = Scalar.t
  type t = scalar array
  type tuple_t = scalar * scalar

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
  val clone : t -> t
  val copy : t -> t -> unit
  val blit : t -> t -> unit
  val to_tuple : t -> scalar * scalar
  val of_tuple : scalar * scalar -> t
  val random : t -> t
  val modulo : t -> scalar -> t
  val below_epsilon : t -> bool
  val for_all : (scalar -> bool) -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val length : t -> scalar
  val normalize : t -> t
  val to_string : t -> string
  val make_system : t -> t * t
end

module Make ( T: Vec.SCALAR) : S with module Scalar = T and type scalar = T.t =
struct

  module Scalar = T
    
  type scalar = Scalar.t

  type t = T.t array

  type tuple_t = T.t * T.t

  let ( + ) = T.add
  let ( - ) = T.sub
  let ( * ) = T.mul
  let ( / ) = T.div
  let abs   = T.abs

  let size = 2

  let init f : t =
    [| f 0 ; f 1 |]

  let fold_left f (acc : 'a) (v : t) =
    f (f acc v.(0)) v.(1)

  let fold_right f (v : t) (acc : 'a) =
    f (f acc v.(1)) v.(0)

  let get (v : t) i = v.(i)

  let set (v : t) i s = v.(i) <- s

  let map f (v : t) : 'a array =
    [| f v.(0) ; f v.(1) |]

  let map2 f (v1 : t) (v2 : t) : 'a array = 
    [| f v1.(0) v2.(0) ; f v1.(1) v2.(1) |]

  let map3 f (v1 : t) (v2 : t) (v3 : t) : 'a array = 
    [| f v1.(0) v2.(0) v3.(0) ; f v1.(1) v2.(1) v3.(1) |]

  let map4 f (v1 : t) (v2 : t) (v3 : t) (v4 : t) : 'a array = 
    [| f v1.(0) v2.(0) v3.(0) v4.(0) ; f v1.(1) v2.(1) v3.(1) v4.(1) |]

  let make v : t = 
    [| v ; v |]

  let null () : t = 
    make T.zero

  let one  () : t = make T.one

  let unit i : t = let u = null () in u.(i) <- T.one ; u

  let opp v : t  = map (fun x -> T.opp x) v

  let neg = opp

  let add (v1 : t) (v2 : t) = 
    init (fun i -> v1.(i) + v2.(i))

  let add3 (v1 : t) (v2 : t) (v3 : t) = 
    map3 (fun x y z -> x + y + z) v1 v2 v3

  let add4 (v1 : t) (v2 : t) (v3 : t) (v4 : t) = 
    map4 (fun x y z w -> x + y + z + w) v1 v2 v3 v4


  let sub (v1 : t) (v2 : t) = 
    init (fun i -> v1.(i) - v2.(i))


  let sub3 (v1 : t) (v2 : t) (v3 : t) = 
    map3 (fun x y z -> x - y - z) v1 v2 v3

  let sub4 (v1 : t) (v2 : t) (v3 : t) (v4 : t) = 
    map4 (fun x y z w -> x - y - z - w) v1 v2 v3 v4


  let scale (v1 : t) s  : t = 
    init (fun i -> v1.(i) * s)


  let mul v1 v2 : t =
    map2 (fun x y -> x * y) v1 v2


  let muladd v1 s v2 : t =
    map2 (fun x y -> x * s + y) v1 v2

  let dot (v1 : t) (v2 : t) : scalar = v1.(0) * v2.(0) + v1.(1) * v2.(1)

  let clone (v : t) = init (fun i -> v.(i))

  let copy (v1 : t) (v2 : t) = Array.blit v1 0 v2 0 size

  let blit = copy

  let to_tuple (v : t) = v.(0),v.(1)

  let of_tuple (x,y) : t = [| x; y |]

  let random (v : t) : t = map (fun x -> if x = T.zero then T.zero else T.rand x) v 

  let modulo (v : t) m : t = map (fun x -> T.modulo x m) v

  let below_epsilon (v : t) = Array.fold_left (fun acc x -> acc && (abs x < T.epsilon)) true v

  (*
  let fold_left  : ('a -> T.t -> 'a) -> 'a -> t -> 'a = Array.fold_left

  let fold_right : (T.t -> 'a -> 'a) -> t -> 'a -> 'a = Array.fold_right
  *)

  let for_all f (v : t) = fold_left (fun acc x -> acc && (f x)) true v

  let min (v1 : t) (v2 : t) = map2 min v1 v2 

  let max (v1 : t) (v2 : t) = map2 max v1 v2 

  let length (v : t) = T.sqrt (dot v v)

  let normalize (v : t) =
    let n = T.div T.one (length v)
    in 
      scale v n

  let to_string (v : t) =
    let to_string = T.to_string in
    "< "^(to_string v.(0))^" ; "^(to_string v.(1))^" >"

  let make_system v =
    let x,y = to_tuple v in
    v, of_tuple (y, T.opp x)

end


module PFloat : Vec.SCALAR with type t = float = Primitives.Float
module PInt : Vec.SCALAR with type t = int = Primitives.Int

module Float : S with module Scalar = PFloat and type scalar = float = Make(PFloat)
module Int : S with module Scalar = PInt and type scalar = int = Make(PInt)
