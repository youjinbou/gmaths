(** a very simple 4D vector module *)

module type S =
sig
  module Scalar : Vec.SCALAR
  type scalar = Scalar.t
  type t
  type tuple_t = scalar * scalar * scalar * scalar

  (** vector dimensions *)
  val size : int

  (** null vector *)
  val null : unit -> t

  (** < 1 ; 1 ; 1 ; 1 > *)
  val one : unit -> t

  (** unit vector generator *)
  val unit : int -> t

  (** tuple conversion *)

  val to_tuple : t -> scalar * scalar * scalar * scalar
  val of_tuple : scalar * scalar * scalar * scalar -> t

  (** array conversion *)

  val of_array : scalar array -> t
  val to_array : t -> scalar array

  (** vector creation *)

  val init : (int -> scalar) -> t
  val make : scalar -> t

  (** accessors *)

  val get : t -> int -> scalar
  val set : t -> int -> scalar -> unit

  (** map operators *)

  val map  : (scalar -> scalar) -> t -> t
  val map2 : (scalar -> scalar -> scalar) -> t -> t -> t
  val map3 : (scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> t
  val map4 :
    (scalar -> scalar -> scalar -> scalar -> scalar) ->
    t -> t -> t -> t -> t

  val mapset  : (scalar -> scalar) -> t -> unit
  val map2set : (scalar -> scalar -> scalar) -> t -> t -> unit
  val map3set : (scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> unit
  val map4set : (scalar -> scalar -> scalar -> scalar -> scalar) -> t -> t -> t -> t -> unit


  (** opposit vector operator *)
  val opp : t -> t

  (** additions *)

  val add  : t -> t -> t
  val add3 : t -> t -> t -> t
  val add4 : t -> t -> t -> t -> t
  val sub  : t -> t -> t
  val sub3 : t -> t -> t -> t
  val sub4 : t -> t -> t -> t -> t

  (** vector scaling *)
  val scale : t -> scalar -> t

  (** equivalent of map (Scalar.mul) *)
  val mul : t -> t -> t

  (** this is actually scale and add *)
  val muladd : t -> scalar -> t -> t

  (** 4D dot product *)
  val dot : t -> t -> scalar

  (** cross product *)
  val cross : t array -> t

  (** equivalent of map id *)
  val clone : t -> t

  (** copy the first operand in the second *)
  val copy : t -> t -> unit


  (** random vector generator *)
  val random : t -> t

  (** equivalent of map Scalar.mod *)
  val modulo : t -> scalar -> t

  (** check that the operand components are all below Scalar.epsilon in absolute value *)
  val below_epsilon : t -> bool

  (** fold operators *)

  val fold_left  : ('a -> scalar -> 'a) -> 'a -> t -> 'a
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

module Make ( T: Vec.SCALAR) : S with module Scalar = T and type scalar = T.t  =
struct

  module Scalar = T

  type scalar = Scalar.t
    
  type t = scalar array

  type tuple_t = scalar * scalar * scalar * scalar

  let ( +. ) = T.add
  let ( -. ) = T.sub
  let ( *. ) = T.mul
  let ( /. ) = T.div
  let abs   = T.abs

  let size = 4

  let init f : t =
    [| f 0 ; f 1 ; f 2 ; f 3 |]

  let init3 f : t =
    [| f 0 ; f 1 ; f 2 ; T.one |]

  let fold_left f (acc : 'a) (v : t) =
    f (f (f (f acc v.(0)) v.(1)) v.(2)) v.(3)

  let fold_right f (v : t) (acc : 'a) =
    f v.(0) (f v.(1) (f v.(2) (f v.(3) acc)))

  let get (v : t) i = v.(i)

  let set (v : t) i s = v.(i) <- s

  let map f (v : t) =
    [| f v.(0) ; f v.(1) ; f v.(2) ; f v.(3) |]

  let map2 f (v1 : t) (v2 : t) = 
    [| f v1.(0) v2.(0) ; f v1.(1) v2.(1) ; f v1.(2) v2.(2) ; f v1.(3) v2.(3) |]

  let map3 f (v1 : t) (v2 : t) (v3 : t) = 
    [| f v1.(0) v2.(0) v3.(0) ; f v1.(1) v2.(1) v3.(1) ; f v1.(2) v2.(2) v3.(2)  ; f v1.(3) v2.(3) v3.(3) |]

  let map4 f (v1 : t) (v2 : t) (v3 : t) (v4 : t) = 
    [| f v1.(0) v2.(0) v3.(0) v4.(0) ; f v1.(1) v2.(1) v3.(1) v4.(1) ; f v1.(2) v2.(2) v3.(2) v4.(2) ; f v1.(3) v2.(3) v3.(3) v4.(3) |]

  let mapset f (v : t) =
    v.(0) <- f v.(0);
    v.(1) <- f v.(1);
    v.(2) <- f v.(2);
    v.(3) <- f v.(3)

  let map2set f (v1 : t) (v2 : t) =
    v1.(0) <- f v1.(0) v2.(0);
    v1.(1) <- f v1.(1) v2.(1);
    v1.(2) <- f v1.(2) v2.(2);
    v1.(3) <- f v1.(3) v2.(3)

  let map3set f (v1 : t) (v2 : t) (v3 : t) =
    v1.(0) <- f v1.(0) v2.(0) v3.(0);
    v1.(1) <- f v1.(1) v2.(1) v3.(1);
    v1.(2) <- f v1.(2) v2.(2) v3.(2);
    v1.(3) <- f v1.(3) v2.(3) v3.(3)

  let map4set f (v1 : t) (v2 : t) (v3 : t) (v4 : t) =
    v1.(0) <- f v1.(0) v2.(0) v3.(0) v4.(0);
    v1.(1) <- f v1.(1) v2.(1) v3.(1) v4.(1);
    v1.(2) <- f v1.(2) v2.(2) v3.(2) v4.(2);
    v1.(3) <- f v1.(3) v2.(3) v3.(3) v4.(3)

  let make (v : scalar) : t = 
    [| v ; v ; v ; v |]

  let null () = 
    make T.zero

  let one  () = make T.one

  let unit : int -> t = 
    let one = T.one and zero = T.zero in function
      | 0 -> [| one ; zero ; zero ; zero |]
      | 1 -> [| zero ; one ; zero ; zero |]
      | 2 -> [| zero ; zero ; one ; zero |]
      | 3 -> [| zero ; zero ; zero ; one |]
      | _ -> invalid_arg "Vector.unit"

  let opp v   = map (fun x -> T.opp x) v

  let add (v1 : t) (v2 : t) : t = 
    init (fun i -> v1.(i) +. v2.(i))

  let add3 v1 v2 v3 : t = 
    map3 (fun x y z -> x +. y +. z) v1 v2 v3

  let add4 v1 v2 v3 v4 : t = 
    map4 (fun x y z w -> x +. y +. z +. w) v1 v2 v3 v4

  let sub (v1 : t) (v2 : t) : t = 
    init (fun i -> v1.(i) -. v2.(i))

  let sub3 v1 v2 v3 : t = 
    map3 (fun x y z -> x -. y -. z) v1 v2 v3

  let sub4 v1 v2 v3 v4 : t = 
    map4 (fun x y z w -> x -. y -. z -. w) v1 v2 v3 v4

  let scale (v1 : t) s : t = 
    init (fun i -> v1.(i) *. s)

  let mul v1 v2 : t =
    map2 (fun x y -> x *. y) v1 v2

  let muladd v1 s v2 : t =
    map2 (fun x y -> x *. s +. y) v1 v2

  (* 4D dot product ! *)
  let dot (v1 : t) (v2 : t) : scalar = v1.(0) *. v2.(0) +. v1.(1) *. v2.(1) +. v1.(2) *. v2.(2) +. v1.(3) *. v2.(3)

  (* 3D cross product *)
  let cross3d (av : t array) : t = 
    let v1, v2 = av.(0), av.(1) in [|
      (v1.(1) *. v2.(2)) -. (v1.(2) *. v2.(1)) ;
      (v1.(2) *. v2.(0)) -. (v1.(0) *. v2.(2)) ;
      (v1.(0) *. v2.(1)) -. (v1.(1) *. v2.(0)) ;
      T.one
  |]

  (* fix-me: implement 4D cross product *)
  let cross (av : t array) : t = 
    invalid_arg "4D cross product not implemented"

  let clone (v : t) = init (fun i -> v.(i))

  let copy (v1 : t) (v2 : t) = Array.blit v1 0 v2 0 size

  let to_tuple (v : t)  = (v.(0),v.(1),v.(2),v.(3))
  let of_tuple (x,y,z,w) : t = [| x; y; z; w |]

  let to_array v = Array.copy v
  let of_array v = Array.sub v 0 size

  let random (v : t) : t = map (fun x -> if x = T.zero then T.zero else T.rand x) v 

  let modulo (v : t) m : t = map (fun x -> T.modulo x m) v

  let below_epsilon (v : t) = 
    let check i =
      abs v.(i) < T.epsilon 
    in
    check 0 && check 1 && check 2 && check 3

  let for_all f (v : t) = Array.fold_left (fun acc x -> acc && (f x)) true v

  let min (v1 : t) (v2 : t) = map2 min v1 v2 

  let max (v1 : t) (v2 : t) = map2 max v1 v2 

  (* 4D length *)
  let length (v : t) = T.sqrt (dot v v)

  let normalize (v : t) =
    let n = T.one /. (length v)
    in 
      scale v n

  let to_string (v : t) =
    let to_s = T.to_string 
    and x,y,z,w = to_tuple v in
    "< "^to_s x^" ; "^to_s y^" ; "^to_s z^" ; "^to_s w^" >"

      
end

module PFloat : Vec.SCALAR with type t = float = Primitives.Float
module PInt : Vec.SCALAR with type t = int = Primitives.Int

module Float : S with module Scalar = PFloat and type scalar = float = Make(PFloat)
module Int : S with module Scalar = PInt and type scalar = int = Make(PInt)
