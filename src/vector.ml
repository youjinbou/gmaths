(** an homogeneous vector module *)
(** this module uses the Vec4.Float module and overrides a few functions to
    handle homogeneous coordinates
*)

module V4 = Vec4.Float

include V4

module T = Scalar

let vec3 (a,b,c) = of_tuple (a,b,c, 1.0)
let vec4 x       = of_tuple x

module HomogeneousF =
struct

  let h (x,y,z,w) = x /. w , y /. w, z /. w, 1.0

  let homogenize v =
    V4.of_tuple (h @@ V4.to_tuple v)

  let to_tuple v = h (to_tuple v)

  let get v k =
    let c =
    match k with
      | 0 -> 0
      | 1 -> 1
      | 2 -> 2
      | _ -> invalid_arg "Vector.get"
    in V4.get v c /. V4.get v 3

  let set v k x =
    let c =
    match k with
      | 0 -> 0
      | 1 -> 1
      | 2 -> 2
      | _ -> invalid_arg "Vector.get"
    in V4.set v c (x *. V4.get v 3)


  let null () : t = V4.unit 3

  let make k : t = V4.of_tuple (k, k, k, 1.0)

  let unit : int -> t = function
    | 0 -> V4.of_tuple (1., 0., 0., 1.0)
    | 1 -> V4.of_tuple (0., 1., 0., 1.0)
    | 2 -> V4.of_tuple (0., 0., 1., 1.0)
    | _ -> invalid_arg "Vector.unit"

  let opp (v : t) : t =
    let x,y,z,w = V4.to_tuple v in
    V4.of_tuple (x,y,z,-.w)

  let apply1 op v =
    let x,y,z,_ = to_tuple v in
    V4.of_tuple (op x, op y, op z, 1.0)

  let apply2 op (v1 : t) (v2 : t) : t =
    let ( +. ) = op in
    let (x1,y1,z1,_), (x2,y2,z2,_) = to_tuple v1, to_tuple v2 in
    V4.of_tuple (x1 +. x2, y1 +. y2, z1 +. z2, 1.0)

  let apply3 op (v1 : t) (v2 : t) (v3 : t) : t =
    let ( +. ) = op in
    let (x1,y1,z1,_), (x2,y2,z2,_), (x3,y3,z3,_) = to_tuple v1, to_tuple v2, to_tuple v3 in
    V4.of_tuple (x1 +. x2 +. x3, y1 +. y2 +. y3, z1 +. z2 +. z3, 1.0)

  let apply4 op (v1 : t) (v2 : t) (v3 : t) (v4 : t) : t =
    let ( +. ) = op in
    let (x1,y1,z1,_), (x2,y2,z2,_), (x3,y3,z3,_), (x4,y4,z4,_) =
      to_tuple v1, to_tuple v2, to_tuple v3, to_tuple v4 in
    V4.of_tuple (x1 +. x2 +. x3  +. x4, y1 +. y2 +. y3 +. y4, z1 +. z2 +. z3 +. z4, 1.0)

  let add = apply2 ( +. )
  let add3 = apply3 ( +. )
  let add4 = apply4 ( +. )

  let sub = apply2 ( -. )
  let sub3 = apply3 ( -. )
  let sub4 = apply4 ( -. )

  let scale (v : t) (s : scalar) : t =
    let x,y,z,w = V4.to_tuple v in
    V4.of_tuple (x,y,z, w /. s)

  let invscale (v : t) (s : scalar) : t =
    let x,y,z,w = V4.to_tuple v in
    V4.of_tuple (x,y,z, w *. s)

  let muladd (v1 : t) (s : scalar) (v2 : t) : t =
    let op x y = x *. s +. y in
    apply2 op v1 v2

  let dot (v1 : t) (v2 : t) =
    let (x1,y1,z1,_), (x2,y2,z2,_) = to_tuple v1, to_tuple v2 in
    x1 *. x2 +. y1 *. y2 +. z1 *. z2

  let cross (av : t array) : t =
    let (x1,y1,z1,_), (x2,y2,z2,_) = to_tuple av.(0), to_tuple av.(1) in
    V4.of_tuple (
      (y1 *. z2) -. (z1 *. y2),
      (z1 *. x2) -. (x1 *. z2),
      (x1 *. y2) -. (y1 *. x2),
      1.0
      )

  let random (v : t) : t =
    apply1 (fun x -> if x = T.zero then T.zero else T.rand x) v

  let modulo (v : t) m : t =
    apply1 (fun x -> T.modulo x m) v

  let min = apply2 Pervasives.min

  let max = apply2 Pervasives.max

  let length (v : t) = sqrt (dot v v)

  let normalize (v : t) = invscale v (length v)

  let below_epsilon (v : t) =
    let x,y,z,_ = to_tuple v in
    T.abs x < T.epsilon && T.abs y < T.epsilon && T.abs z < T.epsilon

  let to_string (v : t) =
    let to_s = T.to_string
    and x,y,z,w = to_tuple v in
    "< "^to_s x^" ; "^to_s y^" ; "^to_s z^" ; "^to_s w^" >"

  let make_system (v : t) : t * t * t =
    let zero = T.zero
    and one = T.one in
    let v' =
      let x,y,z,_ = to_tuple v in
      if T.abs x > zero || T.abs y > zero
      then of_tuple (T.opp y, x, zero, one)
      else
	if T.abs z > zero
	then of_tuple (zero, z, T.opp y, one)
	else
	  invalid_arg "make_system : null vector"
    in
    let v'' = cross [| v' ; v |] in
    let v'  = cross [| v''; v |] in
    v', v, v''

end

(* returns the angle between vector v1 and v2 *)
let alpha v1 v2 = acos ( (dot v1 v2) /. ((length v1) *. (length v2)) )

(* override defaults with homogeneous operators *)
include HomogeneousF

(* distance between two points v1 & v2 *)
let distance v1 v2 =
  length (sub v2 v1)

(** compute the unit vector on the line from p1 to p2 *)
let unit_vector p1 p2 =
  normalize (sub p2 p1)
