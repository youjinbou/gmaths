(** an homogeneous vector module *)
(** this module uses the Vec4.Float module and overrides a few functions to 
    handle homogeneous coordinates 
*)

include Vec4.Float

module T = Scalar

let vec3 (a,b,c) = of_tuple (a,b,c, 1.0)
let vec4         = of_tuple 


module HomogeneousF =
struct

  let h (x,y,z,w) = (x /. w , y /. w, z /. w, 1.0)

  let homogenize v =
    of_tuple (h (to_tuple v))

  let null () : t = [| 0.0 ; 0.0 ; 0.0 ; 1.0 |]
    
  let unit k : t = 
    let v = null () in v.(k) <- 1.0; v

  let opp (v : t) : t = 
    [|  -. v.(0) ;  -. v.(1) ;  -. v.(2) ; v.(3) |]

  let neg = opp

  let apply2 op (v1 : t) (v2 : t) : t = 
    let ( +. ) = op in
    let (x1,y1,z1,_), (x2,y2,z2,_) = h (to_tuple v1), h (to_tuple v2) in
    [| x1 +. x2 ; y1 +. y2 ; z1 +. z2 ; 1.0 |]

  let apply3 op (v1 : t) (v2 : t) (v3 : t) : t =
    let ( +. ) = op in
    let (x1,y1,z1,_), (x2,y2,z2,_), (x3,y3,z3,_) = h (to_tuple v1), h (to_tuple v2), h (to_tuple v3) in
    [| x1 +. x2 +. x3  ; y1 +. y2 +. y3 ; z1 +. z2 +. z3 ; 1.0 |]

  let apply4 op (v1 : t) (v2 : t) (v3 : t) (v4 : t) : t =
    let ( +. ) = op in
    let (x1,y1,z1,_), (x2,y2,z2,_), (x3,y3,z3,_), (x4,y4,z4,_) = 
      h (to_tuple v1), h (to_tuple v2), h (to_tuple v3), h (to_tuple v4) in
    [| x1 +. x2 +. x3  +. x4 ; y1 +. y2 +. y3 +. y4 ; z1 +. z2 +. z3 +. z4 ; 1.0 |]

  let add = apply2 ( +. )
  let add3 = apply3 ( +. )
  let add4 = apply4 ( +. )

  let sub = apply2 ( -. )
  let sub3 = apply3 ( -. )
  let sub4 = apply4 ( -. )

  let scale (v : t) (s : scalar) : t =
    [| v.(0) ; v.(1) ; v.(2) ; v.(3) /. s |]

  let muladd (v1 : t) (s : scalar) (v2 : t) : t = 
    let op x y = x *. s +. y in
    apply2 op v1 v2

  let dot (v1 : t) (v2 : t) = 
    let (x1,y1,z1,_), (x2,y2,z2,_) = h (to_tuple v1), h (to_tuple v2) in
    x1 *. x2 +. y1 *. y2 +. z1 *. z2

  let cross (av : t array) : t = 
    let (x1,y1,z1,_), (x2,y2,z2,_) = h (to_tuple av.(0)), h (to_tuple av.(1)) in [|
      (y1 *. z2) -. (z1 *. y2) ;
      (z1 *. x2) -. (x1 *. z2) ;
      (x1 *. y2) -. (y1 *. x2) ;
      1.0
  |]

  let length (v : t) = sqrt (dot v v)

  let normalize (v : t) = scale v (1. /. (length v))

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

let alpha v1 v2 = acos ( (dot v1 v2) /. ((length v1) *. (length v2)) )

(* override defaults with homogeneous operators *)
include HomogeneousF

let distance v1 v2 = 
  length (sub v2 v1)

(** compute the unit vector on the line from p1 to p2 *)
let unit_vector p1 p2 = 
  normalize (sub p2 p1)
