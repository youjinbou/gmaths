(** an homogeneous vector module *)
(** this module uses the Vec4.Float module and overrides a few functions to 
    handle homogeneous coordinates 
*)

include Vec4.Float

let vec3 (a,b,c) = of_tuple (a,b,c, 1.0)
let vec4         = of_tuple 

let alpha v1 v2 = acos ( (dot v1 v2) /. ((length v1) *. (length v2)) )

(*
(** homogeneous computations *)
module Homogeneous =
struct

  let homogeneous f () = 
    let v = f () in 
    set v (pred size) 1.; v

  let homogeneous1 f v1 =
    let v = f v1 in 
    set v (pred size) 1.; v

  let homogeneous2 f v1 v2 =
    let v = f v1 v2 in
    set v (pred size) 1.; v

  let null = homogeneous null

  let unit =  homogeneous1 unit

  let opp = homogeneous1 opp

  let neg = opp

  let add = homogeneous2 add

  let sub = homogeneous2 sub

  let scale = homogeneous2 scale

  let muladd v1 s v2 =
    let f v1 v2 = muladd v1 s v2 in
    (homogeneous2 f) v1 v2

  let dot v1 v2 = v1.(0) *. v2.(0) +. v1.(1) *. v2.(1) +. v1.(2) *. v2.(2)

  let cross = cross3d

  let length v = sqrt (dot v v)

  let normalize v = scale v (1. /. (length v))

end
*)

module HomogeneousF =
struct

  let null () : t = [| 0.0 ; 0.0 ; 0.0 ; 1.0 |]
    
  let unit k : t = 
    let v = null () in v.(k) <- 1.0; v

  let opp (v : t) : t = 
    [|  -. v.(0) ;  -. v.(1) ;  -. v.(2) ; v.(3) |]

  let neg = opp

  let add (v1 : t) (v2 : t) = 
    [| v1.(0) +. v2.(0) ; v1.(1) +. v2.(1) ; v1.(2) +. v2.(2) ; v1.(3) |]

  let add3 (v1 : t) (v2 : t) (v3 : t) =
    [| v1.(0) +. v2.(0) +. v3.(0) ; v1.(1) +. v2.(1) +. v3.(1) ; v1.(2) +. v2.(2) +. v3.(2) ; v1.(3) |]

  let sub (v1 : t) (v2 : t) = 
    [| v1.(0) -. v2.(0) ; v1.(1) -. v2.(1) ; v1.(2) -. v2.(2) ; v1.(3) |]

  let scale (v : t) (s : scalar) =
    [| v.(0) *. s ; v.(1) *. s ; v.(2) *. s ; v.(3) |]    

  let muladd (v1 : t) (s : scalar) (v2 : t) = 
    [| v1.(0) *. s +. v2.(0) ; v1.(1) *. s +. v2.(1) ; v1.(2) *. s +. v2.(2) ; v1.(3) |]

  let dot (v1 : t) (v2 : t) = v1.(0) *. v2.(0) +. v1.(1) *. v2.(1) +. v1.(2) *. v2.(2)

  let cross = cross3d

  let length (v : t) = sqrt (dot v v)

  let normalize (v : t) = scale v (1. /. (length v))

end

(* override defaults with homogeneous operators *)
include HomogeneousF

let distance v1 v2 = 
  length (sub v2 v1)

(** compute the unit vector on the line from p1 to p2 *)
let unit_vector p1 p2 = 
  normalize (sub p2 p1)


