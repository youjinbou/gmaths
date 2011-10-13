(** basic Quaternion module *)


type vector = Vec4.Float.t
type mat4x4 = Matrix.mat4x4

type t = vector
type scalar = Vec4.Float.scalar

(* shortcut *)
module Fvec4 = Vec4.Float
module M = Matrix

(* unary operations *)
let of_tuple : (float * float * float * float) -> t = Fvec4.of_tuple
let to_tuple : t -> (float * float * float * float) = Fvec4.to_tuple

let of_xyz (x,y,z) : t = 
  let w = 1.0 -. x *. x -. y *. y -. z *. z in
  let w = if w < 0.0 then 0.0 else -.(sqrt w) in
  of_tuple (x,y,z,w)

let pure (q : t) = 
  let x,y,z,_ = to_tuple q in
  x,y,z

let norm (q : t) = 
  Fvec4.dot q q

let conjugate (q : t) = 
  let x,y,z,w = Fvec4.to_tuple q in
  Fvec4.of_tuple (-.x,-.y,-.z,w)

let inverse (q : t) =
  Fvec4.scale (conjugate q) (1. /. (norm q))

let copy : t -> t -> unit = Fvec4.copy

let add (qa : t) (qb : t) = Fvec4.add qa qb

let mul (qa : t) (qb : t) =
  let qax,qay,qaz,qaw = Fvec4.to_tuple qa
  and qbx,qby,qbz,qbw = Fvec4.to_tuple qb
  in
  Fvec4.of_tuple (
    (qax *. qbw) +. (qaw *. qbx) +. (qay *. qbz) -. (qaz *. qby),
    (qay *. qbw) +. (qaw *. qby) +. (qaz *. qbx) -. (qax *. qbz),
    (qaz *. qbw) +. (qaw *. qbz) +. (qax *. qby) -. (qay *. qbx),
    (qaw *. qbw) -. (qax *. qbx) -. (qay *. qby) -. (qaz *. qbz)
  )

let rotate (q : t) v = 
  mul q (mul v (inverse q))

let to_matrix (q : t) = 
  let s = let n = norm q in if n > 0. then 2. /. n else 0.0 in
  let x,y,z,w = to_tuple q in
  let xs = x *. s and ys = y *. s and zs = y *. s in
  let wx = w *. xs and wy = w *. ys and wz = w *. zs
  and xx = x *. xs and xy = x *. ys and xz = x *. zs 
  and yy = y *. ys and yz = y *. zs and zz = z *. zs
  and m = M.make 4 4 0.0 in
  M.set m 0 0 (1.0 -. yy -. zz); M.set m 0 1 (xy -. wz); M.set m 0 2 (xz +. wy); M.set m 0 3 0.0;
  M.set m 1 0 (xy +. wz); M.set m 1 1 (1.0 -. xx -. zz); M.set m 1 2 (yz -. wx); M.set m 1 3 0.0;
  M.set m 2 0 (xz -. wy); M.set m 2 1 (yz +. wx); M.set m 2 2 (1.0 -. yy -. zz); M.set m 2 3 0.0;
  M.set m 3 0 0.0; M.set m 3 1 0.0; M.set m 3 2 0.0; M.set m 3 3 1.0;
  m

let of_matrix (m : M.Mat4.t) = 
  let fix q = 
    let s = 1.0 /. (sqrt (M.get m 3 3)) in
    Fvec4.scale q s
  in
  let tr = (M.get m 0 0) *. (M.get m 1 1) *. (M.get m 2 2) in
  if tr >= 0.0 
  then
    let s = sqrt (tr +. (M.get m 3 3)) in
    let w = s *. 0.5
    and x = (M.get m 2 1) *. (M.get m 1 2) *. s
    and y = (M.get m 2 0) *. (M.get m 0 2) *. s
    and z = (M.get m 1 0) *. (M.get m 0 1) *. s in
    fix (of_tuple (x,y,z,w))
  else
    let compute m f a b c =
      let s = sqrt ((M.get m a a) -. (M.get m b b) -. (M.get m c c)) +. (M.get m 3 3) in
      f (((M.get m c b) -. (M.get m b c)) *. 0.5 /. s)
	(0.5 *. s)
	(((M.get m a b) +. (M.get m b a)) *. 0.5 /. s)
	(((M.get m c a) +. (M.get m a c)) *. 0.5 /. s)
    in
    match M.get m 0 0, M.get m 1 1, M.get m 2 2 with
      | x,y,z when (y > x) && (z > x) -> fix (compute m (fun w x y z -> of_tuple (x,y,z,w)) 0 1 2)
      | x,y,z when (y > x) && (z < x) -> fix (compute m (fun w x y z -> of_tuple (y,z,x,w)) 1 2 0)
      | _                             -> fix (compute m (fun w x y z -> of_tuple (z,x,y,w)) 2 0 1)
	

let of_axis_angle (x,y,z) a : t = 
  let a  = a /. 2. in
  let sin_a = sin a 
  and cos_a = cos a in
  of_tuple (x *. sin_a, y *. sin_a, z *. sin_a, cos_a)

let to_axis_angle (q : t) =
  let w,x,y,z = to_tuple q in
  let t = acos w in
  let s = sin t in
  ( x /. s, y /. s, z /. s),(2. *. t)

let of_angles (x,y,z) : t = 
  let q0 = of_axis_angle (1.,0.,0.) x
  and q1 = of_axis_angle (0.,1.,0.) y
  and q2 = of_axis_angle (0.,0.,1.) z
  in
  mul (mul q0 q1) q2

