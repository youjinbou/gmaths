module V4 = Vec4.Float

type vector = float array

type t = vector array (* n x m matrix *)

let width (m : t) = Array.length m.(0)

let height (m : t) = Array.length m
  
let column_get (m : t) i j = m.(i).(j)
let line_get (m : t) i j = m.(j).(i)

let column_set (m : t) i j v = m.(i).(j) <- v
let line_set (m : t) i j v = m.(j).(i) <- v

(* column matrix access *)
let get (m : t) x y = column_get m x y
  
let set (m : t) x y v = column_set m x y v
  
(* returns a copy of the nth row of m *)
let row (m : t) n =
  let w = width m in
  Array.init w (fun i -> line_get m n i)
    
(* returns a copy of the nth column of m *)
let col (m : t) n =
  let h = height m in
  Array.init h (fun i -> column_get m n i)

(* generic functions --------------- *)
  
(* column matrix creation *)
let init w h f : t = 
  Array.init h (fun y -> Array.init w (fun x -> (f y x)))

let transpose w h (m : t) =
  init h w (fun x y -> get m y x)

(* identity matrix *)
let id w : t =
  init w w (fun x y -> if x = y then 1.0 else 0.0)

let minor w x y = fun m a b -> m ((a + x + 1) mod w) ((b + y + 1) mod w)

let dotf s v1 v2 = 
  let rec dotf i f1 f2 r = 
    if i < 0 then r else dotf (pred i) v1 v2 (r +. (f1 i) *. (f2 i))
  in
  dotf (pred s) v1 v2 0.0

let mult (m1 : t) (m2 : t) =
  let h = height m1 
  and w = width m2 
  and s = width m1 
  and s' = height m2
  in
  if s = s' then
    init w h (fun a b -> dotf s (line_get m1 a) (column_get m2 b))
  else raise (Invalid_argument "Matrix.mult : sizes of matrices don't match")

let make w h v =
  init w h (fun _ _ -> v)

let map f (m : t) =
  let w, h = width m, height m in
  init w h (fun i j -> f (get m i j))

let mapi f (m : t) =
  let w, h = width m, height m in
  init w h (fun i j -> f i j (get m i j))

let copy (m : t)  = 
  let w, h = width m, height m in
  init w h (fun i j -> get m i j)
  (* init (fun i j -> get m i j) *)
    
let null w h = 
  make w h 0.

let fscale (m : t) (k : float) : t =
  let w, h = width m, height m in
  init w h (fun i j -> k *. (get m i j))


type mat2x2 = t
type mat3x3 = t
type mat4x4 = t


(* size 2 matrices --------------- *)
module Mat2 = 
struct

  type t = mat2x2

  let size = 2

  let identity () : t = id 2

  let null () : t =
    null size size

  (* determinant of - functionally accessed - square matrix of size 2 *)
  let detf m = 
    (m 0 0) *. (m 1 1) -. (m 1 0) *. (m 0 1)

  let transposef m = [| 
    [| get m 0 0 ; get m 1 0 |] ; 
    [| get m 0 1 ; get m 1 1 |] 
		     |]


  let rotation_z angle : t = 
    let c = cos angle
    and s = sin angle 
    in [| 
      [|   c ; -.s |];
      [|   s ;   c |];
       |]
    
  let rotation_y angle : t = 
    let c = cos angle
    in [| 
      [|   c ; 0.0 |];
      [| 0.0 ; 1.0 |];
       |]

  let rotation_x angle : t = 
    let c = cos angle
    in [| 
      [| 1.0 ; 0.0 |];
      [| 0.0 ;   c |];
       |]

end

(* size 3 matrices --------------- *)
module Mat3 = 
struct

  type t = mat3x3

  let size = 3

  let identity () : t = id size

  let null () : t =
    null size size

  (* determinant of - functionally accessed - square matrix of size 3 *)
  let detf m = 
    let det2f = Mat2.detf in
    let f k = fun a b -> m (a+1) ((b+k) mod 3) in
    (m 0 0) *. (det2f (f 1)) +.
      (m 0 1) *. (det2f (f 2)) +.
      (m 0 2) *. (det2f (f 3))

  let transpose (m : t) = [| 
    [| get m 0 0 ; get m 1 0 ; get m 2 0 |] ; 
    [| get m 0 1 ; get m 1 1 ; get m 2 1 |] ;
    [| get m 0 2 ; get m 1 2 ; get m 2 2 |] 
		    |]

  let dotf v1 v2 = (v1 0) *. (v2 0) +. (v1 1) *. (v2 1) +. (v1 2) *. (v2 2)

  let apply (m : t) v = 
    Array.init 3 (fun i -> dotf (get m i) (fun i -> v.(i)))

  let mult (m1 : t) (m2 : t) =
    init 3 3 (fun a b -> dotf (line_get m1 a) (column_get m2 b))


  let rotation_z angle : t = 
    let c = cos angle
    and s = sin angle 
    in [| 
      [|   c ; -.s ; 0.0 |];
      [|   s ;   c ; 0.0 |];
      [| 0.0 ; 0.0 ; 1.0 |];
       |]

  let rotation_y angle : t = 
    let c = cos angle
    and s = sin angle 
    in [| 
      [|   c ; 0.0 ;   s |];
      [| 0.0 ; 1.0 ; 0.0 |];
      [| -.s ; 0.0 ;   c |];
       |]

  let rotation_x angle : t = 
    let c = cos angle
    and s = sin angle 
    in [| 
      [| 1.0 ; 0.0 ; 0.0 |];
      [| 0.0 ;   c ; -.s |];
      [| 0.0 ;   s ;   c |];
       |]
    
  let rotation n theta : t =
    match n with
      | 0 -> rotation_x theta
      | 1 -> rotation_y theta
      | 2 -> rotation_z theta
      | _ -> assert false

  (* rotation on 1 axis *)
  let rotation2 n theta : t =
    let n = n mod 3 
    and m = null ()
    and c = cos theta 
    and s = sin theta in
    let ms = -.s in
    set m n n 1.;
    set m 3 3 1.;
    (match n with
	0 -> (
	  set m 1 1 c;
	  set m 1 2 ms;
	  set m 2 1 s;
	  set m 2 2 c
	)
      | 1 -> (
	set m 0 0 c;
	set m 0 2 s;
	set m 2 0 ms;
	set m 2 2 c
      )
      | _ -> (
	set m 0 0 c;
	set m 0 1 ms;
	set m 1 0 s;
	set m 1 1 c
      ));
    m

  let minor = minor 3 

  let inversef m =
    let dim = 3 
    and det2f = Mat2.detf in
    let d = detf m 
    and f x y = (minor x y) m 
    in
    if d != 0.0 then
      let cof = init dim dim (fun a b -> (det2f (f a b)) /. d)
      in
      cof
    else raise (Invalid_argument "Matrix.inverse : null determinant")
      
  let inverse (m : t) : t =
    inversef (get m)

end

(* size 4 matrices --------------- *)

module Mat4 =
struct

  type t = mat4x4

  let size = 4

  (* returns a copy of the nth row of m *)
  let row (m : t) n =
    V4.of_tuple (m.(0).(n), m.(1).(n), m.(2).(n), m.(3).(n))

  (* returns a copy of the nth column of m *)
  let col (m : t) n =
    V4.of_tuple (m.(n).(0), m.(n).(1), m.(n).(2), m.(n).(3))

  let setrow (m : t) n v = 
    let x,y,z,w = V4.to_tuple v in
    m.(0).(n) <- x;
    m.(1).(n) <- y;
    m.(2).(n) <- z;
    m.(3).(n) <- w

  let setcol (m : t) n v =
    let x,y,z,w = V4.to_tuple v in
    m.(n).(0) <- x;
    m.(n).(1) <- y;
    m.(n).(2) <- z;
    m.(n).(3) <- w


  (* determinant of - functionally accessed - square matrix of size 4 *)
  let detf m = 
    let det3f = Mat3.detf in
    let f k = fun a b -> m (a+1) ((b+k) mod 4) in
    (m 0 0) *. (det3f (f 1)) -.
      (m 0 1) *. (det3f (f 2)) +.
      (m 0 2) *. (det3f (f 3)) -.
      (m 0 3) *. (det3f (f 4))
      
  let det (m : t) =
    detf (get m)

  let null () : t =
    null size size

  (* identity matrix of size 4 *)
  let identity () : t = 
    id size

  let rotation_z angle : t =
    let c = cos angle
    and s = sin angle 
    in [| 
      [|   c ; -.s ; 0.0 ; 0.0 |];
      [|   s ;   c ; 0.0 ; 0.0 |];
      [| 0.0 ; 0.0 ; 1.0 ; 0.0 |];
      [| 0.0 ; 0.0 ; 0.0 ; 1.0 |];
       |]

  let rotation_y angle : t = 
    let c = cos angle
    and s = sin angle 
    in [| 
      [|   c ; 0.0 ;   s ; 0.0 |];
      [| 0.0 ; 1.0 ; 0.0 ; 0.0 |];
      [| -.s ; 0.0 ;   c ; 0.0 |];
      [| 0.0 ; 0.0 ; 0.0 ; 1.0 |];
       |]

  let rotation_x angle : t = 
    let c = cos angle
    and s = sin angle 
    in [| 
      [| 1.0 ; 0.0 ; 0.0 ; 0.0 |];
      [| 0.0 ;   c ; -.s ; 0.0 |];
      [| 0.0 ;   s ;   c ; 0.0 |];
      [| 0.0 ; 0.0 ; 0.0 ; 1.0 |];
       |]

  let transpose (m : t) : t =
    transpose size size m

  let minor = minor size

  let inversef m =
    let dim = size in
    let det3f = Mat3.detf in
    let d = detf m 
    and f x y = (minor x y) m
    in
    if d != 0.0 then
      let cof = init dim dim (fun a b -> (let s = if (a + b) mod 2 = 0 then 1. else -1. in  s *. det3f (f a b)) /. d)
      in
      transpose cof
    else raise (Invalid_argument "Matrix.inverse : null determinant")

  let inverse (m : t) : t =
    inversef (get m)

  let dotf v1 v2 = (v1 0) *. (v2 0) +. (v1 1) *. (v2 1) +. (v1 2) *. (v2 2) +. (v1 3) *. (v2 3)

(*
  let apply m v = 
    Array.init 4 (fun i -> dotf (get m i) (fun i -> v.(i)))
*)

  let mult (m1 : t) (m2 : t) =
    init 4 4 (fun a b -> dotf (line_get m1 b) (column_get m2 a)) 
      
  let apply (m : t) v = 
    let (x,y,z,w) = V4.to_tuple v 
    in 
    if (let d = w -. 1.0 in (abs_float d) > epsilon_float) then (
(*      Debug.fvec "Matrix.apply : homogeneous vector error = " v; *)
      assert (let d = w -. 1.0 in (abs_float d) < epsilon_float);
    );
    let x = m.(0).(0) *. x +. m.(1).(0) *. y +. m.(2).(0) *. z +. m.(3).(0) *. w 
    and y = m.(0).(1) *. x +. m.(1).(1) *. y +. m.(2).(1) *. z +. m.(3).(1) *. w 
    and z = m.(0).(2) *. x +. m.(1).(2) *. y +. m.(2).(2) *. z +. m.(3).(2) *. w 
    and w = m.(0).(3) *. x +. m.(1).(3) *. y +. m.(2).(3) *. z +. m.(3).(3) *. w 
    in 
    V4.of_tuple (x,y,z,w)

  let translation x y z : t =
    let m = identity ()
    in
    set m 3 0 x;
    set m 3 1 y;
    set m 3 2 z;
    m

  (* vector translation *)
  let translationv vt : t = 
    let x = V4.get vt 0
    and y = V4.get vt 1
    and z = V4.get vt 2 
    in
    translation x y z

  let invtranslationv v : t =
    let opp = V4.opp in
    translationv (opp v)

  (* rotation on 1 axis *)
  let rotation2 n theta : t =
    let n = n mod 3 
    and m = null ()
    and c = cos theta 
    and s = sin theta in
    let ms = -.s in
    set m n n 1.;
    set m 3 3 1.;
    (match n with
	0 -> (
	  set m 1 1 c;
	  set m 1 2 ms;
	  set m 2 1 s;
	  set m 2 2 c
	)
      | 1 -> (
	set m 0 0 c;
	set m 0 2 s;
	set m 2 0 ms;
	set m 2 2 c
      )
      | _ -> (
	set m 0 0 c;
	set m 0 1 ms;
	set m 1 0 s;
	set m 1 1 c
      ));
    m

  let rotation n theta : t =
    match n with
      | 0 -> rotation_x theta
      | 1 -> rotation_y theta
      | 2 -> rotation_z theta
      | _ -> assert false

  (* rotation on 3 axes *)
  let invrotationv vr : t = 
    let m1 = rotation 0 (V4.get vr 0)
    and m2 = rotation 1 (V4.get vr 1)
    and m3 = rotation 2 (V4.get vr 2)
    in 
    mult m1 (mult m2 m3)

  let rotationv vr : t =
    let m1 = rotation 0 (-.(V4.get vr 0))
    and m2 = rotation 1 (-.(V4.get vr 1))
    and m3 = rotation 2 (-.(V4.get vr 2))
    in 
    mult m3 (mult m2 m1)

  (* create a scaling  *)
  let scale x y z : t =
    let m = null () in
    set m 0 0 x;
    set m 1 1 y;
    set m 2 2 z;
    set m 3 3 1.;
    m

  (* scaling on 3 axes *)
  let scalev v : t =
    let x,y,z = V4.get v 0,V4.get v 1,V4.get v 2
    in
    scale x y z

  let invscalev v : t =
    let inv v = V4.map (fun x -> 1./.x) v in
    scalev (inv v)

  (* combine a scaling, a rotation and a translation *)
  let prepare vt vs vr : t =
    let s = scalev vs
    and t = translationv vt  
    and r = rotationv vr 
    in
    mult t (mult r s)

  (* combine the inverse of a scaling, a rotation and a translation *)
  let invprepare (vt : V4.t) (vs : V4.t) (vr : V4.t) : t = 
    let s = invscalev vs
    and t = invtranslationv vt
    and r = invrotationv vr
    in
    mult s (mult r t)

end

(*
let dumpf (m : int -> int -> float) w h =
  for j = 0 to (pred h) do 
    Debug.string "<";
    for i = 0 to (w - 2) do
      Debug.raw_float (m i j);
      Debug.string " ; "
    done;
    Debug.raw_float (m (pred w) j);
    Debug.string ">";
    Debug.newline ()
  done
    
let dump ?(msg="mat") v =
  let w, h = width v, height v in
  Debug.msg (msg^" {");
  dumpf (get v) w h;
  Debug.msg "}"

*)
