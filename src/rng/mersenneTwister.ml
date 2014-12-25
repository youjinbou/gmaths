(* Ocaml translation of the following:

   A C-program for MT19937: Integer version (1998/4/6)
   genrand() generates one pseudorandom unsigned integer (32bit)
   which is uniformly distributed among 0 to 2^32-1  for each
   call. sgenrand(seed) set initial values to the working area
   of 624 words. Before genrand(), sgenrand(seed) must be
   called once. (seed is any 32-bit integer except for 0).
   Coded by Takuji Nishimura, considering the suggestions by
   Topher Cooper and Marc Rieffel in July-Aug. 1997.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later
   version.
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU Library General Public License for more details.
   You should have received a copy of the GNU Library General
   Public License along with this library; if not, write to the
   Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307  USA

   Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
   When you use this, send an email to: matumoto@math.keio.ac.jp
   with an appropriate reference to your work.

   REFERENCE
   M. Matsumoto and T. Nishimura,
   "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
   Pseudo-Random Number Generator",
   ACM Transactions on Modeling and Computer Simulation,
   Vol. 8, No. 1, January 1998, pp 3--30.

   ------------------------------------------------------------
   This was implemented as an alternative to the Random module
   of Ocaml. Hopefully, this code is faithful to the original C
   implementation.

*)

module Period =
struct
  let n = 624
  let m = 397
  let matrix_a   = 0x9908b0dfL (* constant vector a *)
  let upper_mask = 0x80000000L (* most significant w-r bits *)
  let lower_mask = 0x7fffffffL (* least significant r bits *)
end

(* tempering parameters *)
module Tempering =
struct
  open Int64

  let mask_b = 0x9d2c5680L
  let mask_c = 0xefc60000L
  let shift_u y = shift_right_logical y 11
  let shift_s y = shift_left y 7
  let shift_t y = shift_left y 15
  let shift_l y = shift_right_logical y 18
end

external random_seed: unit -> int array = "caml_sys_random_seed";;

module State =
struct

  type t = {mt : int64 array; mutable idx : int}

  let create () = {
    mt = Array.make Period.n 0L; (* the array for the state vector  *)
    idx = Period.n;
  }

  let mt v = v.mt

  let idx v = v.idx

  let incr_idx v = v.idx <- succ v.idx

  let reset_idx v = v.idx <- 0

  let lxor64 a b = Int64.logand (Int64.logxor a b) 0xffffffffL
  and land64 = Int64.logand
  and lsr64 = Int64.shift_right_logical
  let overflow p a b = land64 (p a b) 0xffffffffL
  let add64 = overflow Int64.add
  and sub64 = overflow Int64.sub
  and mul64 = overflow Int64.mul
  and i64 = Int64.of_int

  (* initializing the array with a seed *)
  let init v seed : t =
    v.mt.(0) <- Int64.logand seed 0xffffffffL; 
    for i = 1 to pred Period.n do
      let k = add64 (mul64 1812433253L  (lxor64 v.mt.(pred i) (lsr64 v.mt.(pred i) 30))) (i64 i) in
        (* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier.
         * In the previous versions, MSBs of the seed affect
         * only MSBs of the array mt[].
         * 2002/01/09 modified by Makoto Matsumoto             *)
      v.mt.(i) <- k
    done;
    v

  let make seed =
    init (create ()) seed

  let full_init init_key =
    let key_length = Array.length init_key in
    let s = make 19650218L in
    let k = max Period.n key_length in
    let rec loop1 k i j =
      if k > 0 then (
        s.mt.(i) <- add64 (lxor64 s.mt.(i) 
                                  (mul64 (lxor64 s.mt.(pred i) (lsr64 s.mt.(pred i) 30)) 1664525L))
                          (add64 (i64 init_key.(j)) (i64 j)); (* non linear *)
        let i = if i >= pred Period.n
                then ( s.mt.(0) <- s.mt.(pred Period.n); 1)
                else succ i
        and j = if j >= pred key_length then 0 else succ j in
        loop1 (pred k) i j
      ) else i in
    let i = loop1 k 1 0 in
    let rec loop2 k i =
      if k > 0 then (
        s.mt.(i) <- sub64 (lxor64 s.mt.(i) 
                                  (mul64 (lxor64 s.mt.(pred i) (lsr64 s.mt.(pred i) 30)) 1566083941L))
                          (i64 i); (* non linear *)
        let i =
          if i >= pred Period.n then ( s.mt.(0) <- s.mt.(pred Period.n); 1) else succ i 
        in loop2 (pred k) i
      ) in
    loop2 (pred Period.n) i;
    s.mt.(0) <- 0x80000000L (* MSB is 1; assuring non-zero initial array *);
    s

  let make_self_init () =
    full_init (random_seed ())

end

(* minimal random generator *)
module Core =
struct

  let lxor64 a b = Int64.logand (Int64.logxor a b) 0xffffffffL
  and land64 = Int64.logand
  and lsr64 = Int64.shift_right_logical
  and lor64 = Int64.logor
  let overflow p a b = land64 (p a b) 0xffffffffL
  let add64 = overflow Int64.add
  and sub64 = overflow Int64.sub
  and mul64 = overflow Int64.mul
  and i64 = Int64.of_int

  (* returns a full range 32bits *signed* int random value *)
  let rand state =
    let open State in
    let mag01 = [| 0L ; Period.matrix_a |] in
    let refill s =
      let new_y s v1 v2 =
	lor64 (land64 (mt s).(v1) Period.upper_mask)
              (land64 (mt s).(v2) Period.lower_mask)
      and store_mt s idx1 idx2 y =
	(mt s).(idx1) <- lxor64
	                   (lxor64 (mt s).(idx2) (lsr64 y 1))
                           mag01.((Int64.to_int y) land 0x1)
      in
      for kk = 0 to pred (Period.n - Period.m) do
	let y = new_y s kk (succ kk) in
        store_mt s kk (kk + Period.m) y
      done;
      for kk = Period.n - Period.m to Period.n - 2 do
	let y = new_y s kk (succ kk) in
	store_mt s kk (kk + Period.m - Period.n) y
      done;
      let y = new_y s (pred Period.n) 0 in
      store_mt s (pred Period.n) (pred Period.m) y;
      reset_idx s
    in
    let s = state in
    if idx s >= Period.n
    then refill s;
    let y = (mt s).(idx s) in
    let y = (lxor64 y (Tempering.shift_u y)) in
    let y = (lxor64 y (land64 (Tempering.shift_s y) Tempering.mask_b)) in
    let y = (lxor64 y (land64 (Tempering.shift_t y) Tempering.mask_c)) in
    incr_idx s;
    let y = lxor64 y (Tempering.shift_l y) in
    Int64.to_int32 y

  (* 32 bits generator between 0 and {x} *)
  let int32 state (x : int32) =
    Int32.rem (rand state) x

end

module Rng = struct

  open Core

  let single_scale = 1. /. 4294967296.0  (* 2^32 *)

  (* 64 bits generator between 0 and {x} *)
  let int64 state (x : int64) =
    let v1 = Int64.of_int32 (Core.rand state)
    and v2 =
      let r = Int64.of_int32 (Core.rand state)
      and mask = Int64.lognot 0xffL in
      (* clear up 2 lsbits and shift left 30bits *)
      Int64.shift_left (land64 r mask) 30
    in
    Int64.rem (Int64.logor v1 v2) x

  let int32 state (x : int32) =
    Core.int32 state x

  (* returns a random int between 0 and {x} *)
  let int state (x : int) =
    match Sys.word_size with
      32 -> Int32.to_int (Core.int32 state (Int32.of_int x))
    | 64 -> Int64.to_int (int64 state (Int64.of_int x))
    | _  -> failwith "Mersenne Twister : unsupported word size"

  (* returns a random nativeint between 0 and {x} *)
  let nativeint state (x : nativeint) =
    match Sys.word_size with
      32 -> Nativeint.of_int32 (Core.int32 state (Nativeint.to_int32 x))
    | 64 -> Int64.to_nativeint (int64 state (Int64.of_nativeint x))
    | _  -> failwith "Mersenne Twister : unsupported word size"

  (* helper for single precision range floats *) 
  let rawsingle state =
    let r1 = Int32.to_float (Core.rand state)
    in r1 *. single_scale

  (* helper for double precision range floats *)
  let rawdouble state =
    let r1 = Int32.to_float (Core.rand state)
    and r2 = Int32.to_float (Core.rand state)
    in (r1 *. single_scale +. r2) *. single_scale

  let single s bound = rawsingle s *. bound

  let float s bound = rawdouble s *. bound

  let bool s = (Int32.to_int (Core.rand s)) land 1 = 0

end

(* state as a module-scoped value *)

module type DATA =
sig
  val state : State.t
end

module Make (D : DATA) = struct

  let state = D.state

  let rand () =
    Core.rand state

  let int32 x =
    Core.int32 state x

  let int64 x =
    Rng.int64 state x

  let int x =
    Rng.int state x

  let nativeint x =
    Rng.nativeint state x

  let float x =
    Rng.float state x

  let bool =
    Rng.bool state

end
