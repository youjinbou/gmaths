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
  let matrix_a   = 0x9908b0dfl (* constant vector a *)
  let upper_mask = 0x80000000l (* most significant w-r bits *)
  let lower_mask = 0x7fffffffl (* least significant r bits *)
end

(* tempering parameters *)
module Tempering =
struct
  open Int32

  let mask_b = 0x9d2c5680l
  let mask_c = 0xefc60000l
  let shift_u y = shift_right y 11
  let shift_s y = shift_left y 7
  let shift_t y = shift_left y 15
  let shift_l y = shift_right y 18
end

external random_seed: unit -> int array = "caml_sys_random_seed";;

module State =
struct

  type t = {mt : int32 array; mutable idx : int}

  let create () = {
    mt = Array.make Period.n 0l; (* the array for the state vector  *)
    idx = 0
  }

  let mt v = v.mt

  let idx v = v.idx

  let incr_idx v = v.idx <- succ v.idx

  let reset_idx v = v.idx <- 0

  (* initializing the array with a NONZERO seed *)
  let init v seed : t =
    (* setting initial seeds to mt[N] using
       the generator Line 25 of Table 1 in
       [KNUTH 1981, The Art of Computer Programming
       Vol. 2 (2nd Ed.), pp102]                     *)
    v.mt.(0) <- seed; (* Int32.logand seed 0xffffffffl; *)
    for i = 1 to pred Period.n do
      let k = Int32.mul 69069l v.mt.(pred i) in
      v.mt.(i) <- k (* Int32.logand k 0xffffffffl *)
    done;
    v

  let make seed =
    init (create ()) seed

  let full_init state v =
    let prime = 104729 in
    let seed = 
      match Array.length v with
        0 -> failwith "MersenneTwister : cannot self init with null seed"
      | k when k < 4 -> v.(0) * prime + v.(1)
      | k when k < 8 -> ((v.(0) * prime + v.(1)) * prime + v.(2)) * prime + v.(3)
      | k -> 
         let k = ref v.(0) in
         for i = 1 to 7 do
           k := prime * !k
         done;
         !k
    in
    init state (Int32.of_int seed)

  let make_self_init () =
    full_init (create ()) (random_seed ())

end

(* minimal random generator *)
module Core =
struct

  (* returns a full range 32bits int random value *)
  let rand state =
    let open State in
    let mag01 = [| 0l ; Period.matrix_a |]
    (* mag01[x] = x * MATRIX_A  for x=0,1 *)
    in
    let refill s =
      let newy s v1 v2 =
	Int32.logor (Int32.logand (mt s).(v1) Period.upper_mask) 
                    (Int32.logand (mt s).(v2) Period.lower_mask)
      and storemt s idx1 idx2 y =
	(mt s).(idx1) <- Int32.logxor
	                   (Int32.logxor (mt s).(idx2) (Int32.shift_right y 1))
                           mag01.((Int32.to_int y) land 0x1)
      in
      for kk = 0 to pred (Period.n - Period.m) do
	let y = newy s kk (succ kk)
	in
        storemt s kk (kk + Period.m) y
      done;
      for kk = Period.n - Period.m to Period.n - 2 do
	let y = newy s kk (succ kk)
	in
	storemt s kk (kk + Period.m - Period.n) y
      done;
      let y = newy s (Period.n - 1) 0
      in
      storemt s (Period.n - 1) (Period.m - 1) y;
      reset_idx s
    in
    let s = state in
    if idx s >= Period.n
    then refill s;
    let y = (mt s).(idx s) in
    let y = (Int32.logxor y (Tempering.shift_u y)) in
    let y = (Int32.logxor y (Int32.logand (Tempering.shift_s y) Tempering.mask_b)) in
    let y = (Int32.logxor y (Int32.logand (Tempering.shift_t y) Tempering.mask_c)) in
    incr_idx s ; Int32.logxor y (Tempering.shift_l y)

  (* 32 bits generator between 0 and {x} *)
  let int32 state (x : int32) =
    Int32.rem (rand state) x

end

module Rng = struct

  (* 64 bits generator between 0 and {x} *)
  let int64 state (x : int64) =
    let v1 = Int64.of_int32 (Core.rand state)
    and v2 =
      let r = Int64.of_int32 (Core.rand state)
      and mask = Int64.lognot 0xffL in
      (* clear up 2 lsbits and shift left 30bits *)
      Int64.shift_left (Int64.logand r mask) 30
    in
    Int64.rem (Int64.logor v1 v2) x

  (* returns a random int between 0 and x *)
  let int state (x : int) =
    match Sys.word_size with
      32 -> Int32.to_int (Core.int32 state (Int32.of_int x))
    | 64 -> Int64.to_int (int64 state (Int64.of_int x))
    | _  -> failwith "Mersenne Twister : unsupported word size"

  (* returns a random nativeint between 0 and x *)
  let nativeint state (x : nativeint) =
    match Sys.word_size with
      32 -> Nativeint.of_int32 (Core.int32 state (Nativeint.to_int32 x))
    | 64 -> Int64.to_nativeint (int64 state (Int64.of_nativeint x))
    | _  -> failwith "Mersenne Twister : unsupported word size"

  (* helper for single precision range floats *) 
  let rawsingle state =
    let scale = 4294967296.0  (* 2^32 *)
    and r1 = Int32.to_float (Core.rand state)
    in r1 /. scale

  (* helper for double precision range floats *)
  let rawdouble state =
    let scale = 4294967296.0  (* 2^32 *)
    and r1 = Int32.to_float (Core.rand state)
    and r2 = Int32.to_float (Core.rand state)
    in (r1 /. scale +. r2) /. scale

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
