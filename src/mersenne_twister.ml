(*
  a perlin noise library

  Copyright (C) 2010  Didier Cassirame

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

*)
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
   of  Ocaml for a perlin noise library. Of course, the Random 
   module is perfectly fit (if not better).
   Hopefully,  this code is faithfull to the original C imple-
   mentation.

*)

(* convenient shortcuts *)
let ( land_ ) = Int32.logand 
let ( lor_ )  = Int32.logor 
let ( lxor_ ) = Int32.logxor
let ( *! )    = Int32.mul
let ( lsl_ )  = Int32.shift_left
let ( lsr_ )  = Int32.shift_right
let to_int    = Int32.to_int
let of_int    = Int32.of_int
let mod_      = Int32.rem

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

  let mask_b = 0x9d2c5680l
  let mask_c = 0xefc60000l
  let shift_u y = (lsr_ y 11)
  let shift_s y = (lsl_ y 7)
  let shift_t y = (lsl_ y 15)
  let shift_l y = (lsr_ y 18)
end

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
    v.mt.(0) <- land_ seed 0xffffffffl;
    for i = 1 to pred Period.n do
      let k = 69069l *! v.mt.(pred i)
      in
      v.mt.(i) <- land_ k 0xffffffffl
    done;
    v
      
end

module type DATA = 
sig
  val state : State.t
end

module Make (D : DATA) =
struct

  let rand () = 
    let mt        = State.mt
    and idx       = State.idx
    and reset_idx = State.reset_idx
    and incr_idx  = State.incr_idx
    and mag01 = [| 0l ; Period.matrix_a |]
    (* mag01[x] = x * MATRIX_A  for x=0,1 *)
    in
    let refill s =
      let newy s v1 v2 = 
	lor_ (land_ (mt s).(v1) Period.upper_mask) (land_ (mt s).(v2) Period.lower_mask)
      and storemt s idx1 idx2 y =
	(mt s).(idx1) <- lxor_ 
	  (lxor_ (mt s).(idx2) (lsr_ y 1)) mag01.((to_int y) land 0x1)
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
    let s = D.state in
    if idx s >= Period.n
    then refill s;
    let y = (mt s).(idx s) in
    let y = (lxor_ y (Tempering.shift_u y)) in
    let y = (lxor_ y (land_ (Tempering.shift_s y) Tempering.mask_b)) in
    let y = (lxor_ y (land_ (Tempering.shift_t y) Tempering.mask_c)) in
    incr_idx s ; lxor_ y (Tempering.shift_l y)


  (* 32 bits generator *)
  let int32 (x : int) = 
    let res = mod_ (rand ()) (Int32.of_int x)
    in Int32.to_int res
    
  (* 64 bits generator *)
  let int64 (x : int) = 
    let res = 
      let v1 = Int64.of_int32 (rand ())
      and v2 = 
	let r = Int64.of_int32 (rand ())
	and mask = Int64.lognot 0xffL
	in
	(* clear up 2 lsbits and shift left 30bits *)
	Int64.shift_left (Int64.logand r mask) 30 
      in 
      Int64.rem (Int64.logor v1 v2) (Int64.of_int x)
    in
    Int64.to_int res
end

module DefaultData =
struct 
  let state = State.init (State.create ()) 4357l
end

module Default = Make(DefaultData)

let int32 = Default.int32
let int64 = Default.int64

let int (x : int) = 
  match Sys.word_size with
      32 -> Default.int32 x
    | 64 -> Default.int64 x
    | _  -> failwith "Mersenne Twister : unsupported word size"
