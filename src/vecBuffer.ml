(* ---------------------------------------------------------------------

     This code provided under the LGPL License V2


     Buffer code lifted from the original Buffer module distributed with 
     Ocaml:
     "
		Xavier Leroy, projet Cristal, INRIA Rocquencourt         

      Copyright 1996 Institut National de Recherche en Informatique et   
      en Automatique.  All rights reserved.  This file is distributed    
      under the terms of the GNU Library General Public License, with    
      the special exception on linking described in file ../LICENSE."


     Thanks!

   --------------------------------------------------------------------- *)


module type VECTOR =
sig
  
  type 'a t

  val set        : 'a t -> int -> 'a -> unit
  val get        : 'a t -> int -> 'a
  val make       : int -> 'a -> 'a t
  val sub        : 'a t -> int -> int -> 'a t
  val blit       : 'a t -> int -> 'a t -> int -> int -> unit
  val length     : 'a t -> int
  val max_length : int

end

module Array : VECTOR with type 'a t = 'a array = 
struct

  include Array
  
  type 'a t = 'a array

  let max_length = Sys.max_array_length

end

module type MONO_VECTOR =
sig
  
  type t
  type e

  val set        : t -> int -> e -> unit
  val get        : t -> int -> e
  val make       : int -> e -> t
  val sub        : t -> int -> int -> t
  val blit       : t -> int -> t -> int -> int -> unit
  val length     : t -> int
  val max_length : int

end

module String : MONO_VECTOR with type t = string and type e = char =
struct

  include String
  type e = char


  let max_length = Sys.max_string_length


end


module type S =
sig

  type 'a t
  type 'a vector

  val create     : int -> 'a -> 'a t
  val contents   : 'a t -> 'a vector
  val sub        : 'a t -> int -> int -> 'a vector
  val blit       : 'a t -> int -> 'a vector -> int -> int -> unit
  val nth        : 'a t -> int -> 'a
  val length     : 'a t -> int
  val clear      : 'a t -> unit
  val reset      : 'a t -> unit
  val resize     : 'a t -> int -> unit
  val add        : 'a t -> 'a -> int
  val add_vector : 'a t -> 'a vector -> int
  val add_subv   : 'a t -> 'a vector -> int -> int -> int
  val add_buffer : 'a t -> 'a t -> int

end

module type SMONO =
sig

  type t
  type vector
  type e

  val create     : int -> e -> t
  val contents   : t -> vector
  val sub        : t -> int -> int -> vector
  val blit       : t -> int -> vector -> int -> int -> unit
  val nth        : t -> int -> e
  val length     : t -> int
  val clear      : t -> unit
  val reset      : t -> unit
  val resize     : t -> int -> unit
  val add        : t -> e -> int
  val add_vector : t -> vector -> int
  val add_subv   : t -> vector -> int -> int -> int
  val add_buffer : t -> t -> int

end


module Make (V : VECTOR) =
struct

  type 'a vector = 'a V.t

  type 'a t = {
    mutable buffer   : 'a vector;
    mutable position : int;
    mutable length   : int;
    initial_buffer   : 'a vector;
    default_value    : 'a;
  }

  let create n v =
    let n = if n < 1 then 1 else n in
    let n = if n > V.max_length then V.max_length else n in
    let s = V.make n v in
    {buffer = s; position = 0; length = n; initial_buffer = s; default_value = v}

  let contents b = V.sub b.buffer 0 b.position

  let sub b ofs len =
    if ofs < 0 || len < 0 || ofs > b.position - len
    then invalid_arg "VecBuffer.sub"
    else begin
      let r = V.make len b.default_value in
      V.blit b.buffer ofs r 0 len;
      r
    end

  let blit src srcoff dst dstoff len =
    if len < 0 || srcoff < 0 || srcoff > src.position - len
      || dstoff < 0 || dstoff > (V.length dst) - len
    then invalid_arg "VecBuffer.blit"
    else
      V.blit src.buffer srcoff dst dstoff len

  let nth b ofs =
    if ofs < 0 || ofs >= b.position then
      invalid_arg "VecBuffer.nth"
    else V.get b.buffer ofs

  let length b = b.position

  let clear b = b.position <- 0

  let reset b =
    b.position <- 0; b.buffer <- b.initial_buffer;
    b.length <- V.length b.buffer

  let resize b more =
    let len = b.length in
    let new_len = ref len in
    while b.position + more > !new_len do new_len := 2 * !new_len done;
    if !new_len > V.max_length then begin
      if b.position + more <= V.max_length
      then new_len := V.max_length
      else failwith "VecBuffer: cannot grow buffer"
    end;
    let new_buffer = V.make !new_len b.default_value in
    V.blit b.buffer 0 new_buffer 0 b.position;
    b.buffer <- new_buffer;
    b.length <- !new_len

  let add b c =
    let pos = b.position in
    if pos >= b.length then resize b 1;
    V.set b.buffer pos c;
    b.position <- pos + 1;
    pos

  let add_subv b s offset len =
    if offset < 0 || len < 0 || offset > V.length s - len
    then invalid_arg "VecBuffer.add_subv";
    let new_position = b.position + len in
    if new_position > b.length then resize b len;
    V.blit s offset b.buffer b.position len;
    let pos = b.position in
    b.position <- new_position; pos

  let add_vector b s =
    let len = V.length s in
    let new_position = b.position + len in
    if new_position > b.length then resize b len;
    V.blit s 0 b.buffer b.position len;
    let pos = b.position in
    b.position <- new_position; pos

  let add_buffer b bs =
    add_subv b bs.buffer 0 bs.position

end

module ArrayBuffer = Make(Array)

module MakeMono (V : MONO_VECTOR) : SMONO =
struct

  type vector = V.t
  type e      = V.e

  type t = {
    mutable buffer   : vector;
    mutable position : int;
    mutable length   : int;
    initial_buffer   : vector;
    default_value    : e;
  }

  let create n v =
    let n = if n < 1 then 1 else n in
    let n = if n > V.max_length then V.max_length else n in
    let s = V.make n v in
    {buffer = s; position = 0; length = n; initial_buffer = s; default_value = v}

  let contents b = V.sub b.buffer 0 b.position

  let sub b ofs len =
    if ofs < 0 || len < 0 || ofs > b.position - len
    then invalid_arg "VecBuffer.sub"
    else begin
      let r = V.make len b.default_value in
      V.blit b.buffer ofs r 0 len;
      r
    end

  let blit src srcoff dst dstoff len =
    if len < 0 || srcoff < 0 || srcoff > src.position - len
      || dstoff < 0 || dstoff > (V.length dst) - len
    then invalid_arg "VecBuffer.blit"
    else
      V.blit src.buffer srcoff dst dstoff len


  let nth b ofs =
    if ofs < 0 || ofs >= b.position then
      invalid_arg "VecBuffer.nth"
    else V.get b.buffer ofs

  let length b = b.position

  let clear b = b.position <- 0

  let reset b =
    b.position <- 0; b.buffer <- b.initial_buffer;
    b.length <- V.length b.buffer

  let resize b more =
    let len = b.length in
    let new_len = ref len in
    while b.position + more > !new_len do new_len := 2 * !new_len done;
    if !new_len > V.max_length then begin
      if b.position + more <= V.max_length
      then new_len := V.max_length
      else failwith "VecBuffer: cannot grow buffer"
    end;
    let new_buffer = V.make !new_len b.default_value in
    V.blit b.buffer 0 new_buffer 0 b.position;
    b.buffer <- new_buffer;
    b.length <- !new_len

  let add b c =
    let pos = b.position in
    if pos >= b.length then resize b 1;
    V.set b.buffer pos c;
    b.position <- pos + 1; pos

  let add_subv b s offset len =
    if offset < 0 || len < 0 || offset > V.length s - len
    then invalid_arg "VecBuffer.add_subv";
    let new_position = b.position + len in
    if new_position > b.length then resize b len;
    V.blit s offset b.buffer b.position len;
    let pos = b.position in
    b.position <- new_position; pos

  let add_vector b s =
    let len = V.length s in
    let new_position = b.position + len in
    if new_position > b.length then resize b len;
    V.blit s 0 b.buffer b.position len;
    let pos = b.position in 
    b.position <- new_position; pos

  let add_buffer b bs =
    add_subv b bs.buffer 0 bs.position

end


module StringBuffer = MakeMono(String)
