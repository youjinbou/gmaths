module type T =
  sig
    type t
    val zero : t
    val one : t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val opp : t -> t
    val rand : t -> t
    val modulo : t -> t -> t
    val epsilon : t
    val abs : t -> t
    val sqrt : t -> t
    val to_string : t -> string
  end
module Make :
  functor (T : T) ->
    sig
      module Scalar :
        sig
          type t = T.t
          val zero : t
          val one : t
          val add : t -> t -> t
          val sub : t -> t -> t
          val mul : t -> t -> t
          val div : t -> t -> t
          val opp : t -> t
          val rand : t -> t
          val modulo : t -> t -> t
          val epsilon : t
          val abs : t -> t
          val sqrt : t -> t
          val to_string : t -> string
        end
      type t = T.t array
      type tuple_t = T.t * T.t
      val ( + ) : T.t -> T.t -> T.t
      val ( - ) : T.t -> T.t -> T.t
      val ( * ) : T.t -> T.t -> T.t
      val ( / ) : T.t -> T.t -> T.t
      val abs : T.t -> T.t
      val size : int
      val init : (int -> 'a) -> 'a array
      val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
      val fold_right : ('a -> 'b -> 'a) -> 'b array -> 'a -> 'a
      val get : 'a array -> int -> 'a
      val set : 'a array -> int -> 'a -> unit
      val map : ('a -> 'b) -> 'a array -> 'b array
      val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
      val map3 :
        ('a -> 'b -> 'c -> 'd) ->
        'a array -> 'b array -> 'c array -> 'd array
      val map4 :
        ('a -> 'b -> 'c -> 'd -> 'e) ->
        'a array -> 'b array -> 'c array -> 'd array -> 'e array
      val make : 'a -> 'a array
      val null : unit -> T.t array
      val one : unit -> T.t array
      val unit : int -> T.t array
      val opp : T.t array -> T.t array
      val neg : T.t array -> T.t array
      val add : T.t array -> T.t array -> T.t array
      val add3 : T.t array -> T.t array -> T.t array -> T.t array
      val add4 :
        T.t array -> T.t array -> T.t array -> T.t array -> T.t array
      val sub : T.t array -> T.t array -> T.t array
      val sub3 : T.t array -> T.t array -> T.t array -> T.t array
      val sub4 :
        T.t array -> T.t array -> T.t array -> T.t array -> T.t array
      val scale : T.t array -> T.t -> T.t array
      val mul : T.t array -> T.t array -> T.t array
      val muladd : T.t array -> T.t -> T.t array -> T.t array
      val dot : T.t array -> T.t array -> T.t
      val clone : 'a array -> 'a array
      val copy : 'a array -> 'a array -> unit
      val to_tuple : 'a array -> 'a * 'a
      val of_tuple : 'a * 'a -> 'a array
      val random : T.t array -> T.t array
      val modulo : T.t array -> T.t -> T.t array
      val below_epsilon : T.t array -> bool
      val for_all : ('a -> bool) -> 'a array -> bool
      val min : 'a array -> 'a array -> 'a array
      val max : 'a array -> 'a array -> 'a array
      val length : T.t array -> T.t
      val normalize : T.t array -> T.t array
      val to_string : T.t array -> string
    end
module Float :
  sig
    module Scalar :
      sig
        type t = Primitives.Float.t
        val zero : t
        val one : t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val opp : t -> t
        val rand : t -> t
        val modulo : t -> t -> t
        val epsilon : t
        val abs : t -> t
        val sqrt : t -> t
        val to_string : t -> string
      end
    type t = Primitives.Float.t array
    type tuple_t = Primitives.Float.t * Primitives.Float.t
    val ( + ) :
      Primitives.Float.t -> Primitives.Float.t -> Primitives.Float.t
    val ( - ) :
      Primitives.Float.t -> Primitives.Float.t -> Primitives.Float.t
    val ( * ) :
      Primitives.Float.t -> Primitives.Float.t -> Primitives.Float.t
    val ( / ) :
      Primitives.Float.t -> Primitives.Float.t -> Primitives.Float.t
    val abs : Primitives.Float.t -> Primitives.Float.t
    val size : int
    val init : (int -> 'a) -> 'a array
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val fold_right : ('a -> 'b -> 'a) -> 'b array -> 'a -> 'a
    val get : 'a array -> int -> 'a
    val set : 'a array -> int -> 'a -> unit
    val map : ('a -> 'b) -> 'a array -> 'b array
    val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
    val map3 :
      ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array
    val map4 :
      ('a -> 'b -> 'c -> 'd -> 'e) ->
      'a array -> 'b array -> 'c array -> 'd array -> 'e array
    val make : 'a -> 'a array
    val null : unit -> Primitives.Float.t array
    val one : unit -> Primitives.Float.t array
    val unit : int -> Primitives.Float.t array
    val opp : Primitives.Float.t array -> Primitives.Float.t array
    val neg : Primitives.Float.t array -> Primitives.Float.t array
    val add :
      Primitives.Float.t array ->
      Primitives.Float.t array -> Primitives.Float.t array
    val add3 :
      Primitives.Float.t array ->
      Primitives.Float.t array ->
      Primitives.Float.t array -> Primitives.Float.t array
    val add4 :
      Primitives.Float.t array ->
      Primitives.Float.t array ->
      Primitives.Float.t array ->
      Primitives.Float.t array -> Primitives.Float.t array
    val sub :
      Primitives.Float.t array ->
      Primitives.Float.t array -> Primitives.Float.t array
    val sub3 :
      Primitives.Float.t array ->
      Primitives.Float.t array ->
      Primitives.Float.t array -> Primitives.Float.t array
    val sub4 :
      Primitives.Float.t array ->
      Primitives.Float.t array ->
      Primitives.Float.t array ->
      Primitives.Float.t array -> Primitives.Float.t array
    val scale :
      Primitives.Float.t array ->
      Primitives.Float.t -> Primitives.Float.t array
    val mul :
      Primitives.Float.t array ->
      Primitives.Float.t array -> Primitives.Float.t array
    val muladd :
      Primitives.Float.t array ->
      Primitives.Float.t ->
      Primitives.Float.t array -> Primitives.Float.t array
    val dot :
      Primitives.Float.t array ->
      Primitives.Float.t array -> Primitives.Float.t
    val clone : 'a array -> 'a array
    val copy : 'a array -> 'a array -> unit
    val to_tuple : 'a array -> 'a * 'a
    val of_tuple : 'a * 'a -> 'a array
    val random : Primitives.Float.t array -> Primitives.Float.t array
    val modulo :
      Primitives.Float.t array ->
      Primitives.Float.t -> Primitives.Float.t array
    val below_epsilon : Primitives.Float.t array -> bool
    val for_all : ('a -> bool) -> 'a array -> bool
    val min : 'a array -> 'a array -> 'a array
    val max : 'a array -> 'a array -> 'a array
    val length : Primitives.Float.t array -> Primitives.Float.t
    val normalize : Primitives.Float.t array -> Primitives.Float.t array
    val to_string : Primitives.Float.t array -> string
  end
module Int :
  sig
    module Scalar :
      sig
        type t = Primitives.Int.t
        val zero : t
        val one : t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val opp : t -> t
        val rand : t -> t
        val modulo : t -> t -> t
        val epsilon : t
        val abs : t -> t
        val sqrt : t -> t
        val to_string : t -> string
      end
    type t = Primitives.Int.t array
    type tuple_t = Primitives.Int.t * Primitives.Int.t
    val ( + ) : Primitives.Int.t -> Primitives.Int.t -> Primitives.Int.t
    val ( - ) : Primitives.Int.t -> Primitives.Int.t -> Primitives.Int.t
    val ( * ) : Primitives.Int.t -> Primitives.Int.t -> Primitives.Int.t
    val ( / ) : Primitives.Int.t -> Primitives.Int.t -> Primitives.Int.t
    val abs : Primitives.Int.t -> Primitives.Int.t
    val size : int
    val init : (int -> 'a) -> 'a array
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val fold_right : ('a -> 'b -> 'a) -> 'b array -> 'a -> 'a
    val get : 'a array -> int -> 'a
    val set : 'a array -> int -> 'a -> unit
    val map : ('a -> 'b) -> 'a array -> 'b array
    val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
    val map3 :
      ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array
    val map4 :
      ('a -> 'b -> 'c -> 'd -> 'e) ->
      'a array -> 'b array -> 'c array -> 'd array -> 'e array
    val make : 'a -> 'a array
    val null : unit -> Primitives.Int.t array
    val one : unit -> Primitives.Int.t array
    val unit : int -> Primitives.Int.t array
    val opp : Primitives.Int.t array -> Primitives.Int.t array
    val neg : Primitives.Int.t array -> Primitives.Int.t array
    val add :
      Primitives.Int.t array ->
      Primitives.Int.t array -> Primitives.Int.t array
    val add3 :
      Primitives.Int.t array ->
      Primitives.Int.t array ->
      Primitives.Int.t array -> Primitives.Int.t array
    val add4 :
      Primitives.Int.t array ->
      Primitives.Int.t array ->
      Primitives.Int.t array ->
      Primitives.Int.t array -> Primitives.Int.t array
    val sub :
      Primitives.Int.t array ->
      Primitives.Int.t array -> Primitives.Int.t array
    val sub3 :
      Primitives.Int.t array ->
      Primitives.Int.t array ->
      Primitives.Int.t array -> Primitives.Int.t array
    val sub4 :
      Primitives.Int.t array ->
      Primitives.Int.t array ->
      Primitives.Int.t array ->
      Primitives.Int.t array -> Primitives.Int.t array
    val scale :
      Primitives.Int.t array -> Primitives.Int.t -> Primitives.Int.t array
    val mul :
      Primitives.Int.t array ->
      Primitives.Int.t array -> Primitives.Int.t array
    val muladd :
      Primitives.Int.t array ->
      Primitives.Int.t -> Primitives.Int.t array -> Primitives.Int.t array
    val dot :
      Primitives.Int.t array -> Primitives.Int.t array -> Primitives.Int.t
    val clone : 'a array -> 'a array
    val copy : 'a array -> 'a array -> unit
    val to_tuple : 'a array -> 'a * 'a
    val of_tuple : 'a * 'a -> 'a array
    val random : Primitives.Int.t array -> Primitives.Int.t array
    val modulo :
      Primitives.Int.t array -> Primitives.Int.t -> Primitives.Int.t array
    val below_epsilon : Primitives.Int.t array -> bool
    val for_all : ('a -> bool) -> 'a array -> bool
    val min : 'a array -> 'a array -> 'a array
    val max : 'a array -> 'a array -> 'a array
    val length : Primitives.Int.t array -> Primitives.Int.t
    val normalize : Primitives.Int.t array -> Primitives.Int.t array
    val to_string : Primitives.Int.t array -> string
  end
