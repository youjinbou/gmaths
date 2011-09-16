
type vector = float array
type t = vector array

val width : t -> int
val height : t -> int
val column_get : t -> int -> int -> float
val line_get : t -> int -> int -> float
val column_set : t -> int -> int -> float -> unit
val line_set : t -> int -> int -> float -> unit
val get : t -> int -> int -> float
val set : t -> int -> int -> float -> unit
val row : t -> int -> vector
val col : t -> int -> vector
val init : int -> int -> (int -> int -> float) -> t
val transpose : int -> int -> t -> t
val id : int -> t
val minor : int -> int -> int -> (int -> int -> 'a) -> int -> int -> 'a
val dotf : int -> (int -> float) -> (int -> float) -> float
val mult : t -> t -> t
val make : int -> int -> float -> t
val map : (float -> float) -> t -> t
val mapi : (int -> int -> float -> float) -> t -> t
val copy : t -> t
val null : int -> int -> t
val fscale : t -> float -> t

type mat2x2 = t
type mat3x3 = t
type mat4x4 = t

module Mat2 :
sig
  type t = mat2x2
  val size : int
  val identity : unit -> t
  val null : unit -> t
  val detf : (int -> int -> float) -> float
  val transposef : t -> t
  val rotation_z : float -> t
  val rotation_y : float -> t
  val rotation_x : float -> t
end

module Mat3 :
sig
  type t = mat3x3
  val size : int
  val identity : unit -> t
  val null : unit -> t
  val detf : (int -> int -> float) -> float
  val transpose : t -> t
  val dotf : (int -> float) -> (int -> float) -> float
  val apply : t -> vector -> vector
  val mult : t -> t -> t
  val rotation_z : float -> t
  val rotation_y : float -> t
  val rotation_x : float -> t
  val rotation : int -> float -> t
  val rotation2 : int -> float -> t
  val minor : int -> int -> (int -> int -> float) -> int -> int -> float
  val inversef : (int -> int -> float) -> t
  val inverse : t -> t
end

module Mat4 :
sig
  type t = mat4x4
  val size : int
  val row : t -> int -> vector
  val col : t -> int -> vector
  val setrow : t -> int -> vector -> unit
  val setcol : t -> int -> vector -> unit
  val detf : (int -> int -> float) -> float
  val det : t -> float
  val null : unit -> t
  val identity : unit -> t
  val rotation_z : float -> t
  val rotation_y : float -> t
  val rotation_x : float -> t
  val transpose : t -> t
  val minor : int -> int -> (int -> int -> float) -> int -> int -> float
  val inversef : (int -> int -> float) -> t
  val inverse : t -> t
  val dotf : (int -> float) -> (int -> float) -> float
  val mult : t -> t -> t
  val apply : t -> vector -> vector
  val translation : float -> float -> float -> t
  val translationv : vector -> t
  val invtranslationv : Primitives.Float.t array -> t
  val rotation2 : int -> float -> t
  val rotation : int -> float -> t
  val invrotationv : vector -> t
  val rotationv : vector -> t
  val scale : float -> float -> float -> t
  val scalev : vector -> t
  val invscalev : vector -> t
  val prepare : vector -> vector -> vector -> t
  val invprepare : vector -> vector -> vector -> t
end
