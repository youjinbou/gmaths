(** basic Quaternion module *)

type vector = Vec4.Float.t
type scalar = Vec4.Float.scalar

type mat4x4 = Matrix.mat4x4
type t = vector

val of_tuple : scalar * scalar * scalar * scalar -> t
val to_tuple : t -> scalar * scalar * scalar * scalar
val of_xyz : scalar * scalar * scalar -> t
val pure : t -> scalar * scalar * scalar
val norm : t -> scalar
val conjugate : t -> vector
val inverse : t -> vector
val copy : t -> t -> unit
val add : t -> t -> vector
val mul : t -> t -> vector
val rotate : t -> t -> vector
val to_matrix : t -> mat4x4
val of_matrix : mat4x4 -> vector
val of_axis_angle : float * float * float -> float -> t
val to_axis_angle : t -> (float * float * float) * float
val of_angles : float * float * float -> t
