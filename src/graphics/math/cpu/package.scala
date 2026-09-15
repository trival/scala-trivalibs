package trivalibs.graphics.math.cpu

inline given Conversion[Float, Float] = identity
inline given Conversion[Double, Double] = identity
inline given Conversion[Float, Double] = identity
inline given Conversion[Double, Float]:
  inline def apply(x: Double): Float = x.toFloat

// `lerpInstance` is excluded on purpose: exporting it would put its `lerp`
// extension in identifier scope, where it hides the ops traits' richer `lerp`
// overload sets. Implicit search still finds it in the companion.
export Vec2Buffer.given
export Vec2dBuffer.given
export Vec2Tuple.given
export Vec2.{lerpInstance as _, given}

export Vec3Buffer.given
export Vec3dBuffer.given
export Vec3Tuple.given
export Vec3.{lerpInstance as _, given}

export Vec4Buffer.given
export Vec4dBuffer.given
export Vec4Tuple.given
export Vec4.{lerpInstance as _, given}

// Mat2Buffer and Mat2Tuple collide with Vec4Buffer and Vec4Tuple, so not exported here
// export Mat2Buffer.given
// export Mat2Tuple.given
export Mat2.given

export Mat3Buffer.given
export Mat3PaddedBuffer.given
export Mat3Tuple.given
export Mat3.given

export Mat4Buffer.given
export Mat4Tuple.given
export Mat4.given
