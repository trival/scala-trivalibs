package trivalibs.graphics.math.cpu

import trivalibs.graphics.math.*
import trivalibs.bufferdata.F32
import trivalibs.bufferdata.StructRef

// === implementations for common matrix types ===
// Note: Mat2Buffer uses F32 by default (for GPU upload)

type Mat2Buffer = (F32, F32, F32, F32)

object Mat2Buffer:
  given Mat2Mutable[StructRef[Mat2Buffer]]:
    extension (m: StructRef[Mat2Buffer])
      inline def m00 = m.getAt(0): Double
      inline def m01 = m.getAt(1): Double
      inline def m10 = m.getAt(2): Double
      inline def m11 = m.getAt(3): Double
      inline def m00_=(v: Double) = m.setAt(0)(v.toFloat)
      inline def m01_=(v: Double) = m.setAt(1)(v.toFloat)
      inline def m10_=(v: Double) = m.setAt(2)(v.toFloat)
      inline def m11_=(v: Double) = m.setAt(3)(v.toFloat)

  given Mat2MutableOps[StructRef[Mat2Buffer]] =
    new Mat2MutableOps[StructRef[Mat2Buffer]] {}

type Mat2Tuple = (Double, Double, Double, Double)

// format: off
object Mat2Tuple extends Mat2ImmutableOps[Mat2Tuple]:
  inline def create(m00: Double, m01: Double, m10: Double, m11: Double) = (m00, m01, m10, m11)
  given Mat2ImmutableOps[Mat2Tuple] = Mat2Tuple
// format: on

  given Mat2Base[Mat2Tuple]:
    extension (m: Mat2Tuple)
      inline def m00 = m._1
      inline def m01 = m._2
      inline def m10 = m._3
      inline def m11 = m._4


// format: off
/** Mutable 2×2 matrix — 2D rotation/scale. `*` (matrix or vector),
  * `.determinant`, `.inverse`, `.transpose`. Column-major; siblings
  * `Mat2Tuple`/`Mat2Buffer`. */
class Mat2(
    var m00: Double = 1.0, var m01: Double = 0.0,
    var m10: Double = 0.0, var m11: Double = 1.0
)
// format: on

// format: off
object Mat2 extends Mat2ImmutableOps[Mat2]:
  inline def create(m00: Double, m01: Double, m10: Double, m11: Double) = Mat2(m00, m01, m10, m11)
  given Mat2ImmutableOps[Mat2] = Mat2
// format: on

  given Mat2Mutable[Mat2]:
    extension (m: Mat2)
      inline def m00: Double = m.m00
      inline def m01: Double = m.m01
      inline def m10: Double = m.m10
      inline def m11: Double = m.m11
      inline def m00_=(v: Double) = m.m00 = v
      inline def m01_=(v: Double) = m.m01 = v
      inline def m10_=(v: Double) = m.m10 = v
      inline def m11_=(v: Double) = m.m11 = v

  given Mat2MutableOps[Mat2] = new Mat2MutableOps[Mat2] {}
