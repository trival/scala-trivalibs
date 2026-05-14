package trivalibs.graphics.math.cpu

import trivalibs.graphics.math.*
import trivalibs.bufferdata.F32
import trivalibs.bufferdata.StructRef

// === implementations for common matrix types ===
// Note: Mat4Buffer uses F32 by default (for GPU upload)

// format: off
type Mat4Buffer = (
  F32, F32, F32, F32,
  F32, F32, F32, F32,
  F32, F32, F32, F32,
  F32, F32, F32, F32
)
// format: on

object Mat4Buffer:
  given Mat4Mutable[StructRef[Mat4Buffer]]:
    extension (m: StructRef[Mat4Buffer])
      inline def m00 = m.getAt(0): Double
      inline def m01 = m.getAt(1): Double
      inline def m02 = m.getAt(2): Double
      inline def m03 = m.getAt(3): Double
      inline def m10 = m.getAt(4): Double
      inline def m11 = m.getAt(5): Double
      inline def m12 = m.getAt(6): Double
      inline def m13 = m.getAt(7): Double
      inline def m20 = m.getAt(8): Double
      inline def m21 = m.getAt(9): Double
      inline def m22 = m.getAt(10): Double
      inline def m23 = m.getAt(11): Double
      inline def m30 = m.getAt(12): Double
      inline def m31 = m.getAt(13): Double
      inline def m32 = m.getAt(14): Double
      inline def m33 = m.getAt(15): Double
      inline def m00_=(v: Double) = m.setAt(0)(v.toFloat)
      inline def m01_=(v: Double) = m.setAt(1)(v.toFloat)
      inline def m02_=(v: Double) = m.setAt(2)(v.toFloat)
      inline def m03_=(v: Double) = m.setAt(3)(v.toFloat)
      inline def m10_=(v: Double) = m.setAt(4)(v.toFloat)
      inline def m11_=(v: Double) = m.setAt(5)(v.toFloat)
      inline def m12_=(v: Double) = m.setAt(6)(v.toFloat)
      inline def m13_=(v: Double) = m.setAt(7)(v.toFloat)
      inline def m20_=(v: Double) = m.setAt(8)(v.toFloat)
      inline def m21_=(v: Double) = m.setAt(9)(v.toFloat)
      inline def m22_=(v: Double) = m.setAt(10)(v.toFloat)
      inline def m23_=(v: Double) = m.setAt(11)(v.toFloat)
      inline def m30_=(v: Double) = m.setAt(12)(v.toFloat)
      inline def m31_=(v: Double) = m.setAt(13)(v.toFloat)
      inline def m32_=(v: Double) = m.setAt(14)(v.toFloat)
      inline def m33_=(v: Double) = m.setAt(15)(v.toFloat)

  given Mat4MutableOps[StructRef[Mat4Buffer]] =
    new Mat4MutableOps[StructRef[Mat4Buffer]] {}

// format: off
type Mat4Tuple = (
    Double, Double, Double, Double,
    Double, Double, Double, Double,
    Double, Double, Double, Double,
    Double, Double, Double, Double
)
// format: on

// format: off
object Mat4Tuple extends Mat4ImmutableOps[Mat4Tuple]:
  inline def create(
      m00: Double, m01: Double, m02: Double, m03: Double,
      m10: Double, m11: Double, m12: Double, m13: Double,
      m20: Double, m21: Double, m22: Double, m23: Double,
      m30: Double, m31: Double, m32: Double, m33: Double,
  ) = (m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33)
  given Mat4ImmutableOps[Mat4Tuple] = Mat4Tuple
// format: on

  given Mat4Base[Mat4Tuple]:
    extension (m: Mat4Tuple)
      inline def m00 = m._1
      inline def m01 = m._2
      inline def m02 = m._3
      inline def m03 = m._4
      inline def m10 = m._5
      inline def m11 = m._6
      inline def m12 = m._7
      inline def m13 = m._8
      inline def m20 = m._9
      inline def m21 = m._10
      inline def m22 = m._11
      inline def m23 = m._12
      inline def m30 = m._13
      inline def m31 = m._14
      inline def m32 = m._15
      inline def m33 = m._16


// format: off
class Mat4(
    var m00: Double = 1.0, var m01: Double = 0.0, var m02: Double = 0.0, var m03: Double = 0.0,
    var m10: Double = 0.0, var m11: Double = 1.0, var m12: Double = 0.0, var m13: Double = 0.0,
    var m20: Double = 0.0, var m21: Double = 0.0, var m22: Double = 1.0, var m23: Double = 0.0,
    var m30: Double = 0.0, var m31: Double = 0.0, var m32: Double = 0.0, var m33: Double = 1.0
)
// format: on

// format: off
object Mat4 extends Mat4ImmutableOps[Mat4]:
  inline def create(
      m00: Double, m01: Double, m02: Double, m03: Double,
      m10: Double, m11: Double, m12: Double, m13: Double,
      m20: Double, m21: Double, m22: Double, m23: Double,
      m30: Double, m31: Double, m32: Double, m33: Double,
  ) = new Mat4(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33)

  given Mat4ImmutableOps[Mat4] = Mat4

  def fromTranslation(t: Vec3): Mat4 = fromTranslation(t.x, t.y, t.z)
  def fromScale(s: Vec3): Mat4       = fromScale(s.x, s.y, s.z)

  /** Build a TRS model matrix directly from translation, rotation (Quat), and scale. */
  // format: off
  def fromTranslationRotationScale(t: Vec3, r: Quat, s: Vec3): Mat4 =
    val x = r.x
    val y = r.y
    val z = r.z
    val w = r.w
    val x2 = x+x
    val y2 = y+y
    val z2 = z+z
    val xx = x*x2
    val xy = x*y2
    val xz = x*z2
    val yy = y*y2
    val yz = y*z2
    val zz = z*z2
    val wx = w*x2
    val wy = w*y2
    val wz = w*z2
    create(
      (1-(yy+zz))*s.x, (xy+wz)*s.x,     (xz-wy)*s.x,     0.0,
      (xy-wz)*s.y,     (1-(xx+zz))*s.y, (yz+wx)*s.y,     0.0,
      (xz+wy)*s.z,     (yz-wx)*s.z,     (1-(xx+yy))*s.z, 0.0,
      t.x,             t.y,             t.z,             1.0,
    )
  // format: on

  given Mat4Mutable[Mat4]:
    extension (m: Mat4)
      inline def m00: Double = m.m00
      inline def m01: Double = m.m01
      inline def m02: Double = m.m02
      inline def m03: Double = m.m03
      inline def m10: Double = m.m10
      inline def m11: Double = m.m11
      inline def m12: Double = m.m12
      inline def m13: Double = m.m13
      inline def m20: Double = m.m20
      inline def m21: Double = m.m21
      inline def m22: Double = m.m22
      inline def m23: Double = m.m23
      inline def m30: Double = m.m30
      inline def m31: Double = m.m31
      inline def m32: Double = m.m32
      inline def m33: Double = m.m33
      inline def m00_=(v: Double) = m.m00 = v
      inline def m01_=(v: Double) = m.m01 = v
      inline def m02_=(v: Double) = m.m02 = v
      inline def m03_=(v: Double) = m.m03 = v
      inline def m10_=(v: Double) = m.m10 = v
      inline def m11_=(v: Double) = m.m11 = v
      inline def m12_=(v: Double) = m.m12 = v
      inline def m13_=(v: Double) = m.m13 = v
      inline def m20_=(v: Double) = m.m20 = v
      inline def m21_=(v: Double) = m.m21 = v
      inline def m22_=(v: Double) = m.m22 = v
      inline def m23_=(v: Double) = m.m23 = v
      inline def m30_=(v: Double) = m.m30 = v
      inline def m31_=(v: Double) = m.m31 = v
      inline def m32_=(v: Double) = m.m32 = v
      inline def m33_=(v: Double) = m.m33 = v

  given Mat4MutableOps[Mat4] = new Mat4MutableOps[Mat4] {}
