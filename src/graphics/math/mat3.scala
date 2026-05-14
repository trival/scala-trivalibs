package trivalibs.graphics.math

import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// Generic variants — purely abstract shared contract between CPU and GPU.
// ---------------------------------------------------------------------------

// Column-major 3x3 matrix traits
// Storage order: m00, m01, m02, m10, m11, m12, m20, m21, m22
// mColRow convention: m10 = column 1, row 0

trait Mat3BaseG[Num, Mat]:
  extension (m: Mat)
    def m00: Num
    def m01: Num
    def m02: Num
    def m10: Num
    def m11: Num
    def m12: Num
    def m20: Num
    def m21: Num
    def m22: Num
    def determinant: Num

// format: off
trait Mat3ImmutableOpsG[Num, Mat]:

  def create(
      m00: Num, m01: Num, m02: Num,
      m10: Num, m11: Num, m12: Num,
      m20: Num, m21: Num, m22: Num,
  ): Mat

  extension (m: Mat)(using Mat3BaseG[Num, Mat])
    @scala.annotation.targetName("matMulG")
    def *(other: Mat): Mat
    @scala.annotation.targetName("vecMulG")
    def *[Vec](v: Vec)(using Vec3BaseG[Num, Vec], Vec3ImmutableOpsG[Num, Vec]): Vec
// format: on

// ---------------------------------------------------------------------------
// CPU-specific variants — concrete Double implementations + CPU-only ops.
// ---------------------------------------------------------------------------

trait Mat3Base[Mat] extends Mat3BaseG[Double, Mat]:
  extension (m: Mat)
    // Column getters
    inline def col0: (Double, Double, Double) = (m.m00, m.m01, m.m02)
    inline def col1: (Double, Double, Double) = (m.m10, m.m11, m.m12)
    inline def col2: (Double, Double, Double) = (m.m20, m.m21, m.m22)

    // Row getters
    inline def row0: (Double, Double, Double) = (m.m00, m.m10, m.m20)
    inline def row1: (Double, Double, Double) = (m.m01, m.m11, m.m21)
    inline def row2: (Double, Double, Double) = (m.m02, m.m12, m.m22)

    def determinant: Double =
      m.m00 * (m.m11 * m.m22 - m.m12 * m.m21) -
        m.m10 * (m.m01 * m.m22 - m.m02 * m.m21) +
        m.m20 * (m.m01 * m.m12 - m.m02 * m.m11)

trait Mat3Mutable[Mat] extends Mat3Base[Mat]:
  extension (m: Mat)
    def m00_=(v: Double): Unit
    def m01_=(v: Double): Unit
    def m02_=(v: Double): Unit
    def m10_=(v: Double): Unit
    def m11_=(v: Double): Unit
    def m12_=(v: Double): Unit
    def m20_=(v: Double): Unit
    def m21_=(v: Double): Unit
    def m22_=(v: Double): Unit

    // Column setters
    inline def col0_=(c: (Double, Double, Double)): Unit =
      m.m00 = c._1
      m.m01 = c._2
      m.m02 = c._3
    inline def col1_=(c: (Double, Double, Double)): Unit =
      m.m10 = c._1
      m.m11 = c._2
      m.m12 = c._3
    inline def col2_=(c: (Double, Double, Double)): Unit =
      m.m20 = c._1
      m.m21 = c._2
      m.m22 = c._3

    // Row setters
    inline def row0_=(r: (Double, Double, Double)): Unit =
      m.m00 = r._1
      m.m10 = r._2
      m.m20 = r._3
    inline def row1_=(r: (Double, Double, Double)): Unit =
      m.m01 = r._1
      m.m11 = r._2
      m.m21 = r._3
    inline def row2_=(r: (Double, Double, Double)): Unit =
      m.m02 = r._1
      m.m12 = r._2
      m.m22 = r._3

// format: off
trait Mat3ImmutableOps[Mat]:

  def create(
      m00: Double, m01: Double, m02: Double,
      m10: Double, m11: Double, m12: Double,
      m20: Double, m21: Double, m22: Double,
  ): Mat

  def from[Mat3_](other: Mat3_)(using Mat3Base[Mat3_]): Mat =
    create(
      other.m00, other.m01, other.m02,
      other.m10, other.m11, other.m12,
      other.m20, other.m21, other.m22,
    )

  def fromRotationX(angle: Double): Mat =
    val c = angle.cos
    val s = angle.sin
    val ns = -s
    create(1.0, 0.0, 0.0, 0.0, c, s, 0.0, ns, c)

  def fromRotationY(angle: Double): Mat =
    val c = angle.cos
    val s = angle.sin
    val ns = -s
    create(c, 0.0, ns, 0.0, 1.0, 0.0, s, 0.0, c)

  def fromRotationZ(angle: Double): Mat =
    val c = angle.cos
    val s = angle.sin
    val ns = -s
    create(c, s, 0.0, ns, c, 0.0, 0.0, 0.0, 1.0)

  def identity: Mat =
    create(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)

  extension (m: Mat)(using Mat3Base[Mat])

    @scala.annotation.targetName("matMul")
    def *(other: Mat): Mat =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      val b00 = other.m00
      val b01 = other.m01
      val b02 = other.m02
      val b10 = other.m10
      val b11 = other.m11
      val b12 = other.m12
      val b20 = other.m20
      val b21 = other.m21
      val b22 = other.m22
      create(
        a00 * b00 + a10 * b01 + a20 * b02,
        a01 * b00 + a11 * b01 + a21 * b02,
        a02 * b00 + a12 * b01 + a22 * b02,
        a00 * b10 + a10 * b11 + a20 * b12,
        a01 * b10 + a11 * b11 + a21 * b12,
        a02 * b10 + a12 * b11 + a22 * b12,
        a00 * b20 + a10 * b21 + a20 * b22,
        a01 * b20 + a11 * b21 + a21 * b22,
        a02 * b20 + a12 * b21 + a22 * b22,
      )

    @scala.annotation.targetName("vecMul")
    def *[Vec](v: Vec)(using Vec3Base[Vec], Vec3ImmutableOps[Vec]): Vec =
      summon[Vec3ImmutableOps[Vec]].create(
        m.m00 * v.x + m.m10 * v.y + m.m20 * v.z,
        m.m01 * v.x + m.m11 * v.y + m.m21 * v.z,
        m.m02 * v.x + m.m12 * v.y + m.m22 * v.z,
      )

    def transpose: Mat = create(
      m.m00, m.m10, m.m20,
      m.m01, m.m11, m.m21,
      m.m02, m.m12, m.m22,
    )

    def inverse: Mat =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      val c00 = a11 * a22 - a12 * a21
      val c01 = -(a10 * a22 - a12 * a20)
      val c02 = a10 * a21 - a11 * a20
      val c10 = -(a01 * a22 - a02 * a21)
      val c11 = a00 * a22 - a02 * a20
      val c12 = -(a00 * a21 - a01 * a20)
      val c20 = a01 * a12 - a02 * a11
      val c21 = -(a00 * a12 - a02 * a10)
      val c22 = a00 * a11 - a01 * a10
      val det = a00 * c00 + a10 * c10 + a20 * c20
      val invDet = 1.0 / det
      create(
        c00 * invDet, c10 * invDet, c20 * invDet,
        c01 * invDet, c11 * invDet, c21 * invDet,
        c02 * invDet, c12 * invDet, c22 * invDet,
      )

    def rotateX(angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      create(
        m.m00, m.m01, m.m02,
        c * m.m10 + ns * m.m20, c * m.m11 + ns * m.m21, c * m.m12 + ns * m.m22,
        s * m.m10 + c * m.m20,  s * m.m11 + c * m.m21,  s * m.m12 + c * m.m22,
      )

    def rotateY(angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      create(
        c * m.m00 + s * m.m20,  c * m.m01 + s * m.m21,  c * m.m02 + s * m.m22,
        m.m10, m.m11, m.m12,
        ns * m.m00 + c * m.m20, ns * m.m01 + c * m.m21, ns * m.m02 + c * m.m22,
      )

    def rotateZ(angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      create(
        c * m.m00 + ns * m.m10, c * m.m01 + ns * m.m11, c * m.m02 + ns * m.m12,
        s * m.m00 + c * m.m10,  s * m.m01 + c * m.m11,  s * m.m02 + c * m.m12,
        m.m20, m.m21, m.m22,
      )

trait Mat3MutableOps[Mat]:

  extension (m: Mat)(using mb: Mat3Mutable[Mat])
    def set[Mat3_](other: Mat3_)(using Mat3Base[Mat3_]): Unit =
      m.m00 = other.m00
      m.m01 = other.m01
      m.m02 = other.m02
      m.m10 = other.m10
      m.m11 = other.m11
      m.m12 = other.m12
      m.m20 = other.m20
      m.m21 = other.m21
      m.m22 = other.m22
    def :=[Mat3_](other: Mat3_)(using Mat3Base[Mat3_]): Unit =
      m.set(other)

    def setIdentity(): Unit =
      m.m00 = 1.0
      m.m01 = 0.0
      m.m02 = 0.0
      m.m10 = 0.0
      m.m11 = 1.0
      m.m12 = 0.0
      m.m20 = 0.0
      m.m21 = 0.0
      m.m22 = 1.0

    def transposeTo(out: Mat): Mat =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      out.m00 = a00
      out.m01 = a10
      out.m02 = a20
      out.m10 = a01
      out.m11 = a11
      out.m12 = a21
      out.m20 = a02
      out.m21 = a12
      out.m22 = a22
      out
    inline def transposeSelf: Mat = m.transposeTo(m)

    def inverseTo(out: Mat): Mat =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      val c00 = a11 * a22 - a12 * a21
      val c01 = -(a10 * a22 - a12 * a20)
      val c02 = a10 * a21 - a11 * a20
      val c10 = -(a01 * a22 - a02 * a21)
      val c11 = a00 * a22 - a02 * a20
      val c12 = -(a00 * a21 - a01 * a20)
      val c20 = a01 * a12 - a02 * a11
      val c21 = -(a00 * a12 - a02 * a10)
      val c22 = a00 * a11 - a01 * a10
      val det = a00 * c00 + a10 * c10 + a20 * c20
      val invDet = 1.0 / det
      out.m00 = c00 * invDet
      out.m01 = c10 * invDet
      out.m02 = c20 * invDet
      out.m10 = c01 * invDet
      out.m11 = c11 * invDet
      out.m12 = c21 * invDet
      out.m20 = c02 * invDet
      out.m21 = c12 * invDet
      out.m22 = c22 * invDet
      out
    inline def inverseSelf: Mat = m.inverseTo(m)

    def rotateXTo(out: Mat, angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      val t10 = m.m10
      val t11 = m.m11
      val t12 = m.m12
      val t20 = m.m20
      val t21 = m.m21
      val t22 = m.m22
      out.m00 = m.m00
      out.m01 = m.m01
      out.m02 = m.m02
      out.m10 = c * t10 + ns * t20
      out.m11 = c * t11 + ns * t21
      out.m12 = c * t12 + ns * t22
      out.m20 = s * t10 + c * t20
      out.m21 = s * t11 + c * t21
      out.m22 = s * t12 + c * t22
      out
    inline def rotateXSelf(angle: Double): Mat =
      m.rotateXTo(m, angle)

    def rotateYTo(out: Mat, angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      val t00 = m.m00
      val t01 = m.m01
      val t02 = m.m02
      val t20 = m.m20
      val t21 = m.m21
      val t22 = m.m22
      out.m00 = c * t00 + s * t20
      out.m01 = c * t01 + s * t21
      out.m02 = c * t02 + s * t22
      out.m10 = m.m10
      out.m11 = m.m11
      out.m12 = m.m12
      out.m20 = ns * t00 + c * t20
      out.m21 = ns * t01 + c * t21
      out.m22 = ns * t02 + c * t22
      out
    inline def rotateYSelf(angle: Double): Mat =
      m.rotateYTo(m, angle)

    def rotateZTo(out: Mat, angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      val t00 = m.m00
      val t01 = m.m01
      val t02 = m.m02
      val t10 = m.m10
      val t11 = m.m11
      val t12 = m.m12
      out.m00 = c * t00 + ns * t10
      out.m01 = c * t01 + ns * t11
      out.m02 = c * t02 + ns * t12
      out.m10 = s * t00 + c * t10
      out.m11 = s * t01 + c * t11
      out.m12 = s * t02 + c * t12
      out.m20 = m.m20
      out.m21 = m.m21
      out.m22 = m.m22
      out
    inline def rotateZSelf(angle: Double): Mat =
      m.rotateZTo(m, angle)

// format: on
