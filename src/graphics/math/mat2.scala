package trivalibs.graphics.math

import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// Generic variants — used to define shared contract between CPU and GPU expression layer (shader DSL).
// ---------------------------------------------------------------------------

// Column-major 2x2 matrix traits
// Storage order: m00, m01, m10, m11
// mColRow convention: m10 = column 1, row 0

trait Mat2BaseG[Num, Mat]:
  extension (m: Mat)
    def m00: Num
    def m01: Num
    def m10: Num
    def m11: Num
    def determinant: Num

trait Mat2ImmutableOpsG[Num, Mat]:
  def create(m00: Num, m01: Num, m10: Num, m11: Num): Mat

  extension (m: Mat)(using Mat2BaseG[Num, Mat])
    @scala.annotation.targetName("matMulG")
    def *(other: Mat): Mat
    @scala.annotation.targetName("vecMulG")
    def *[Vec](
        v: Vec,
    )(using Vec2BaseG[Num, Vec], Vec2ImmutableOpsG[Num, Vec]): Vec

// ---------------------------------------------------------------------------
// CPU-specific variants — they implement functionality in terms of Double numbers,
// and provide additional CPU only operations (e.g. inverse, transpose).
// ---------------------------------------------------------------------------

trait Mat2Base[Mat] extends Mat2BaseG[Double, Mat]:
  extension (m: Mat)
    inline def col0: (Double, Double) = (m.m00, m.m01)
    inline def col1: (Double, Double) = (m.m10, m.m11)
    inline def row0: (Double, Double) = (m.m00, m.m10)
    inline def row1: (Double, Double) = (m.m01, m.m11)
    def determinant: Double = m.m00 * m.m11 - m.m10 * m.m01

trait Mat2Mutable[Mat] extends Mat2Base[Mat]:
  extension (m: Mat)
    def m00_=(v: Double): Unit
    def m01_=(v: Double): Unit
    def m10_=(v: Double): Unit
    def m11_=(v: Double): Unit

    // Column setters (derived from abstract mNN_= setters)
    inline def col0_=(c: (Double, Double)): Unit =
      m.m00 = c._1
      m.m01 = c._2
    inline def col1_=(c: (Double, Double)): Unit =
      m.m10 = c._1
      m.m11 = c._2

    // Row setters (derived from abstract mNN_= setters)
    inline def row0_=(r: (Double, Double)): Unit =
      m.m00 = r._1
      m.m10 = r._2
    inline def row1_=(r: (Double, Double)): Unit =
      m.m01 = r._1
      m.m11 = r._2

// format: off
trait Mat2ImmutableOps[Mat]:

  def create(m00: Double, m01: Double, m10: Double, m11: Double): Mat

  def from[Mat2_](other: Mat2_)(using Mat2Base[Mat2_]): Mat =
    create(other.m00, other.m01, other.m10, other.m11)

  def fromRotation(angle: Double): Mat =
    val c = angle.cos
    val s = angle.sin
    create(c, s, -s, c)

  def identity: Mat =
    create(1.0, 0.0, 0.0, 1.0)

  extension (m: Mat)(using Mat2Base[Mat])

    @scala.annotation.targetName("matMul")
    def *(other: Mat): Mat = create(
      m.m00 * other.m00 + m.m10 * other.m01,
      m.m01 * other.m00 + m.m11 * other.m01,
      m.m00 * other.m10 + m.m10 * other.m11,
      m.m01 * other.m10 + m.m11 * other.m11
    )

    @scala.annotation.targetName("vecMul")
    def *[Vec](v: Vec)(using Vec2Base[Vec], Vec2ImmutableOps[Vec]): Vec =
      summon[Vec2ImmutableOps[Vec]].create(
        m.m00 * v.x + m.m10 * v.y,
        m.m01 * v.x + m.m11 * v.y,
      )

    def transpose: Mat = create(
      m.m00, m.m10,
      m.m01, m.m11
    )

    def inverse: Mat =
      val det = m.m00 * m.m11 - m.m10 * m.m01
      val invDet = 1.0 / det
      create(
         m.m11 * invDet, -m.m01 * invDet,
        -m.m10 * invDet,  m.m00 * invDet
      )

    def rotate(angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      create(
        c * m.m00 + ns * m.m01, s * m.m00 + c * m.m01,
        c * m.m10 + ns * m.m11, s * m.m10 + c * m.m11
      )

trait Mat2MutableOps[Mat]:

  extension (m: Mat)(using mb: Mat2Mutable[Mat])
    def set[Mat2_](other: Mat2_)(using Mat2Base[Mat2_]): Unit =
      m.m00 = other.m00
      m.m01 = other.m01
      m.m10 = other.m10
      m.m11 = other.m11
    def :=[Mat2_](other: Mat2_)(using Mat2Base[Mat2_]): Unit =
      m.set(other)

    def setIdentity(): Unit =
      m.m00 = 1.0
      m.m01 = 0.0
      m.m10 = 0.0
      m.m11 = 1.0

    def transposeTo(out: Mat): Mat =
      val t00 = m.m00
      val t01 = m.m01
      val t10 = m.m10
      val t11 = m.m11
      out.m00 = t00
      out.m01 = t10
      out.m10 = t01
      out.m11 = t11
      out
    inline def transposeSelf: Mat = m.transposeTo(m)

    def inverseTo(out: Mat): Mat =
      val det = m.m00 * m.m11 - m.m10 * m.m01
      val invDet = 1.0 / det
      val t00 = m.m00
      val t01 = m.m01
      val t10 = m.m10
      val t11 = m.m11
      out.m00 =  t11 * invDet
      out.m01 = -t01 * invDet
      out.m10 = -t10 * invDet
      out.m11 =  t00 * invDet
      out
    inline def inverseSelf: Mat = m.inverseTo(m)

    def rotateTo(out: Mat, angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      val t00 = m.m00
      val t01 = m.m01
      val t10 = m.m10
      val t11 = m.m11
      out.m00 = c * t00 + ns * t01
      out.m01 = s * t00 + c * t01
      out.m10 = c * t10 + ns * t11
      out.m11 = s * t10 + c * t11
      out
    inline def rotateSelf(angle: Double): Mat =
      m.rotateTo(m, angle)

// format: on
