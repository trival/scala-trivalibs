package trivalibs.graphics.math

import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// Generic variants — purely abstract shared contract between CPU and GPU.
// ---------------------------------------------------------------------------

// Column-major 4x4 matrix traits
// Storage order: m00, m01, m02, m03, m10, m11, ..., m33
// mColRow convention: m10 = column 1, row 0

trait Mat4BaseG[Num, Mat]:
  extension (m: Mat)
    def m00: Num
    def m01: Num
    def m02: Num
    def m03: Num
    def m10: Num
    def m11: Num
    def m12: Num
    def m13: Num
    def m20: Num
    def m21: Num
    def m22: Num
    def m23: Num
    def m30: Num
    def m31: Num
    def m32: Num
    def m33: Num
    def determinant: Num

// format: off
trait Mat4ImmutableOpsG[Num, Mat]:

  def create(
      m00: Num, m01: Num, m02: Num, m03: Num,
      m10: Num, m11: Num, m12: Num, m13: Num,
      m20: Num, m21: Num, m22: Num, m23: Num,
      m30: Num, m31: Num, m32: Num, m33: Num,
  ): Mat

  extension (m: Mat)(using Mat4BaseG[Num, Mat])
    @scala.annotation.targetName("matMulG")
    def *(other: Mat): Mat
    @scala.annotation.targetName("vecMulG")
    def *[Vec](v: Vec)(using Vec4BaseG[Num, Vec], Vec4ImmutableOpsG[Num, Vec]): Vec
// format: on

// ---------------------------------------------------------------------------
// CPU-specific variants — concrete Double implementations + CPU-only ops.
// ---------------------------------------------------------------------------

// format: off
trait Mat4Base[Mat] extends Mat4BaseG[Double, Mat]:
  extension (m: Mat)
    // Column getters
    inline def col0: (Double, Double, Double, Double) = (m.m00, m.m01, m.m02, m.m03)
    inline def col1: (Double, Double, Double, Double) = (m.m10, m.m11, m.m12, m.m13)
    inline def col2: (Double, Double, Double, Double) = (m.m20, m.m21, m.m22, m.m23)
    inline def col3: (Double, Double, Double, Double) = (m.m30, m.m31, m.m32, m.m33)

    // Row getters
    inline def row0: (Double, Double, Double, Double) = (m.m00, m.m10, m.m20, m.m30)
    inline def row1: (Double, Double, Double, Double) = (m.m01, m.m11, m.m21, m.m31)
    inline def row2: (Double, Double, Double, Double) = (m.m02, m.m12, m.m22, m.m32)
    inline def row3: (Double, Double, Double, Double) = (m.m03, m.m13, m.m23, m.m33)

    def determinant: Double =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a03 = m.m03
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a13 = m.m13
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      val a23 = m.m23
      val a30 = m.m30
      val a31 = m.m31
      val a32 = m.m32
      val a33 = m.m33
      a00 * (a11 * (a22 * a33 - a23 * a32) - a21 * (a12 * a33 - a13 * a32) + a31 * (a12 * a23 - a13 * a22)) -
        a10 * (a01 * (a22 * a33 - a23 * a32) - a21 * (a02 * a33 - a03 * a32) + a31 * (a02 * a23 - a03 * a22)) +
        a20 * (a01 * (a12 * a33 - a13 * a32) - a11 * (a02 * a33 - a03 * a32) + a31 * (a02 * a13 - a03 * a12)) -
        a30 * (a01 * (a12 * a23 - a13 * a22) - a11 * (a02 * a23 - a03 * a22) + a21 * (a02 * a13 - a03 * a12))

trait Mat4Mutable[Mat] extends Mat4Base[Mat]:
  extension (m: Mat)
    def m00_=(v: Double): Unit
    def m01_=(v: Double): Unit
    def m02_=(v: Double): Unit
    def m03_=(v: Double): Unit
    def m10_=(v: Double): Unit
    def m11_=(v: Double): Unit
    def m12_=(v: Double): Unit
    def m13_=(v: Double): Unit
    def m20_=(v: Double): Unit
    def m21_=(v: Double): Unit
    def m22_=(v: Double): Unit
    def m23_=(v: Double): Unit
    def m30_=(v: Double): Unit
    def m31_=(v: Double): Unit
    def m32_=(v: Double): Unit
    def m33_=(v: Double): Unit
  extension (m: Mat)
    // Column setters
    inline def col0_=(c: (Double, Double, Double, Double)): Unit =
      m.m00 = c._1
      m.m01 = c._2
      m.m02 = c._3
      m.m03 = c._4
    inline def col1_=(c: (Double, Double, Double, Double)): Unit =
      m.m10 = c._1
      m.m11 = c._2
      m.m12 = c._3
      m.m13 = c._4
    inline def col2_=(c: (Double, Double, Double, Double)): Unit =
      m.m20 = c._1
      m.m21 = c._2
      m.m22 = c._3
      m.m23 = c._4
    inline def col3_=(c: (Double, Double, Double, Double)): Unit =
      m.m30 = c._1
      m.m31 = c._2
      m.m32 = c._3
      m.m33 = c._4

    // Row setters
    inline def row0_=(r: (Double, Double, Double, Double)): Unit =
      m.m00 = r._1
      m.m10 = r._2
      m.m20 = r._3
      m.m30 = r._4
    inline def row1_=(r: (Double, Double, Double, Double)): Unit =
      m.m01 = r._1
      m.m11 = r._2
      m.m21 = r._3
      m.m31 = r._4
    inline def row2_=(r: (Double, Double, Double, Double)): Unit =
      m.m02 = r._1
      m.m12 = r._2
      m.m22 = r._3
      m.m32 = r._4
    inline def row3_=(r: (Double, Double, Double, Double)): Unit =
      m.m03 = r._1
      m.m13 = r._2
      m.m23 = r._3
      m.m33 = r._4

trait Mat4ImmutableOps[Mat]:

  def create(
      m00: Double, m01: Double, m02: Double, m03: Double,
      m10: Double, m11: Double, m12: Double, m13: Double,
      m20: Double, m21: Double, m22: Double, m23: Double,
      m30: Double, m31: Double, m32: Double, m33: Double,
  ): Mat

  def from[Mat4_](other: Mat4_)(using Mat4Base[Mat4_]): Mat =
    create(
      other.m00, other.m01, other.m02, other.m03,
      other.m10, other.m11, other.m12, other.m13,
      other.m20, other.m21, other.m22, other.m23,
      other.m30, other.m31, other.m32, other.m33,
    )

  def fromRotationX(angle: Double): Mat =
    val c = angle.cos
    val s = angle.sin
    val ns = -s
    create(1.0, 0.0, 0.0, 0.0, 0.0, c, s, 0.0, 0.0, ns, c, 0.0, 0.0, 0.0, 0.0, 1.0)

  def fromRotationY(angle: Double): Mat =
    val c = angle.cos
    val s = angle.sin
    val ns = -s
    create(c, 0.0, ns, 0.0, 0.0, 1.0, 0.0, 0.0, s, 0.0, c, 0.0, 0.0, 0.0, 0.0, 1.0)

  def fromRotationZ(angle: Double): Mat =
    val c = angle.cos
    val s = angle.sin
    val ns = -s
    create(c, s, 0.0, 0.0, ns, c, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)

  def identity: Mat =
    create(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)

  def fromTranslation(tx: Double, ty: Double, tz: Double): Mat =
    create(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, tx, ty, tz, 1.0)

  def fromScale(sx: Double, sy: Double, sz: Double): Mat =
    create(sx, 0.0, 0.0, 0.0, 0.0, sy, 0.0, 0.0, 0.0, 0.0, sz, 0.0, 0.0, 0.0, 0.0, 1.0)

  /** Perspective projection for WebGPU clip-space depth [0, 1], right-handed.
    * @param fovY vertical field of view in radians
    */
  def perspective(fovY: Double, aspect: Double, near: Double, far: Double): Mat =
    val f    = 1.0 / math.tan(fovY * 0.5)
    val rInv = 1.0 / (near - far)
    create(
      f / aspect, 0.0,        0.0,               0.0,
      0.0,        f,          0.0,               0.0,
      0.0,        0.0,        far * rInv,        -1.0,
      0.0,        0.0,        near * far * rInv, 0.0,
    )

  /** View matrix — right-handed lookAt. Eye position and target given as xyz components. */
  def lookAt(
      ex: Double, ey: Double, ez: Double,
      cx: Double, cy: Double, cz: Double,
      upX: Double, upY: Double, upZ: Double,
  ): Mat =
    // forward = normalize(center - eye)
    var fx = cx - ex
    var fy = cy - ey
    var fz = cz - ez
    val fl = math.sqrt(fx*fx + fy*fy + fz*fz)
    fx /= fl
    fy /= fl
    fz /= fl
    // right = normalize(forward × up)
    var rx = fy*upZ - fz*upY
    var ry = fz*upX - fx*upZ
    var rz = fx*upY - fy*upX
    val rl = math.sqrt(rx*rx + ry*ry + rz*rz)
    rx /= rl
    ry /= rl
    rz /= rl
    // recomputed up = right × forward
    val ux = ry*fz - rz*fy
    val uy = rz*fx - rx*fz
    val uz = rx*fy - ry*fx
    create(
      rx,                    ux,                    -fx,                  0.0,
      ry,                    uy,                    -fy,                  0.0,
      rz,                    uz,                    -fz,                  0.0,
      -(rx*ex+ry*ey+rz*ez),  -(ux*ex+uy*ey+uz*ez),  fx*ex+fy*ey+fz*ez,  1.0,
    )

  extension (m: Mat)(using Mat4Base[Mat])

    @scala.annotation.targetName("matMul")
    def *(other: Mat): Mat =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a03 = m.m03
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a13 = m.m13
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      val a23 = m.m23
      val a30 = m.m30
      val a31 = m.m31
      val a32 = m.m32
      val a33 = m.m33
      val b00 = other.m00
      val b01 = other.m01
      val b02 = other.m02
      val b03 = other.m03
      val b10 = other.m10
      val b11 = other.m11
      val b12 = other.m12
      val b13 = other.m13
      val b20 = other.m20
      val b21 = other.m21
      val b22 = other.m22
      val b23 = other.m23
      val b30 = other.m30
      val b31 = other.m31
      val b32 = other.m32
      val b33 = other.m33
      create(
        a00 * b00 + a10 * b01 + a20 * b02 + a30 * b03,
        a01 * b00 + a11 * b01 + a21 * b02 + a31 * b03,
        a02 * b00 + a12 * b01 + a22 * b02 + a32 * b03,
        a03 * b00 + a13 * b01 + a23 * b02 + a33 * b03,
        a00 * b10 + a10 * b11 + a20 * b12 + a30 * b13,
        a01 * b10 + a11 * b11 + a21 * b12 + a31 * b13,
        a02 * b10 + a12 * b11 + a22 * b12 + a32 * b13,
        a03 * b10 + a13 * b11 + a23 * b12 + a33 * b13,
        a00 * b20 + a10 * b21 + a20 * b22 + a30 * b23,
        a01 * b20 + a11 * b21 + a21 * b22 + a31 * b23,
        a02 * b20 + a12 * b21 + a22 * b22 + a32 * b23,
        a03 * b20 + a13 * b21 + a23 * b22 + a33 * b23,
        a00 * b30 + a10 * b31 + a20 * b32 + a30 * b33,
        a01 * b30 + a11 * b31 + a21 * b32 + a31 * b33,
        a02 * b30 + a12 * b31 + a22 * b32 + a32 * b33,
        a03 * b30 + a13 * b31 + a23 * b32 + a33 * b33,
      )

    @scala.annotation.targetName("vecMul")
    def *[Vec](v: Vec)(using Vec4Base[Vec], Vec4ImmutableOps[Vec]): Vec =
      val vx = v.x
      val vy = v.y
      val vz = v.z
      val vw = v.w
      summon[Vec4ImmutableOps[Vec]].create(
        m.m00 * vx + m.m10 * vy + m.m20 * vz + m.m30 * vw,
        m.m01 * vx + m.m11 * vy + m.m21 * vz + m.m31 * vw,
        m.m02 * vx + m.m12 * vy + m.m22 * vz + m.m32 * vw,
        m.m03 * vx + m.m13 * vy + m.m23 * vz + m.m33 * vw,
      )

    def transpose: Mat = create(
      m.m00, m.m10, m.m20, m.m30,
      m.m01, m.m11, m.m21, m.m31,
      m.m02, m.m12, m.m22, m.m32,
      m.m03, m.m13, m.m23, m.m33,
    )

    def inverse: Mat =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a03 = m.m03
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a13 = m.m13
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      val a23 = m.m23
      val a30 = m.m30
      val a31 = m.m31
      val a32 = m.m32
      val a33 = m.m33
      val b00 = a00 * a11 - a01 * a10
      val b01 = a00 * a12 - a02 * a10
      val b02 = a00 * a13 - a03 * a10
      val b03 = a01 * a12 - a02 * a11
      val b04 = a01 * a13 - a03 * a11
      val b05 = a02 * a13 - a03 * a12
      val b06 = a20 * a31 - a21 * a30
      val b07 = a20 * a32 - a22 * a30
      val b08 = a20 * a33 - a23 * a30
      val b09 = a21 * a32 - a22 * a31
      val b10 = a21 * a33 - a23 * a31
      val b11 = a22 * a33 - a23 * a32
      val det = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06
      val invDet = 1.0 / det
      create(
        ( a11 * b11 - a12 * b10 + a13 * b09) * invDet,
        (-a01 * b11 + a02 * b10 - a03 * b09) * invDet,
        ( a31 * b05 - a32 * b04 + a33 * b03) * invDet,
        (-a21 * b05 + a22 * b04 - a23 * b03) * invDet,
        (-a10 * b11 + a12 * b08 - a13 * b07) * invDet,
        ( a00 * b11 - a02 * b08 + a03 * b07) * invDet,
        (-a30 * b05 + a32 * b02 - a33 * b01) * invDet,
        ( a20 * b05 - a22 * b02 + a23 * b01) * invDet,
        ( a10 * b10 - a11 * b08 + a13 * b06) * invDet,
        (-a00 * b10 + a01 * b08 - a03 * b06) * invDet,
        ( a30 * b04 - a31 * b02 + a33 * b00) * invDet,
        (-a20 * b04 + a21 * b02 - a23 * b00) * invDet,
        (-a10 * b09 + a11 * b07 - a12 * b06) * invDet,
        ( a00 * b09 - a01 * b07 + a02 * b06) * invDet,
        (-a30 * b03 + a31 * b01 - a32 * b00) * invDet,
        ( a20 * b03 - a21 * b01 + a22 * b00) * invDet,
      )

    def rotateX(angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      create(
        m.m00, m.m01, m.m02, m.m03,
        c * m.m10 + ns * m.m20, c * m.m11 + ns * m.m21, c * m.m12 + ns * m.m22, c * m.m13 + ns * m.m23,
        s * m.m10 + c * m.m20,  s * m.m11 + c * m.m21,  s * m.m12 + c * m.m22,  s * m.m13 + c * m.m23,
        m.m30, m.m31, m.m32, m.m33,
      )

    def rotateY(angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      create(
        c * m.m00 + s * m.m20,  c * m.m01 + s * m.m21,  c * m.m02 + s * m.m22,  c * m.m03 + s * m.m23,
        m.m10, m.m11, m.m12, m.m13,
        ns * m.m00 + c * m.m20, ns * m.m01 + c * m.m21, ns * m.m02 + c * m.m22, ns * m.m03 + c * m.m23,
        m.m30, m.m31, m.m32, m.m33,
      )

    def rotateZ(angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      create(
        c * m.m00 + ns * m.m10, c * m.m01 + ns * m.m11, c * m.m02 + ns * m.m12, c * m.m03 + ns * m.m13,
        s * m.m00 + c * m.m10,  s * m.m01 + c * m.m11,  s * m.m02 + c * m.m12,  s * m.m03 + c * m.m13,
        m.m20, m.m21, m.m22, m.m23,
        m.m30, m.m31, m.m32, m.m33,
      )

trait Mat4MutableOps[Mat]:

  extension (m: Mat)(using mb: Mat4Mutable[Mat])
    def set[Mat4_](other: Mat4_)(using Mat4Base[Mat4_]): Unit =
      m.m00 = other.m00
      m.m01 = other.m01
      m.m02 = other.m02
      m.m03 = other.m03
      m.m10 = other.m10
      m.m11 = other.m11
      m.m12 = other.m12
      m.m13 = other.m13
      m.m20 = other.m20
      m.m21 = other.m21
      m.m22 = other.m22
      m.m23 = other.m23
      m.m30 = other.m30
      m.m31 = other.m31
      m.m32 = other.m32
      m.m33 = other.m33
    def :=[Mat4_](other: Mat4_)(using Mat4Base[Mat4_]): Unit =
      m.set(other)

    def setIdentity(): Unit =
      m.m00 = 1.0
      m.m01 = 0.0
      m.m02 = 0.0
      m.m03 = 0.0
      m.m10 = 0.0
      m.m11 = 1.0
      m.m12 = 0.0
      m.m13 = 0.0
      m.m20 = 0.0
      m.m21 = 0.0
      m.m22 = 1.0
      m.m23 = 0.0
      m.m30 = 0.0
      m.m31 = 0.0
      m.m32 = 0.0
      m.m33 = 1.0

    def transposeTo(out: Mat): Mat =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a03 = m.m03
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a13 = m.m13
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      val a23 = m.m23
      val a30 = m.m30
      val a31 = m.m31
      val a32 = m.m32
      val a33 = m.m33
      out.m00 = a00
      out.m01 = a10
      out.m02 = a20
      out.m03 = a30
      out.m10 = a01
      out.m11 = a11
      out.m12 = a21
      out.m13 = a31
      out.m20 = a02
      out.m21 = a12
      out.m22 = a22
      out.m23 = a32
      out.m30 = a03
      out.m31 = a13
      out.m32 = a23
      out.m33 = a33
      out
    inline def transposeSelf: Mat = m.transposeTo(m)

    def inverseTo(out: Mat): Mat =
      val a00 = m.m00
      val a01 = m.m01
      val a02 = m.m02
      val a03 = m.m03
      val a10 = m.m10
      val a11 = m.m11
      val a12 = m.m12
      val a13 = m.m13
      val a20 = m.m20
      val a21 = m.m21
      val a22 = m.m22
      val a23 = m.m23
      val a30 = m.m30
      val a31 = m.m31
      val a32 = m.m32
      val a33 = m.m33
      val b00 = a00 * a11 - a01 * a10
      val b01 = a00 * a12 - a02 * a10
      val b02 = a00 * a13 - a03 * a10
      val b03 = a01 * a12 - a02 * a11
      val b04 = a01 * a13 - a03 * a11
      val b05 = a02 * a13 - a03 * a12
      val b06 = a20 * a31 - a21 * a30
      val b07 = a20 * a32 - a22 * a30
      val b08 = a20 * a33 - a23 * a30
      val b09 = a21 * a32 - a22 * a31
      val b10 = a21 * a33 - a23 * a31
      val b11 = a22 * a33 - a23 * a32
      val det = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06
      val invDet = 1.0 / det
      out.m00 = ( a11 * b11 - a12 * b10 + a13 * b09) * invDet
      out.m01 = (-a01 * b11 + a02 * b10 - a03 * b09) * invDet
      out.m02 = ( a31 * b05 - a32 * b04 + a33 * b03) * invDet
      out.m03 = (-a21 * b05 + a22 * b04 - a23 * b03) * invDet
      out.m10 = (-a10 * b11 + a12 * b08 - a13 * b07) * invDet
      out.m11 = ( a00 * b11 - a02 * b08 + a03 * b07) * invDet
      out.m12 = (-a30 * b05 + a32 * b02 - a33 * b01) * invDet
      out.m13 = ( a20 * b05 - a22 * b02 + a23 * b01) * invDet
      out.m20 = ( a10 * b10 - a11 * b08 + a13 * b06) * invDet
      out.m21 = (-a00 * b10 + a01 * b08 - a03 * b06) * invDet
      out.m22 = ( a30 * b04 - a31 * b02 + a33 * b00) * invDet
      out.m23 = (-a20 * b04 + a21 * b02 - a23 * b00) * invDet
      out.m30 = (-a10 * b09 + a11 * b07 - a12 * b06) * invDet
      out.m31 = ( a00 * b09 - a01 * b07 + a02 * b06) * invDet
      out.m32 = (-a30 * b03 + a31 * b01 - a32 * b00) * invDet
      out.m33 = ( a20 * b03 - a21 * b01 + a22 * b00) * invDet
      out
    inline def inverseSelf: Mat = m.inverseTo(m)

    def rotateXTo(out: Mat, angle: Double): Mat =
      val c = angle.cos
      val s = angle.sin
      val ns = -s
      val t10 = m.m10
      val t11 = m.m11
      val t12 = m.m12
      val t13 = m.m13
      val t20 = m.m20
      val t21 = m.m21
      val t22 = m.m22
      val t23 = m.m23
      out.m00 = m.m00
      out.m01 = m.m01
      out.m02 = m.m02
      out.m03 = m.m03
      out.m10 = c * t10 + ns * t20
      out.m11 = c * t11 + ns * t21
      out.m12 = c * t12 + ns * t22
      out.m13 = c * t13 + ns * t23
      out.m20 = s * t10 + c * t20
      out.m21 = s * t11 + c * t21
      out.m22 = s * t12 + c * t22
      out.m23 = s * t13 + c * t23
      out.m30 = m.m30
      out.m31 = m.m31
      out.m32 = m.m32
      out.m33 = m.m33
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
      val t03 = m.m03
      val t20 = m.m20
      val t21 = m.m21
      val t22 = m.m22
      val t23 = m.m23
      out.m00 = c * t00 + s * t20
      out.m01 = c * t01 + s * t21
      out.m02 = c * t02 + s * t22
      out.m03 = c * t03 + s * t23
      out.m10 = m.m10
      out.m11 = m.m11
      out.m12 = m.m12
      out.m13 = m.m13
      out.m20 = ns * t00 + c * t20
      out.m21 = ns * t01 + c * t21
      out.m22 = ns * t02 + c * t22
      out.m23 = ns * t03 + c * t23
      out.m30 = m.m30
      out.m31 = m.m31
      out.m32 = m.m32
      out.m33 = m.m33
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
      val t03 = m.m03
      val t10 = m.m10
      val t11 = m.m11
      val t12 = m.m12
      val t13 = m.m13
      out.m00 = c * t00 + ns * t10
      out.m01 = c * t01 + ns * t11
      out.m02 = c * t02 + ns * t12
      out.m03 = c * t03 + ns * t13
      out.m10 = s * t00 + c * t10
      out.m11 = s * t01 + c * t11
      out.m12 = s * t02 + c * t12
      out.m13 = s * t03 + c * t13
      out.m20 = m.m20
      out.m21 = m.m21
      out.m22 = m.m22
      out.m23 = m.m23
      out.m30 = m.m30
      out.m31 = m.m31
      out.m32 = m.m32
      out.m33 = m.m33
      out
    inline def rotateZSelf(angle: Double): Mat =
      m.rotateZTo(m, angle)

// format: on
