package trivalibs.graphics.math.cpu

import trivalibs.graphics.math.*
import trivalibs.bufferdata.StructRef
import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// Quaternion storage — (x, y, z, w), imaginary first, real last
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// QuatImmutableOps — allocating quaternion operations
// ---------------------------------------------------------------------------

trait QuatImmutableOps[Q]:

  def create(x: Double, y: Double, z: Double, w: Double): Q

  extension (q: Q)(using Vec4Base[Q])

    /** Hamilton product → new Q. */
    def quatMul(p: Q): Q =
      create(
        q.w * p.x + q.x * p.w + q.y * p.z - q.z * p.y,
        q.w * p.y - q.x * p.z + q.y * p.w + q.z * p.x,
        q.w * p.z + q.x * p.y - q.y * p.x + q.z * p.w,
        q.w * p.w - q.x * p.x - q.y * p.y - q.z * p.z,
      )

    /** Rotate a Vec3 by this unit quaternion (Rodrigues formula). */
    def quatRotate[V](v: V)(using Vec3Base[V], Vec3ImmutableOps[V]): V =
      val vops = summon[Vec3ImmutableOps[V]]
      val tx = (q.y * v.z - q.z * v.y) + (q.y * v.z - q.z * v.y)
      val ty = (q.z * v.x - q.x * v.z) + (q.z * v.x - q.x * v.z)
      val tz = (q.x * v.y - q.y * v.x) + (q.x * v.y - q.y * v.x)
      vops.create(
        v.x + q.w * tx + (q.y * tz - q.z * ty),
        v.y + q.w * ty + (q.z * tx - q.x * tz),
        v.z + q.w * tz + (q.x * ty - q.y * tx),
      )

    def quatConjugate: Q = create(-q.x, -q.y, -q.z, q.w)

    def quatInverse: Q =
      val n2 = q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w
      create(-q.x / n2, -q.y / n2, -q.z / n2, q.w / n2)

    def slerp(p: Q, t: Double): Q =
      var cosH = q.x * p.x + q.y * p.y + q.z * p.z + q.w * p.w
      var px = p.x
      var py = p.y
      var pz = p.z
      var pw = p.w
      if cosH < 0.0 then
        cosH = -cosH
        px = -px
        py = -py
        pz = -pz
        pw = -pw
      val qx = q.x
      val qy = q.y
      val qz = q.z
      val qw = q.w
      val (s0, s1) =
        if 1.0 - cosH > 1e-10 then
          val a = cosH.acos
          val sa = a.sin
          (((1.0 - t) * a).sin / sa, (t * a).sin / sa)
        else (1.0 - t, t)
      create(
        s0 * qx + s1 * px,
        s0 * qy + s1 * py,
        s0 * qz + s1 * pz,
        s0 * qw + s1 * pw,
      )

    def toMat4: Mat4 =
      val x = q.x
      val y = q.y
      val z = q.z
      val w = q.w
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
      // format: off
      Mat4.create(
        1-(yy+zz), xy+wz,     xz-wy,     0.0,
        xy-wz,     1-(xx+zz), yz+wx,     0.0,
        xz+wy,     yz-wx,     1-(xx+yy), 0.0,
        0.0,       0.0,       0.0,       1.0,
      )
      // format: on

// ---------------------------------------------------------------------------
// QuatMutableOps — in-place operations
// ---------------------------------------------------------------------------

trait QuatMutableOps[Q]:

  extension (q: Q)(using Vec4Mutable[Q])

    /** Pre-multiply: self = p * self (apply p on top of current rotation). */
    def quatMulSelf(p: Q): Unit =
      val nx = p.w * q.x + p.x * q.w + p.y * q.z - p.z * q.y
      val ny = p.w * q.y - p.x * q.z + p.y * q.w + p.z * q.x
      val nz = p.w * q.z + p.x * q.y - p.y * q.x + p.z * q.w
      val nw = p.w * q.w - p.x * q.x - p.y * q.y - p.z * q.z
      q.x = nx
      q.y = ny
      q.z = nz
      q.w = nw

    def conjugateSelf: Unit =
      q.x = -q.x
      q.y = -q.y
      q.z = -q.z

    def inverseSelf: Unit =
      val n2 = q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w
      q.x = -q.x / n2
      q.y = -q.y / n2
      q.z = -q.z / n2
      q.w = q.w / n2

    def normalizeSelf: Unit =
      val len = (q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w).sqrt
      q.x = q.x / len
      q.y = q.y / len
      q.z = q.z / len
      q.w = q.w / len

    def setFromRotationX(angle: Double): Unit =
      val a = angle * 0.5
      q.x = a.sin
      q.y = 0.0
      q.z = 0.0
      q.w = a.cos

    def setFromRotationY(angle: Double): Unit =
      val a = angle * 0.5
      q.x = 0.0
      q.y = a.sin
      q.z = 0.0
      q.w = a.cos

    def setFromRotationZ(angle: Double): Unit =
      val a = angle * 0.5
      q.x = 0.0
      q.y = 0.0
      q.z = a.sin
      q.w = a.cos

    def setFromAxisAngle[V](axis: V, angle: Double)(using Vec3Base[V]): Unit =
      val a = angle * 0.5
      val s = a.sin
      q.x = axis.x * s
      q.y = axis.y * s
      q.z = axis.z * s
      q.w = a.cos

    def setFromLookRotation[V](fwd: V, up: V)(using Vec3Base[V]): Unit =
      val r = Quat.fromLookRotationDoubles(
        fwd.x,
        fwd.y,
        fwd.z,
        up.x,
        up.y,
        up.z,
      )
      q.x = r.x
      q.y = r.y
      q.z = r.z
      q.w = r.w

// ---------------------------------------------------------------------------
// Concrete Quat — mutable class with both op sets + operator aliases
// ---------------------------------------------------------------------------

/** Mutable quaternion `(x, y, z, w)` (imaginary first, real last) for 3D
  * rotations. Companion: `Quat.identity`, `.fromAxisAngle`, `.fromRotationX/Y/Z`,
  * `.fromLookRotation`, `.fromMat4`. Ops: `*` (compose), `.rotate(vec3)`,
  * `.conjugate`, `.inverse`, `.slerp`, `.normalize`, `.toMat4`. Note: WGSL has no
  * quaternion type — upload a rotation `Mat3`/`Mat4` to shaders. Siblings
  * `QuatTuple`/`QuatBuffer`. */
class Quat(
    var x: Double = 0.0,
    var y: Double = 0.0,
    var z: Double = 0.0,
    var w: Double = 1.0,
)

object Quat:
  def identity: Quat = Quat(0.0, 0.0, 0.0, 1.0)

  def fromRotationX(angle: Double): Quat =
    val h = angle * 0.5
    Quat(h.sin, 0.0, 0.0, h.cos)

  def fromRotationY(angle: Double): Quat =
    val h = angle * 0.5
    Quat(0.0, h.sin, 0.0, h.cos)

  def fromRotationZ(angle: Double): Quat =
    val h = angle * 0.5
    Quat(0.0, 0.0, h.sin, h.cos)

  def fromAxisAngle(axis: Vec3, angle: Double): Quat =
    val h = angle * 0.5
    val s = h.sin
    Quat(axis.x * s, axis.y * s, axis.z * s, h.cos)

  /** `fwd` = backward direction (+Z_local). Use `(eye - target).normalize` for
    * cameras.
    */
  def fromLookRotation(fwd: Vec3, up: Vec3 = Vec3.Y): Quat =
    fromLookRotationDoubles(fwd.x, fwd.y, fwd.z, up.x, up.y, up.z)

  private[cpu] def fromLookRotationDoubles(
      fx: Double,
      fy: Double,
      fz: Double,
      ux: Double,
      uy: Double,
      uz: Double,
  ): Quat =
    // bz = normalize(fwd), bx = normalize(cross(up, bz)), by = cross(bz, bx)
    val fLen = (fx * fx + fy * fy + fz * fz).sqrt
    val bzx = fx / fLen
    val bzy = fy / fLen
    val bzz = fz / fLen
    var bxx = uy * bzz - uz * bzy
    var bxy = uz * bzx - ux * bzz
    var bxz = ux * bzy - uy * bzx
    val bxLen = (bxx * bxx + bxy * bxy + bxz * bxz).sqrt
    bxx /= bxLen; bxy /= bxLen; bxz /= bxLen
    val byx = bzy * bxz - bzz * bxy
    val byy = bzz * bxx - bzx * bxz
    val byz = bzx * bxy - bzy * bxx
    // Rotation matrix → quaternion (Shoemake). R[row][col] diagonal = (bxx, byy, bzz)
    val trace = bxx + byy + bzz
    if trace > 0.0 then
      val s = (trace + 1.0).sqrt * 2.0
      Quat((byz - bzy) / s, (bzx - bxz) / s, (bxy - byx) / s, s / 4.0)
    else if bxx > byy && bxx > bzz then
      val s = (1.0 + bxx - byy - bzz).sqrt * 2.0
      Quat(s / 4.0, (byx + bxy) / s, (bzx + bxz) / s, (byz - bzy) / s)
    else if byy > bzz then
      val s = (1.0 + byy - bxx - bzz).sqrt * 2.0
      Quat((byx + bxy) / s, s / 4.0, (bzy + byz) / s, (bzx - bxz) / s)
    else
      val s = (1.0 + bzz - bxx - byy).sqrt * 2.0
      Quat((bzx + bxz) / s, (bzy + byz) / s, s / 4.0, (bxy - byx) / s)

  // Type class instances — Vec4Mutable for field access, no Vec4ImmutableOps
  given Vec4Mutable[Quat]:
    extension (q: Quat)
      inline def x = q.x
      inline def y = q.y
      inline def z = q.z
      inline def w = q.w
      inline def x_=(v: Double) = q.x = v
      inline def y_=(v: Double) = q.y = v
      inline def z_=(v: Double) = q.z = v
      inline def w_=(v: Double) = q.w = v

  given QuatImmutableOps[Quat]:
    inline def create(x: Double, y: Double, z: Double, w: Double) =
      Quat(x, y, z, w)

  given QuatMutableOps[Quat] = new QuatMutableOps[Quat] {}

  // Operator aliases — pure delegation, no reimplementation
  extension (q: Quat)
    @scala.annotation.targetName("quatMulOp")
    def *(p: Quat): Quat = q.quatMul(p)

    @scala.annotation.targetName("quatRotateVec3")
    def *(v: Vec3): Vec3 = q.quatRotate(v)

    /** Operator alias for quatMulSelf — pre-multiply semantics. */
    def *=(p: Quat): Unit = q.quatMulSelf(p)

    def unary_- : Quat = q.quatConjugate
    def conjugate: Quat = q.quatConjugate
    def inverse: Quat = q.quatInverse

    def normalize: Quat =
      val len = (q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w).sqrt
      Quat(q.x / len, q.y / len, q.z / len, q.w / len)

// ---------------------------------------------------------------------------
// QuatImmutableOps for tuple types, QuatMutableOps for buffer types
// ---------------------------------------------------------------------------

given QuatImmutableOps[Vec4Tuple]:
  inline def create(x: Double, y: Double, z: Double, w: Double) = (x, y, z, w)

given QuatMutableOps[StructRef[Vec4Buffer]] =
  new QuatMutableOps[StructRef[Vec4Buffer]] {}
