package trivalibs.graphics.scene

import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.utils.js.*

// ---------------------------------------------------------------------------
// Transform — TRS (Translation, Rotation, Scale) stored separately.
// Matrix is computed on demand via toMatrix(). Mutable, zero-allocation.
// ---------------------------------------------------------------------------

class Transform(
    var translation: Vec3 = Vec3.zero,
    var rotation: Quat = Quat.identity,
    var scale: Vec3 = Vec3(1.0, 1.0, 1.0),
)

object Transform:
  def identity = Transform()

  def fromTranslation(t: Vec3) = Transform(translation = t)
  def fromTranslation(x: Double, y: Double, z: Double) =
    Transform(translation = Vec3(x, y, z))

  def fromRotation(r: Quat) = Transform(rotation = r)
  def fromRotation(angle: Double, axis: Vec3) =
    Transform(rotation = Quat.fromAxisAngle(axis, angle))
  def fromRotationX(angle: Double) =
    Transform(rotation = Quat.fromRotationX(angle))
  def fromRotationY(angle: Double) =
    Transform(rotation = Quat.fromRotationY(angle))
  def fromRotationZ(angle: Double) =
    Transform(rotation = Quat.fromRotationZ(angle))

  def fromScale(s: Vec3) = Transform(scale = s)
  def fromScale(s: Double) = Transform(scale = Vec3(s, s, s))

  /** Decompose a 3D affine matrix (no shear) into TRS. */
  def fromMatrix(m: Mat4) =
    val col0 = Vec3(m.m00, m.m01, m.m02)
    val col1 = Vec3(m.m10, m.m11, m.m12)
    val col2 = Vec3(m.m20, m.m21, m.m22)
    val sx = col0.length
    val sy = col1.length
    val sz = col2.length
    // Rotation matrix columns with scale removed
    val bz = col2 / sz // local Z (back)
    val by = col1 / sy // local Y (up)
    Transform(
      translation = Vec3(m.m30, m.m31, m.m32),
      rotation = Quat.fromLookRotation(bz, by),
      scale = Vec3(sx, sy, sz),
    )

extension (t: Transform)

  // ---- Matrix output ----

  def matrix: Mat4 =
    Mat4.fromTranslationRotationScale(t.translation, t.rotation, t.scale)

  // ---- Local axes (derived from rotation) ----

  def localX: Vec3 = t.rotation * Vec3.X
  def localY: Vec3 = t.rotation * Vec3.Y
  def localZ: Vec3 = t.rotation * Vec3.Z

  def right: Vec3 = t.localX
  def left: Vec3 = -t.localX
  def up: Vec3 = t.localY
  def down: Vec3 = -t.localY
  def forward: Vec3 = -t.localZ // right-handed: forward is -Z
  def back: Vec3 = t.localZ

  // ---- Rotation (parent-space) ----
  // rotate: q is applied in parent/world space (pre-multiply: result = q * current).
  // Use when the rotation axis is defined in the parent's coordinate frame.
  // Example: rotating an object around the world Y axis.

  def rotate(q: Quat) =
    Transform(t.translation, q * t.rotation, t.scale)

  def rotateSelf(q: Quat): Unit =
    t.rotation = q * t.rotation

  def rotateAxis(axis: Vec3, angle: Double) =
    t.rotate(Quat.fromAxisAngle(axis, angle))

  def rotateAxisSelf(axis: Vec3, angle: Double): Unit =
    t.rotateSelf(Quat.fromAxisAngle(axis, angle))

  def rotateX(angle: Double) = t.rotate(Quat.fromRotationX(angle))
  def rotateY(angle: Double) = t.rotate(Quat.fromRotationY(angle))
  def rotateZ(angle: Double) = t.rotate(Quat.fromRotationZ(angle))

  def rotateXSelf(angle: Double): Unit = t.rotateSelf(Quat.fromRotationX(angle))
  def rotateYSelf(angle: Double): Unit = t.rotateSelf(Quat.fromRotationY(angle))
  def rotateZSelf(angle: Double): Unit = t.rotateSelf(Quat.fromRotationZ(angle))

  // ---- Rotation (local-space) ----
  // rotateLocal: q is applied in the object's own local space (post-multiply: result = current * q).
  // Use when the rotation axis is defined relative to the object itself.
  // Example: an airplane pitching up around its own local X axis.

  def rotateLocal(q: Quat) =
    Transform(t.translation, t.rotation * q, t.scale)

  def rotateLocalSelf(q: Quat): Unit =
    t.rotation = t.rotation * q

  def rotateLocalAxis(axis: Vec3, angle: Double) =
    t.rotateLocal(Quat.fromAxisAngle(axis, angle))

  def rotateLocalAxisSelf(axis: Vec3, angle: Double): Unit =
    t.rotateLocalSelf(Quat.fromAxisAngle(axis, angle))

  def rotateLocalX(angle: Double) = t.rotateLocal(Quat.fromRotationX(angle))
  def rotateLocalY(angle: Double) = t.rotateLocal(Quat.fromRotationY(angle))
  def rotateLocalZ(angle: Double) = t.rotateLocal(Quat.fromRotationZ(angle))

  def rotateLocalXSelf(angle: Double): Unit =
    t.rotateLocalSelf(Quat.fromRotationX(angle))
  def rotateLocalYSelf(angle: Double): Unit =
    t.rotateLocalSelf(Quat.fromRotationY(angle))
  def rotateLocalZSelf(angle: Double): Unit =
    t.rotateLocalSelf(Quat.fromRotationZ(angle))

  // ---- Look-at orientation ----

  /** Orient so that forward (-Z) points at target. Handles degenerate cases:
    * zero-length direction falls back to -Z, zero-length or parallel up falls
    * back to Y (or an orthogonal vector).
    */
  def lookAt(target: Vec3, upHint: Vec3 = Vec3.Y) =
    Transform(
      t.translation,
      lookRotation(t.translation - target, upHint),
      t.scale,
    )

  def lookAtSelf(target: Vec3, upHint: Vec3 = Vec3.Y): Unit =
    t.rotation = lookRotation(t.translation - target, upHint)

  /** Orient so that forward (-Z) points in the given direction. Same
    * degenerate-case handling as lookAt.
    */
  def lookTo(direction: Vec3, upHint: Vec3 = Vec3.Y) =
    Transform(t.translation, lookRotation(-direction, upHint), t.scale)

  def lookToSelf(direction: Vec3, upHint: Vec3 = Vec3.Y): Unit =
    t.rotation = lookRotation(-direction, upHint)

  // ---- Composition ----

  /** Combine: parent * child — applies child first, then parent. */
  @scala.annotation.targetName("transformCompose")
  def *(other: Transform) =
    Transform(
      translation = t.transformPoint(other.translation),
      rotation = t.rotation * other.rotation,
      scale = t.scale * other.scale,
    )

  /** Apply this transform to a point: scale, rotate, translate. */
  @scala.annotation.targetName("transformMulVec3")
  def *(v: Vec3): Vec3 = t.transformPoint(v)

  /** Apply this transform to a point: scale, rotate, translate. */
  def transformPoint(v: Vec3): Vec3 =
    t.rotation * (t.scale * v) + t.translation

  // ---- Translation ----

  def translate(delta: Vec3) =
    Transform(t.translation + delta, t.rotation, t.scale)

  def translateSelf(delta: Vec3): Unit = t.translation += delta

  // ---- Orbit ----

  /** Move position around a pivot point without changing orientation. */
  def translateAround(pivot: Vec3, q: Quat) =
    Transform(pivot + q * (t.translation - pivot), t.rotation, t.scale)

  def translateAroundSelf(pivot: Vec3, q: Quat): Unit =
    t.translation = pivot + q * (t.translation - pivot)

  /** Move position and rotate orientation around a pivot point. */
  def rotateAround(pivot: Vec3, q: Quat) =
    val newTrans = pivot + q * (t.translation - pivot)
    Transform(newTrans, q * t.rotation, t.scale)

  def rotateAroundSelf(pivot: Vec3, q: Quat): Unit =
    t.translation = pivot + q * (t.translation - pivot)
    t.rotation = q * t.rotation

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

/** Build a look rotation quaternion from a back vector (+Z direction) and up
  * hint. Handles degenerate inputs: zero-length back falls back to +Z,
  * zero-length or parallel up falls back to Y (or finds an orthogonal vector).
  */
private def lookRotation(back: Vec3, upHint: Vec3): Quat =
  val backLen = back.length
  val bz = if backLen > 1e-10 then back / backLen else Vec3.Z

  val upLen = upHint.length
  val up = if upLen > 1e-10 then upHint / upLen else Vec3.Y

  var rx = up.cross(bz)
  val rxLen = rx.length
  if rxLen > 1e-10 then rx = rx / rxLen
  else
    // up is parallel to back — pick any orthogonal vector
    rx =
      if Math.abs(bz.x) < 0.9 then Vec3.X.cross(bz).normalize
      else Vec3.Y.cross(bz).normalize

  val ry = bz.cross(rx)
  Quat.fromLookRotation(bz, ry)
