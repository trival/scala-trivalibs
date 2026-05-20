package trivalibs.graphics.scene

import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// PerspectiveCamera — FPS-style orientation with yaw + pitch angles.
// Projection matrix is cached and only recomputed when params change.
// ---------------------------------------------------------------------------

class PerspectiveCamera private (
    var fov: Double,
    var aspect: Double,
    var near: Double,
    var far: Double,
    var rotH: Double,
    var rotV: Double,
    var pos: Vec3,
    private var proj: Mat4,
):
  import PerspectiveCamera.{normalizeH, clampV}

  def apply(
      fov: Double = this.fov,
      aspect: Double = this.aspect,
      near: Double = this.near,
      far: Double = this.far,
      rotH: Double = this.rotH,
      rotV: Double = this.rotV,
      pos: Vec3 = this.pos,
  ): Unit =
    val needsProj = fov != this.fov || aspect != this.aspect ||
      near != this.near || far != this.far
    this.fov = fov
    this.aspect = aspect
    this.near = near
    this.far = far
    if rotH != this.rotH then this.rotH = normalizeH(rotH)
    if rotV != this.rotV then this.rotV = clampV(rotV)
    this.pos = pos
    if needsProj then proj = Mat4.perspective(fov, aspect, near, far)

  // ---- FPS-style movement ----

  def move(
      forward: Double = 0.0,
      left: Double = 0.0,
      up: Double = 0.0,
      deltaH: Double = 0.0,
      deltaV: Double = 0.0,
  ): Unit =
    if deltaH != 0.0 then rotH = normalizeH(rotH + deltaH)
    if deltaV != 0.0 then rotV = clampV(rotV + deltaV)
    if up != 0.0 then pos = Vec3(pos.x, pos.y + up, pos.z)
    if forward != 0.0 then
      pos = pos +
        Vec3(-rotH.sin, 0.0, -rotH.cos) * forward
    if left != 0.0 then
      pos = pos +
        Vec3(-rotH.cos, 0.0, rotH.sin) * left

  // ---- Transform ----

  def transform = Transform(
    pos,
    Quat.fromRotationY(rotH) * Quat.fromRotationX(rotV),
  )

  // ---- Matrix accessors ----

  def projectionMat: Mat4 = proj
  def viewMat: Mat4 = transform.matrix.inverse
  def viewProjMat: Mat4 = projectionMat * viewMat

  // ---- Ground reflection (for water / mirror effects) ----

  def reflectedGroundCam(): PerspectiveCamera =
    val c = PerspectiveCamera(fov, aspect, near, far)
    c.rotH = rotH
    c.rotV = clampV(-rotV)
    c.pos = Vec3(pos.x, -pos.y, pos.z)
    c.proj = proj
    c

object PerspectiveCamera:
  private def normalizeH(a: Double): Double =
    val r = a % (2 * math.Pi)
    if r < 0 then r + 2 * math.Pi else r

  private def clampV(a: Double): Double =
    a.clamp(-math.Pi / 2.0, math.Pi / 2.0)

  def apply(
      fov: Double = math.Pi / 4.0,
      aspect: Double = 1.0,
      near: Double = 0.1,
      far: Double = 1000.0,
      rotH: Double = 0.0,
      rotV: Double = 0.0,
      pos: Vec3 = Vec3.zero,
  ): PerspectiveCamera =
    val proj = Mat4.perspective(fov, aspect, near, far)
    new PerspectiveCamera(
      fov,
      aspect,
      near,
      far,
      normalizeH(rotH),
      clampV(rotV),
      pos,
      proj,
    )
