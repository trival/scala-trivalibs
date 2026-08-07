package trivalibs.graphics.scene

import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// PerspectiveCamera — FPS-style orientation with yaw + pitch angles.
// Projection matrix is cached and only recomputed when params change.
//
// `fov` is the vertical field of view at the *square* aspect ratio (1:1) — it
// is not simply "the" vertical FOV. As the viewport widens past square,
// vertical FOV stays at `fov` and horizontal grows to fill the width (the
// classic vertical-fit camera). As it narrows past square into portrait
// instead, horizontal FOV stays at `fov` and vertical grows to fill the
// height. This keeps a composition tuned on a wide desktop viewport framed
// the same way on a narrow mobile one, rather than losing its sides to a
// shrinking horizontal FOV. See `effectiveFovY` for the actual angle fed to
// the projection matrix on any given frame.
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

  /** The vertical FOV actually fed to the projection this frame — `fov`
    * unchanged at `aspect >= 1`, or solved so the *horizontal* FOV stays at
    * `fov` instead when `aspect < 1` (portrait). See the class comment. Useful
    * to compensate screen-space sizes (e.g. a blur radius as a percentage of
    * canvas height) that were tuned assuming a fixed vertical FOV, so they keep
    * the same object-space size across aspect ratios.
    */
  def effectiveFovY: Double = PerspectiveCamera.effectiveFovY(fov, aspect)

  def set(
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
    if needsProj then proj = Mat4.perspective(effectiveFovY, aspect, near, far)

  inline def apply(
      fov: Double = this.fov,
      aspect: Double = this.aspect,
      near: Double = this.near,
      far: Double = this.far,
      rotH: Double = this.rotH,
      rotV: Double = this.rotV,
      pos: Vec3 = this.pos,
  ): Unit =
    set(fov, aspect, near, far, rotH, rotV, pos)

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

  /** See the class comment: `fov` unchanged at `aspect >= 1`; below it, solved
    * so `tan(fov/2) == tan(fovY/2) * aspect` — the horizontal half-angle this
    * would produce at `fov` unchanged — holds with `fovY` in place of `fov`,
    * i.e. horizontal FOV stays at `fov` and vertical grows. Continuous at
    * `aspect == 1`, where both branches agree.
    */
  private def effectiveFovY(fov: Double, aspect: Double): Double =
    2.0 * ((fov * 0.5).tan / aspect.min(1.0)).atan

  def apply(
      fov: Double = math.Pi / 4.0,
      aspect: Double = 1.0,
      near: Double = 0.1,
      far: Double = 1000.0,
      rotH: Double = 0.0,
      rotV: Double = 0.0,
      pos: Vec3 = Vec3.zero,
  ): PerspectiveCamera =
    val proj = Mat4.perspective(effectiveFovY(fov, aspect), aspect, near, far)
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
