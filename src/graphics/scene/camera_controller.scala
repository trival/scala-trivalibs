package trivalibs.graphics.scene

import trivalibs.utils.events.CanvasInput
import trivalibs.utils.events.Key
import trivalibs.utils.events.PointerButton

// ---------------------------------------------------------------------------
// BasicFirstPersonCameraController — maps polled CanvasInput to FPS camera
// movement. WASD walks (forward/strafe), Space/Shift rise/sink, drag looks
// around. For touch-only devices: holding the primary pointer walks forward,
// and walking backward is triggered by a secondary mouse button OR a genuine
// two-finger touch. Drag-look stays bound to the primary pointer and survives a
// pointer hand-off without a jump. It only translates input into parameters and
// delegates the transform update to `PerspectiveCamera.move`.
//
//   speed       — metres per second for translation
//   sensitivity — radians of look per pixel of drag
// ---------------------------------------------------------------------------

/** Maps a polled [[trivalibs.utils.events.CanvasInput]] to first-person
  * movement of a bound [[PerspectiveCamera]].
  *
  * @param cam
  *   the camera to move.
  * @param in
  *   the input bundle (state + drag/hold gestures) to read each frame.
  * @param sensitivity
  *   radians of look per ~1000 px of drag.
  * @param speed
  *   metres per second of translation.
  */
class BasicFirstPersonCameraController(
    cam: PerspectiveCamera,
    in: CanvasInput,
    val sensitivity: Double = 1.0,
    val speed: Double = 3.0,
):
  /** Advance the bound camera by one frame, given the frame's `tpf` (ms). The
    * caller must have ticked the input with `in.update(tpf)` this frame (the
    * controller reads `in.drag.delta` / `in.hold.holding`, it does not advance
    * them).
    */
  def update(tpf: Double): Unit =
    val dist = speed * tpf / 1000.0

    // Touch: walking backward is a secondary mouse button or a two-finger touch;
    // holding the (single) primary pointer walks forward. Two-finger backward
    // wins over the hold — otherwise a held two-finger press cancels itself out
    // (and lurches backward during the hold's init window), so the hold-forward
    // is suppressed while two or more pointers are down.
    val backward =
      in.isKeyDown(Key.KeyS) || in.isKeyDown(Key.ArrowDown) ||
        in.isDown(PointerButton.Secondary) || in.pointersDown >= 2
    var forward = 0.0
    if in.isKeyDown(Key.KeyW) || in.isKeyDown(Key.ArrowUp) ||
      (in.hold.holding && in.pointersDown < 2)
    then forward += dist
    if backward then forward -= dist

    var left = 0.0
    if in.isKeyDown(Key.KeyA) || in.isKeyDown(Key.ArrowLeft) then left += dist
    if in.isKeyDown(Key.KeyD) || in.isKeyDown(Key.ArrowRight) then left -= dist

    var up = 0.0
    if in.isKeyDown(Key.Space) then up += dist
    if in.isKeyDown(Key.ShiftLeft) || in.isKeyDown(Key.ShiftRight) then
      up -= dist

    val drag = in.drag.delta
    // 1000px of drag ≈ one sensitivity-scaled radian; drag right/down looks
    // right/down (negative yaw / negative pitch in the camera's convention).
    val deltaH = -drag.dx * sensitivity / 1000.0
    val deltaV = -drag.dy * sensitivity / 1000.0

    cam.move(forward, left, up, deltaH, deltaV)
