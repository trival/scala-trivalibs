package trivalibs.graphics.scene

import trivalibs.utils.events.InputState
import trivalibs.utils.events.Key
import trivalibs.utils.events.PointerButton

// ---------------------------------------------------------------------------
// BasicFirstPersonCameraController — maps polled InputState to FPS camera
// movement. WASD walks (forward/strafe), Space/Shift rise/sink, primary-pointer
// drag looks around. For touch-only devices: holding the primary pointer walks
// forward and a secondary-button press walks backward. It only translates input
// into parameters and delegates the transform update to `PerspectiveCamera.move`.
//
//   speed       — metres per second for translation
//   sensitivity — radians of look per pixel of drag
// ---------------------------------------------------------------------------

class BasicFirstPersonCameraController(
    val sensitivity: Double = 1.0,
    val speed: Double = 3.0,
):
  def updateCamera(
      cam: PerspectiveCamera,
      input: InputState,
      tpf: Double,
  ): Unit =
    val dist = speed * tpf / 1000.0

    // Touch: holding the primary pointer walks forward, secondary press back.
    var forward = 0.0
    if input.isKeyDown(Key.KeyW) || input.isKeyDown(Key.ArrowUp) ||
      input.holding
    then forward += dist
    if input.isKeyDown(Key.KeyS) || input.isKeyDown(Key.ArrowDown) ||
      input.isDown(PointerButton.Secondary)
    then forward -= dist

    var left = 0.0
    if input.isKeyDown(Key.KeyA) || input.isKeyDown(Key.ArrowLeft) then
      left += dist
    if input.isKeyDown(Key.KeyD) || input.isKeyDown(Key.ArrowRight) then
      left -= dist

    var up = 0.0
    if input.isKeyDown(Key.Space) then up += dist
    if input.isKeyDown(Key.ShiftLeft) || input.isKeyDown(Key.ShiftRight) then
      up -= dist

    val drag = input.consumeDragDelta()
    // 1000px of drag ≈ one sensitivity-scaled radian; drag right/down looks
    // right/down (negative yaw / negative pitch in the camera's convention).
    val deltaH = -drag.dx * sensitivity / 1000.0
    val deltaV = -drag.dy * sensitivity / 1000.0

    cam.move(forward, left, up, deltaH, deltaV)
