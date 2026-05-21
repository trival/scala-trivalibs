package trivalibs.utils.events

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.timers.{SetTimeoutHandle, setTimeout, clearTimeout}
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// PointerTracker — the drag/hold state machine, kept free of DOM/timer types
// so it can be unit-tested directly. The relay drives it with raw coordinates.
//
//   dragging : true between a primary down and its up, once the pointer moves
//   holding  : true once the press has lasted `holdDelay` without the pointer
//              straying further than `holdRadius` from its origin
// ---------------------------------------------------------------------------

class PointerTracker(
    val holdRadius: Double = 5.0,
):
  var dragging = false
  var holding = false
  private var pressed = false
  private var oX = 0.0
  private var oY = 0.0
  private var _maxDist = 0.0

  def maxDist: Double = _maxDist

  def down(x: Double, y: Double): Unit =
    pressed = true
    dragging = true
    holding = false
    oX = x
    oY = y
    _maxDist = 0.0

  def move(x: Double, y: Double): Unit =
    if pressed then
      val ddx = x - oX
      val ddy = y - oY
      val d = (ddx * ddx + ddy * ddy).sqrt
      if d > _maxDist then _maxDist = d

  def up(): Unit =
    pressed = false
    dragging = false
    holding = false

  /** Called when the hold delay elapses. Activates `holding` only if the press
    * is still active and never strayed beyond `holdRadius`. Returns whether
    * holding just became active.
    */
  def checkHold(): Boolean =
    if pressed && !holding && _maxDist <= holdRadius then
      holding = true
      true
    else false

// ---------------------------------------------------------------------------
// Raw pointer relay — wraps DOM pointer events and reports primary/secondary
// down/up, per-move deltas, and the abstracted drag/hold gestures via optional
// callbacks. Returns a teardown function that removes the listeners.
// ---------------------------------------------------------------------------

def pointerRelay(
    el: dom.EventTarget,
    moveTarget: dom.EventTarget = dom.window,
    onDown: (PointerButton, Double, Double) => Unit = (_, _, _) => (),
    onUp: (PointerButton, Double, Double) => Unit = (_, _, _) => (),
    onMove: (Double, Double, Double, Double) => Unit = (_, _, _, _) => (),
    onDragStart: () => Unit = () => (),
    onDrag: (Double, Double) => Unit = (_, _) => (),
    onDragEnd: () => Unit = () => (),
    onHold: (Double, Double) => Unit = (_, _) => (),
    holdDelay: Double = 400.0,
    holdRadius: Double = 5.0,
    suppressContextMenu: Boolean = true,
): () => Unit =
  val tracker = PointerTracker(holdRadius)
  var holdTimer: Opt[SetTimeoutHandle] = null
  var lastX = 0.0
  var lastY = 0.0
  // Drag/hold tracks only the primary button (not just the primary pointer —
  // a secondary-button press is also `isPrimary`). Set while that press is held.
  var primaryActive = false

  def clearHold(): Unit =
    if holdTimer.notNull then
      clearTimeout(holdTimer.get)
      holdTimer = null

  def endPrimary(): Unit =
    val wasDragging = tracker.dragging
    tracker.up()
    clearHold()
    primaryActive = false
    if wasDragging then onDragEnd()

  val downFn: js.Function1[dom.PointerEvent, Unit] = e =>
    val btn = e.button.asInstanceOf[PointerButton]
    onDown(btn, e.clientX, e.clientY)
    if e.isPrimary && btn == PointerButton.Primary then
      lastX = e.clientX
      lastY = e.clientY
      primaryActive = true
      tracker.down(e.clientX, e.clientY)
      onDragStart()
      clearHold()
      holdTimer = setTimeout(holdDelay):
        if tracker.checkHold() then onHold(lastX, lastY)

  val moveFn: js.Function1[dom.PointerEvent, Unit] = e =>
    val dx = e.clientX - lastX
    val dy = e.clientY - lastY
    lastX = e.clientX
    lastY = e.clientY
    onMove(e.clientX, e.clientY, dx, dy)
    if primaryActive then
      tracker.move(e.clientX, e.clientY)
      if tracker.dragging then onDrag(dx, dy)

  val upFn: js.Function1[dom.PointerEvent, Unit] = e =>
    val btn = e.button.asInstanceOf[PointerButton]
    onUp(btn, e.clientX, e.clientY)
    if primaryActive && btn == PointerButton.Primary then endPrimary()

  val cancelFn: js.Function1[dom.PointerEvent, Unit] = e =>
    if primaryActive then endPrimary()

  val ctxFn: js.Function1[dom.MouseEvent, Unit] = e => e.preventDefault()

  // The press starts on `el` (the canvas), but move/up/cancel listen on
  // `moveTarget` (the window by default) so a drag keeps tracking even when the
  // pointer leaves the canvas. `pointerleave` is intentionally not used.
  el.addEventListener[dom.PointerEvent]("pointerdown", downFn)
  moveTarget.addEventListener[dom.PointerEvent]("pointermove", moveFn)
  moveTarget.addEventListener[dom.PointerEvent]("pointerup", upFn)
  moveTarget.addEventListener[dom.PointerEvent]("pointercancel", cancelFn)
  if suppressContextMenu then
    el.addEventListener[dom.MouseEvent]("contextmenu", ctxFn)

  () =>
    clearHold()
    el.removeEventListener[dom.PointerEvent]("pointerdown", downFn)
    moveTarget.removeEventListener[dom.PointerEvent]("pointermove", moveFn)
    moveTarget.removeEventListener[dom.PointerEvent]("pointerup", upFn)
    moveTarget.removeEventListener[dom.PointerEvent]("pointercancel", cancelFn)
    if suppressContextMenu then
      el.removeEventListener[dom.MouseEvent]("contextmenu", ctxFn)
