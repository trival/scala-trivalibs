package trivalibs.utils.events

import org.scalajs.dom
import trivalibs.utils.js.*

import scala.scalajs.js

// ---------------------------------------------------------------------------
// InputState — the pollable abstraction a render loop queries each frame. It
// builds on the raw keyboard/pointer relays and keeps the current snapshot:
// which keys are down (and since when), which pointer buttons are down, whether
// the primary pointer is dragging or holding, and the drag delta accumulated
// since the last poll.
//
// Keyboard listens on `keyTarget` (window by default, so keys are caught
// regardless of focus); pointer listens on `el` (typically the canvas).
//
// `onActivity` (if provided) fires on any state-changing event (key/button down
// or up, drag, hold) — not on bare pointer moves. Together with `isIdle` it lets
// a scene skip a fixed render loop and instead start one on the first activity
// and stop it once idle, so static scenes cost nothing.
// ---------------------------------------------------------------------------

class InputState(
    el: dom.EventTarget,
    keyTarget: dom.EventTarget = dom.window,
    holdDelay: Double = 400.0,
    holdRadius: Double = 5.0,
    suppressContextMenu: Boolean = true,
    onActivity: Maybe[js.Function0[Unit]] = Maybe.Not,
):
  private inline def fireActivity(): Unit =
    if !js.isUndefined(onActivity.asInstanceOf[js.Any]) then onActivity.safe()

  // code -> timestamp (ms) when the key was pressed
  val keysDown: Dict[Double] = Dict()
  // pointer-button value (as string) -> true while down
  private val buttonsDown: Dict[Boolean] = Dict()

  var pointerX: Double = 0.0
  var pointerY: Double = 0.0
  private var dragDx: Double = 0.0
  private var dragDy: Double = 0.0
  private var _dragging: Boolean = false
  private var _holding: Boolean = false

  private val disposeKeyboard = keyboardRelay(
    keyTarget,
    onDown = k =>
      if !keysDown.has(k.code) then
        keysDown.set(k.code, js.Date.now())
        fireActivity(),
    onUp = k =>
      js.special.delete(keysDown, k.code)
      fireActivity(),
  )

  private val disposePointer = pointerRelay(
    el,
    onDown = (b, x, y) =>
      buttonsDown.set(b.key, true)
      pointerX = x
      pointerY = y
      fireActivity()
    ,
    onUp = (b, x, y) =>
      js.special.delete(buttonsDown, b.key)
      pointerX = x
      pointerY = y
      fireActivity()
    ,
    onMove = (x, y, _, _) =>
      pointerX = x
      pointerY = y
    ,
    onDragStart = () =>
      _dragging = true
      fireActivity()
    ,
    onDrag = (dx, dy) =>
      dragDx += dx
      dragDy += dy
      fireActivity()
    ,
    onDragEnd = () =>
      _dragging = false
      _holding = false
      fireActivity()
    ,
    onHold = (_, _) =>
      _holding = true
      fireActivity()
    ,
    holdDelay = holdDelay,
    holdRadius = holdRadius,
    suppressContextMenu = suppressContextMenu,
  )

  // ---- queries ----

  def isKeyDown(key: Key): Boolean = keysDown.has(key.code)

  /** Milliseconds the key has been held, or 0 if it is not currently down. */
  def keyHeldMs(key: Key, now: Double = js.Date.now()): Double =
    if keysDown.has(key.code) then now - keysDown.at(key.code) else 0.0

  def isDown(button: PointerButton): Boolean = buttonsDown.has(button.key)

  def dragging: Boolean = _dragging
  def holding: Boolean = _holding

  /** True when nothing is driving the scene: no keys or pointer buttons down
    * and no active drag/hold. A render loop can stop itself once this holds
    * (after any easing has settled) and restart from the `onActivity` callback.
    */
  def isIdle: Boolean =
    !_dragging && !_holding &&
      js.Object.keys(keysDown.asInstanceOf[js.Object]).length == 0 &&
      js.Object.keys(buttonsDown.asInstanceOf[js.Object]).length == 0

  /** Returns the drag delta accumulated since the previous call and resets the
    * accumulator, so per-frame polling is independent of event frequency.
    */
  def consumeDragDelta(): (dx: Double, dy: Double) =
    val r = (dx = dragDx, dy = dragDy)
    dragDx = 0.0
    dragDy = 0.0
    r

  def dispose(): Unit =
    disposeKeyboard()
    disposePointer()
