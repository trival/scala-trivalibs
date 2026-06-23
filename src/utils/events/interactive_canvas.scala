package trivalibs.utils.events

import org.scalajs.dom

import scala.scalajs.js
import trivalibs.utils.js.*

// ---------------------------------------------------------------------------
// interactiveCanvas — the opinionated preset for the default use case: a canvas
// that drives a rendered scene. It makes the canvas keyboard-focusable
// (`tabindex`), clears the native focus outline, focuses on pointer-down, and
// optionally focuses immediately. Keyboard is scoped to the canvas (not window)
// so it never steals keys from surrounding UI / form fields. It wires the
// default gesture preset (drag + hold) and returns them bundled with the
// `InputState` as a `CanvasInput`.
// ---------------------------------------------------------------------------

/** The bundle returned by [[interactiveCanvas]] / `Painter.input`: an
  * [[InputState]] plus the default gesture preset. The full `InputState` query
  * surface is re-exported, so `ci.isKeyDown(...)`, `ci.pointersDown`, etc. work
  * directly; reach the gestures via `ci.drag` / `ci.hold`.
  *
  * @param input
  *   the underlying pollable input snapshot.
  * @param drag
  *   default drag gesture over `input`.
  * @param hold
  *   default hold gesture over `input`.
  */
class CanvasInput(
    val input: InputState,
    val drag: DragGesture,
    val hold: HoldGesture,
):
  export input.{dispose => _, *}

  /** Advance the default gestures by one frame, given the frame's `tpf` (ms
    * since last frame). Call once per render frame before reading `drag.delta`
    * / `hold.holding`.
    */
  def update(tpf: Double): Unit =
    drag.update()
    hold.update(tpf)

  /** Remove all DOM listeners (the gestures hold no resources of their own). */
  def dispose(): Unit = input.dispose()

/** Set up an [[InputState]] for `canvas` with interactive-canvas defaults plus
  * the default drag/hold gesture preset.
  *
  * @param canvas
  *   the canvas to wire input on.
  * @param initialFocus
  *   focus the canvas immediately so keys work without a first click.
  * @param holdDelay
  *   ms a press must last to count as a hold.
  * @param holdRadius
  *   px a press may stray and still count as a hold.
  * @param suppressContextMenu
  *   prevent the native context menu on secondary-button press.
  * @param onActivity
  *   optional callback fired on any state-changing input event.
  * @return
  *   a [[CanvasInput]] bundling the state and the default gestures.
  */
def interactiveCanvas(
    canvas: dom.HTMLCanvasElement,
    initialFocus: Boolean = true,
    holdDelay: Double = 400.0,
    holdRadius: Double = 5.0,
    suppressContextMenu: Boolean = true,
    onActivity: Maybe[js.Function0[Unit]] = Maybe.Not,
): CanvasInput =
  canvas.setAttribute("tabindex", "0")
  canvas.style.setProperty("outline", "none")

  // Suppress native touch gestures so only our own pointer handling fires.
  // touch-action disables browser scroll/zoom/pan on the canvas; user-select
  // stops the long-press selection overlay; touch-callout suppresses the iOS
  // callout / magnifier; tap-highlight removes the grey tap flash.
  canvas.style.setProperty("touch-action", "none")
  canvas.style.setProperty("user-select", "none")
  canvas.style.setProperty("-webkit-user-select", "none")
  canvas.style.setProperty("-webkit-touch-callout", "none")
  canvas.style.setProperty("-webkit-tap-highlight-color", "transparent")

  val input = InputState(
    el = canvas,
    keyTarget = canvas,
    suppressContextMenu = suppressContextMenu,
    onActivity = onActivity,
    focusOnPointerDown = true,
  )

  if initialFocus then canvas.focus()

  CanvasInput(
    input,
    DragGesture(input),
    HoldGesture(input, holdDelay, holdRadius),
  )
