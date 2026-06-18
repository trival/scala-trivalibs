package trivalibs.utils.events

import org.scalajs.dom
import scala.scalajs.js
import trivalibs.utils.js.*

// ---------------------------------------------------------------------------
// interactiveCanvas — InputState tailored for the default use case: a canvas
// that drives a rendered scene. It makes the canvas keyboard-focusable
// (`tabindex`), clears the native focus outline, focuses on pointer-down, and
// optionally focuses immediately. Keyboard is scoped to the canvas (not window)
// so it never steals keys from surrounding UI / form fields.
//
// `hasFocus` on the returned InputState lets a complex UI render its own focus
// affordance in place of the cleared outline.
// ---------------------------------------------------------------------------

def interactiveCanvas(
    canvas: dom.HTMLCanvasElement,
    initialFocus: Boolean = true,
    holdDelay: Double = 400.0,
    holdRadius: Double = 5.0,
    suppressContextMenu: Boolean = true,
    onActivity: Maybe[js.Function0[Unit]] = Maybe.Not,
): InputState =
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
    holdDelay = holdDelay,
    holdRadius = holdRadius,
    suppressContextMenu = suppressContextMenu,
    onActivity = onActivity,
    focusOnPointerDown = true,
  )

  if initialFocus then canvas.focus()
  input
