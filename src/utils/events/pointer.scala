package trivalibs.utils.events

import org.scalajs.dom

import scala.scalajs.js

// ---------------------------------------------------------------------------
// Raw pointer relay — Layer 1: a stateless wrapper that sanitizes DOM pointer
// events into typed callbacks and returns a teardown function. It holds no
// state and interprets no gestures (drag/hold/etc. live in the gesture layer,
// built on top of `InputState`). Each callback carries the pointer's `id`
// (`PointerEvent.pointerId`) so consumers can track several pointers at once.
//
// The press starts on `el` (the canvas), but move/up/cancel listen on
// `moveTarget` (the window by default) so a drag keeps tracking even when the
// pointer leaves the canvas. `pointerleave` is intentionally not used.
// ---------------------------------------------------------------------------

/** Attach pointer listeners that report raw, per-pointer down/move/up/cancel
  * events as typed callbacks.
  *
  * @param el
  *   target the press (`pointerdown`) and context-menu listeners attach to —
  *   typically the canvas.
  * @param moveTarget
  *   target the move/up/cancel listeners attach to (the window by default) so a
  *   drag keeps tracking when the pointer leaves `el`.
  * @param onDown
  *   `(button, id, x, y, isPrimary)` on `pointerdown`.
  * @param onMove
  *   `(id, x, y)` on `pointermove`.
  * @param onUp
  *   `(button, id, x, y)` on `pointerup`.
  * @param onCancel
  *   `(id)` on `pointercancel` (e.g. the OS took over the gesture).
  * @param suppressContextMenu
  *   when true, `contextmenu` on `el` is prevented so a secondary-button press
  *   does not pop the native menu.
  * @return
  *   a teardown function that removes every listener.
  */
def pointerRelay(
    el: dom.EventTarget,
    moveTarget: dom.EventTarget = dom.window,
    onDown: (PointerButton, Double, Double, Double, Boolean) => Unit =
      (_, _, _, _, _) => (),
    onMove: (Double, Double, Double) => Unit = (_, _, _) => (),
    onUp: (PointerButton, Double, Double, Double) => Unit = (_, _, _, _) => (),
    onCancel: Double => Unit = _ => (),
    suppressContextMenu: Boolean = true,
): () => Unit =
  val downFn: js.Function1[dom.PointerEvent, Unit] = e =>
    onDown(
      e.button.asInstanceOf[PointerButton],
      e.pointerId,
      e.clientX,
      e.clientY,
      e.isPrimary,
    )

  val moveFn: js.Function1[dom.PointerEvent, Unit] = e =>
    onMove(e.pointerId, e.clientX, e.clientY)

  val upFn: js.Function1[dom.PointerEvent, Unit] = e =>
    onUp(
      e.button.asInstanceOf[PointerButton],
      e.pointerId,
      e.clientX,
      e.clientY,
    )

  val cancelFn: js.Function1[dom.PointerEvent, Unit] = e =>
    onCancel(e.pointerId)

  val ctxFn: js.Function1[dom.MouseEvent, Unit] = e => e.preventDefault()

  el.addEventListener[dom.PointerEvent]("pointerdown", downFn)
  moveTarget.addEventListener[dom.PointerEvent]("pointermove", moveFn)
  moveTarget.addEventListener[dom.PointerEvent]("pointerup", upFn)
  moveTarget.addEventListener[dom.PointerEvent]("pointercancel", cancelFn)
  if suppressContextMenu then
    el.addEventListener[dom.MouseEvent]("contextmenu", ctxFn)

  () =>
    el.removeEventListener[dom.PointerEvent]("pointerdown", downFn)
    moveTarget.removeEventListener[dom.PointerEvent]("pointermove", moveFn)
    moveTarget.removeEventListener[dom.PointerEvent]("pointerup", upFn)
    moveTarget.removeEventListener[dom.PointerEvent]("pointercancel", cancelFn)
    if suppressContextMenu then
      el.removeEventListener[dom.MouseEvent]("contextmenu", ctxFn)
