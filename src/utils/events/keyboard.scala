package trivalibs.utils.events

import org.scalajs.dom

import scala.scalajs.js

// ---------------------------------------------------------------------------
// Raw keyboard relay — a thin wrapper over DOM keydown/keyup that reports each
// physical key as a `Key` constant. OS auto-repeat keydowns are deduped (a held
// key reports one `onDown`), since smooth held-key motion is better driven by a
// render loop's frame delta than by the uneven repeat rate. Tab keeps its
// default browser behaviour so focus traversal still works. Returns a teardown
// function that removes the listeners.
// ---------------------------------------------------------------------------

def keyboardRelay(
    el: dom.EventTarget,
    onDown: Key => Unit,
    onUp: Key => Unit,
    keepDefault: Boolean = false,
): () => Unit =
  val down: js.Function1[dom.KeyboardEvent, Unit] = e =>
    val isTab = e.code == "Tab"
    if !keepDefault && !isTab then e.preventDefault()
    if !e.repeat then onDown(e.code.asInstanceOf[Key])

  val up: js.Function1[dom.KeyboardEvent, Unit] = e =>
    onUp(e.code.asInstanceOf[Key])

  el.addEventListener[dom.KeyboardEvent]("keydown", down)
  el.addEventListener[dom.KeyboardEvent]("keyup", up)

  () =>
    el.removeEventListener[dom.KeyboardEvent]("keydown", down)
    el.removeEventListener[dom.KeyboardEvent]("keyup", up)
