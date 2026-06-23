package trivalibs.utils.events

import munit.FunSuite
import trivalibs.utils.js.*

// DOM-free gesture tests: gestures read pointers via the package-private
// `() => Arr[Pointer]` constructor, so we drive them with a hand-built active
// list instead of constructing a DOM-bound InputState. Each frame is simulated
// by an `update()` call followed by reading the pure `delta` / `holding` member.

class GesturesTest extends FunSuite:

  // Helper: a fresh down pointer.
  private def mkPointer(
      id: Double,
      x: Double,
      y: Double,
      button: PointerButton = PointerButton.Primary,
      downSince: Double = 0.0,
  ): Pointer =
    val p = Pointer()
    p.id = id
    p.button = button
    p.downSince = downSince
    p.downX = x
    p.downY = y
    p.x = x
    p.y = y
    p

  // ---- DragGesture ----

  test("drag: no driver -> not dragging, zero delta"):
    val ps: Arr[Pointer] = Arr()
    val drag = DragGesture(() => ps)
    assert(!drag.dragging)
    drag.update()
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)
    assertEqualsDouble(drag.delta.dy, 0.0, 1e-9)

  test("drag: first frame reseeds (no jump), then reports movement"):
    val p = mkPointer(1, 100, 100)
    val ps: Arr[Pointer] = Arr(p)
    val drag = DragGesture(() => ps)
    assert(drag.dragging)
    // first frame after a fresh press contributes no delta
    drag.update()
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)
    assertEqualsDouble(drag.delta.dy, 0.0, 1e-9)
    p.x = 110
    p.y = 95
    drag.update()
    assertEqualsDouble(drag.delta.dx, 10.0, 1e-9)
    assertEqualsDouble(drag.delta.dy, -5.0, 1e-9)
    // delta is an idempotent read within the frame
    assertEqualsDouble(drag.delta.dx, 10.0, 1e-9)

  test("drag: hand-off promotes second pointer without a jump"):
    val p1 = mkPointer(1, 0, 0)
    val p2 = mkPointer(2, 500, 500)
    val ps: Arr[Pointer] = Arr(p1, p2)
    val drag = DragGesture(() => ps)
    drag.update() // reseed on p1
    p1.x = 20
    drag.update()
    assertEqualsDouble(drag.delta.dx, 20.0, 1e-9)
    // p1 lifts; p2 (far away) is promoted to front
    p1.id = null
    ps.splice(0, 1)
    // first frame after hand-off contributes no delta despite the 500px gap
    drag.update()
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)
    assertEqualsDouble(drag.delta.dy, 0.0, 1e-9)
    // subsequent movement of the new driver is reported normally
    p2.x = 530
    drag.update()
    assertEqualsDouble(drag.delta.dx, 30.0, 1e-9)

  test("drag: secondary-button pointer is not a driver"):
    val p = mkPointer(1, 0, 0, button = PointerButton.Secondary)
    val ps: Arr[Pointer] = Arr(p)
    val drag = DragGesture(() => ps)
    assert(!drag.dragging)
    drag.update()
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)

  // ---- HoldGesture ----

  // Simulate one frame: tick by `tpf` ms, then read the pure `holding` member.
  private def step(hold: HoldGesture, tpf: Double): Boolean =
    hold.update(tpf)
    hold.holding

  test("hold: activates after accumulated delay when within radius"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val hold = HoldGesture(() => ps, holdDelay = 400, holdRadius = 5)
    assert(!step(hold, 100)) // 100ms held, within radius
    assert(!step(hold, 200)) // 300ms < 400ms
    assert(step(hold, 200)) // 500ms >= 400ms -> activated

  test("hold: straying during the init window disqualifies the press"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val hold = HoldGesture(() => ps, holdDelay = 400, holdRadius = 5)
    p.x = 10 // stray (distance 10 > 5) during the init window
    assert(!step(hold, 100)) // observed within init -> disqualified
    // returning within the radius does not re-arm the hold for this press
    p.x = 0
    assert(!step(hold, 400)) // 500ms accumulated but strayed -> no hold

  test("hold: stray after activation keeps holding"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val hold = HoldGesture(() => ps, holdDelay = 400, holdRadius = 5)
    assert(!step(hold, 100)) // init, still
    assert(step(hold, 400)) // 500ms -> activated
    p.x = 100 // large stray AFTER activation
    p.y = 200
    assert(step(hold, 100)) // still holding (movement allowed)

  test("hold: no driver / hand-off restarts the timer and latch"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val hold = HoldGesture(() => ps, holdDelay = 400, holdRadius = 5)
    p.x = 100 // stray within init
    assert(!step(hold, 100))
    assert(!step(hold, 400)) // strayed during init -> no hold
    // pointer lifts
    ps.splice(0, 1)
    assert(!step(hold, 100))
    // a fresh press within radius can hold again (state was reset)
    val p2 = mkPointer(2, 0, 0)
    ps.push(p2)
    assert(!step(hold, 100)) // init window for p2 (timer restarted)
    assert(step(hold, 400)) // p2 activates
