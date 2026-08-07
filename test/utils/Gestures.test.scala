package trivalibs.utils.events

import munit.FunSuite
import trivalibs.utils.js.*

// DOM-free gesture tests: gestures read pointers via the package-private
// `() => Arr[Pointer]` constructor, so we drive them with a hand-built active
// list instead of constructing a DOM-bound InputState. Each frame is simulated
// by an `update(tpf)` call followed by reading the pure `delta` / `holding`
// member.

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

  // Plain drag (no glide) unless a half-life is given.
  private def mkDrag(
      ps: Arr[Pointer],
      glideMinSpeed: Double = 60.0,
      glideHalfLife: Double = 0.0,
  ): DragGesture = DragGesture(() => ps, glideMinSpeed, glideHalfLife)

  private val Frame = 16.0 // ms per simulated frame

  test("drag: no driver -> not dragging, zero delta"):
    val ps: Arr[Pointer] = Arr()
    val drag = mkDrag(ps)
    assert(!drag.dragging)
    drag.update(Frame)
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)
    assertEqualsDouble(drag.delta.dy, 0.0, 1e-9)

  test("drag: first frame reseeds (no jump), then reports movement"):
    val p = mkPointer(1, 100, 100)
    val ps: Arr[Pointer] = Arr(p)
    val drag = mkDrag(ps)
    assert(drag.dragging)
    // first frame after a fresh press contributes no delta
    drag.update(Frame)
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)
    assertEqualsDouble(drag.delta.dy, 0.0, 1e-9)
    p.x = 110
    p.y = 95
    drag.update(Frame)
    assertEqualsDouble(drag.delta.dx, 10.0, 1e-9)
    assertEqualsDouble(drag.delta.dy, -5.0, 1e-9)
    // delta is an idempotent read within the frame
    assertEqualsDouble(drag.delta.dx, 10.0, 1e-9)

  test("drag: hand-off promotes second pointer without a jump"):
    val p1 = mkPointer(1, 0, 0)
    val p2 = mkPointer(2, 500, 500)
    val ps: Arr[Pointer] = Arr(p1, p2)
    val drag = mkDrag(ps)
    drag.update(Frame) // reseed on p1
    p1.x = 20
    drag.update(Frame)
    assertEqualsDouble(drag.delta.dx, 20.0, 1e-9)
    // p1 lifts; p2 (far away) is promoted to front
    p1.id = null
    ps.splice(0, 1)
    // first frame after hand-off contributes no delta despite the 500px gap
    drag.update(Frame)
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)
    assertEqualsDouble(drag.delta.dy, 0.0, 1e-9)
    // subsequent movement of the new driver is reported normally
    p2.x = 530
    drag.update(Frame)
    assertEqualsDouble(drag.delta.dx, 30.0, 1e-9)

  test("drag: secondary-button pointer is not a driver"):
    val p = mkPointer(1, 0, 0, button = PointerButton.Secondary)
    val ps: Arr[Pointer] = Arr(p)
    val drag = mkDrag(ps)
    assert(!drag.dragging)
    drag.update(Frame)
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)

  test("drag: without a half-life the release stops dead"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val drag = mkDrag(ps)
    drag.update(Frame) // reseed
    var i = 0
    while i < 10 do
      p.x += 16.0 // 1 px/ms
      drag.update(Frame)
      i += 1
    assertEqualsDouble(drag.delta.dx, 16.0, 1e-9)
    ps.splice(0, 1) // release
    drag.update(Frame)
    assert(!drag.gliding)
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)

  // ---- DragGesture: post-release glide ----

  // Drag `n` frames at a constant `pxPerFrame`, leaving the pointer down.
  private def dragFrames(
      drag: DragGesture,
      p: Pointer,
      n: Int,
      pxPerFrame: Double,
      tpf: Double = 16.0,
  ): Unit =
    var i = 0
    while i < n do
      p.x += pxPerFrame
      p.y -= pxPerFrame
      drag.update(tpf)
      i += 1

  test("glide: a flick keeps moving after release and fades out"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val drag = mkDrag(ps, glideHalfLife = 120.0)
    drag.update(Frame) // reseed
    dragFrames(drag, p, 10, 16.0) // ~1 px/ms
    ps.splice(0, 1) // release above the threshold
    drag.update(Frame)
    assert(drag.gliding)
    assert(!drag.dragging)
    val first = drag.delta.dx
    assert(first > 0.0, s"expected a coasting delta, got $first")
    // sign follows the drag on both axes
    assert(drag.delta.dy < 0.0)
    // strictly decaying
    var prev = first
    var i = 0
    while i < 5 do
      drag.update(Frame)
      assert(drag.delta.dx < prev, s"expected decay, ${drag.delta.dx} !< $prev")
      prev = drag.delta.dx
      i += 1

  test("glide: speed halves after one half-life"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val drag = mkDrag(ps, glideHalfLife = 120.0)
    drag.update(15.0)
    dragFrames(drag, p, 12, 15.0, tpf = 15.0)
    ps.splice(0, 1)
    drag.update(15.0)
    val v0 = drag.delta.dx / 15.0
    // 8 further frames of 15ms = 120ms = one half-life
    var i = 0
    while i < 8 do
      drag.update(15.0)
      i += 1
    val v1 = drag.delta.dx / 15.0
    assertEqualsDouble(v1 / v0, 0.5, 1e-9)

  test("glide: a release below the threshold stops dead"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    // 1 px/frame ≈ 62 px/s, well under the 200 px/s threshold
    val drag = mkDrag(ps, glideMinSpeed = 200.0, glideHalfLife = 120.0)
    drag.update(Frame)
    dragFrames(drag, p, 10, 1.0)
    ps.splice(0, 1)
    drag.update(Frame)
    assert(!drag.gliding)
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)

  test("glide: parking the pointer before release does not fling"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val drag = mkDrag(ps, glideHalfLife = 120.0)
    drag.update(Frame)
    dragFrames(drag, p, 10, 16.0) // fast drag
    dragFrames(drag, p, 12, 0.0) // then held still ~190ms
    ps.splice(0, 1)
    drag.update(Frame)
    assert(!drag.gliding)
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)

  test("glide: a fresh press cancels it"):
    val p = mkPointer(1, 0, 0)
    val ps: Arr[Pointer] = Arr(p)
    val drag = mkDrag(ps, glideHalfLife = 120.0)
    drag.update(Frame)
    dragFrames(drag, p, 10, 16.0)
    ps.splice(0, 1)
    drag.update(Frame)
    assert(drag.gliding)
    // tap: a fresh press reseeds and kills the coast
    val p2 = mkPointer(2, 300, 300)
    ps.push(p2)
    drag.update(Frame)
    assert(!drag.gliding)
    assertEqualsDouble(drag.delta.dx, 0.0, 1e-9)
    // and releasing that still press does not start a new glide
    ps.splice(0, 1)
    drag.update(Frame)
    assert(!drag.gliding)
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
