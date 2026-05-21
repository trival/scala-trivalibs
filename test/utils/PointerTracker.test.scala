package trivalibs.utils.events

import munit.FunSuite

class PointerTrackerTest extends FunSuite:

  test("down starts a drag, up ends it"):
    val t = PointerTracker(holdRadius = 5.0)
    assert(!t.dragging)
    t.down(100, 100)
    assert(t.dragging)
    assert(!t.holding)
    t.up()
    assert(!t.dragging)

  test("move accumulates max displacement from origin"):
    val t = PointerTracker(holdRadius = 5.0)
    t.down(0, 0)
    t.move(3, 4) // distance 5
    assertEqualsDouble(t.maxDist, 5.0, 1e-9)
    t.move(1, 1) // closer, max stays
    assertEqualsDouble(t.maxDist, 5.0, 1e-9)
    t.move(6, 8) // distance 10
    assertEqualsDouble(t.maxDist, 10.0, 1e-9)

  test("checkHold activates when movement stayed within radius"):
    val t = PointerTracker(holdRadius = 5.0)
    t.down(0, 0)
    t.move(2, 2) // within radius
    assert(t.checkHold())
    assert(t.holding)

  test("checkHold does not activate when pointer strayed beyond radius"):
    val t = PointerTracker(holdRadius = 5.0)
    t.down(0, 0)
    t.move(10, 0) // beyond radius
    assert(!t.checkHold())
    assert(!t.holding)

  test("checkHold is a no-op without an active press"):
    val t = PointerTracker(holdRadius = 5.0)
    assert(!t.checkHold())
    t.down(0, 0)
    t.up()
    assert(!t.checkHold())

  test("a new press resets hold and max displacement"):
    val t = PointerTracker(holdRadius = 5.0)
    t.down(0, 0)
    t.move(2, 2)
    assert(t.checkHold())
    t.up()
    t.down(0, 0)
    assert(!t.holding)
    assertEqualsDouble(t.maxDist, 0.0, 1e-9)
