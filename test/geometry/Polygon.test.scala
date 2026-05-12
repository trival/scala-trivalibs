package trivalibs.graphics.geometry

import munit.FunSuite
import trivalibs.graphics.math.cpu.{Vec3, Vec3Tuple}
import trivalibs.graphics.math.cpu.given
import trivalibs.utils.js.*

class PolygonTest extends FunSuite:

  // Vec3 has Position and Lerp instances via Vec3Base and Vec3ImmutableOps

  // ---------------------------------------------------------------------------
  // Triangle tests
  // ---------------------------------------------------------------------------

  test("Triangle accessors"):
    val t = Triangle(Vec3(1, 0, 0), Vec3(0, 1, 0), Vec3(0, 0, 0))
    assert(t.a.approxEq(Vec3(1, 0, 0)))
    assert(t.b.approxEq(Vec3(0, 1, 0)))
    assert(t.c.approxEq(Vec3(0, 0, 0)))

  test("Triangle normal points +Z for CCW triangle in XY plane"):
    // CCW when viewed from +Z: (0,0,0) → (1,0,0) → (0,1,0)
    val t      = Triangle(Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(0, 1, 0))
    val n      = t.normal
    assertEqualsDouble(n.x, 0.0, 1e-9)
    assertEqualsDouble(n.y, 0.0, 1e-9)
    assertEqualsDouble(n.z, 1.0, 1e-9)

  test("Triangle normal is unit length"):
    val t = Triangle(Vec3(0, 0, 0), Vec3(2, 0, 0), Vec3(0, 3, 0))
    val n = t.normal
    assertEqualsDouble(n.length, 1.0, 1e-9)

  test("Triangle splitByPlane — all in front → 1 triangle"):
    val plane = Plane(Vec3(0, 1, 0), 0.0) // y = 0
    val t     = Triangle(Vec3(-1, 1, 0), Vec3(1, 1, 0), Vec3(0, 2, 0))
    val result = t.splitByPlane(plane)
    assertEquals(result.length, 1)

  test("Triangle splitByPlane — all behind → 0 triangles"):
    val plane = Plane(Vec3(0, 1, 0), 0.0)
    val t     = Triangle(Vec3(-1, -1, 0), Vec3(1, -1, 0), Vec3(0, -2, 0))
    val result = t.splitByPlane(plane)
    assertEquals(result.length, 0)

  test("Triangle splitByPlane — 1 vertex in front → 1 triangle"):
    val plane  = Plane(Vec3(0, 1, 0), 0.0)
    val t      = Triangle(Vec3(0, 1, 0), Vec3(-1, -1, 0), Vec3(1, -1, 0))
    val result = t.splitByPlane(plane)
    assertEquals(result.length, 1)

  test("Triangle splitByPlane — 2 vertices in front → 2 triangles"):
    val plane  = Plane(Vec3(0, 1, 0), 0.0)
    val t      = Triangle(Vec3(-1, 1, 0), Vec3(1, 1, 0), Vec3(0, -1, 0))
    val result = t.splitByPlane(plane)
    assertEquals(result.length, 2)

  // ---------------------------------------------------------------------------
  // Quad tests
  // ---------------------------------------------------------------------------

  test("Quad accessors (tl=0, bl=1, br=2, tr=3)"):
    val q = Quad(Vec3(0, 1, 0), Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(1, 1, 0))
    assert(q.tl.approxEq(Vec3(0, 1, 0)))
    assert(q.bl.approxEq(Vec3(0, 0, 0)))
    assert(q.br.approxEq(Vec3(1, 0, 0)))
    assert(q.tr.approxEq(Vec3(1, 1, 0)))

  test("Quad normal points +Z for flat CCW quad in XY plane"):
    val q = Quad(Vec3(0, 1, 0), Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(1, 1, 0))
    val n = q.normal
    assertEqualsDouble(n.x, 0.0, 1e-9)
    assertEqualsDouble(n.y, 0.0, 1e-9)
    assertEqualsDouble(n.z, 1.0, 1e-9)

  test("Quad normal is unit length"):
    val q = Quad(Vec3(0, 3, 0), Vec3(0, 0, 0), Vec3(2, 0, 0), Vec3(2, 3, 0))
    val n = q.normal
    assertEqualsDouble(n.length, 1.0, 1e-9)

  test("Quad subdivideH produces midpoints"):
    val q         = Quad(Vec3(0, 2, 0), Vec3(0, 0, 0), Vec3(2, 0, 0), Vec3(2, 2, 0))
    val (bot, top) = q.subdivideH
    // bot: bl=(0,0,0), br=(2,0,0), tr=(2,1,0), tl=(0,1,0)
    assertEqualsDouble(bot.tr.y, 1.0, 1e-9)
    assertEqualsDouble(bot.tl.y, 1.0, 1e-9)
    // top: bl=(0,1,0), br=(2,1,0), tr=(2,2,0), tl=(0,2,0)
    assertEqualsDouble(top.bl.y, 1.0, 1e-9)

  test("Quad subdivideV produces midpoints"):
    val q           = Quad(Vec3(0, 2, 0), Vec3(0, 0, 0), Vec3(2, 0, 0), Vec3(2, 2, 0))
    val (left, right) = q.subdivideV
    assertEqualsDouble(left.br.x, 1.0, 1e-9)
    assertEqualsDouble(right.bl.x, 1.0, 1e-9)

  test("Quad toTriangles produces CCW triangles"):
    val q        = Quad(Vec3(0, 1, 0), Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(1, 1, 0))
    val (t1, t2) = q.toTriangles
    // Both triangles should have +Z normal
    val n1 = t1.normal
    val n2 = t2.normal
    assertEqualsDouble(n1.z, 1.0, 1e-9)
    assertEqualsDouble(n2.z, 1.0, 1e-9)

  test("Quad splitByPlane — all behind → empty"):
    val plane  = Plane(Vec3(0, 1, 0), 0.0)
    val q      = Quad(Vec3(-1, -2, 0), Vec3(-1, -1, 0), Vec3(1, -1, 0), Vec3(1, -2, 0))
    val result = q.splitByPlane(plane)
    assertEquals(result.length, 0)

  test("Quad splitByPlane — all in front → 1 quad"):
    val plane  = Plane(Vec3(0, 1, 0), 0.0)
    val q      = Quad(Vec3(-1, 2, 0), Vec3(-1, 1, 0), Vec3(1, 1, 0), Vec3(1, 2, 0))
    val result = q.splitByPlane(plane)
    assertEquals(result.length, 1)

  test("Quad splitByPlane — half clipped → 1 quad"):
    // Bottom half below plane y=0, top half above
    val plane = Plane(Vec3(0, 1, 0), 0.0)
    val q     = Quad(Vec3(-1, 1, 0), Vec3(-1, -1, 0), Vec3(1, -1, 0), Vec3(1, 1, 0))
    val result = q.splitByPlane(plane)
    // 2 vertices above, 2 below → 4 output verts → 1 quad
    assertEquals(result.length, 1)
