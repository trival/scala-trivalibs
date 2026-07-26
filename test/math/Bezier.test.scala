package trivalibs.graphics.math

import munit.FunSuite
import trivalibs.graphics.math.cpu.{*, given}

// Quadratic / cubic Bézier interpolation on the CPU vector ops — ported from
// Rust `math/interpolation` (`impl_Interpolate!`).
class BezierTest extends FunSuite:

  private val eps = 1e-12

  test("Vec2 cubic — endpoints are exact"):
    val a = Vec2(1.0, 2.0)
    val b = Vec2(7.0, -3.0)
    val c1 = Vec2(4.0, 9.0)
    val c2 = Vec2(-2.0, 0.5)
    val at0 = Vec2.cubicBezier(0.0, a, c1, c2, b)
    val at1 = Vec2.cubicBezier(1.0, a, c1, c2, b)
    assertEqualsDouble(at0.x, a.x, eps)
    assertEqualsDouble(at0.y, a.y, eps)
    assertEqualsDouble(at1.x, b.x, eps)
    assertEqualsDouble(at1.y, b.y, eps)

  test("Vec2 quadratic — endpoints are exact"):
    val a = Vec2(0.0, 0.0)
    val b = Vec2(4.0, 0.0)
    val c = Vec2(2.0, 6.0)
    val at0 = Vec2.quadraticBezier(0.0, a, c, b)
    val at1 = Vec2.quadraticBezier(1.0, a, c, b)
    assertEqualsDouble(at0.x, a.x, eps)
    assertEqualsDouble(at0.y, a.y, eps)
    assertEqualsDouble(at1.x, b.x, eps)
    assertEqualsDouble(at1.y, b.y, eps)

  test("Vec2 quadratic — midpoint is (a + 2c + b) / 4"):
    val a = Vec2(0.0, 0.0)
    val b = Vec2(4.0, 0.0)
    val c = Vec2(2.0, 6.0)
    val m = Vec2.quadraticBezier(0.5, a, c, b)
    assertEqualsDouble(m.x, 2.0, eps)
    assertEqualsDouble(m.y, 3.0, eps)

  test("Vec2 cubic — evenly spaced controls reduce to a straight lerp"):
    val a = Vec2(0.0, 0.0)
    val b = Vec2(3.0, 6.0)
    val c1 = a.lerp(b, 1.0 / 3.0)
    val c2 = a.lerp(b, 2.0 / 3.0)
    for i <- 0 to 10 do
      val t = i / 10.0
      val p = Vec2.cubicBezier(t, a, c1, c2, b)
      val l = a.lerp(b, t)
      assertEqualsDouble(p.x, l.x, 1e-9)
      assertEqualsDouble(p.y, l.y, 1e-9)

  test("Vec2 cubic — a doubled control point matches the quadratic"):
    val a = Vec2(-1.0, 2.0)
    val b = Vec2(5.0, 1.0)
    val c = Vec2(2.0, 8.0)
    // Cubic controls at a + 2/3(c-a) and b + 2/3(c-b) render the same curve.
    val c1 = a.lerp(c, 2.0 / 3.0)
    val c2 = b.lerp(c, 2.0 / 3.0)
    for i <- 0 to 10 do
      val t = i / 10.0
      val cub = Vec2.cubicBezier(t, a, c1, c2, b)
      val quad = Vec2.quadraticBezier(t, a, c, b)
      assertEqualsDouble(cub.x, quad.x, 1e-9)
      assertEqualsDouble(cub.y, quad.y, 1e-9)

  test("Vec3 cubic / quadratic — endpoints and midpoint"):
    val a = Vec3(0.0, 0.0, 0.0)
    val b = Vec3(2.0, 4.0, 8.0)
    val c = Vec3(1.0, 0.0, 4.0)
    val at0 = Vec3.quadraticBezier(0.0, a, c, b)
    assertEqualsDouble(at0.z, 0.0, eps)
    val at1 = Vec3.cubicBezier(1.0, a, c, c, b)
    assertEqualsDouble(at1.z, b.z, eps)
    val m = Vec3.quadraticBezier(0.5, a, c, b)
    assertEqualsDouble(m.x, 1.0, eps)
    assertEqualsDouble(m.y, 1.0, eps)
    assertEqualsDouble(m.z, 4.0, eps)

  test("Vec2Tuple gets the same ops"):
    val p =
      Vec2Tuple.cubicBezier(0.5, (0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0))
    assertEqualsDouble(p._1, 0.5, eps)
    assertEqualsDouble(p._2, 0.75, eps)
