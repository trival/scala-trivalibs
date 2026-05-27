package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.math.gpu.Expr.*
import munit.FunSuite

class LeftScalarTest extends FunSuite:

  // ---------------------------------------------------------------------------
  // Double literal on the left of a FloatExpr operator
  // ---------------------------------------------------------------------------

  val f = FloatExpr("x")

  test("0.5 * floatExpr"):
    assertEquals((0.5 * f).toString, "(0.5 * x)")

  test("2.0 - floatExpr keeps operand order"):
    assertEquals((2.0 - f).toString, "(2.0 - x)")

  test("1.0 / floatExpr keeps operand order"):
    assertEquals((1.0 / f).toString, "(1.0 / x)")

  test("0.5 + floatExpr"):
    assertEquals((0.5 + f).toString, "(0.5 + x)")

  // ---------------------------------------------------------------------------
  // Int literal on the left is treated as a float (f32), like the right side
  // ---------------------------------------------------------------------------

  test("2 * floatExpr lifts the Int to f32"):
    assertEquals((2 * f).toString, "(f32(2) * x)")

  test("2 - floatExpr keeps operand order"):
    assertEquals((2 - f).toString, "(f32(2) - x)")

  // ---------------------------------------------------------------------------
  // Double / Int on the left of vector operators
  // ---------------------------------------------------------------------------

  val v2 = Vec2Expr("uv")
  val v3 = Vec3Expr("col")
  val v4 = Vec4Expr("rgba")

  test("0.25 * vec2Expr"):
    assertEquals((0.25 * v2).toString, "(0.25 * uv)")

  test("1.0 - vec3Expr keeps operand order"):
    assertEquals((1.0 - v3).toString, "(1.0 - col)")

  test("2 * vec4Expr lifts the Int to f32"):
    assertEquals((2 * v4).toString, "(f32(2) * rgba)")

  // ---------------------------------------------------------------------------
  // Numeric literal on the left of a scalar comparison
  // ---------------------------------------------------------------------------

  test("0.5 < floatExpr"):
    assertEquals((0.5 < f).toString, "(0.5 < x)")

  test("0.5 >= floatExpr"):
    assertEquals((0.5 >= f).toString, "(0.5 >= x)")

  test("2 > floatExpr lifts the Int to f32"):
    assertEquals((2 > f).toString, "(f32(2) > x)")

  test("0.0 === floatExpr"):
    assertEquals((0.0 === f).toString, "(0.0 == x)")

  // ---------------------------------------------------------------------------
  // Plain Double/Int arithmetic is unaffected (member ops still win)
  // ---------------------------------------------------------------------------

  test("Double * Double stays a Double"):
    val d: Double = 0.5 * 2.0
    assertEquals(d, 1.0)

  test("Int * Int stays an Int"):
    val i: Int = 2 * 3
    assertEquals(i, 6)
