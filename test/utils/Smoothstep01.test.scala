package trivalibs.utils

import munit.FunSuite
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.utils.numbers.NumExt.given

// `.smoothstep01` — the unit-interval shorthand — must agree with
// `.smoothstep(0, 1)` on every NumExt instance: Double, Float and FloatExpr.
class Smoothstep01Test extends FunSuite:

  private val samples =
    List(-0.5, 0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0, 1.5)

  test("Double smoothstep01 matches smoothstep(0, 1)"):
    samples.foreach: x =>
      assertEqualsDouble(x.smoothstep01, x.smoothstep(0.0, 1.0), 1e-12, s"x=$x")

  test("Double smoothstep01 endpoints, midpoint and clamping"):
    assertEquals(0.0.smoothstep01, 0.0)
    assertEquals(1.0.smoothstep01, 1.0)
    assertEqualsDouble(0.5.smoothstep01, 0.5, 1e-12)
    // Outside [0,1] it saturates rather than continuing the cubic.
    assertEquals((-2.0).smoothstep01, 0.0)
    assertEquals(3.0.smoothstep01, 1.0)

  test("Float smoothstep01 matches smoothstep(0, 1)"):
    samples.foreach: x =>
      val f = x.toFloat
      assertEqualsFloat(f.smoothstep01, f.smoothstep(0f, 1f), 1e-6f, s"x=$x")

  test("GPU smoothstep01 emits the constant-edge builtin"):
    assertEquals(FloatExpr("t").smoothstep01.wgsl, "smoothstep(0.0, 1.0, t)")
