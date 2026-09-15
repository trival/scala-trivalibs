package trivalibs.graphics.math

import munit.FunSuite
import trivalibs.prelude.core.{*, given}
import trivalibs.prelude.painter.{*, given}

// Both preludes are imported on purpose: `lerpIn` exists on the `NumExt[Double]`
// given (core) and as plain extensions on `Double` and `FloatExpr` (painter),
// and an extension method resolves by name from a single source. The explicit
// result types below pin which one wins — a future shadowing change fails to
// compile here instead of silently turning CPU math into shader expressions.
class LerpInTest extends FunSuite:

  test("cpu bounds: scalar and every vec width"):
    val s: Double = 0.5.lerpIn(0.0, 1.0)
    assertEquals(s, 0.5)

    val v2: Vec2 = 0.25.lerpIn(Vec2(0, 0), Vec2(4, 8))
    assertEquals((v2.x, v2.y), (1.0, 2.0))

    val v3: Vec3 = 0.5.lerpIn(Vec3(0, 0, 0), Vec3(1, 2, 3))
    assertEquals((v3.x, v3.y, v3.z), (0.5, 1.0, 1.5))

    val v4: Vec4 = 0.5.lerpIn(Vec4(0, 0, 0, 0), Vec4(1, 2, 3, 4))
    assertEquals((v4.x, v4.y, v4.z, v4.w), (0.5, 1.0, 1.5, 2.0))

  test("cpu bounds: any Lerp representation, not just the class"):
    val tuple: Vec3Tuple = 0.5.lerpIn((0.0, 0.0, 0.0), (1.0, 2.0, 3.0))
    assertEquals(tuple, (0.5, 1.0, 1.5))

  test("gpu bounds: scalar literals and vec exprs"):
    val t = FloatExpr("t")
    val scalar: FloatExpr = t.lerpIn(0.25, 1.0)
    val ints: FloatExpr = t.lerpIn(0, 1)
    val vec: Vec3Expr = t.lerpIn(vec3(0), vec3(1))
    assertEquals(scalar.wgsl, "mix(0.25, 1.0, t)")
    assertEquals(ints.wgsl, "mix(f32(0), f32(1), t)")
    assertEquals(vec.wgsl, "mix(vec3<f32>(0.0), vec3<f32>(1.0), t)")

  test("gpu lerpIn is lerp with the arguments reversed"):
    val t = FloatExpr("t")
    assertEquals(t.lerpIn(vec2(0), vec2(1)).wgsl, vec2(0).lerp(vec2(1), t).wgsl)
    assertEquals(t.lerpIn(vec4(0), vec4(1)).wgsl, vec4(0).lerp(vec4(1), t).wgsl)

  test("gpu bounds: a FloatExpr parameter, from the NumExt-derived instance"):
    val t = FloatExpr("t")
    val lo = FloatExpr("lo")
    val hi = FloatExpr("hi")
    val r: FloatExpr = t.lerpIn(lo, hi)
    assertEquals(r.wgsl, "mix(lo, hi, t)")

  // `lerp` deliberately stays on the ops traits / NumExt rather than being
  // reached through LerpBy: the type class is one signature, these are whole
  // overload sets, and an imported LerpBy given would hide them.
  test("the vector ops keep their own lerp overloads, unshadowed"):
    val t = FloatExpr("t")
    assertEquals(vec3(0).lerp(vec3(1), t).wgsl, "mix(vec3<f32>(0.0), vec3<f32>(1.0), t)")
    assertEquals(vec3(0).lerp(Vec3(1, 1, 1), t).wgsl, "mix(vec3<f32>(0.0), vec3<f32>(1.0, 1.0, 1.0), t)")
    assertEquals(FloatExpr("n").lerp(FloatExpr("m"), t).wgsl, "mix(n, m, t)")

  test("Lerp[T] is what the geometry algorithms ask for"):
    def midpoint[T: Lerp](a: T, b: T): T = a.lerp(b, 0.5)
    assertEquals(midpoint(0.0, 4.0), 2.0)
    assertEquals(midpoint((0.0, 0.0, 0.0), (1.0, 2.0, 3.0)), (0.5, 1.0, 1.5))
