package trivalibs.graphics.math

import munit.FunSuite
import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.gpu.{*, given}

/** CPU `Vec*` ↔ GPU `Vec*Expr` interop overloads.
  *
  * See `documents/cpu-gpu-vec-interop-plan.md`. The regression block at the
  * bottom is the important part: adding overloads to a method disables the
  * `Conversion[Double, FloatExpr]` path on its other parameters, so every
  * `Double` call site that used to rely on that conversion is pinned here.
  */
class CpuVecInteropTest extends FunSuite:

  val f = FloatExpr("k")
  val v2e = Vec2Expr("a")
  val v3e = Vec3Expr("v")
  val v4e = Vec4Expr("q")

  val c2 = Vec2(0.5, 0.25)
  val c3 = Vec3(0.5, 0.25, 0.125)
  val c4 = Vec4(0.5, 0.25, 0.125, 1.0)

  val w2 = "vec2<f32>(0.5, 0.25)"
  val w3 = "vec3<f32>(0.5, 0.25, 0.125)"
  val w4 = "vec4<f32>(0.5, 0.25, 0.125, 1.0)"

  // ---------------------------------------------------------------------------
  // Stage 1 — arithmetic, expr receiver
  // ---------------------------------------------------------------------------

  test("Vec2Expr arithmetic with CPU Vec2"):
    assertEquals((v2e + c2).wgsl, s"(a + $w2)")
    assertEquals((v2e - c2).wgsl, s"(a - $w2)")
    assertEquals((v2e * c2).wgsl, s"(a * $w2)")
    assertEquals((v2e / c2).wgsl, s"(a / $w2)")

  test("Vec3Expr arithmetic with CPU Vec3"):
    assertEquals((v3e + c3).wgsl, s"(v + $w3)")
    assertEquals((v3e - c3).wgsl, s"(v - $w3)")
    assertEquals((v3e * c3).wgsl, s"(v * $w3)")
    assertEquals((v3e / c3).wgsl, s"(v / $w3)")

  test("Vec4Expr arithmetic with CPU Vec4"):
    assertEquals((v4e + c4).wgsl, s"(q + $w4)")
    assertEquals((v4e - c4).wgsl, s"(q - $w4)")
    assertEquals((v4e * c4).wgsl, s"(q * $w4)")
    assertEquals((v4e / c4).wgsl, s"(q / $w4)")

  test("FloatExpr broadcast against a CPU Vec"):
    // The motivating call site: `band * HaloColor`.
    assertEquals((f * c3).wgsl, s"(k * $w3)")
    assertEquals((f + c2).wgsl, s"(k + $w2)")
    assertEquals((f - c3).wgsl, s"(k - $w3)")
    assertEquals((f / c4).wgsl, s"(k / $w4)")

  // ---------------------------------------------------------------------------
  // Stage 3 — named binary ops, expr receiver
  // ---------------------------------------------------------------------------

  test("dot / distance with a CPU Vec"):
    assertEquals(v2e.dot(c2).wgsl, s"dot(a, $w2)")
    assertEquals(v3e.dot(c3).wgsl, s"dot(v, $w3)")
    assertEquals(v4e.dot(c4).wgsl, s"dot(q, $w4)")
    assertEquals(v3e.distance(c3).wgsl, s"distance(v, $w3)")

  test("min / max with a CPU Vec"):
    assertEquals(v3e.min(c3).wgsl, s"min(v, $w3)")
    assertEquals(v3e.max(c3).wgsl, s"max(v, $w3)")

  test("pow / step with a CPU Vec"):
    assertEquals(v3e.pow(c3).wgsl, s"pow(v, $w3)")
    assertEquals(v3e.step(c3).wgsl, s"step($w3, v)")

  test("cross with a CPU Vec3"):
    assertEquals(v3e.cross(c3).wgsl, s"cross(v, $w3)")

  test("reflect / refract with a CPU Vec"):
    assertEquals(v3e.reflect(c3).wgsl, s"reflect(v, $w3)")
    assertEquals(v3e.refract(c3, f).wgsl, s"refract(v, $w3, k)")
    assertEquals(v3e.refract(c3, 1.4).wgsl, s"refract(v, $w3, 1.4)")
    assertEquals(v3e.refract(v3e, 1.4).wgsl, "refract(v, v, 1.4)")

  // ---------------------------------------------------------------------------
  // Stage 3 — pre-existing `Double` gaps this work closes.
  // `step(0.5)` and `mix(b, 0.5)` did not compile before: both were already
  // overloaded, so Conversion[Double, FloatExpr] could not fire. `min`/`max`
  // had no scalar form at all.
  // ---------------------------------------------------------------------------

  test("scalar Double forms now available on vector methods"):
    assertEquals(v3e.step(0.5).wgsl, "step(0.5, v)")
    assertEquals(v3e.mix(v3e, 0.5).wgsl, "mix(v, v, 0.5)")
    assertEquals(v3e.min(0.5).wgsl, "min(v, vec3<f32>(0.5))")
    assertEquals(v3e.max(0.5).wgsl, "max(v, vec3<f32>(0.5))")
    assertEquals(v2e.min(0.5).wgsl, "min(a, vec2<f32>(0.5))")
    assertEquals(v4e.max(0.5).wgsl, "max(q, vec4<f32>(0.5))")

  // ---------------------------------------------------------------------------
  // Regression: Double call sites that resolve via Conversion[Double, FloatExpr]
  // and would silently break if an overload set swallowed them.
  // ---------------------------------------------------------------------------

  test("Double literals still resolve on overloaded vector methods"):
    assertEquals((v3e * 2.0).wgsl, "(v * 2.0)")
    assertEquals((v3e + 2.0).wgsl, "(v + 2.0)")
    assertEquals(v3e.pow(2.0).wgsl, "pow(v, vec3<f32>(2.0))")
    assertEquals(v3e.smoothstep(0.0, 1.0).wgsl,
      "smoothstep(vec3<f32>(0.0), vec3<f32>(1.0), v)")

  test("clamp keeps taking Double edges"):
    // `clamp` gained an explicit Double overload defensively in Stage 3; before
    // that it relied on the Conversion, which still worked because clamp was
    // not overloaded.
    assertEquals(v3e.clamp(0.0, 1.0).wgsl, "clamp(v, 0.0, 1.0)")
    assertEquals(v2e.clamp(0.0, 1.0).wgsl, "clamp(a, 0.0, 1.0)")
    assertEquals(v4e.clamp(0.0, 1.0).wgsl, "clamp(q, 0.0, 1.0)")

  test("CPU-only vector code is unaffected"):
    val a = Vec3(1.0, 2.0, 3.0)
    val b = Vec3(2.0, 2.0, 2.0)
    assertEquals((a * b).x, 2.0)
    assertEquals((a * 2.0).x, 2.0)

  // A CPU-`Vec3`-receiver operator (`tint * someExpr`) was probed and rejected —
  // a top-level `*` extension on `Vec3` in the gpu package shadows the CPU
  // `Vec3ImmutableOps.*` for any file wildcard-importing both namespaces, which
  // silently retypes `cpuVec * 2.0` to `Vec3Expr`. The two assertions above are
  // what caught it. See documents/cpu-gpu-vec-interop-plan.md, finding 7.
