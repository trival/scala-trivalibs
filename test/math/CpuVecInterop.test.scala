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
  // Stage 4 — two-Vec-param ops and comparisons
  // ---------------------------------------------------------------------------

  test("mix over every CPU/GPU combination of b and t"):
    assertEquals(v3e.mix(v3e, v3e).wgsl, "mix(v, v, v)")
    assertEquals(v3e.mix(v3e, f).wgsl, "mix(v, v, k)")
    assertEquals(v3e.mix(v3e, 0.5).wgsl, "mix(v, v, 0.5)")
    assertEquals(v3e.mix(v3e, c3).wgsl, s"mix(v, v, $w3)")
    assertEquals(v3e.mix(c3, v3e).wgsl, s"mix(v, $w3, v)")
    assertEquals(v3e.mix(c3, f).wgsl, s"mix(v, $w3, k)")
    assertEquals(v3e.mix(c3, c3).wgsl, s"mix(v, $w3, $w3)")
    assertEquals(v3e.mix(c3, 0.5).wgsl, s"mix(v, $w3, 0.5)")

  test("lerp mirrors mix over the same combinations"):
    // The motivating call site: `vec3(WallTintLow).lerp(WallTintHigh, t)`.
    assertEquals(v3e.lerp(c3, f).wgsl, s"mix(v, $w3, k)")
    assertEquals(v3e.lerp(c3, 0.5).wgsl, s"mix(v, $w3, 0.5)")
    assertEquals(v3e.lerp(c3, c3).wgsl, s"mix(v, $w3, $w3)")
    assertEquals(v3e.lerp(c3, v3e).wgsl, s"mix(v, $w3, v)")
    assertEquals(v3e.lerp(v3e, 0.5).wgsl, "mix(v, v, 0.5)")
    assertEquals(v3e.lerp(v3e, c3).wgsl, s"mix(v, v, $w3)")
    assertEquals(v2e.lerp(c2, 0.5).wgsl, s"mix(a, $w2, 0.5)")
    assertEquals(v4e.lerp(c4, 0.5).wgsl, s"mix(q, $w4, 0.5)")

  test("smoothstep with matching edge kinds"):
    assertEquals(v3e.smoothstep(c3, c3).wgsl, s"smoothstep($w3, $w3, v)")
    assertEquals(v3e.smoothstep(c3, v3e).wgsl, s"smoothstep($w3, v, v)")
    assertEquals(v3e.smoothstep(v3e, c3).wgsl, s"smoothstep(v, $w3, v)")
    assertEquals(v3e.smoothstep(0.0, f).wgsl,
      "smoothstep(vec3<f32>(0.0), vec3<f32>(k), v)")
    assertEquals(v3e.smoothstep(f, 1.0).wgsl,
      "smoothstep(vec3<f32>(k), vec3<f32>(1.0), v)")

  test("comparisons with CPU Vec and scalar operands"):
    assertEquals((v3e < c3).wgsl, s"(1.0 - step($w3, v))")
    assertEquals((v3e <= c3).wgsl, s"step(v, $w3)")
    assertEquals((v3e > c3).wgsl, s"(1.0 - step(v, $w3))")
    assertEquals((v3e >= c3).wgsl, s"step($w3, v)")

  test("comparisons broadcast a scalar edge"):
    assertEquals((v3e > 0.5).wgsl, "(1.0 - step(v, vec3<f32>(0.5)))")
    assertEquals((v3e >= 0.5).wgsl, "step(vec3<f32>(0.5), v)")
    assertEquals((v3e < f).wgsl, "(1.0 - step(vec3<f32>(k), v))")
    assertEquals((v2e <= 0.5).wgsl, "step(a, vec2<f32>(0.5))")
    assertEquals((v4e > 0.5).wgsl, "(1.0 - step(q, vec4<f32>(0.5)))")

  // ---------------------------------------------------------------------------
  // Equivalence: dropping an explicit `vecN(...)` wrapper must not change the
  // emitted WGSL. This is what licenses migrating live sketch call sites — the
  // compiled bundles are minified, so this is the only real check that an
  // overload resolved to the intended alternative.
  // ---------------------------------------------------------------------------

  test("unwrapped CPU operand emits the same WGSL as the wrapped form"):
    assertEquals((v3e * c3).wgsl, (v3e * vec3(c3)).wgsl)
    assertEquals((f * c3).wgsl, (f * vec3(c3)).wgsl)
    assertEquals(v3e.lerp(c3, f).wgsl, v3e.lerp(vec3(c3), f).wgsl)
    assertEquals(v3e.mix(c3, 0.5).wgsl, v3e.mix(vec3(c3), 0.5).wgsl)
    assertEquals(v3e.dot(c3).wgsl, v3e.dot(vec3(c3)).wgsl)
    assertEquals(v3e.min(c3).wgsl, v3e.min(vec3(c3)).wgsl)
    assertEquals(v3e.reflect(c3).wgsl, v3e.reflect(vec3(c3)).wgsl)
    assertEquals((v3e >= c3).wgsl, (v3e >= vec3(c3)).wgsl)
    assertEquals(
      v3e.smoothstep(c3, c3).wgsl,
      v3e.smoothstep(vec3(c3), vec3(c3)).wgsl,
    )
    assertEquals(v2e.lerp(c2, 0.5).wgsl, v2e.lerp(vec2(c2), 0.5).wgsl)
    assertEquals(v4e.lerp(c4, 0.5).wgsl, v4e.lerp(vec4(c4), 0.5).wgsl)

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

  // ---------------------------------------------------------------------------
  // Assignment operators — `:=` on locals and output slots, and the compound
  // forms on `var` locals. These take a bare `Expr`, so before this change they
  // accepted a `Double` only through Conversion[Double, FloatExpr] (which
  // conforms to Conversion[Double, Expr] — Conversion is covariant in its
  // result). Overloading them blocks that path, hence the Double/Int forms.
  // ---------------------------------------------------------------------------

  test(":= assigns a CPU Vec to a local"):
    assertEquals((VarVec3("col") := c3).toString.trim, s"var col = $w3;")
    assertEquals((VarVec2("p") := c2).toString.trim, s"var p = $w2;")
    assertEquals((LetVec4("q") := c4).toString.trim, s"let q = $w4;")

  test(":= still accepts Double and Int literals"):
    assertEquals((VarFloat("n") := 0.5).toString.trim, "var n = 0.5;")
    assertEquals((VarFloat("n") := 2).toString.trim, "var n = f32(2);")

  test(":= emits the same WGSL as the wrapped form"):
    assertEquals(
      (VarVec3("col") := c3).toString,
      (VarVec3("col") := vec3(c3)).toString,
    )

  test("compound assignment with CPU Vec and literals"):
    val a = VarVec3("col")
    a := c3
    assertEquals((a += c3).toString.trim, s"col += $w3;")
    assertEquals((a *= 0.5).toString.trim, "col *= 0.5;")
    assertEquals((a -= c3).toString.trim, s"col -= $w3;")

  test("AssignTarget := accepts CPU values and literals"):
    // `ctx.out.color := WallColor` — the output-slot path, which is a separate
    // class from the local-variable `:=` above.
    val t = trivalibs.graphics.shader.dsl.AssignTarget("out.color")
    assertEquals((t := c4).toString.trim, s"out.color = $w4;")
    assertEquals((t := 1.0).toString.trim, "out.color = 1.0;")
    assertEquals((t := c3).toString.trim, s"out.color = $w3;")
    assertEquals((t := c4).toString, (t := vec4(c4)).toString)

  // ---------------------------------------------------------------------------
  // Stage 5 — matrix products with CPU operands
  // ---------------------------------------------------------------------------

  test("MatNExpr * CPU VecN"):
    val m4 = Mat4Expr("M")
    val m3 = Mat3Expr("N")
    val m2 = Mat2Expr("O")
    assertEquals((m4 * c4).wgsl, s"(M * $w4)")
    assertEquals((m3 * c3).wgsl, s"(N * $w3)")
    assertEquals((m2 * c2).wgsl, s"(O * $w2)")

  test("MatNExpr * CPU MatN"):
    val m2 = Mat2Expr("O")
    val cm2 = Mat2(1.0, 0.0, 0.0, 1.0)
    assertEquals(
      (m2 * cm2).wgsl,
      "(O * mat2x2<f32>(1.0, 0.0, 0.0, 1.0))",
    )

  test("matrix products still take expr operands"):
    // The generic `*[Vec]` and the `MatNExpr` overload must not be shadowed by
    // the CPU ones added alongside them.
    val m4 = Mat4Expr("M")
    assertEquals((m4 * Vec4Expr("p")).wgsl, "(M * p)")
    assertEquals((m4 * Mat4Expr("V")).wgsl, "(M * V)")

  test("matrix products emit the same WGSL as the wrapped form"):
    val m4 = Mat4Expr("M")
    assertEquals((m4 * c4).wgsl, (m4 * vec4(c4)).wgsl)
