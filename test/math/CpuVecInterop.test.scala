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

  val v3e = Vec3Expr("v")
  val tint = Vec3(0.5, 0.25, 0.125)

  test("Vec3Expr * CPU Vec3"):
    assertEquals(
      (v3e * tint).wgsl,
      "(v * vec3<f32>(0.5, 0.25, 0.125))",
    )

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

  test("Double literals resolve on not-yet-overloaded vector methods"):
    // `clamp` is the only one of this family that is not already overloaded,
    // so it is the only one where the Conversion still fires. `step(0.5)` and
    // `mix(b, 0.5)` are *already* broken by their existing Vec/scalar overload
    // pairs — see the Stage 3 note in documents/cpu-gpu-vec-interop-plan.md.
    assertEquals(v3e.clamp(0.0, 1.0).wgsl, "clamp(v, 0.0, 1.0)")

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
