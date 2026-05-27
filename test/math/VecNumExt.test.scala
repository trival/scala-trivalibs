package trivalibs.graphics.math

import munit.FunSuite
import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.gpu.{*, given}

// Componentwise NumExt ops added to vec2/3/4: pow, inverseSqrt, trunc, exp2,
// clamp01, and scalar-edge smoothstep — on both CPU (values) and GPU (WGSL).
class VecNumExtTest extends FunSuite:

  test("CPU pow — scalar and vector exponent"):
    val v = Vec3(2.0, 3.0, 4.0)
    val s = v.pow(2.0)
    assertEquals((s.x, s.y, s.z), (4.0, 9.0, 16.0))
    val e = v.pow(Vec3(2.0, 1.0, 0.5))
    assertEquals((e.x, e.y), (4.0, 3.0))
    assertEqualsDouble(e.z, 2.0, 1e-9)

  test("CPU clamp01 clamps each component to [0,1]"):
    val v = Vec4(-1.0, 0.5, 2.0, 0.0).clamp01
    assertEquals((v.x, v.y, v.z, v.w), (0.0, 0.5, 1.0, 0.0))

  test("CPU trunc / exp2 / inverseSqrt"):
    val t = Vec2(1.7, -1.7).trunc
    assertEquals((t.x, t.y), (1.0, -1.0))
    val e = Vec2(2.0, 3.0).exp2
    assertEquals((e.x, e.y), (4.0, 8.0))
    assertEqualsDouble(Vec2(4.0, 4.0).inverseSqrt.x, 0.5, 1e-9)

  test("CPU scalar-edge smoothstep"):
    val v = Vec3(0.0, 0.5, 1.0).smoothstep(0.0, 1.0)
    assertEqualsDouble(v.y, 0.5, 1e-9)
    assertEquals((v.x, v.z), (0.0, 1.0))

  test("GPU emits native WGSL"):
    val v = Vec3Expr("c")
    assertEquals(v.pow(2.3).wgsl, "pow(c, vec3<f32>(2.3))")
    assertEquals(v.pow(Vec3Expr("e")).wgsl, "pow(c, e)")
    assertEquals(v.clamp01.wgsl, "saturate(c)")
    assertEquals(v.inverseSqrt.wgsl, "inverseSqrt(c)")
    assertEquals(v.trunc.wgsl, "trunc(c)")
    assertEquals(v.exp2.wgsl, "exp2(c)")
    assertEquals(
      v.smoothstep(0.0, 1.0).wgsl,
      "smoothstep(vec3<f32>(0.0), vec3<f32>(1.0), c)",
    )
