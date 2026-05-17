package trivalibs.graphics.math

import munit.FunSuite
import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.gpu.{*, given}

class SwizzleTest extends FunSuite:

  // sanity: CPU + GPU swizzles can coexist in the same file
  test("CPU + GPU swizzles coexist (sanity)"):
    val v3 = Vec3(1.0, 2.0, 3.0)
    val v3e = Vec3Expr("v")
    assertEquals(v3.zyx.x, 3.0)
    assertEquals(v3e.zyx.wgsl, "v.zyx")


  test("Vec2.yx reverses components"):
    val v = Vec2(1.0, 2.0)
    assertEquals(v.yx.x, 2.0)
    assertEquals(v.yx.y, 1.0)

  test("Vec2Tuple.yx preserves tuple type"):
    val t: Vec2Tuple = (1.0, 2.0)
    val r = t.yx
    assertEquals(r, (2.0, 1.0))

  test("Vec3 splits and reverse"):
    val v = Vec3(1.0, 2.0, 3.0)
    assertEquals((v.xy.x, v.xy.y), (1.0, 2.0))
    assertEquals((v.yz.x, v.yz.y), (2.0, 3.0))
    assertEquals((v.zyx.x, v.zyx.y, v.zyx.z), (3.0, 2.0, 1.0))

  test("Vec3Tuple.zyx preserves tuple type"):
    val t: Vec3Tuple = (1.0, 2.0, 3.0)
    assertEquals(t.zyx, (3.0, 2.0, 1.0))

  test("Vec4 splits and reverse"):
    val v = Vec4(1.0, 2.0, 3.0, 4.0)
    assertEquals((v.xy.x, v.xy.y), (1.0, 2.0))
    assertEquals((v.yz.x, v.yz.y), (2.0, 3.0))
    assertEquals((v.zw.x, v.zw.y), (3.0, 4.0))
    assertEquals((v.xyz.x, v.xyz.y, v.xyz.z), (1.0, 2.0, 3.0))
    assertEquals((v.yzw.x, v.yzw.y, v.yzw.z), (2.0, 3.0, 4.0))
    val r = v.wzyx
    assertEquals((r.x, r.y, r.z, r.w), (4.0, 3.0, 2.0, 1.0))

  test("Vec4Tuple.wzyx preserves tuple type"):
    val t: Vec4Tuple = (1.0, 2.0, 3.0, 4.0)
    assertEquals(t.wzyx, (4.0, 3.0, 2.0, 1.0))

  test("rgba aliases match xyzw swizzles on Vec3"):
    val v = Vec3(1.0, 2.0, 3.0)
    assertEquals((v.rg.x, v.rg.y), (1.0, 2.0))
    assertEquals((v.gb.x, v.gb.y), (2.0, 3.0))
    val r = v.bgr
    assertEquals((r.x, r.y, r.z), (3.0, 2.0, 1.0))

  test("rgba aliases match xyzw swizzles on Vec4"):
    val v = Vec4(1.0, 2.0, 3.0, 4.0)
    assertEquals((v.rg.x, v.rg.y), (1.0, 2.0))
    assertEquals((v.ba.x, v.ba.y), (3.0, 4.0))
    assertEquals((v.rgb.x, v.rgb.y, v.rgb.z), (1.0, 2.0, 3.0))
    val r = v.abgr
    assertEquals((r.x, r.y, r.z, r.w), (4.0, 3.0, 2.0, 1.0))

  test("rgba aliases on Vec4Tuple preserve tuple type"):
    val t: Vec4Tuple = (1.0, 2.0, 3.0, 4.0)
    assertEquals(t.rgb, (1.0, 2.0, 3.0))
    assertEquals(t.abgr, (4.0, 3.0, 2.0, 1.0))

  // ---------------------------------------------------------------------------
  // distance / reflect / refract — CPU
  // ---------------------------------------------------------------------------

  test("Vec2.distance"):
    assertEqualsDouble(Vec2(0, 0).distance(Vec2(3, 4)), 5.0, 1e-12)

  test("Vec3.distance"):
    assertEqualsDouble(Vec3(0, 0, 0).distance(Vec3(2, 3, 6)), 7.0, 1e-12)

  test("Vec4.distance"):
    assertEqualsDouble(
      Vec4(1, 2, 3, 4).distance(Vec4(1, 2, 3, 4)),
      0.0,
      1e-12,
    )

  test("Vec3.reflect about Y-axis flips Y"):
    val r = Vec3(1, -1, 0).reflect(Vec3(0, 1, 0))
    assertEqualsDouble(r.x, 1.0, 1e-12)
    assertEqualsDouble(r.y, 1.0, 1e-12)
    assertEqualsDouble(r.z, 0.0, 1e-12)

  test("Vec2.refract eta=1 returns the incident direction"):
    val i = Vec2(0, -1)
    val n = Vec2(0, 1)
    val r = i.refract(n, 1.0)
    assertEqualsDouble(r.x, 0.0, 1e-12)
    assertEqualsDouble(r.y, -1.0, 1e-12)

  test("Vec3.refract total internal reflection returns zero"):
    val i = Vec3(1, 0, 0)
    val n = Vec3(0, 1, 0)
    val r = i.refract(n, 2.0)
    assertEqualsDouble(r.x, 0.0, 1e-12)
    assertEqualsDouble(r.y, 0.0, 1e-12)
    assertEqualsDouble(r.z, 0.0, 1e-12)

  // ---------------------------------------------------------------------------
  // GPU DSL — emitted WGSL
  // ---------------------------------------------------------------------------

  test("GPU Vec2.yx emits .yx swizzle"):
    val v = Vec2Expr("v")
    assertEquals(v.yx.wgsl, "v.yx")

  test("GPU Vec3 swizzles"):
    val v = Vec3Expr("v")
    assertEquals(v.xy.wgsl, "v.xy")
    assertEquals(v.yz.wgsl, "v.yz")
    assertEquals(v.zyx.wgsl, "v.zyx")

  test("GPU Vec4 swizzles"):
    val v = Vec4Expr("v")
    assertEquals(v.xy.wgsl, "v.xy")
    assertEquals(v.xyz.wgsl, "v.xyz")
    assertEquals(v.yzw.wgsl, "v.yzw")
    assertEquals(v.wzyx.wgsl, "v.wzyx")

  test("GPU distance emits distance(...)"):
    val a = Vec3Expr("a")
    val b = Vec3Expr("b")
    assertEquals(a.distance(b).wgsl, "distance(a, b)")

  test("GPU reflect / refract emit WGSL builtins"):
    val v = Vec3Expr("v")
    val n = Vec3Expr("n")
    assertEquals(v.reflect(n).wgsl, "reflect(v, n)")
    assertEquals(v.refract(n, FloatExpr("eta")).wgsl, "refract(v, n, eta)")

  test("GPU rgba aliases delegate to xyzw swizzles in WGSL"):
    val v3 = Vec3Expr("v")
    assertEquals(v3.rg.wgsl, "v.xy")
    assertEquals(v3.bgr.wgsl, "v.zyx")
    val v4 = Vec4Expr("v")
    assertEquals(v4.rgb.wgsl, "v.xyz")
    assertEquals(v4.abgr.wgsl, "v.wzyx")
