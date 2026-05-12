package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.math.gpu.Expr.*
import trivalibs.graphics.shader.{given}
import munit.FunSuite

class IntDslTest extends FunSuite:

  // ---------------------------------------------------------------------------
  // Literals
  // ---------------------------------------------------------------------------

  test("42.i produces IntExpr"):
    assertEquals(42.i.toString, "42")

  test("(-7).i preserves negative literal"):
    assertEquals((-7).i.toString, "-7")

  test("42.u chains Int -> UInt -> UIntExpr with u suffix"):
    assertEquals((42.u: UIntExpr).toString, "42u")

  test("hex literal flows through Scala Int.toString as decimal"):
    assertEquals((0x21f0aaad.u: UIntExpr).toString, "569420461u")

  test("IntExpr string constructor for symbol expressions"):
    assertEquals(IntExpr("count").toString, "count")

  // ---------------------------------------------------------------------------
  // Scalar arithmetic
  // ---------------------------------------------------------------------------

  test("IntExpr + IntExpr"):
    assertEquals((IntExpr("a") + IntExpr("b")).toString, "(a + b)")

  test("UIntExpr * UIntExpr"):
    assertEquals((UIntExpr("a") * UIntExpr("b")).toString, "(a * b)")

  test("IntExpr.abs"):
    assertEquals(IntExpr("a").abs.toString, "abs(a)")

  test("IntExpr unary_-"):
    assertEquals((-IntExpr("a")).toString, "(-a)")

  // ---------------------------------------------------------------------------
  // Bitwise operators
  // ---------------------------------------------------------------------------

  test("UIntExpr ^ UIntExpr"):
    assertEquals((UIntExpr("a") ^ UIntExpr("b")).toString, "(a ^ b)")

  test("UIntExpr & UIntExpr"):
    assertEquals((UIntExpr("a") & UIntExpr("b")).toString, "(a & b)")

  test("UIntExpr | UIntExpr"):
    assertEquals((UIntExpr("a") | UIntExpr("b")).toString, "(a | b)")

  test("~UIntExpr"):
    assertEquals((~UIntExpr("a")).toString, "(~a)")

  test("UIntExpr >> Int emits u suffix"):
    assertEquals((UIntExpr("a") >> 16).toString, "(a >> 16u)")

  test("IntExpr & Int emits decimal constant"):
    assertEquals((IntExpr("a") & 0xff).toString, s"(a & ${0xff})")

  test(".u literal composes with bitwise XOR"):
    assertEquals(
      (UIntExpr("a") ^ (0x21f0aaad.u: UIntExpr)).toString,
      "(a ^ 569420461u)",
    )

  test("named aliases emit same WGSL as operators"):
    assertEquals(UIntExpr("a").xor(UIntExpr("b")).toString, "(a ^ b)")
    assertEquals(UIntExpr("a").shr(16.u).toString, "(a >> 16u)")
    assertEquals(UIntExpr("a").not.toString, "(~a)")

  test("precedence: + binds tighter than |"):
    assertEquals(
      (IntExpr("a") + IntExpr("b") | IntExpr("c")).toString,
      "((a + b) | c)",
    )

  // ---------------------------------------------------------------------------
  // Conversions
  // ---------------------------------------------------------------------------

  test("FloatExpr.toU32"):
    assertEquals(FloatExpr("x").toU32.toString, "u32(x)")

  test("UIntExpr.toF32"):
    assertEquals(UIntExpr("x").toF32.toString, "f32(x)")

  test("IntExpr.toU32"):
    assertEquals(IntExpr("x").toU32.toString, "u32(x)")

  test("UIntExpr.toI32"):
    assertEquals(UIntExpr("x").toI32.toString, "i32(x)")

  // ---------------------------------------------------------------------------
  // Bitcasts
  // ---------------------------------------------------------------------------

  test("FloatExpr.bitsToU32"):
    assertEquals(FloatExpr("x").bitsToU32.toString, "bitcast<u32>(x)")

  test("UIntExpr.bitsToF32"):
    assertEquals(UIntExpr("x").bitsToF32.toString, "bitcast<f32>(x)")

  test("Vec2Expr.bitsToU32"):
    assertEquals(Vec2Expr("v").bitsToU32.toString, "bitcast<vec2<u32>>(v)")

  test("UVec2Expr.bitsToF32"):
    assertEquals(UVec2Expr("u").bitsToF32.toString, "bitcast<vec2<f32>>(u)")

  // ---------------------------------------------------------------------------
  // Vector algebra
  // ---------------------------------------------------------------------------

  test("uvec2 constructor with symbol components"):
    assertEquals(
      uvec2(UIntExpr("a"), UIntExpr("b")).toString,
      "vec2<u32>(a, b)",
    )

  test("uvec2 with .u literals"):
    assertEquals(uvec2(0.u, 1.u).toString, "vec2<u32>(0u, 1u)")

  test("UVec2Expr.x"):
    assertEquals(UVec2Expr("u").x.toString, "u.x")

  test("UVec2Expr component-wise XOR"):
    assertEquals(
      (UVec2Expr("u") ^ UVec2Expr("v")).toString,
      "(u ^ v)",
    )

  // ---------------------------------------------------------------------------
  // WgslFn integration
  // ---------------------------------------------------------------------------

  test("WgslFn with UInt param and return — emits u32"):
    val f: WgslFn[(seed: UInt), UInt] =
      WgslFn.raw("hash_u32")("  return seed ^ (seed >> 16u);")
    val data = f.asInstanceOf[WgslFnData]
    assert(data.src.contains("seed: u32"), data.src)
    assert(data.src.contains("-> u32"), data.src)

  test("WgslFn with UVec2 param and return — emits vec2<u32>"):
    val f: WgslFn[(v: UVec2), UVec2] =
      WgslFn.raw("hash_uvec2")("  return v ^ (v >> vec2<u32>(16u, 16u));")
    val data = f.asInstanceOf[WgslFnData]
    assert(data.src.contains("v: vec2<u32>"), data.src)
    assert(data.src.contains("-> vec2<u32>"), data.src)

  test("WgslFn apply-site produces correct WGSL and type"):
    val f: WgslFn[(x: UInt), UInt] = WgslFn.raw("f")("  return x;")
    assertEquals(f(5.u).toString, "f(5u)")

  // ---------------------------------------------------------------------------
  // Implicit-conversion guards (deferred-decision boundary)
  // ---------------------------------------------------------------------------

  test("Int -> FloatExpr conversion is preserved"):
    assertEquals((42: FloatExpr).toString, "f32(42)")

  test(".i extension produces IntExpr (not FloatExpr)"):
    assertEquals(42.i.toString, "42")

  test(".u chains to UIntExpr"):
    assertEquals((42.u: UIntExpr).toString, "42u")
