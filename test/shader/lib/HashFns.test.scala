package trivalibs.graphics.shader.lib.random

import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.math.gpu.Expr.*
import trivalibs.graphics.shader.{given}
import trivalibs.graphics.shader.dsl.WgslFnData
import munit.FunSuite

class HashFnsTest extends FunSuite:

  // ---------------------------------------------------------------------------
  // u32_to_f32
  // ---------------------------------------------------------------------------

  test("u32ToF32 emits correct WGSL"):
    val data = Hash.u32ToF32.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn u32_to_f32(x: u32)"), data.src)
    assert(data.src.contains("-> f32"), data.src)
    assert(data.src.contains("f32(x) / f32(0xffffffffu)"), data.src)

  // ---------------------------------------------------------------------------
  // Scalar u32 hashes — WGSL structure checks
  // ---------------------------------------------------------------------------

  test("hash1i emits hash1i with correct body"):
    val data = Hash.hash1i.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash1i(x: u32)"), data.src)
    assert(data.src.contains("-> u32"), data.src)
    assert(data.src.contains("0x21f0aaadu"), data.src)
    assert(data.src.contains("0xd35a2d97u"), data.src)
    assert(data.src.contains(">> 16u"), data.src)

  test("hash1iTriple32 emits hash1iTriple32 with correct body"):
    val data = Hash.hash1iTriple32.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash1i_triple32(x: u32)"), data.src)
    assert(data.src.contains("0xed5ad4bbu"), data.src)
    assert(data.src.contains("0xac4c1b51u"), data.src)
    assert(data.src.contains("0x31848babu"), data.src)

  test("hash1 calls hash1i and u32_to_f32"):
    val data = Hash.hash1.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash1(x: u32)"), data.src)
    assert(data.src.contains("-> f32"), data.src)
    assert(data.src.contains("hash1i(x)"), data.src)
    assert(data.src.contains("u32_to_f32("), data.src)

  // ---------------------------------------------------------------------------
  // 2D scalar (Inigo Quilez family)
  // ---------------------------------------------------------------------------

  test("hash21i emits hash21i with u32 return"):
    val data = Hash.hash21i.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash21i(p: vec2<u32>)"), data.src)
    assert(data.src.contains("-> u32"), data.src)
    assert(data.src.contains("73333u"), data.src)
    assert(data.src.contains("3333777777u"), data.src)

  test("hash21 calls hash21i and u32_to_f32"):
    val data = Hash.hash21.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash21(p: vec2<u32>)"), data.src)
    assert(data.src.contains("hash21i(p)"), data.src)
    assert(data.src.contains("u32_to_f32("), data.src)

  // ---------------------------------------------------------------------------
  // 2D → 2D (PCG family)
  // ---------------------------------------------------------------------------

  test("hash2i emits hash2i with vec2<u32> return"):
    val data = Hash.hash2i.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash2i(v: vec2<u32>)"), data.src)
    assert(data.src.contains("-> vec2<u32>"), data.src)
    assert(data.src.contains("1664525u"), data.src)
    assert(data.src.contains("1013904223u"), data.src)

  test("hash2 calls hash2i and u32_to_f32"):
    val data = Hash.hash2.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash2(v: vec2<u32>)"), data.src)
    assert(data.src.contains("-> vec2<f32>"), data.src)
    assert(data.src.contains("hash2i(v)"), data.src)

  // ---------------------------------------------------------------------------
  // 3D → 3D
  // ---------------------------------------------------------------------------

  test("hash3i emits hash3i with vec3<u32> return"):
    val data = Hash.hash3i.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash3i(v: vec3<u32>)"), data.src)
    assert(data.src.contains("-> vec3<u32>"), data.src)

  test("hash3 calls hash3i"):
    val data = Hash.hash3.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash3(v: vec3<u32>)"), data.src)
    assert(data.src.contains("-> vec3<f32>"), data.src)
    assert(data.src.contains("hash3i(v)"), data.src)

  // ---------------------------------------------------------------------------
  // 4D → 4D
  // ---------------------------------------------------------------------------

  test("hash4i emits hash4i with vec4<u32> return"):
    val data = Hash.hash4i.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash4i(v: vec4<u32>)"), data.src)
    assert(data.src.contains("-> vec4<u32>"), data.src)

  test("hash4 calls hash4i"):
    val data = Hash.hash4.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash4(v: vec4<u32>)"), data.src)
    assert(data.src.contains("-> vec4<f32>"), data.src)
    assert(data.src.contains("hash4i(v)"), data.src)

  // ---------------------------------------------------------------------------
  // Float-input ergonomic wrappers
  // ---------------------------------------------------------------------------

  test("hash1f uses bitcast<u32>"):
    val data = Hash.hash1f.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash1f(x: f32)"), data.src)
    assert(data.src.contains("bitcast<u32>(x)"), data.src)
    assert(data.src.contains("hash1("), data.src)

  test("hash2f uses bitcast<vec2<u32>>"):
    val data = Hash.hash2f.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash2f(v: vec2<f32>)"), data.src)
    assert(data.src.contains("bitcast<vec2<u32>>(v)"), data.src)

  test("hash3f uses bitcast<vec3<u32>>"):
    val data = Hash.hash3f.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash3f(v: vec3<f32>)"), data.src)
    assert(data.src.contains("bitcast<vec3<u32>>(v)"), data.src)

  test("hash4f uses bitcast<vec4<u32>>"):
    val data = Hash.hash4f.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn hash4f(v: vec4<f32>)"), data.src)
    assert(data.src.contains("bitcast<vec4<u32>>(v)"), data.src)

  // ---------------------------------------------------------------------------
  // Apply-site typing
  // ---------------------------------------------------------------------------

  test("hash1 apply-site produces FloatExpr"):
    val result = Hash.hash1(5.u)
    assertEquals(result.toString, "hash1(5u)")

  test("hash2 apply-site produces Vec2Expr"):
    val result = Hash.hash2(uvec2(1.u, 2.u))
    assertEquals(result.toString, "hash2(vec2<u32>(1u, 2u))")

  // ---------------------------------------------------------------------------
  // Dependency resolution
  // ---------------------------------------------------------------------------

  test("hash1 carries hash1i and u32ToF32 as deps"):
    val data = Hash.hash1.asInstanceOf[WgslFnData]
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("hash1i"), depNames)
    assert(depNames.contains("u32_to_f32"), depNames)

  test("hash2 carries hash2i and u32ToF32 as deps"):
    val data = Hash.hash2.asInstanceOf[WgslFnData]
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("hash2i"), depNames)
    assert(depNames.contains("u32_to_f32"), depNames)

  test("hash1f carries hash1 as dep"):
    val data = Hash.hash1f.asInstanceOf[WgslFnData]
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("hash1"), depNames)

  test("hash2f carries hash2 as dep"):
    val data = Hash.hash2f.asInstanceOf[WgslFnData]
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("hash2"), depNames)

  test("hash1i has no deps"):
    val data = Hash.hash1i.asInstanceOf[WgslFnData]
    assertEquals(data.deps.length, 0)
