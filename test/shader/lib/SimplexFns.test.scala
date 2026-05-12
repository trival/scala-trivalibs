package trivalibs.graphics.shader.lib.random

import trivalibs.graphics.shader.dsl.WgslFnData
import munit.FunSuite

class SimplexFnsTest extends FunSuite:

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  test("permute3 emits permute_3_ with vec3<f32>"):
    val data = Simplex.permute3.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn permute_3_(x: vec3<f32>)"), data.src)
    assert(data.src.contains("-> vec3<f32>"), data.src)
    assert(data.src.contains("289."), data.src)
    assertEquals(data.deps.length, 0)

  test("permute4 emits permute_4_ with vec4<f32>"):
    val data = Simplex.permute4.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn permute_4_(x: vec4<f32>)"), data.src)
    assert(data.src.contains("-> vec4<f32>"), data.src)

  test("taylorInvSqrt4 emits taylor_inv_sqrt_4_"):
    val data = Simplex.taylorInvSqrt4.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn taylor_inv_sqrt_4_(r: vec4<f32>)"), data.src)
    assert(data.src.contains("1.79284291400159"), data.src)

  // ---------------------------------------------------------------------------
  // 2D simplex
  // ---------------------------------------------------------------------------

  test("simplexNoise2d emits simplex_noise_2d with vec2<f32> input"):
    val data = Simplex.simplexNoise2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn simplex_noise_2d(v: vec2<f32>)"), data.src)
    assert(data.src.contains("-> f32"), data.src)
    assert(data.src.contains("permute_3_"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("permute_3_"), depNames.toString)

  test("simplexNoise2dSeeded emits correct signature and calls permute_3_"):
    val data = Simplex.simplexNoise2dSeeded.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn simplex_noise_2d_seeded(v: vec2<f32>, seed: f32)"), data.src)
    assert(data.src.contains("permute_3_(p + vec3(seed))"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("permute_3_"), depNames.toString)

  // ---------------------------------------------------------------------------
  // 3D simplex
  // ---------------------------------------------------------------------------

  test("simplexNoise3d emits simplex_noise_3d with vec3<f32> input"):
    val data = Simplex.simplexNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn simplex_noise_3d(v: vec3<f32>)"), data.src)
    assert(data.src.contains("permute_4_"), data.src)
    assert(data.src.contains("taylor_inv_sqrt_4_"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("permute_4_"), depNames.toString)
    assert(depNames.contains("taylor_inv_sqrt_4_"), depNames.toString)

  test("simplexNoise3dSeeded emits correct signature"):
    val data = Simplex.simplexNoise3dSeeded.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn simplex_noise_3d_seeded(v: vec3<f32>, seed: vec3<f32>)"), data.src)

  // ---------------------------------------------------------------------------
  // fBm
  // ---------------------------------------------------------------------------

  test("fbmSimplex2d emits fbm_simplex_2d with loop"):
    val data = Simplex.fbmSimplex2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn fbm_simplex_2d(pos: vec2<f32>, octaves: i32, lacunarity: f32, gain: f32)"), data.src)
    assert(data.src.contains("simplex_noise_2d"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("simplex_noise_2d"), depNames.toString)

  test("fbmSimplex2dSeeded emits fbm_simplex_2d_seeded"):
    val data = Simplex.fbmSimplex2dSeeded.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn fbm_simplex_2d_seeded(pos: vec2<f32>, octaves: i32, lacunarity: f32, gain: f32, seed: f32)"), data.src)
    assert(data.src.contains("simplex_noise_2d_seeded"), data.src)

  test("fbmSimplex3d emits fbm_simplex_3d"):
    val data = Simplex.fbmSimplex3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn fbm_simplex_3d(pos: vec3<f32>, octaves: i32, lacunarity: f32, gain: f32)"), data.src)
    assert(data.src.contains("simplex_noise_3d"), data.src)

  test("fbmSimplex3dSeeded emits fbm_simplex_3d_seeded"):
    val data = Simplex.fbmSimplex3dSeeded.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn fbm_simplex_3d_seeded(pos: vec3<f32>, octaves: i32, lacunarity: f32, gain: f32, seed: vec3<f32>)"), data.src)

  // ---------------------------------------------------------------------------
  // Worley
  // ---------------------------------------------------------------------------

  test("worley2d emits worley_2d and depends on permute3"):
    val data = Simplex.worley2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn worley_2d(pos: vec2<f32>, jitter: f32)"), data.src)
    assert(data.src.contains("-> vec2<f32>"), data.src)
    assert(data.src.contains("permute_3_"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("permute_3_"), depNames.toString)

  // ---------------------------------------------------------------------------
  // 4D simplex helpers
  // ---------------------------------------------------------------------------

  test("permute1 emits permute_1_ with f32 scalar"):
    val data = Simplex.simplexNoise4d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn simplex_noise_4d(pos: vec4<f32>)"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("permute_1_"), depNames.toString)
    assert(depNames.contains("taylor_inv_sqrt_1_"), depNames.toString)
    assert(depNames.contains("grad_4_"), depNames.toString)
    assert(depNames.contains("permute_4_"), depNames.toString)
    assert(depNames.contains("taylor_inv_sqrt_4_"), depNames.toString)

  test("simplexNoise4d emits correct signature and rank-sort swizzles"):
    val data = Simplex.simplexNoise4d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn simplex_noise_4d(pos: vec4<f32>)"), data.src)
    assert(data.src.contains("-> f32"), data.src)
    assert(data.src.contains("step(x0.yzw, x0.xxx)"), data.src)
    assert(data.src.contains("step(x0.zww, x0.yyz)"), data.src)
    assert(data.src.contains("permute_1_"), data.src)
    assert(data.src.contains("grad_4_"), data.src)

  test("tilingSimplexNoise2d emits correct signature and calls simplex_noise_4d"):
    val data = Simplex.tilingSimplexNoise2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn tiling_simplex_noise_2d(pos: vec2<f32>, scale: f32)"), data.src)
    assert(data.src.contains("-> f32"), data.src)
    assert(data.src.contains("simplex_noise_4d"), data.src)
    assert(data.src.contains("6.28318530718"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("simplex_noise_4d"), depNames.toString)

  // ---------------------------------------------------------------------------
  // Dep chain: fbmSimplex2d → simplexNoise2d → permute3
  // ---------------------------------------------------------------------------

  test("fbmSimplex2d dep chain reaches permute_3_ transitively"):
    import trivalibs.graphics.shader.dsl.{Program, WgslFn}
    import trivalibs.graphics.math.cpu.*
    import trivalibs.graphics.math.gpu.*
    import trivalibs.graphics.shader.{given}
    val prog = Program[EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple]()
    prog.fn(Simplex.fbmSimplex2d)
    val src = prog.helperFnsStr
    assert(src.contains("fn permute_3_"), src)
    assert(src.contains("fn simplex_noise_2d"), src)
    assert(src.contains("fn fbm_simplex_2d"), src)
    // permute_3_ must appear before simplex_noise_2d
    assert(src.indexOf("fn permute_3_") < src.indexOf("fn simplex_noise_2d"), src)
    assert(src.indexOf("fn simplex_noise_2d") < src.indexOf("fn fbm_simplex_2d"), src)
