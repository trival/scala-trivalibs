package trivalibs.graphics.shader.lib.noise

import munit.FunSuite
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.shader.dsl.{Program, WgslFn, WgslFnData}
import trivalibs.graphics.shader.given

class NoiseFnsTest extends FunSuite:

  private def src(fn: WgslFnData): String = fn.src

  // ---------------------------------------------------------------------------
  // Simplex — upstream webgl-noise formulation
  // ---------------------------------------------------------------------------

  test("simplex wgsl names follow C11 (simplex_ + snake member)"):
    assert(src(Simplex.wgsl.noise2d).contains("fn simplex_noise_2d(v: vec2<f32>) -> f32"))
    assert(src(Simplex.wgsl.noise3d).contains("fn simplex_noise_3d(v: vec3<f32>) -> f32"))
    assert(src(Simplex.wgsl.noise4d).contains("fn simplex_noise_4d(v: vec4<f32>) -> f32"))
    assert(src(Simplex.wgsl.fbm2dSeeded).contains("fn simplex_fbm_2d_seeded("))
    assert(src(Simplex.wgsl.torusNoise2d).contains("fn simplex_torus_noise_2d("))

  test("shared permute is upstream's (34x + 10)x mod 289 with floor-based mod"):
    val permute = src(NoiseCommon.permute3)
    assert(permute.contains("* 34.0) + 10.0) * x"), permute)
    val mod = src(NoiseCommon.mod289v3)
    assert(mod.contains("x - floor(x * (1.0 / 289.0)) * 289.0"), mod)

  test("4D simplex uses upstream kernel radius 0.57 and scale 60.1"):
    val s = src(Simplex.wgsl.noise4d)
    assert(s.contains("0.57 - vec3<f32>"), s)
    assert(s.contains("return 60.1 *"), s)

  test("unseeded simplex bodies carry no seed stage"):
    for fn <- Seq[WgslFnData](Simplex.wgsl.noise2d, Simplex.wgsl.noise3d, Simplex.wgsl.noise4d) do
      assert(!src(fn).contains("so."), src(fn))

  test("seeded simplex adds exactly the two seed permute stages"):
    val s = src(Simplex.wgsl.noise3dSo)
    assert(s.contains("p = noise_permute_4(noise_permute_4(p + so.x) + so.y);"), s)
    val seeded = src(Simplex.wgsl.noise3dSeeded)
    assert(seeded.contains("simplex_noise_3d_so(v, noise_seed_offsets(seed))"), seeded)

  test("seeded fbm hashes the seed once, before the octave loop"):
    val s = src(Simplex.wgsl.fbm2dSeeded)
    val soAt = s.indexOf("noise_seed_offsets(seed)")
    val loopAt = s.indexOf("for (")
    assert(soAt >= 0 && soAt < loopAt, s)
    assertEquals(s.split("noise_seed_offsets").length - 1, 1)
    assert(s.contains("(so.x + f32(i)) % 289.0"), s)

  test("fbm returns the normalized sum"):
    assert(src(Simplex.wgsl.fbm3d).contains("return sum / total;"))

  test("seed offsets hash the f32 bits with hash1i"):
    val s = src(NoiseCommon.seedOffsets)
    assert(s.contains("hash1i(bitcast<u32>(seed))"), s)
    assert(s.contains("h % 289u"), s)

  test("Simplex wrappers pick the unseeded or seeded fn at build time"):
    val uv = vec2(0.1, 0.2)
    val plain = Simplex.fbm2d(uv).toString
    assert(plain.startsWith("simplex_fbm_2d("), plain)
    assert(plain.contains(", 4, 2.0, 0.5)"), plain)
    val seeded = Simplex.fbm2d(uv, octaves = 3, seed = 7.0).toString
    assert(seeded.startsWith("simplex_fbm_2d_seeded("), seeded)
    assert(seeded.contains(", 3, 2.0, 0.5, 7.0)"), seeded)
    val runtime = Simplex.fbm2d(uv, octaves = IntExpr("u.oct"), gain = FloatExpr("u.gain")).toString
    assert(runtime.contains("u.oct"), runtime)
    assert(runtime.contains("u.gain"), runtime)

  test("fbm dep chain reaches the permute helpers, in dependency order"):
    val prog = Program[EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple]()
    prog.fn(Simplex.wgsl.fbm2d)
    val s = prog.helperFnsStr
    assert(s.contains("fn noise_permute_3"), s)
    assert(s.indexOf("fn noise_mod289_3") < s.indexOf("fn noise_permute_3"), s)
    assert(s.indexOf("fn noise_permute_3") < s.indexOf("fn simplex_noise_2d"), s)
    assert(s.indexOf("fn simplex_noise_2d") < s.indexOf("fn simplex_fbm_2d"), s)

  // ---------------------------------------------------------------------------
  // Extended — psrdnoise's upstream variant set
  // ---------------------------------------------------------------------------

  test("extended mirrors the upstream 8 variants per dimension"):
    val names2 = Seq[WgslFnData](
      Extended.wgsl.psrnoise2,
      Extended.wgsl.psnoise2,
      Extended.wgsl.srnoise2,
      Extended.wgsl.snoise2,
      Extended.wgsl.psrdnoise2,
      Extended.wgsl.psdnoise2,
      Extended.wgsl.srdnoise2,
      Extended.wgsl.sdnoise2,
    ).map(_.name)
    assertEquals(
      names2,
      Seq("psr", "ps", "sr", "s", "psrd", "psd", "srd", "sd").map(v => s"extended_${v}noise_2"),
    )
    val names3 = Seq[WgslFnData](
      Extended.wgsl.psrnoise3,
      Extended.wgsl.psnoise3,
      Extended.wgsl.srnoise3,
      Extended.wgsl.snoise3,
      Extended.wgsl.psrdnoise3,
      Extended.wgsl.psdnoise3,
      Extended.wgsl.srdnoise3,
      Extended.wgsl.sdnoise3,
    ).map(_.name)
    assertEquals(
      names3,
      Seq("psr", "ps", "sr", "s", "psrd", "psd", "srd", "sd").map(v => s"extended_${v}noise_3"),
    )

  test("2D ps / s variants forward with alpha = 0, as upstream"):
    assert(src(Extended.wgsl.psnoise2).contains("return extended_psrnoise_2(x, p, 0.0);"))
    assert(src(Extended.wgsl.sdnoise2).contains("return extended_srdnoise_2(x, 0.0);"))

  test("3D variants drop the period block and the rotation branch independently"):
    val s3 = src(Extended.wgsl.snoise3)
    assert(!s3.contains("any(p >"), s3)
    assert(!s3.contains("alpha"), s3)
    val ps3 = src(Extended.wgsl.psnoise3)
    assert(ps3.contains("any(p > vec3<f32>(0.0))"), ps3)
    assert(!ps3.contains("alpha"), ps3)
    val sr3 = src(Extended.wgsl.srnoise3)
    assert(!sr3.contains("any(p >"), sr3)
    assert(sr3.contains("if (alpha != 0.0)"), sr3)

  test("gradient variants return (value, gradient) vectors"):
    assert(src(Extended.wgsl.psrdnoise2).contains("-> vec3<f32>"))
    assert(src(Extended.wgsl.psrdnoise2).contains("return vec3<f32>(n, g);"))
    assert(src(Extended.wgsl.psrdnoise3).contains("-> vec4<f32>"))
    assert(src(Extended.wgsl.psrdnoise3).contains("return vec4<f32>(n, g);"))

  test("upstream constants: 10.9 / 0.8 in 2D, 39.5 / 0.5 in 3D"):
    val s2 = src(Extended.wgsl.srnoise2)
    assert(s2.contains("0.8 - vec3<f32>"), s2)
    assert(s2.contains("return 10.9 * n;"), s2)
    val s3 = src(Extended.wgsl.snoise3)
    assert(s3.contains("0.5 - vec4<f32>"), s3)
    assert(s3.contains("return 39.5 * n;"), s3)

  test("seeded extended adds the seed stage after the hash (tiling preserved)"):
    val s = src(Extended.wgsl.psrdnoise3So)
    val hashAt = s.indexOf("var hash = extended_permute_4")
    val seedAt = s.indexOf("hash = extended_permute_4(extended_permute_4(hash + so.x) + so.y);")
    val wrapAt = s.indexOf("any(p > vec3<f32>(0.0))")
    assert(wrapAt >= 0 && wrapAt < hashAt && hashAt < seedAt, s)

  test("gradient fbm scales each octave's gradient by its frequency"):
    val s = src(Extended.wgsl.sdfbm3)
    assert(s.contains("ng.yzw * frequency"), s)
    assert(s.contains("return sum / total;"), s)

  test("Extended wrappers select the variant from the options present"):
    val p = vec3(0.1, 0.2, 0.3)
    assert(Extended.noise3d(p).toString.startsWith("extended_sdnoise_3("))
    assert(Extended.noise3d(p, tilingPeriod = vec3(8.0, 0.0, 8.0)).toString.startsWith("extended_psdnoise_3("))
    val rot = Extended.noise3d(p, rot = 0.25.toExpr).toString
    assert(rot.startsWith("extended_srdnoise_3("), rot)
    assert(rot.contains("6.283185307179586"), rot)
    assert(Extended.noiseValue3d(p, seed = 3.0).toString.startsWith("extended_snoise_3_seeded("))
    assert(
      Extended.fbmValue2d(vec2(0.1, 0.2), tilingPeriod = vec2(4.0, 4.0)).toString
        .startsWith("extended_psfbm_2("),
    )

  // ---------------------------------------------------------------------------
  // Worley
  // ---------------------------------------------------------------------------

  test("worley 2D / 3D follow cellular2D / cellular3D"):
    val s2 = src(Worley.wgsl.noise2d)
    assert(s2.contains("fn worley_noise_2d(P: vec2<f32>, jitter: f32) -> vec2<f32>"), s2)
    assert(s2.contains("return sqrt(d1.xy);"), s2)
    val s3 = src(Worley.wgsl.noise3d)
    assert(s3.contains("fn worley_noise_3d(P: vec3<f32>, jitter: f32) -> vec2<f32>"), s3)
    assert(s3.contains("let K2 = 0.020408163265306;"), s3)
    assert(s3.contains("let p33 = noise_permute_3(p3 + Pi.z + 1.0);"), s3)
    assert(s3.contains("return sqrt(d11.xy);"), s3)

  test("Worley wrapper routes the seed"):
    assert(Worley.noise3d(vec3(0.1, 0.2, 0.3)).toString.startsWith("worley_noise_3d("))
    assert(
      Worley.noise2d(vec2(0.1, 0.2), seed = 1.0).toString.startsWith("worley_noise_2d_seeded("),
    )

  // ---------------------------------------------------------------------------
  // withDeps only accepts the wgsl layer
  // ---------------------------------------------------------------------------

  test("withDeps rejects wrapper defs at compile time"):
    val myFn: WgslFn[(p: Vec2), Float] =
      WgslFn.raw("my_fn")("  return simplex_fbm_2d(p, 4, 2.0, 0.5);")
    val ok = myFn.withDeps(Simplex.wgsl.fbm2d)
    assert(ok.asInstanceOf[WgslFnData].deps.length == 1)
    val etaErrors = compiletime.testing.typeCheckErrors("myFn.withDeps(Simplex.fbm2d)")
    assert(etaErrors.nonEmpty)
    val callErrors =
      compiletime.testing.typeCheckErrors("myFn.withDeps(Simplex.fbm2d(vec2(0.0, 0.0)))")
    assert(callErrors.nonEmpty)
