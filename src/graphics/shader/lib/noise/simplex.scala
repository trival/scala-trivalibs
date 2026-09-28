package trivalibs.graphics.shader.lib.noise

// Simplex noise ported from stegu/webgl-noise (Ashima Arts / Ian McEwan,
// Stefan Gustavson; MIT): https://github.com/stegu/webgl-noise
// src/noise2D.glsl, noise3D.glsl, noise4D.glsl. The WGSL bodies follow the
// upstream statement for statement; the seeded twins add only the two seed
// permute stages right after the lattice hash.

import trivalibs.graphics.lib.IntArg
import trivalibs.graphics.lib.toIntExpr
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given

/** Simplex noise (value only), 2D / 3D / 4D, on the GPU.
  *
  * Outputs are nominally in `[-1, 1]`; the fbms are normalized (the
  * amplitude-weighted average of the octaves) and stay strictly inside it. An
  * optional `seed` selects an unrelated field per seed value; `null` uses the
  * unseeded upstream code path unchanged.
  *
  * The CPU mirror is `trivalibs.graphics.lib.noise.Simplex`; the shared
  * extensions (`p.simplexNoise`, `p.simplexFbm`, …) cover both.
  */
object Simplex:

  /** Noise value at `pos`, in about `[-1, 1]`. */
  def noise2d(pos: Vec2Expr, seed: FloatExpr | Null = null): FloatExpr =
    if seed == null then wgsl.noise2d(pos)
    else wgsl.noise2dSeeded(pos, seed.asInstanceOf[FloatExpr])

  /** Noise value at `pos`, in about `[-1, 1]`. */
  def noise3d(pos: Vec3Expr, seed: FloatExpr | Null = null): FloatExpr =
    if seed == null then wgsl.noise3d(pos)
    else wgsl.noise3dSeeded(pos, seed.asInstanceOf[FloatExpr])

  /** Noise value at `pos`, in about `[-1, 1]`. */
  def noise4d(pos: Vec4Expr, seed: FloatExpr | Null = null): FloatExpr =
    if seed == null then wgsl.noise4d(pos)
    else wgsl.noise4dSeeded(pos, seed.asInstanceOf[FloatExpr])

  /** Fractal Brownian motion: `octaves` layers, each at `lacunarity`× the
    * previous frequency and `gain`× the amplitude. Returns the
    * amplitude-weighted average, strictly in `[-1, 1]`.
    */
  def fbm2d(
      pos: Vec2Expr,
      octaves: IntArg = 4,
      lacunarity: FloatExpr = 2.0.toExpr,
      gain: FloatExpr = 0.5.toExpr,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    if seed == null then wgsl.fbm2d(pos, toIntExpr(octaves), lacunarity, gain)
    else
      wgsl.fbm2dSeeded(
        pos,
        toIntExpr(octaves),
        lacunarity,
        gain,
        seed.asInstanceOf[FloatExpr],
      )

  /** 3D [[fbm2d]]. */
  def fbm3d(
      pos: Vec3Expr,
      octaves: IntArg = 4,
      lacunarity: FloatExpr = 2.0.toExpr,
      gain: FloatExpr = 0.5.toExpr,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    if seed == null then wgsl.fbm3d(pos, toIntExpr(octaves), lacunarity, gain)
    else
      wgsl.fbm3dSeeded(
        pos,
        toIntExpr(octaves),
        lacunarity,
        gain,
        seed.asInstanceOf[FloatExpr],
      )

  /** 4D [[fbm2d]]. */
  def fbm4d(
      pos: Vec4Expr,
      octaves: IntArg = 4,
      lacunarity: FloatExpr = 2.0.toExpr,
      gain: FloatExpr = 0.5.toExpr,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    if seed == null then wgsl.fbm4d(pos, toIntExpr(octaves), lacunarity, gain)
    else
      wgsl.fbm4dSeeded(
        pos,
        toIntExpr(octaves),
        lacunarity,
        gain,
        seed.asInstanceOf[FloatExpr],
      )

  /** Seamlessly tiling 2D noise: 4D simplex sampled on a torus. `pos` in
    * `[0, 1]` covers one tile; `scale` sets the frequency.
    */
  def torusNoise2d(
      pos: Vec2Expr,
      scale: FloatExpr,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    if seed == null then wgsl.torusNoise2d(pos, scale)
    else wgsl.torusNoise2dSeeded(pos, scale, seed.asInstanceOf[FloatExpr])

  /** The WgslFn definitions, one per code path, with every argument explicit.
    *
    * This is the layer for `.withDeps` and raw WGSL composition: a raw WGSL
    * string calls a fn by name, so depend on exactly the variant you call (the
    * WGSL name is `simplex_` + the snake-cased member name, e.g.
    * `fbm2dSeeded` → `simplex_fbm_2d_seeded`). Shader DSL code uses the
    * wrappers above or the extensions, which register their deps themselves.
    */
  object wgsl:
    import NoiseCommon.*

    private def body2(seeded: Boolean): String =
      s"""  let C = vec4<f32>(0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439);
  var i = floor(v + dot(v, C.yy));
  let x0 = v - i + dot(i, C.xx);
  let i1 = select(vec2<f32>(0.0, 1.0), vec2<f32>(1.0, 0.0), x0.x > x0.y);
  var x12 = x0.xyxy + C.xxzz;
  x12 = vec4<f32>(x12.xy - i1, x12.zw);
  i = noise_mod289_2(i);
  var p = noise_permute_3(noise_permute_3(i.y + vec3<f32>(0.0, i1.y, 1.0)) + i.x + vec3<f32>(0.0, i1.x, 1.0));
${seedStage(seeded, "p", "noise_permute_3")}  var m = max(0.5 - vec3<f32>(dot(x0, x0), dot(x12.xy, x12.xy), dot(x12.zw, x12.zw)), vec3<f32>(0.0));
  m = m * m;
  m = m * m;
  let x = 2.0 * fract(p * C.www) - 1.0;
  let h = abs(x) - 0.5;
  let ox = floor(x + 0.5);
  let a0 = x - ox;
  m = m * (1.79284291400159 - 0.85373472095314 * (a0 * a0 + h * h));
  let g = vec3<f32>(a0.x * x0.x + h.x * x0.y, a0.yz * x12.xz + h.yz * x12.yw);
  return 130.0 * dot(m, g);"""

    private def body3(seeded: Boolean): String =
      s"""  let C = vec2<f32>(1.0 / 6.0, 1.0 / 3.0);
  let D = vec4<f32>(0.0, 0.5, 1.0, 2.0);
  var i = floor(v + dot(v, C.yyy));
  let x0 = v - i + dot(i, C.xxx);
  let g = step(x0.yzx, x0.xyz);
  let l = 1.0 - g;
  let i1 = min(g.xyz, l.zxy);
  let i2 = max(g.xyz, l.zxy);
  let x1 = x0 - i1 + C.xxx;
  let x2 = x0 - i2 + C.yyy;
  let x3 = x0 - D.yyy;
  i = noise_mod289_3(i);
  var p = noise_permute_4(noise_permute_4(noise_permute_4(i.z + vec4<f32>(0.0, i1.z, i2.z, 1.0)) + i.y + vec4<f32>(0.0, i1.y, i2.y, 1.0)) + i.x + vec4<f32>(0.0, i1.x, i2.x, 1.0));
${seedStage(seeded, "p", "noise_permute_4")}  let n_ = 0.142857142857;
  let ns = n_ * D.wyz - D.xzx;
  let j = p - 49.0 * floor(p * ns.z * ns.z);
  let x_ = floor(j * ns.z);
  let y_ = floor(j - 7.0 * x_);
  let x = x_ * ns.x + ns.yyyy;
  let y = y_ * ns.x + ns.yyyy;
  let h = 1.0 - abs(x) - abs(y);
  let b0 = vec4<f32>(x.xy, y.xy);
  let b1 = vec4<f32>(x.zw, y.zw);
  let s0 = floor(b0) * 2.0 + 1.0;
  let s1 = floor(b1) * 2.0 + 1.0;
  let sh = -step(h, vec4<f32>(0.0));
  let a0 = b0.xzyw + s0.xzyw * sh.xxyy;
  let a1 = b1.xzyw + s1.xzyw * sh.zzww;
  var p0 = vec3<f32>(a0.xy, h.x);
  var p1 = vec3<f32>(a0.zw, h.y);
  var p2 = vec3<f32>(a1.xy, h.z);
  var p3 = vec3<f32>(a1.zw, h.w);
  let norm = noise_taylor_inv_sqrt_4(vec4<f32>(dot(p0, p0), dot(p1, p1), dot(p2, p2), dot(p3, p3)));
  p0 = p0 * norm.x;
  p1 = p1 * norm.y;
  p2 = p2 * norm.z;
  p3 = p3 * norm.w;
  var m = max(0.5 - vec4<f32>(dot(x0, x0), dot(x1, x1), dot(x2, x2), dot(x3, x3)), vec4<f32>(0.0));
  m = m * m;
  return 105.0 * dot(m * m, vec4<f32>(dot(p0, x0), dot(p1, x1), dot(p2, x2), dot(p3, x3)));"""

    private def body4(seeded: Boolean): String =
      val seed =
        if seeded then
          """  j0 = noise_permute_1(noise_permute_1(j0 + so.x) + so.y);
  j1 = noise_permute_4(noise_permute_4(j1 + so.x) + so.y);
"""
        else ""
      s"""  let C = vec4<f32>(0.138196601125011, 0.276393202250021, 0.414589803375032, -0.447213595499958);
  let F4 = 0.309016994374947451;
  var i = floor(v + dot(v, vec4<f32>(F4)));
  let x0 = v - i + dot(i, C.xxxx);
  let isX = step(x0.yzw, x0.xxx);
  let isYZ = step(x0.zww, x0.yyz);
  var i0 = vec4<f32>(isX.x + isX.y + isX.z, 1.0 - isX);
  i0.y = i0.y + isYZ.x + isYZ.y;
  i0.z = i0.z + 1.0 - isYZ.x;
  i0.w = i0.w + 1.0 - isYZ.y;
  i0.z = i0.z + isYZ.z;
  i0.w = i0.w + 1.0 - isYZ.z;
  let i3 = clamp(i0, vec4<f32>(0.0), vec4<f32>(1.0));
  let i2 = clamp(i0 - 1.0, vec4<f32>(0.0), vec4<f32>(1.0));
  let i1 = clamp(i0 - 2.0, vec4<f32>(0.0), vec4<f32>(1.0));
  let x1 = x0 - i1 + C.xxxx;
  let x2 = x0 - i2 + C.yyyy;
  let x3 = x0 - i3 + C.zzzz;
  let x4 = x0 + C.wwww;
  i = noise_mod289_4(i);
  var j0 = noise_permute_1(noise_permute_1(noise_permute_1(noise_permute_1(i.w) + i.z) + i.y) + i.x);
  var j1 = noise_permute_4(noise_permute_4(noise_permute_4(noise_permute_4(i.w + vec4<f32>(i1.w, i2.w, i3.w, 1.0)) + i.z + vec4<f32>(i1.z, i2.z, i3.z, 1.0)) + i.y + vec4<f32>(i1.y, i2.y, i3.y, 1.0)) + i.x + vec4<f32>(i1.x, i2.x, i3.x, 1.0));
${seed}  let ip = vec4<f32>(1.0 / 294.0, 1.0 / 49.0, 1.0 / 7.0, 0.0);
  var p0 = simplex_grad4(j0, ip);
  var p1 = simplex_grad4(j1.x, ip);
  var p2 = simplex_grad4(j1.y, ip);
  var p3 = simplex_grad4(j1.z, ip);
  var p4 = simplex_grad4(j1.w, ip);
  let norm = noise_taylor_inv_sqrt_4(vec4<f32>(dot(p0, p0), dot(p1, p1), dot(p2, p2), dot(p3, p3)));
  p0 = p0 * norm.x;
  p1 = p1 * norm.y;
  p2 = p2 * norm.z;
  p3 = p3 * norm.w;
  p4 = p4 * noise_taylor_inv_sqrt_1(dot(p4, p4));
  var m0 = max(0.57 - vec3<f32>(dot(x0, x0), dot(x1, x1), dot(x2, x2)), vec3<f32>(0.0));
  var m1 = max(0.57 - vec2<f32>(dot(x3, x3), dot(x4, x4)), vec2<f32>(0.0));
  m0 = m0 * m0;
  m1 = m1 * m1;
  return 60.1 * (dot(m0 * m0, vec3<f32>(dot(p0, x0), dot(p1, x1), dot(p2, x2))) + dot(m1 * m1, vec2<f32>(dot(p3, x3), dot(p4, x4))));"""

    private def seedStage(seeded: Boolean, v: String, permute: String): String =
      if seeded then s"  $v = $permute($permute($v + so.x) + so.y);\n" else ""

    private def fbmBody(noise: String, seeded: Boolean): String =
      val sample =
        if seeded then
          s"$noise(v * frequency, vec2<f32>((so.x + f32(i)) % 289.0, so.y))"
        else s"$noise(v * frequency)"
      val soLine = if seeded then "  let so = noise_seed_offsets(seed);\n" else ""
      s"""${soLine}  var sum = 0.0;
  var amplitude = 1.0;
  var total = 0.0;
  var frequency = 1.0;
  for (var i = 0; i < octaves; i += 1) {
    sum += $sample * amplitude;
    total += amplitude;
    amplitude *= gain;
    frequency *= lacunarity;
  }
  return sum / total;"""

    // ---- 2D ----

    lazy val noise2d: WgslFn[(v: Vec2), Float] =
      WgslFn.raw("simplex_noise_2d")(body2(false)).withDeps(mod289v2, permute3)

    private[lib] lazy val noise2dSo: WgslFn[(v: Vec2, so: Vec2), Float] =
      WgslFn.raw("simplex_noise_2d_so")(body2(true)).withDeps(mod289v2, permute3)

    lazy val noise2dSeeded: WgslFn[(v: Vec2, seed: Float), Float] =
      WgslFn
        .raw("simplex_noise_2d_seeded")(
          "  return simplex_noise_2d_so(v, noise_seed_offsets(seed));",
        )
        .withDeps(noise2dSo, seedOffsets)

    // ---- 3D ----

    lazy val noise3d: WgslFn[(v: Vec3), Float] =
      WgslFn
        .raw("simplex_noise_3d")(body3(false))
        .withDeps(mod289v3, permute4, taylorInvSqrt4)

    private[lib] lazy val noise3dSo: WgslFn[(v: Vec3, so: Vec2), Float] =
      WgslFn
        .raw("simplex_noise_3d_so")(body3(true))
        .withDeps(mod289v3, permute4, taylorInvSqrt4)

    lazy val noise3dSeeded: WgslFn[(v: Vec3, seed: Float), Float] =
      WgslFn
        .raw("simplex_noise_3d_seeded")(
          "  return simplex_noise_3d_so(v, noise_seed_offsets(seed));",
        )
        .withDeps(noise3dSo, seedOffsets)

    // ---- 4D ----

    private lazy val grad4: WgslFn[(j: Float, ip: Vec4), Vec4] =
      WgslFn.raw("simplex_grad4")("""  let pxyz = floor(fract(vec3<f32>(j) * ip.xyz) * 7.0) * ip.z - 1.0;
  var p = vec4<f32>(pxyz, 1.5 - dot(abs(pxyz), vec3<f32>(1.0)));
  let s = select(vec4<f32>(0.0), vec4<f32>(1.0), p < vec4<f32>(0.0));
  p = vec4<f32>(p.xyz + (s.xyz * 2.0 - 1.0) * s.www, p.w);
  return p;""")

    lazy val noise4d: WgslFn[(v: Vec4), Float] =
      WgslFn
        .raw("simplex_noise_4d")(body4(false))
        .withDeps(mod289v4, permute1, permute4, taylorInvSqrt1, taylorInvSqrt4, grad4)

    private[lib] lazy val noise4dSo: WgslFn[(v: Vec4, so: Vec2), Float] =
      WgslFn
        .raw("simplex_noise_4d_so")(body4(true))
        .withDeps(mod289v4, permute1, permute4, taylorInvSqrt1, taylorInvSqrt4, grad4)

    lazy val noise4dSeeded: WgslFn[(v: Vec4, seed: Float), Float] =
      WgslFn
        .raw("simplex_noise_4d_seeded")(
          "  return simplex_noise_4d_so(v, noise_seed_offsets(seed));",
        )
        .withDeps(noise4dSo, seedOffsets)

    // ---- fbm ----

    lazy val fbm2d: WgslFn[
      (v: Vec2, octaves: Int, lacunarity: Float, gain: Float),
      Float,
    ] =
      WgslFn
        .raw("simplex_fbm_2d")(fbmBody("simplex_noise_2d", false))
        .withDeps(noise2d)

    lazy val fbm2dSeeded: WgslFn[
      (v: Vec2, octaves: Int, lacunarity: Float, gain: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("simplex_fbm_2d_seeded")(fbmBody("simplex_noise_2d_so", true))
        .withDeps(noise2dSo, seedOffsets)

    lazy val fbm3d: WgslFn[
      (v: Vec3, octaves: Int, lacunarity: Float, gain: Float),
      Float,
    ] =
      WgslFn
        .raw("simplex_fbm_3d")(fbmBody("simplex_noise_3d", false))
        .withDeps(noise3d)

    lazy val fbm3dSeeded: WgslFn[
      (v: Vec3, octaves: Int, lacunarity: Float, gain: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("simplex_fbm_3d_seeded")(fbmBody("simplex_noise_3d_so", true))
        .withDeps(noise3dSo, seedOffsets)

    lazy val fbm4d: WgslFn[
      (v: Vec4, octaves: Int, lacunarity: Float, gain: Float),
      Float,
    ] =
      WgslFn
        .raw("simplex_fbm_4d")(fbmBody("simplex_noise_4d", false))
        .withDeps(noise4d)

    lazy val fbm4dSeeded: WgslFn[
      (v: Vec4, octaves: Int, lacunarity: Float, gain: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("simplex_fbm_4d_seeded")(fbmBody("simplex_noise_4d_so", true))
        .withDeps(noise4dSo, seedOffsets)

    // ---- torus ----

    private val torusMapping = """  let angle_x = pos.x * 6.28318530718;
  let angle_y = pos.y * 6.28318530718;
  let torus = vec4<f32>(cos(angle_x), sin(angle_x), cos(angle_y), sin(angle_y)) * scale;
"""

    lazy val torusNoise2d: WgslFn[(pos: Vec2, scale: Float), Float] =
      WgslFn
        .raw("simplex_torus_noise_2d")(torusMapping + "  return simplex_noise_4d(torus);")
        .withDeps(noise4d)

    lazy val torusNoise2dSeeded: WgslFn[(pos: Vec2, scale: Float, seed: Float), Float] =
      WgslFn
        .raw("simplex_torus_noise_2d_seeded")(
          torusMapping + "  return simplex_noise_4d_seeded(torus, seed);",
        )
        .withDeps(noise4dSeeded)
