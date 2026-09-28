package trivalibs.graphics.shader.lib.noise

// Cellular ("Worley") noise ported from stegu/webgl-noise (Stefan Gustavson;
// MIT): https://github.com/stegu/webgl-noise src/cellular2D.glsl (3×3 search)
// and src/cellular3D.glsl (3×3×3 search, good F2 everywhere). Upstream's
// `#define jitter` is a parameter here. GLSL multi-component swizzle
// assignments become `select` / vector rebuilds (WGSL can only assign single
// components). The seeded twins add the two seed permute stages after the
// first permute, which re-hashes every cell.
//
// The upstream fast variants cellular2x2.glsl / cellular2x2x2.glsl are not
// ported: their F2 "is often wrong and has sharp discontinuities".

import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given
import trivalibs.utils.js.Arr

/** Cellular noise on the GPU: `(F1, F2)`, the distances to the nearest and
  * second-nearest feature point. `jitter` in `[0, 1]` moves the feature
  * points off the cell centers (lower = more regular). An optional `seed`
  * selects an unrelated pattern.
  *
  * The CPU mirror is `trivalibs.graphics.lib.noise.Worley`; the shared
  * extension `p.worleyNoise` covers both.
  */
object Worley:

  def noise2d(
      pos: Vec2Expr,
      jitter: FloatExpr = 1.0.toExpr,
      seed: FloatExpr | Null = null,
  ): Vec2Expr =
    if seed == null then wgsl.noise2d(pos, jitter)
    else wgsl.noise2dSeeded(pos, jitter, seed.asInstanceOf[FloatExpr])

  def noise3d(
      pos: Vec3Expr,
      jitter: FloatExpr = 1.0.toExpr,
      seed: FloatExpr | Null = null,
  ): Vec2Expr =
    if seed == null then wgsl.noise3d(pos, jitter)
    else wgsl.noise3dSeeded(pos, jitter, seed.asInstanceOf[FloatExpr])

  /** The WgslFn definitions — the layer for `.withDeps` and raw WGSL
    * composition (`noise3dSeeded` emits `worley_noise_3d_seeded`).
    */
  object wgsl:
    import NoiseCommon.*

    private def body2(seeded: Boolean): String =
      val seed =
        if seeded then "  px = noise_permute_3(noise_permute_3(px + so.x) + so.y);\n" else ""
      s"""  let K = 0.142857142857;
  let Ko = 0.428571428571;
  let Pi = noise_mod289_2(floor(P));
  let Pf = fract(P);
  let oi = vec3<f32>(-1.0, 0.0, 1.0);
  let of_ = vec3<f32>(-0.5, 0.5, 1.5);
  var px = noise_permute_3(Pi.x + oi);
${seed}  var p = noise_permute_3(px.x + Pi.y + oi);
  var ox = fract(p * K) - Ko;
  var oy = noise_mod7_3(floor(p * K)) * K - Ko;
  var dx = Pf.x + 0.5 + jitter * ox;
  var dy = Pf.y - of_ + jitter * oy;
  var d1 = dx * dx + dy * dy;
  p = noise_permute_3(px.y + Pi.y + oi);
  ox = fract(p * K) - Ko;
  oy = noise_mod7_3(floor(p * K)) * K - Ko;
  dx = Pf.x - 0.5 + jitter * ox;
  dy = Pf.y - of_ + jitter * oy;
  var d2 = dx * dx + dy * dy;
  p = noise_permute_3(px.z + Pi.y + oi);
  ox = fract(p * K) - Ko;
  oy = noise_mod7_3(floor(p * K)) * K - Ko;
  dx = Pf.x - 1.5 + jitter * ox;
  dy = Pf.y - of_ + jitter * oy;
  let d3 = dx * dx + dy * dy;
  let d1a = min(d1, d2);
  d2 = max(d1, d2);
  d2 = min(d2, d3);
  d1 = min(d1a, d2);
  d2 = max(d1a, d2);
  d1 = select(vec3<f32>(d1.y, d1.x, d1.z), d1, d1.x < d1.y);
  d1 = select(vec3<f32>(d1.z, d1.y, d1.x), d1, d1.x < d1.z);
  d1 = vec3<f32>(d1.x, min(d1.yz, d2.yz));
  d1.y = min(d1.y, d1.z);
  d1.y = min(d1.y, d2.x);
  return sqrt(d1.xy);"""

    private def body3(seeded: Boolean): String =
      val rows = Arr("1", "2", "3")
      val offs = Arr(" - 1.0", "", " + 1.0")
      val comps = Arr("x", "y", "z")
      val sb = Arr[String]()
      sb.push("""  let K = 0.142857142857;
  let Ko = 0.428571428571;
  let K2 = 0.020408163265306;
  let Kz = 0.166666666667;
  let Kzo = 0.416666666667;
  let Pi = noise_mod289_3(floor(P));
  let Pf = fract(P) - 0.5;
  let Pfx = Pf.x + vec3<f32>(1.0, 0.0, -1.0);
  let Pfy = Pf.y + vec3<f32>(1.0, 0.0, -1.0);
  let Pfz = Pf.z + vec3<f32>(1.0, 0.0, -1.0);
  var p = noise_permute_3(Pi.x + vec3<f32>(-1.0, 0.0, 1.0));
""")
      if seeded then sb.push("  p = noise_permute_3(noise_permute_3(p + so.x) + so.y);\n")
      var a = 0
      while a < 3 do
        sb.push(s"  let p${rows(a)} = noise_permute_3(p + Pi.y${offs(a)});\n")
        a += 1
      a = 0
      while a < 3 do
        var b = 0
        while b < 3 do
          val ab = rows(a) + rows(b)
          sb.push(s"  let p$ab = noise_permute_3(p${rows(a)} + Pi.z${offs(b)});\n")
          b += 1
        a += 1
      a = 0
      while a < 3 do
        var b = 0
        while b < 3 do
          val ab = rows(a) + rows(b)
          sb.push(s"  let ox$ab = fract(p$ab * K) - Ko;\n")
          sb.push(s"  let oy$ab = noise_mod7_3(floor(p$ab * K)) * K - Ko;\n")
          sb.push(s"  let oz$ab = floor(p$ab * K2) * Kz - Kzo;\n")
          b += 1
        a += 1
      a = 0
      while a < 3 do
        var b = 0
        while b < 3 do
          val ab = rows(a) + rows(b)
          sb.push(s"  let dx$ab = Pfx + jitter * ox$ab;\n")
          sb.push(s"  let dy$ab = Pfy.${comps(a)} + jitter * oy$ab;\n")
          sb.push(s"  let dz$ab = Pfz.${comps(b)} + jitter * oz$ab;\n")
          b += 1
        a += 1
      a = 0
      while a < 3 do
        var b = 0
        while b < 3 do
          val ab = rows(a) + rows(b)
          sb.push(s"  var d$ab = dx$ab * dx$ab + dy$ab * dy$ab + dz$ab * dz$ab;\n")
          b += 1
        a += 1
      sb.push("""  let d1a = min(d11, d12);
  d12 = max(d11, d12);
  d11 = min(d1a, d13);
  d13 = max(d1a, d13);
  d12 = min(d12, d13);
  let d2a = min(d21, d22);
  d22 = max(d21, d22);
  d21 = min(d2a, d23);
  d23 = max(d2a, d23);
  d22 = min(d22, d23);
  let d3a = min(d31, d32);
  d32 = max(d31, d32);
  d31 = min(d3a, d33);
  d33 = max(d3a, d33);
  d32 = min(d32, d33);
  let da = min(d11, d21);
  d21 = max(d11, d21);
  d11 = min(da, d31);
  d31 = max(da, d31);
  d11 = select(vec3<f32>(d11.y, d11.x, d11.z), d11, d11.x < d11.y);
  d11 = select(vec3<f32>(d11.z, d11.y, d11.x), d11, d11.x < d11.z);
  d12 = min(d12, d21);
  d12 = min(d12, d22);
  d12 = min(d12, d31);
  d12 = min(d12, d32);
  d11 = vec3<f32>(d11.x, min(d11.yz, d12.xy));
  d11.y = min(d11.y, d12.z);
  d11.y = min(d11.y, d11.z);
  return sqrt(d11.xy);""")
      sb.join("")

    lazy val noise2d: WgslFn[(P: Vec2, jitter: Float), Vec2] =
      WgslFn
        .raw("worley_noise_2d")(body2(false))
        .withDeps(mod289v2, mod7v3, permute3)

    private[lib] lazy val noise2dSo: WgslFn[(P: Vec2, jitter: Float, so: Vec2), Vec2] =
      WgslFn
        .raw("worley_noise_2d_so")(body2(true))
        .withDeps(mod289v2, mod7v3, permute3)

    lazy val noise2dSeeded: WgslFn[(P: Vec2, jitter: Float, seed: Float), Vec2] =
      WgslFn
        .raw("worley_noise_2d_seeded")(
          "  return worley_noise_2d_so(P, jitter, noise_seed_offsets(seed));",
        )
        .withDeps(noise2dSo, seedOffsets)

    lazy val noise3d: WgslFn[(P: Vec3, jitter: Float), Vec2] =
      WgslFn
        .raw("worley_noise_3d")(body3(false))
        .withDeps(mod289v3, mod7v3, permute3)

    private[lib] lazy val noise3dSo: WgslFn[(P: Vec3, jitter: Float, so: Vec2), Vec2] =
      WgslFn
        .raw("worley_noise_3d_so")(body3(true))
        .withDeps(mod289v3, mod7v3, permute3)

    lazy val noise3dSeeded: WgslFn[(P: Vec3, jitter: Float, seed: Float), Vec2] =
      WgslFn
        .raw("worley_noise_3d_seeded")(
          "  return worley_noise_3d_so(P, jitter, noise_seed_offsets(seed));",
        )
        .withDeps(noise3dSo, seedOffsets)
