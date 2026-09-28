package trivalibs.graphics.shader.lib.noise

// Extended noise: psrdnoise by Stefan Gustavson and Ian McEwan (MIT),
// https://github.com/stegu/psrdnoise — src/psrdnoise2.wgsl, psrdnoise3.wgsl
// (version 2022-02-28). Tiling simplex noise with rotating gradients and
// analytic derivatives, 2D and 3D (upstream has no 4D).
//
// The WGSL mirrors the upstream variant set 1:1: one fn per argument list,
// named after it — p(eriodic) s(implex) r(otating) d(erivative) — because
// upstream does not trust WGSL dead-code removal either. The bodies are built
// from shared fragments of the upstream code, statement for statement; the
// 2D `ps`/`s` variants forward to `psr`/`sr` with `alpha = 0`, as upstream.
// Our additions: the seed permute stages (seeded twins), `vec3`/`vec4`
// returns instead of the `NG2`/`NG3` structs (value in `.x`, gradient in the
// rest) and the fbm loops.
//
// Not ported (yet), available upstream if ever needed:
//   - src/psrddnoise2.glsl / psrddnoise3.glsl: also return the second
//     derivatives (Hessian). GLSL only, no upstream WGSL.
//   - src/mpsrdnoise2.glsl: 2D variant safe for 16-bit (mediump / f16) floats.

import trivalibs.graphics.lib.IntArg
import trivalibs.graphics.lib.toIntExpr
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given

/** Extended noise (psrdnoise), 2D / 3D, on the GPU: simplex-type gradient
  * noise that also returns its analytic gradient, with optional tiling and
  * gradient rotation.
  *
  *   - `tilingPeriod`: the field repeats every `tilingPeriod` units per axis.
  *     Integer components up to 289; a zero component leaves that axis
  *     unwrapped. `null` = no tiling. In **2D the y period must be even** (the
  *     lattice is hexagonal): an odd y period tiles at twice its value, per
  *     upstream.
  *   - `rot`: gradient rotation in turns (`1.0` = 360°), for flow-noise
  *     animation. `null` = no rotation.
  *   - `seed`: an unrelated field per seed value, tiling preserved. `null` =
  *     unseeded.
  *
  * Each option left at `null` selects the cheaper upstream variant, so an
  * unused option costs nothing. `noise*` / `fbm*` return `vec3` (2D) / `vec4`
  * (3D) with the value in `.x` and the gradient in the rest; the `…Value`
  * members return the value only (cheaper). Values are in about `[-1, 1]`, the
  * fbms strictly.
  *
  * The CPU mirror is `trivalibs.graphics.lib.noise.Extended`; the shared
  * extensions (`p.extendedNoise`, `p.extendedFbm`, …) cover both.
  */
object Extended:

  private inline def nn[T](x: T | Null): T = x.asInstanceOf[T]

  private def alpha(rot: FloatExpr | Null): FloatExpr =
    nn(rot) * 6.283185307179586

  // ---- 2D ----

  /** Value + gradient (`vec3`) at `pos`. */
  def noise2d(
      pos: Vec2Expr,
      tilingPeriod: Vec2Expr | Null = null,
      rot: FloatExpr | Null = null,
      seed: FloatExpr | Null = null,
  ): Vec3Expr =
    val p = tilingPeriod != null
    val r = rot != null
    if seed == null then
      if p && r then wgsl.psrdnoise2(pos, nn(tilingPeriod), alpha(rot))
      else if p then wgsl.psdnoise2(pos, nn(tilingPeriod))
      else if r then wgsl.srdnoise2(pos, alpha(rot))
      else wgsl.sdnoise2(pos)
    else
      val s = nn(seed)
      if p && r then wgsl.psrdnoise2Seeded(pos, nn(tilingPeriod), alpha(rot), s)
      else if p then wgsl.psdnoise2Seeded(pos, nn(tilingPeriod), s)
      else if r then wgsl.srdnoise2Seeded(pos, alpha(rot), s)
      else wgsl.sdnoise2Seeded(pos, s)

  /** Value only at `pos`. */
  def noiseValue2d(
      pos: Vec2Expr,
      tilingPeriod: Vec2Expr | Null = null,
      rot: FloatExpr | Null = null,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    val p = tilingPeriod != null
    val r = rot != null
    if seed == null then
      if p && r then wgsl.psrnoise2(pos, nn(tilingPeriod), alpha(rot))
      else if p then wgsl.psnoise2(pos, nn(tilingPeriod))
      else if r then wgsl.srnoise2(pos, alpha(rot))
      else wgsl.snoise2(pos)
    else
      val s = nn(seed)
      if p && r then wgsl.psrnoise2Seeded(pos, nn(tilingPeriod), alpha(rot), s)
      else if p then wgsl.psnoise2Seeded(pos, nn(tilingPeriod), s)
      else if r then wgsl.srnoise2Seeded(pos, alpha(rot), s)
      else wgsl.snoise2Seeded(pos, s)

  /** Fractal Brownian motion, value + gradient (`vec3`). Octave `i` samples
    * `pos · lacunarityⁱ` with tiling period `tilingPeriod · lacunarityⁱ`, so
    * a tiling fbm needs a whole-number `lacunarity`. The value is the
    * amplitude-weighted average (strictly `[-1, 1]`), the gradient its exact
    * derivative.
    */
  def fbm2d(
      pos: Vec2Expr,
      octaves: IntArg = 4,
      lacunarity: FloatExpr = 2.0.toExpr,
      gain: FloatExpr = 0.5.toExpr,
      tilingPeriod: Vec2Expr | Null = null,
      rot: FloatExpr | Null = null,
      seed: FloatExpr | Null = null,
  ): Vec3Expr =
    val o = toIntExpr(octaves)
    val p = tilingPeriod != null
    val r = rot != null
    if seed == null then
      if p && r then wgsl.psrdfbm2(pos, o, lacunarity, gain, nn(tilingPeriod), alpha(rot))
      else if p then wgsl.psdfbm2(pos, o, lacunarity, gain, nn(tilingPeriod))
      else if r then wgsl.srdfbm2(pos, o, lacunarity, gain, alpha(rot))
      else wgsl.sdfbm2(pos, o, lacunarity, gain)
    else
      val s = nn(seed)
      if p && r then
        wgsl.psrdfbm2Seeded(pos, o, lacunarity, gain, nn(tilingPeriod), alpha(rot), s)
      else if p then wgsl.psdfbm2Seeded(pos, o, lacunarity, gain, nn(tilingPeriod), s)
      else if r then wgsl.srdfbm2Seeded(pos, o, lacunarity, gain, alpha(rot), s)
      else wgsl.sdfbm2Seeded(pos, o, lacunarity, gain, s)

  /** [[fbm2d]], value only. */
  def fbmValue2d(
      pos: Vec2Expr,
      octaves: IntArg = 4,
      lacunarity: FloatExpr = 2.0.toExpr,
      gain: FloatExpr = 0.5.toExpr,
      tilingPeriod: Vec2Expr | Null = null,
      rot: FloatExpr | Null = null,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    val o = toIntExpr(octaves)
    val p = tilingPeriod != null
    val r = rot != null
    if seed == null then
      if p && r then wgsl.psrfbm2(pos, o, lacunarity, gain, nn(tilingPeriod), alpha(rot))
      else if p then wgsl.psfbm2(pos, o, lacunarity, gain, nn(tilingPeriod))
      else if r then wgsl.srfbm2(pos, o, lacunarity, gain, alpha(rot))
      else wgsl.sfbm2(pos, o, lacunarity, gain)
    else
      val s = nn(seed)
      if p && r then
        wgsl.psrfbm2Seeded(pos, o, lacunarity, gain, nn(tilingPeriod), alpha(rot), s)
      else if p then wgsl.psfbm2Seeded(pos, o, lacunarity, gain, nn(tilingPeriod), s)
      else if r then wgsl.srfbm2Seeded(pos, o, lacunarity, gain, alpha(rot), s)
      else wgsl.sfbm2Seeded(pos, o, lacunarity, gain, s)

  // ---- 3D ----

  /** Value + gradient (`vec4`) at `pos`. */
  def noise3d(
      pos: Vec3Expr,
      tilingPeriod: Vec3Expr | Null = null,
      rot: FloatExpr | Null = null,
      seed: FloatExpr | Null = null,
  ): Vec4Expr =
    val p = tilingPeriod != null
    val r = rot != null
    if seed == null then
      if p && r then wgsl.psrdnoise3(pos, nn(tilingPeriod), alpha(rot))
      else if p then wgsl.psdnoise3(pos, nn(tilingPeriod))
      else if r then wgsl.srdnoise3(pos, alpha(rot))
      else wgsl.sdnoise3(pos)
    else
      val s = nn(seed)
      if p && r then wgsl.psrdnoise3Seeded(pos, nn(tilingPeriod), alpha(rot), s)
      else if p then wgsl.psdnoise3Seeded(pos, nn(tilingPeriod), s)
      else if r then wgsl.srdnoise3Seeded(pos, alpha(rot), s)
      else wgsl.sdnoise3Seeded(pos, s)

  /** Value only at `pos`. */
  def noiseValue3d(
      pos: Vec3Expr,
      tilingPeriod: Vec3Expr | Null = null,
      rot: FloatExpr | Null = null,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    val p = tilingPeriod != null
    val r = rot != null
    if seed == null then
      if p && r then wgsl.psrnoise3(pos, nn(tilingPeriod), alpha(rot))
      else if p then wgsl.psnoise3(pos, nn(tilingPeriod))
      else if r then wgsl.srnoise3(pos, alpha(rot))
      else wgsl.snoise3(pos)
    else
      val s = nn(seed)
      if p && r then wgsl.psrnoise3Seeded(pos, nn(tilingPeriod), alpha(rot), s)
      else if p then wgsl.psnoise3Seeded(pos, nn(tilingPeriod), s)
      else if r then wgsl.srnoise3Seeded(pos, alpha(rot), s)
      else wgsl.snoise3Seeded(pos, s)

  /** 3D [[fbm2d]], value + gradient (`vec4`). */
  def fbm3d(
      pos: Vec3Expr,
      octaves: IntArg = 4,
      lacunarity: FloatExpr = 2.0.toExpr,
      gain: FloatExpr = 0.5.toExpr,
      tilingPeriod: Vec3Expr | Null = null,
      rot: FloatExpr | Null = null,
      seed: FloatExpr | Null = null,
  ): Vec4Expr =
    val o = toIntExpr(octaves)
    val p = tilingPeriod != null
    val r = rot != null
    if seed == null then
      if p && r then wgsl.psrdfbm3(pos, o, lacunarity, gain, nn(tilingPeriod), alpha(rot))
      else if p then wgsl.psdfbm3(pos, o, lacunarity, gain, nn(tilingPeriod))
      else if r then wgsl.srdfbm3(pos, o, lacunarity, gain, alpha(rot))
      else wgsl.sdfbm3(pos, o, lacunarity, gain)
    else
      val s = nn(seed)
      if p && r then
        wgsl.psrdfbm3Seeded(pos, o, lacunarity, gain, nn(tilingPeriod), alpha(rot), s)
      else if p then wgsl.psdfbm3Seeded(pos, o, lacunarity, gain, nn(tilingPeriod), s)
      else if r then wgsl.srdfbm3Seeded(pos, o, lacunarity, gain, alpha(rot), s)
      else wgsl.sdfbm3Seeded(pos, o, lacunarity, gain, s)

  /** 3D [[fbm2d]], value only. */
  def fbmValue3d(
      pos: Vec3Expr,
      octaves: IntArg = 4,
      lacunarity: FloatExpr = 2.0.toExpr,
      gain: FloatExpr = 0.5.toExpr,
      tilingPeriod: Vec3Expr | Null = null,
      rot: FloatExpr | Null = null,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    val o = toIntExpr(octaves)
    val p = tilingPeriod != null
    val r = rot != null
    if seed == null then
      if p && r then wgsl.psrfbm3(pos, o, lacunarity, gain, nn(tilingPeriod), alpha(rot))
      else if p then wgsl.psfbm3(pos, o, lacunarity, gain, nn(tilingPeriod))
      else if r then wgsl.srfbm3(pos, o, lacunarity, gain, alpha(rot))
      else wgsl.sfbm3(pos, o, lacunarity, gain)
    else
      val s = nn(seed)
      if p && r then
        wgsl.psrfbm3Seeded(pos, o, lacunarity, gain, nn(tilingPeriod), alpha(rot), s)
      else if p then wgsl.psfbm3Seeded(pos, o, lacunarity, gain, nn(tilingPeriod), s)
      else if r then wgsl.srfbm3Seeded(pos, o, lacunarity, gain, alpha(rot), s)
      else wgsl.sfbm3Seeded(pos, o, lacunarity, gain, s)

  /** The WgslFn definitions, mirroring the upstream variant set — the layer
    * for `.withDeps` and raw WGSL composition. A member `psrdnoise3Seeded`
    * emits `extended_psrdnoise_3_seeded`; depend on exactly the variant a raw
    * WGSL string calls. Upstream parameter names: `x` position, `p` period,
    * `alpha` rotation angle in **radians** (the wrappers take turns). Gradient
    * variants return `vec3`/`vec4`: value in `.x`, gradient in the rest.
    */
  object wgsl:
    import NoiseCommon.seedOffsets

    // ---- helpers (upstream mod289 uses a division) ----

    private lazy val mod289v3: WgslFn[(x: Vec3), Vec3] =
      WgslFn.raw("extended_mod289_3")("  return x - floor(x / 289.0) * 289.0;")

    private lazy val mod289v4: WgslFn[(i: Vec4), Vec4] =
      WgslFn.raw("extended_mod289_4")("  return i - floor(i / 289.0) * 289.0;")

    private lazy val permute3: WgslFn[(i: Vec3), Vec3] =
      WgslFn
        .raw("extended_permute_3")("""  let im = extended_mod289_3(i);
  return extended_mod289_3((im * 34.0 + 10.0) * im);""")
        .withDeps(mod289v3)

    private lazy val permute4: WgslFn[(i: Vec4), Vec4] =
      WgslFn
        .raw("extended_permute_4")("""  let im = extended_mod289_4(i);
  return extended_mod289_4((im * 34.0 + 10.0) * im);""")
        .withDeps(mod289v4)

    // ---- 2D body fragments (psrdnoise2.wgsl) ----

    private def body2(periodic: Boolean, gradient: Boolean, seeded: Boolean): String =
      val lattice =
        if periodic then """  if (any(p > vec2<f32>(0.0, 0.0))) {
    var xw = vec3<f32>(v0.x, v1.x, v2.x);
    var yw = vec3<f32>(v0.y, v1.y, v2.y);
    if (p.x > 0.0) {
      xw = xw - floor(vec3<f32>(v0.x, v1.x, v2.x) / p.x) * p.x;
    }
    if (p.y > 0.0) {
      yw = yw - floor(vec3<f32>(v0.y, v1.y, v2.y) / p.y) * p.y;
    }
    iu = floor(xw + 0.5 * yw + 0.5);
    iv = floor(yw + 0.5);
  } else {
    iu = vec3<f32>(i0.x, i1.x, i2.x);
    iv = vec3<f32>(i0.y, i1.y, i2.y);
  }
"""
        else """  iu = vec3<f32>(i0.x, i1.x, i2.x);
  iv = vec3<f32>(i0.y, i1.y, i2.y);
"""
      val seed =
        if seeded then "  hash = extended_permute_3(extended_permute_3(hash + so.x) + so.y);\n"
        else ""
      val result =
        if gradient then """  let n = 10.9 * dot(w4, gdotx);
  let w3 = w2 * w;
  let dw = -8.0 * w3 * gdotx;
  let dn0 = w4.x * g0 + dw.x * x0;
  let dn1 = w4.y * g1 + dw.y * x1;
  let dn2 = w4.z * g2 + dw.z * x2;
  let g = 10.9 * (dn0 + dn1 + dn2);
  return vec3<f32>(n, g);"""
        else """  let n = dot(w4, gdotx);
  return 10.9 * n;"""
      s"""  let uv = vec2<f32>(x.x + x.y * 0.5, x.y);
  let i0 = floor(uv);
  let f0 = uv - i0;
  let o1 = select(vec2<f32>(0.0, 1.0), vec2<f32>(1.0, 0.0), f0.x > f0.y);
  let i1 = i0 + o1;
  let i2 = i0 + vec2<f32>(1.0, 1.0);
  let v0 = vec2<f32>(i0.x - i0.y * 0.5, i0.y);
  let v1 = vec2<f32>(v0.x + o1.x - o1.y * 0.5, v0.y + o1.y);
  let v2 = vec2<f32>(v0.x + 0.5, v0.y + 1.0);
  let x0 = x - v0;
  let x1 = x - v1;
  let x2 = x - v2;
  var iu: vec3<f32>;
  var iv: vec3<f32>;
${lattice}  var hash = extended_mod289_3(iu);
  hash = extended_mod289_3((hash * 51.0 + 2.0) * hash + iv);
  hash = extended_mod289_3((hash * 34.0 + 10.0) * hash);
${seed}  let psi = hash * 0.07482 + alpha;
  let gx = cos(psi);
  let gy = sin(psi);
  let g0 = vec2<f32>(gx.x, gy.x);
  let g1 = vec2<f32>(gx.y, gy.y);
  let g2 = vec2<f32>(gx.z, gy.z);
  var w = 0.8 - vec3<f32>(dot(x0, x0), dot(x1, x1), dot(x2, x2));
  w = max(w, vec3<f32>(0.0, 0.0, 0.0));
  let w2 = w * w;
  let w4 = w2 * w2;
  let gdotx = vec3<f32>(dot(g0, x0), dot(g1, x1), dot(g2, x2));
${result}"""

    // ---- 3D body fragments (psrdnoise3.wgsl) ----

    private def body3(
        periodic: Boolean,
        rotating: Boolean,
        gradient: Boolean,
        seeded: Boolean,
    ): String =
      val wrap =
        if periodic then """  if (any(p > vec3<f32>(0.0))) {
    var vx = vec4<f32>(v0.x, v1.x, v2.x, v3.x);
    var vy = vec4<f32>(v0.y, v1.y, v2.y, v3.y);
    var vz = vec4<f32>(v0.z, v1.z, v2.z, v3.z);
    if (p.x > 0.0) {
      vx = vx - floor(vx / p.x) * p.x;
    }
    if (p.y > 0.0) {
      vy = vy - floor(vy / p.y) * p.y;
    }
    if (p.z > 0.0) {
      vz = vz - floor(vz / p.z) * p.z;
    }
    i0 = floor(M * vec3<f32>(vx.x, vy.x, vz.x) + 0.5);
    i1 = floor(M * vec3<f32>(vx.y, vy.y, vz.y) + 0.5);
    i2 = floor(M * vec3<f32>(vx.z, vy.z, vz.z) + 0.5);
    i3 = floor(M * vec3<f32>(vx.w, vy.w, vz.w) + 0.5);
  }
"""
        else ""
      val seed =
        if seeded then "  hash = extended_permute_4(extended_permute_4(hash + so.x) + so.y);\n"
        else ""
      val gradients =
        if rotating then """  let psi = hash * 0.108705628;
  let Ct = cos(theta);
  let St = sin(theta);
  let sz_ = sqrt(1.0 - sz * sz);
  var gx: vec4<f32>;
  var gy: vec4<f32>;
  var gz: vec4<f32>;
  if (alpha != 0.0) {
    let px = Ct * sz_;
    let py = St * sz_;
    let pz = sz;
    let Sp = sin(psi);
    let Cp = cos(psi);
    let Ctp = St * Sp - Ct * Cp;
    let qx = mix(Ctp * St, Sp, sz);
    let qy = mix(-Ctp * Ct, Cp, sz);
    let qz = -(py * Cp + px * Sp);
    let Sa = vec4<f32>(sin(alpha));
    let Ca = vec4<f32>(cos(alpha));
    gx = Ca * px + Sa * qx;
    gy = Ca * py + Sa * qy;
    gz = Ca * pz + Sa * qz;
  } else {
    gx = Ct * sz_;
    gy = St * sz_;
    gz = sz;
  }
"""
        else """  let Ct = cos(theta);
  let St = sin(theta);
  let sz_ = sqrt(1.0 - sz * sz);
  let gx = Ct * sz_;
  let gy = St * sz_;
  let gz = sz;
"""
      val result =
        if gradient then """  let n = 39.5 * dot(w3, gdotx);
  let dw = -6.0 * w2 * gdotx;
  let dn0 = w3.x * g0 + dw.x * x0;
  let dn1 = w3.y * g1 + dw.y * x1;
  let dn2 = w3.z * g2 + dw.z * x2;
  let dn3 = w3.w * g3 + dw.w * x3;
  let g = 39.5 * (dn0 + dn1 + dn2 + dn3);
  return vec4<f32>(n, g);"""
        else """  let n = dot(w3, gdotx);
  return 39.5 * n;"""
      s"""  let M = mat3x3<f32>(0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0);
  let Mi = mat3x3<f32>(-0.5, 0.5, 0.5, 0.5, -0.5, 0.5, 0.5, 0.5, -0.5);
  let uvw = M * x;
  var i0 = floor(uvw);
  let f0 = uvw - i0;
  let gt_ = step(f0.xyx, f0.yzz);
  let lt_ = 1.0 - gt_;
  let gt = vec3<f32>(lt_.z, gt_.xy);
  let lt = vec3<f32>(lt_.xy, gt_.z);
  let o1 = min(gt, lt);
  let o2 = max(gt, lt);
  var i1 = i0 + o1;
  var i2 = i0 + o2;
  var i3 = i0 + vec3<f32>(1.0, 1.0, 1.0);
  let v0 = Mi * i0;
  let v1 = Mi * i1;
  let v2 = Mi * i2;
  let v3 = Mi * i3;
  let x0 = x - v0;
  let x1 = x - v1;
  let x2 = x - v2;
  let x3 = x - v3;
${wrap}  var hash = extended_permute_4(extended_permute_4(extended_permute_4(vec4<f32>(i0.z, i1.z, i2.z, i3.z)) + vec4<f32>(i0.y, i1.y, i2.y, i3.y)) + vec4<f32>(i0.x, i1.x, i2.x, i3.x));
${seed}  let theta = hash * 3.883222077;
  let sz = hash * -0.006920415 + 0.996539792;
${gradients}  let g0 = vec3<f32>(gx.x, gy.x, gz.x);
  let g1 = vec3<f32>(gx.y, gy.y, gz.y);
  let g2 = vec3<f32>(gx.z, gy.z, gz.z);
  let g3 = vec3<f32>(gx.w, gy.w, gz.w);
  var w = 0.5 - vec4<f32>(dot(x0, x0), dot(x1, x1), dot(x2, x2), dot(x3, x3));
  w = max(w, vec4<f32>(0.0, 0.0, 0.0, 0.0));
  let w2 = w * w;
  let w3 = w2 * w;
  let gdotx = vec4<f32>(dot(g0, x0), dot(g1, x1), dot(g2, x2), dot(g3, x3));
${result}"""

    // ---- fbm loops (ours) ----

    /** `call` is the per-octave noise call with `X` / `P` placeholders for the
      * scaled position and period; `dim` is 2 or 3.
      */
    private def fbmBody(
        call: String,
        dim: Int,
        gradient: Boolean,
        seeded: Boolean,
    ): String =
      val soLine = if seeded then "  let so = noise_seed_offsets(seed);\n" else ""
      val c = call
        .replace("X", "x * frequency")
        .replace("P", "p * frequency")
        .replace("SO", "vec2<f32>((so.x + f32(i)) % 289.0, so.y)")
      val (init, accumulate) =
        if gradient then
          val vt = if dim == 2 then "vec3<f32>" else "vec4<f32>"
          val gs = if dim == 2 then "ng.yz" else "ng.yzw"
          (
            s"  var sum = $vt(0.0);\n",
            s"""    let ng = $c;
    sum += $vt(ng.x, $gs * frequency) * amplitude;
""",
          )
        else ("  var sum = 0.0;\n", s"    sum += $c * amplitude;\n")
      s"""${soLine}${init}  var amplitude = 1.0;
  var total = 0.0;
  var frequency = 1.0;
  for (var i = 0; i < octaves; i += 1) {
${accumulate}    total += amplitude;
    amplitude *= gain;
    frequency *= lacunarity;
  }
  return sum / total;"""

    // Plain concatenation on purpose: `String.format` links java.util.Formatter
    // (BigInteger & co., ~100 KB) into every bundle using extended noise.
    private def seededFwd(variant: String, args: String): String =
      "  return extended_" + variant + "_so(" + args + ", noise_seed_offsets(seed));"

    // =========================================================================
    // 2D noise
    // =========================================================================

    lazy val psrnoise2: WgslFn[(x: Vec2, p: Vec2, alpha: Float), Float] =
      WgslFn.raw("extended_psrnoise_2")(body2(true, false, false)).withDeps(mod289v3)

    lazy val psnoise2: WgslFn[(x: Vec2, p: Vec2), Float] =
      WgslFn
        .raw("extended_psnoise_2")("  return extended_psrnoise_2(x, p, 0.0);")
        .withDeps(psrnoise2)

    lazy val srnoise2: WgslFn[(x: Vec2, alpha: Float), Float] =
      WgslFn.raw("extended_srnoise_2")(body2(false, false, false)).withDeps(mod289v3)

    lazy val snoise2: WgslFn[(x: Vec2), Float] =
      WgslFn
        .raw("extended_snoise_2")("  return extended_srnoise_2(x, 0.0);")
        .withDeps(srnoise2)

    lazy val psrdnoise2: WgslFn[(x: Vec2, p: Vec2, alpha: Float), Vec3] =
      WgslFn.raw("extended_psrdnoise_2")(body2(true, true, false)).withDeps(mod289v3)

    lazy val psdnoise2: WgslFn[(x: Vec2, p: Vec2), Vec3] =
      WgslFn
        .raw("extended_psdnoise_2")("  return extended_psrdnoise_2(x, p, 0.0);")
        .withDeps(psrdnoise2)

    lazy val srdnoise2: WgslFn[(x: Vec2, alpha: Float), Vec3] =
      WgslFn.raw("extended_srdnoise_2")(body2(false, true, false)).withDeps(mod289v3)

    lazy val sdnoise2: WgslFn[(x: Vec2), Vec3] =
      WgslFn
        .raw("extended_sdnoise_2")("  return extended_srdnoise_2(x, 0.0);")
        .withDeps(srdnoise2)

    // ---- 2D seeded ----

    private[lib] lazy val psrnoise2So: WgslFn[
      (x: Vec2, p: Vec2, alpha: Float, so: Vec2),
      Float,
    ] =
      WgslFn
        .raw("extended_psrnoise_2_so")(body2(true, false, true))
        .withDeps(mod289v3, permute3)

    private[lib] lazy val srnoise2So: WgslFn[(x: Vec2, alpha: Float, so: Vec2), Float] =
      WgslFn
        .raw("extended_srnoise_2_so")(body2(false, false, true))
        .withDeps(mod289v3, permute3)

    private[lib] lazy val psrdnoise2So: WgslFn[
      (x: Vec2, p: Vec2, alpha: Float, so: Vec2),
      Vec3,
    ] =
      WgslFn
        .raw("extended_psrdnoise_2_so")(body2(true, true, true))
        .withDeps(mod289v3, permute3)

    private[lib] lazy val srdnoise2So: WgslFn[(x: Vec2, alpha: Float, so: Vec2), Vec3] =
      WgslFn
        .raw("extended_srdnoise_2_so")(body2(false, true, true))
        .withDeps(mod289v3, permute3)

    lazy val psrnoise2Seeded: WgslFn[
      (x: Vec2, p: Vec2, alpha: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_psrnoise_2_seeded")(
          seededFwd("psrnoise_2", "x, p, alpha"),
        )
        .withDeps(psrnoise2So, seedOffsets)

    lazy val psnoise2Seeded: WgslFn[(x: Vec2, p: Vec2, seed: Float), Float] =
      WgslFn
        .raw("extended_psnoise_2_seeded")(
          seededFwd("psrnoise_2", "x, p, 0.0"),
        )
        .withDeps(psrnoise2So, seedOffsets)

    lazy val srnoise2Seeded: WgslFn[(x: Vec2, alpha: Float, seed: Float), Float] =
      WgslFn
        .raw("extended_srnoise_2_seeded")(
          seededFwd("srnoise_2", "x, alpha"),
        )
        .withDeps(srnoise2So, seedOffsets)

    lazy val snoise2Seeded: WgslFn[(x: Vec2, seed: Float), Float] =
      WgslFn
        .raw("extended_snoise_2_seeded")(
          seededFwd("srnoise_2", "x, 0.0"),
        )
        .withDeps(srnoise2So, seedOffsets)

    lazy val psrdnoise2Seeded: WgslFn[
      (x: Vec2, p: Vec2, alpha: Float, seed: Float),
      Vec3,
    ] =
      WgslFn
        .raw("extended_psrdnoise_2_seeded")(
          seededFwd("psrdnoise_2", "x, p, alpha"),
        )
        .withDeps(psrdnoise2So, seedOffsets)

    lazy val psdnoise2Seeded: WgslFn[(x: Vec2, p: Vec2, seed: Float), Vec3] =
      WgslFn
        .raw("extended_psdnoise_2_seeded")(
          seededFwd("psrdnoise_2", "x, p, 0.0"),
        )
        .withDeps(psrdnoise2So, seedOffsets)

    lazy val srdnoise2Seeded: WgslFn[(x: Vec2, alpha: Float, seed: Float), Vec3] =
      WgslFn
        .raw("extended_srdnoise_2_seeded")(
          seededFwd("srdnoise_2", "x, alpha"),
        )
        .withDeps(srdnoise2So, seedOffsets)

    lazy val sdnoise2Seeded: WgslFn[(x: Vec2, seed: Float), Vec3] =
      WgslFn
        .raw("extended_sdnoise_2_seeded")(
          seededFwd("srdnoise_2", "x, 0.0"),
        )
        .withDeps(srdnoise2So, seedOffsets)

    // =========================================================================
    // 3D noise
    // =========================================================================

    lazy val psrnoise3: WgslFn[(x: Vec3, p: Vec3, alpha: Float), Float] =
      WgslFn
        .raw("extended_psrnoise_3")(body3(true, true, false, false))
        .withDeps(permute4)

    lazy val psnoise3: WgslFn[(x: Vec3, p: Vec3), Float] =
      WgslFn
        .raw("extended_psnoise_3")(body3(true, false, false, false))
        .withDeps(permute4)

    lazy val srnoise3: WgslFn[(x: Vec3, alpha: Float), Float] =
      WgslFn
        .raw("extended_srnoise_3")(body3(false, true, false, false))
        .withDeps(permute4)

    lazy val snoise3: WgslFn[(x: Vec3), Float] =
      WgslFn
        .raw("extended_snoise_3")(body3(false, false, false, false))
        .withDeps(permute4)

    lazy val psrdnoise3: WgslFn[(x: Vec3, p: Vec3, alpha: Float), Vec4] =
      WgslFn
        .raw("extended_psrdnoise_3")(body3(true, true, true, false))
        .withDeps(permute4)

    lazy val psdnoise3: WgslFn[(x: Vec3, p: Vec3), Vec4] =
      WgslFn
        .raw("extended_psdnoise_3")(body3(true, false, true, false))
        .withDeps(permute4)

    lazy val srdnoise3: WgslFn[(x: Vec3, alpha: Float), Vec4] =
      WgslFn
        .raw("extended_srdnoise_3")(body3(false, true, true, false))
        .withDeps(permute4)

    lazy val sdnoise3: WgslFn[(x: Vec3), Vec4] =
      WgslFn
        .raw("extended_sdnoise_3")(body3(false, false, true, false))
        .withDeps(permute4)

    // ---- 3D seeded ----

    private[lib] lazy val psrnoise3So: WgslFn[
      (x: Vec3, p: Vec3, alpha: Float, so: Vec2),
      Float,
    ] =
      WgslFn
        .raw("extended_psrnoise_3_so")(body3(true, true, false, true))
        .withDeps(permute4)

    private[lib] lazy val psnoise3So: WgslFn[(x: Vec3, p: Vec3, so: Vec2), Float] =
      WgslFn
        .raw("extended_psnoise_3_so")(body3(true, false, false, true))
        .withDeps(permute4)

    private[lib] lazy val srnoise3So: WgslFn[(x: Vec3, alpha: Float, so: Vec2), Float] =
      WgslFn
        .raw("extended_srnoise_3_so")(body3(false, true, false, true))
        .withDeps(permute4)

    private[lib] lazy val snoise3So: WgslFn[(x: Vec3, so: Vec2), Float] =
      WgslFn
        .raw("extended_snoise_3_so")(body3(false, false, false, true))
        .withDeps(permute4)

    private[lib] lazy val psrdnoise3So: WgslFn[
      (x: Vec3, p: Vec3, alpha: Float, so: Vec2),
      Vec4,
    ] =
      WgslFn
        .raw("extended_psrdnoise_3_so")(body3(true, true, true, true))
        .withDeps(permute4)

    private[lib] lazy val psdnoise3So: WgslFn[(x: Vec3, p: Vec3, so: Vec2), Vec4] =
      WgslFn
        .raw("extended_psdnoise_3_so")(body3(true, false, true, true))
        .withDeps(permute4)

    private[lib] lazy val srdnoise3So: WgslFn[(x: Vec3, alpha: Float, so: Vec2), Vec4] =
      WgslFn
        .raw("extended_srdnoise_3_so")(body3(false, true, true, true))
        .withDeps(permute4)

    private[lib] lazy val sdnoise3So: WgslFn[(x: Vec3, so: Vec2), Vec4] =
      WgslFn
        .raw("extended_sdnoise_3_so")(body3(false, false, true, true))
        .withDeps(permute4)

    lazy val psrnoise3Seeded: WgslFn[
      (x: Vec3, p: Vec3, alpha: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_psrnoise_3_seeded")(
          seededFwd("psrnoise_3", "x, p, alpha"),
        )
        .withDeps(psrnoise3So, seedOffsets)

    lazy val psnoise3Seeded: WgslFn[(x: Vec3, p: Vec3, seed: Float), Float] =
      WgslFn
        .raw("extended_psnoise_3_seeded")(
          seededFwd("psnoise_3", "x, p"),
        )
        .withDeps(psnoise3So, seedOffsets)

    lazy val srnoise3Seeded: WgslFn[(x: Vec3, alpha: Float, seed: Float), Float] =
      WgslFn
        .raw("extended_srnoise_3_seeded")(
          seededFwd("srnoise_3", "x, alpha"),
        )
        .withDeps(srnoise3So, seedOffsets)

    lazy val snoise3Seeded: WgslFn[(x: Vec3, seed: Float), Float] =
      WgslFn
        .raw("extended_snoise_3_seeded")(
          seededFwd("snoise_3", "x"),
        )
        .withDeps(snoise3So, seedOffsets)

    lazy val psrdnoise3Seeded: WgslFn[
      (x: Vec3, p: Vec3, alpha: Float, seed: Float),
      Vec4,
    ] =
      WgslFn
        .raw("extended_psrdnoise_3_seeded")(
          seededFwd("psrdnoise_3", "x, p, alpha"),
        )
        .withDeps(psrdnoise3So, seedOffsets)

    lazy val psdnoise3Seeded: WgslFn[(x: Vec3, p: Vec3, seed: Float), Vec4] =
      WgslFn
        .raw("extended_psdnoise_3_seeded")(
          seededFwd("psdnoise_3", "x, p"),
        )
        .withDeps(psdnoise3So, seedOffsets)

    lazy val srdnoise3Seeded: WgslFn[(x: Vec3, alpha: Float, seed: Float), Vec4] =
      WgslFn
        .raw("extended_srdnoise_3_seeded")(
          seededFwd("srdnoise_3", "x, alpha"),
        )
        .withDeps(srdnoise3So, seedOffsets)

    lazy val sdnoise3Seeded: WgslFn[(x: Vec3, seed: Float), Vec4] =
      WgslFn
        .raw("extended_sdnoise_3_seeded")(
          seededFwd("sdnoise_3", "x"),
        )
        .withDeps(sdnoise3So, seedOffsets)

    // =========================================================================
    // 2D fbm — value (`…fbm2`) and gradient (`…dfbm2`)
    // =========================================================================

    lazy val psrfbm2: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, p: Vec2, alpha: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_psrfbm_2")(fbmBody("extended_psrnoise_2(X, P, alpha)", 2, false, false))
        .withDeps(psrnoise2)

    lazy val psfbm2: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, p: Vec2),
      Float,
    ] =
      WgslFn
        .raw("extended_psfbm_2")(fbmBody("extended_psrnoise_2(X, P, 0.0)", 2, false, false))
        .withDeps(psrnoise2)

    lazy val srfbm2: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, alpha: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_srfbm_2")(fbmBody("extended_srnoise_2(X, alpha)", 2, false, false))
        .withDeps(srnoise2)

    lazy val sfbm2: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_sfbm_2")(fbmBody("extended_srnoise_2(X, 0.0)", 2, false, false))
        .withDeps(srnoise2)

    lazy val psrdfbm2: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, p: Vec2, alpha: Float),
      Vec3,
    ] =
      WgslFn
        .raw("extended_psrdfbm_2")(fbmBody("extended_psrdnoise_2(X, P, alpha)", 2, true, false))
        .withDeps(psrdnoise2)

    lazy val psdfbm2: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, p: Vec2),
      Vec3,
    ] =
      WgslFn
        .raw("extended_psdfbm_2")(fbmBody("extended_psrdnoise_2(X, P, 0.0)", 2, true, false))
        .withDeps(psrdnoise2)

    lazy val srdfbm2: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, alpha: Float),
      Vec3,
    ] =
      WgslFn
        .raw("extended_srdfbm_2")(fbmBody("extended_srdnoise_2(X, alpha)", 2, true, false))
        .withDeps(srdnoise2)

    lazy val sdfbm2: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float),
      Vec3,
    ] =
      WgslFn
        .raw("extended_sdfbm_2")(fbmBody("extended_srdnoise_2(X, 0.0)", 2, true, false))
        .withDeps(srdnoise2)

    lazy val psrfbm2Seeded: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, p: Vec2, alpha: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_psrfbm_2_seeded")(
          fbmBody("extended_psrnoise_2_so(X, P, alpha, SO)", 2, false, true),
        )
        .withDeps(psrnoise2So, seedOffsets)

    lazy val psfbm2Seeded: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, p: Vec2, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_psfbm_2_seeded")(
          fbmBody("extended_psrnoise_2_so(X, P, 0.0, SO)", 2, false, true),
        )
        .withDeps(psrnoise2So, seedOffsets)

    lazy val srfbm2Seeded: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, alpha: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_srfbm_2_seeded")(
          fbmBody("extended_srnoise_2_so(X, alpha, SO)", 2, false, true),
        )
        .withDeps(srnoise2So, seedOffsets)

    lazy val sfbm2Seeded: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_sfbm_2_seeded")(
          fbmBody("extended_srnoise_2_so(X, 0.0, SO)", 2, false, true),
        )
        .withDeps(srnoise2So, seedOffsets)

    lazy val psrdfbm2Seeded: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, p: Vec2, alpha: Float, seed: Float),
      Vec3,
    ] =
      WgslFn
        .raw("extended_psrdfbm_2_seeded")(
          fbmBody("extended_psrdnoise_2_so(X, P, alpha, SO)", 2, true, true),
        )
        .withDeps(psrdnoise2So, seedOffsets)

    lazy val psdfbm2Seeded: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, p: Vec2, seed: Float),
      Vec3,
    ] =
      WgslFn
        .raw("extended_psdfbm_2_seeded")(
          fbmBody("extended_psrdnoise_2_so(X, P, 0.0, SO)", 2, true, true),
        )
        .withDeps(psrdnoise2So, seedOffsets)

    lazy val srdfbm2Seeded: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, alpha: Float, seed: Float),
      Vec3,
    ] =
      WgslFn
        .raw("extended_srdfbm_2_seeded")(
          fbmBody("extended_srdnoise_2_so(X, alpha, SO)", 2, true, true),
        )
        .withDeps(srdnoise2So, seedOffsets)

    lazy val sdfbm2Seeded: WgslFn[
      (x: Vec2, octaves: Int, lacunarity: Float, gain: Float, seed: Float),
      Vec3,
    ] =
      WgslFn
        .raw("extended_sdfbm_2_seeded")(
          fbmBody("extended_srdnoise_2_so(X, 0.0, SO)", 2, true, true),
        )
        .withDeps(srdnoise2So, seedOffsets)

    // =========================================================================
    // 3D fbm — value (`…fbm3`) and gradient (`…dfbm3`)
    // =========================================================================

    lazy val psrfbm3: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, p: Vec3, alpha: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_psrfbm_3")(fbmBody("extended_psrnoise_3(X, P, alpha)", 3, false, false))
        .withDeps(psrnoise3)

    lazy val psfbm3: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, p: Vec3),
      Float,
    ] =
      WgslFn
        .raw("extended_psfbm_3")(fbmBody("extended_psnoise_3(X, P)", 3, false, false))
        .withDeps(psnoise3)

    lazy val srfbm3: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, alpha: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_srfbm_3")(fbmBody("extended_srnoise_3(X, alpha)", 3, false, false))
        .withDeps(srnoise3)

    lazy val sfbm3: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_sfbm_3")(fbmBody("extended_snoise_3(X)", 3, false, false))
        .withDeps(snoise3)

    lazy val psrdfbm3: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, p: Vec3, alpha: Float),
      Vec4,
    ] =
      WgslFn
        .raw("extended_psrdfbm_3")(fbmBody("extended_psrdnoise_3(X, P, alpha)", 3, true, false))
        .withDeps(psrdnoise3)

    lazy val psdfbm3: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, p: Vec3),
      Vec4,
    ] =
      WgslFn
        .raw("extended_psdfbm_3")(fbmBody("extended_psdnoise_3(X, P)", 3, true, false))
        .withDeps(psdnoise3)

    lazy val srdfbm3: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, alpha: Float),
      Vec4,
    ] =
      WgslFn
        .raw("extended_srdfbm_3")(fbmBody("extended_srdnoise_3(X, alpha)", 3, true, false))
        .withDeps(srdnoise3)

    lazy val sdfbm3: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float),
      Vec4,
    ] =
      WgslFn
        .raw("extended_sdfbm_3")(fbmBody("extended_sdnoise_3(X)", 3, true, false))
        .withDeps(sdnoise3)

    lazy val psrfbm3Seeded: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, p: Vec3, alpha: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_psrfbm_3_seeded")(
          fbmBody("extended_psrnoise_3_so(X, P, alpha, SO)", 3, false, true),
        )
        .withDeps(psrnoise3So, seedOffsets)

    lazy val psfbm3Seeded: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, p: Vec3, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_psfbm_3_seeded")(
          fbmBody("extended_psnoise_3_so(X, P, SO)", 3, false, true),
        )
        .withDeps(psnoise3So, seedOffsets)

    lazy val srfbm3Seeded: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, alpha: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_srfbm_3_seeded")(
          fbmBody("extended_srnoise_3_so(X, alpha, SO)", 3, false, true),
        )
        .withDeps(srnoise3So, seedOffsets)

    lazy val sfbm3Seeded: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, seed: Float),
      Float,
    ] =
      WgslFn
        .raw("extended_sfbm_3_seeded")(
          fbmBody("extended_snoise_3_so(X, SO)", 3, false, true),
        )
        .withDeps(snoise3So, seedOffsets)

    lazy val psrdfbm3Seeded: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, p: Vec3, alpha: Float, seed: Float),
      Vec4,
    ] =
      WgslFn
        .raw("extended_psrdfbm_3_seeded")(
          fbmBody("extended_psrdnoise_3_so(X, P, alpha, SO)", 3, true, true),
        )
        .withDeps(psrdnoise3So, seedOffsets)

    lazy val psdfbm3Seeded: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, p: Vec3, seed: Float),
      Vec4,
    ] =
      WgslFn
        .raw("extended_psdfbm_3_seeded")(
          fbmBody("extended_psdnoise_3_so(X, P, SO)", 3, true, true),
        )
        .withDeps(psdnoise3So, seedOffsets)

    lazy val srdfbm3Seeded: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, alpha: Float, seed: Float),
      Vec4,
    ] =
      WgslFn
        .raw("extended_srdfbm_3_seeded")(
          fbmBody("extended_srdnoise_3_so(X, alpha, SO)", 3, true, true),
        )
        .withDeps(srdnoise3So, seedOffsets)

    lazy val sdfbm3Seeded: WgslFn[
      (x: Vec3, octaves: Int, lacunarity: Float, gain: Float, seed: Float),
      Vec4,
    ] =
      WgslFn
        .raw("extended_sdfbm_3_seeded")(
          fbmBody("extended_sdnoise_3_so(X, SO)", 3, true, true),
        )
        .withDeps(sdnoise3So, seedOffsets)
