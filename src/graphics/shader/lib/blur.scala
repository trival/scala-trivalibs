package trivalibs.graphics.shader.lib.blur

// Separable Gaussian + box blurs.
// The 5/9/13-tap variants use precomputed weights (linear-sampling trick —
// every two adjacent samples collapse into one bilinear fetch).
// The general `gaussianBlur` derives σ from a runtime diameter and loops.

import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec4
import trivalibs.graphics.math.gpu.Sampler
import trivalibs.graphics.math.gpu.Texture2D
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given

object Blur:

  /** Separable Gaussian blur with runtime-controlled diameter.
    *
    *   - σ is derived as `diameter * 0.25` (radius is ¼ the diameter, σ is ½
    *     the radius).
    *   - Sample support is `ceil(σ * 1.5)` texels per side — a somewhat
    *     aggressive default that keeps the kernel tight.
    *   - Adjacent pairs of samples collapse into one bilinear fetch via
    *     `mix(c₀, c₁, w₁/(w₀+w₁))` — halves the texture-sample count.
    *
    * Run twice for a 2D blur (horizontal pass with `dir=(1,0)`, vertical pass
    * with `dir=(0,1)`).
    *
    * @param tex
    *   image to blur
    * @param s
    *   sampler (linear filtering — the bilinear trick depends on it)
    * @param diameter
    *   diameter (not radius) of the circle of confusion in pixels
    * @param uv
    *   fragment uv coords
    * @param res
    *   image resolution in pixels — converts pixel offsets to uv space
    * @param dir
    *   step direction in screen-space units. `(1,0)` for horizontal, `(0,1)`
    *   for vertical (need not be axis-aligned).
    */
  val gaussianBlur: WgslFn[
    (
        tex: Texture2D,
        s: Sampler,
        diameter: Float,
        uv: Vec2,
        res: Vec2,
        dir: Vec2,
    ),
    Vec4,
  ] =
    WgslFn.raw("gaussian_blur"):
      """  let sigma = diameter * 0.25;
  let support: i32 = i32(ceil(sigma * 1.5));
  let offset = dir / res;
  let exp_factor = -1.0 / (2.0 * sigma * sigma);
  var sum = textureSample(tex, s, uv);
  var weight_sum = 1.0;
  var i: i32 = 1;
  while (i <= support) {
    let j = f32(i);
    let w0 = exp(exp_factor * j * j);
    let w1 = exp(exp_factor * (j + 1.0) * (j + 1.0));
    let uv_offset = offset * (j + w1 / (w0 + w1));
    let weight = w0 + w1;
    sum += (textureSample(tex, s, uv + uv_offset) + textureSample(tex, s, uv - uv_offset)) * weight;
    weight_sum += weight * 2.0;
    i = i + 2;
  }
  return sum / weight_sum;"""

  /** Precalculated 5-tap Gaussian blur (3 texture fetches, fixed 5px diameter
    * circle of confusion). Cheapest of the three baked variants — use it for
    * subtle blurs where quality matters less than fragment cost.
    *
    * Run twice (H + V) for a 2D blur.
    *
    * @param dir
    *   step direction in screen-space units (`(1,0)` or `(0,1)` typically)
    */
  val gaussianBlur5: WgslFn[
    (tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, dir: Vec2),
    Vec4,
  ] =
    WgslFn.raw("gaussian_blur_5"):
      """  let off1 = vec2<f32>(1.3333333333333333) * dir / res;
  var color = textureSample(tex, s, uv) * 0.29411764705882354;
  color += textureSample(tex, s, uv + off1) * 0.35294117647058826;
  color += textureSample(tex, s, uv - off1) * 0.35294117647058826;
  return color;"""

  /** Precalculated 9-tap Gaussian blur (5 texture fetches, fixed 9px diameter
    * circle of confusion). Good middle ground for screen-space blur.
    *
    * Run twice (H + V) for a 2D blur.
    */
  val gaussianBlur9: WgslFn[
    (tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, dir: Vec2),
    Vec4,
  ] =
    WgslFn.raw("gaussian_blur_9"):
      """  let off1 = vec2<f32>(1.3846153846) * dir / res;
  let off2 = vec2<f32>(3.2307692308) * dir / res;
  var color = textureSample(tex, s, uv) * 0.2270270270;
  color += textureSample(tex, s, uv + off1) * 0.3162162162;
  color += textureSample(tex, s, uv - off1) * 0.3162162162;
  color += textureSample(tex, s, uv + off2) * 0.0702702703;
  color += textureSample(tex, s, uv - off2) * 0.0702702703;
  return color;"""

  /** Precalculated 13-tap Gaussian blur (7 texture fetches, fixed 13px diameter
    * circle of confusion). Highest quality of the baked variants.
    *
    * Run twice (H + V) for a 2D blur.
    */
  val gaussianBlur13: WgslFn[
    (tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, dir: Vec2),
    Vec4,
  ] =
    WgslFn.raw("gaussian_blur_13"):
      """  let off1 = vec2<f32>(1.411764705882353) * dir / res;
  let off2 = vec2<f32>(3.2941176470588234) * dir / res;
  let off3 = vec2<f32>(5.176470588235294) * dir / res;
  var color = textureSample(tex, s, uv) * 0.1964825501511404;
  color += textureSample(tex, s, uv + off1) * 0.2969069646728344;
  color += textureSample(tex, s, uv - off1) * 0.2969069646728344;
  color += textureSample(tex, s, uv + off2) * 0.09447039785044732;
  color += textureSample(tex, s, uv - off2) * 0.09447039785044732;
  color += textureSample(tex, s, uv + off3) * 0.010381362401148057;
  color += textureSample(tex, s, uv - off3) * 0.010381362401148057;
  return color;"""

  /** Single-direction box blur. Average of `1 + 2·support` samples, where
    * `support = floor(diameter * 0.5)` texels per side.
    *
    * Run twice (H + V) for a 2D box blur.
    *
    * @param diameter
    *   diameter (not radius) of the circle of confusion in pixels
    * @param dir
    *   step direction in screen-space units (need not be axis-aligned)
    */
  val boxBlur: WgslFn[
    (
        tex: Texture2D,
        s: Sampler,
        diameter: Float,
        uv: Vec2,
        res: Vec2,
        dir: Vec2,
    ),
    Vec4,
  ] =
    WgslFn.raw("box_blur"):
      """  let support: i32 = i32(floor(diameter * 0.5));
  let offset = dir / res;
  var sum = textureSample(tex, s, uv);
  var i: i32 = 1;
  while (i <= support) {
    sum += textureSample(tex, s, uv + offset * f32(i)) + textureSample(tex, s, uv - offset * f32(i));
    i = i + 1;
  }
  return sum / (1.0 + f32(support) * 2.0);"""

  // ===========================================================================
  // 2D single-pass pyramid kernels
  //
  // The blurs above are SEPARABLE 1D kernels: a wide blur at one resolution
  // needs two passes (horizontal `dir=(1,0)` + vertical `dir=(0,1)`) and they
  // exploit the linear-sampling trick. The two kernels below are a different
  // family — NON-SEPARABLE 2D kernels run in a SINGLE pass with a tiny
  // footprint, meant to fill a mip chain: each level reads the previous
  // (higher-res) level, so the mip chain itself does the spreading (effective
  // radius ≈ doubles per level) and per-level work stays cheap.
  //
  // Use separable (above) for a wide blur at one resolution; use these to build
  // a downsample/blur pyramid. `radius` is the tap offset in destination-mip
  // texels.
  // ===========================================================================

  /** 2D 4-tap box downsample (single pass) — four corners at ±`radius`,
    * averaged. The classic bloom mip-downsample step.
    *
    * @param radius
    *   tap offset in destination-mip texels
    */
  val boxBlur2d: WgslFn[
    (tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, radius: Float),
    Vec4,
  ] =
    WgslFn.raw("box_blur_2d"):
      """  let o = vec2<f32>(radius) / res;
  var color = textureSample(tex, s, uv - o);
  color += textureSample(tex, s, uv + vec2<f32>(o.x, -o.y));
  color += textureSample(tex, s, uv + vec2<f32>(-o.x, o.y));
  color += textureSample(tex, s, uv + o);
  return color * 0.25;"""

  /** 2D 9-tap tent / 3×3 binomial blur (single pass). Weights
    * `[1 2 1; 2 4 2; 1 2 1] / 16` (center .25, edges .125, corners .0625) on a
    * 3×3 lattice spaced `radius` texels. Softer than [[boxBlur2d]]; the
    * per-level downsample of the reflection blur pyramid (and the bloom
    * upsample step).
    *
    * @param radius
    *   tap offset in destination-mip texels
    */
  val tentBlur2d: WgslFn[
    (tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, radius: Float),
    Vec4,
  ] =
    WgslFn.raw("tent_blur_2d"):
      """  let o = vec2<f32>(radius) / res;
  var color = textureSample(tex, s, uv) * 0.25;
  color += (textureSample(tex, s, uv + vec2<f32>(0.0, o.y)) + textureSample(tex, s, uv + vec2<f32>(0.0, -o.y)) + textureSample(tex, s, uv + vec2<f32>(o.x, 0.0)) + textureSample(tex, s, uv + vec2<f32>(-o.x, 0.0))) * 0.125;
  color += (textureSample(tex, s, uv + o) + textureSample(tex, s, uv + vec2<f32>(-o.x, o.y)) + textureSample(tex, s, uv + vec2<f32>(o.x, -o.y)) + textureSample(tex, s, uv - o)) * 0.0625;
  return color;"""

  // ---------------------------------------------------------------------------
  // Res-free (`*Auto`) variants — derive the resolution from
  // `textureDimensions(tex)` instead of a `res` uniform, so a downsample/blur
  // chain needs no per-mip `res` bindings or `onResize` bookkeeping. `radius` is
  // the tap offset in **source** texels (the bound input texture's own texels —
  // for a mip-source layer that's the source mip, since it binds a single-mip
  // view). A mirror/bloom pyramid that used `radius = 2` dst-mip texels with the
  // `res` variants matches `radius = 4` here (dst = src / 2).
  // ---------------------------------------------------------------------------

  /** Res-free [[boxBlur2d]] — resolution from `textureDimensions(tex)`.
    * @param radius
    *   tap offset in source texels
    */
  val boxBlur2dAuto: WgslFn[
    (tex: Texture2D, s: Sampler, uv: Vec2, radius: Float),
    Vec4,
  ] =
    WgslFn.raw("box_blur_2d_auto"):
      """  let o = vec2<f32>(radius) / vec2<f32>(textureDimensions(tex));
  var color = textureSample(tex, s, uv - o);
  color += textureSample(tex, s, uv + vec2<f32>(o.x, -o.y));
  color += textureSample(tex, s, uv + vec2<f32>(-o.x, o.y));
  color += textureSample(tex, s, uv + o);
  return color * 0.25;"""

  /** Res-free [[tentBlur2d]] — resolution from `textureDimensions(tex)`.
    * @param radius
    *   tap offset in source texels
    */
  val tentBlur2dAuto: WgslFn[
    (tex: Texture2D, s: Sampler, uv: Vec2, radius: Float),
    Vec4,
  ] =
    WgslFn.raw("tent_blur_2d_auto"):
      """  let o = vec2<f32>(radius) / vec2<f32>(textureDimensions(tex));
  var color = textureSample(tex, s, uv) * 0.25;
  color += (textureSample(tex, s, uv + vec2<f32>(0.0, o.y)) + textureSample(tex, s, uv + vec2<f32>(0.0, -o.y)) + textureSample(tex, s, uv + vec2<f32>(o.x, 0.0)) + textureSample(tex, s, uv + vec2<f32>(-o.x, 0.0))) * 0.125;
  color += (textureSample(tex, s, uv + o) + textureSample(tex, s, uv + vec2<f32>(-o.x, o.y)) + textureSample(tex, s, uv + vec2<f32>(o.x, -o.y)) + textureSample(tex, s, uv - o)) * 0.0625;
  return color;"""
