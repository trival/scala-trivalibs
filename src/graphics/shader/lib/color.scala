package trivalibs.graphics.shader.lib.color

// hsv2rgb family adapted from Iñigo Quilez — https://www.shadertoy.com/view/MsS3Wc
//
// HSL vs HSV — both share hue and saturation axes but differ in the third
// channel:
//   - HSV "value" V = max(R, G, B). Pure red is (0, 1, 1), white is (0, 0, 1),
//     black is (0, 0, 0). Saturation desaturates toward white only at V = 1.
//   - HSL "lightness" L = (max + min) / 2. Pure red is (0, 1, 0.5), white is
//     (0, 0, 1), black is (0, 0, 0). L is symmetric — moving L from 0.5 toward
//     0 darkens, toward 1 brightens.
// They are NOT round-trip-equivalent: rgb2hsl(hsv2rgb(c)) ≠ c, and likewise.
// Pick the space that matches your intuition for the third channel and stay
// in it.

import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given

object Color:

  // ---------------------------------------------------------------------------
  // RGB → HSV / HSL
  // ---------------------------------------------------------------------------

  /** RGB → HSV. Input components in [0, 1]; output `(hue, saturation, value)`
    * with hue in [0, 1] (1.0 == 360°) and `value = max(R, G, B)`.
    *
    * The natural inverse for [[hsv2rgb]].
    */
  val rgb2hsv: WgslFn[(c: Vec3), Vec3] =
    WgslFn.raw("rgb2hsv"):
      """  let k = vec4<f32>(0.0, -1.0/3.0, 2.0/3.0, -1.0);
  let p = mix(vec4<f32>(c.z, c.y, k.w, k.z), vec4<f32>(c.y, c.z, k.x, k.y), step(c.z, c.y));
  let q = mix(vec4<f32>(p.x, p.y, p.w, c.x), vec4<f32>(c.x, p.y, p.z, p.x), step(p.x, c.x));
  let d = q.x - min(q.w, q.y);
  let e = 1.0e-10;
  return vec3<f32>(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);"""

  /** RGB → HSL. Input components in [0, 1]; output `(hue, saturation,
    * lightness)` with hue in [0, 1] (1.0 == 360°) and `lightness = (max + min)
    * / 2`.
    *
    * The natural inverse for [[hsl2rgb]]. Saturation here uses the HSL formula
    * `chroma / (1 - |2L − 1|)` — different from HSV saturation.
    */
  val rgb2hsl: WgslFn[(c: Vec3), Vec3] =
    WgslFn.raw("rgb2hsl"):
      """  let k = vec4<f32>(0.0, -1.0/3.0, 2.0/3.0, -1.0);
  let p = mix(vec4<f32>(c.z, c.y, k.w, k.z), vec4<f32>(c.y, c.z, k.x, k.y), step(c.z, c.y));
  let q = mix(vec4<f32>(p.x, p.y, p.w, c.x), vec4<f32>(c.x, p.y, p.z, p.x), step(p.x, c.x));
  let d = q.x - min(q.w, q.y);
  let l = q.x - d * 0.5;
  let e = 1.0e-10;
  let h = abs(q.z + (q.w - q.y) / (6.0 * d + e));
  let s = d / (1.0 - abs(2.0 * l - 1.0) + e);
  return vec3<f32>(h, s, l);"""

  // ---------------------------------------------------------------------------
  // HSV → RGB (Iñigo Quilez piecewise-linear, plus smoothed variants)
  // ---------------------------------------------------------------------------

  /** HSV → RGB (Iñigo Quilez piecewise-linear). Input `(hue, saturation,
    * value)` in [0, 1]; output RGB in [0, 1]. Cheapest of the four hsv→rgb
    * variants — visible color-band edges where the ramp changes slope.
    */
  val hsv2rgb: WgslFn[(c: Vec3), Vec3] =
    WgslFn.raw("hsv2rgb"):
      """  let rgb = clamp(abs(((c.x * 6.0 + vec3<f32>(0.0, 4.0, 2.0)) % 6.0) - 3.0) - 1.0, vec3<f32>(0.0), vec3<f32>(1.0));
  return c.z * mix(vec3<f32>(1.0), rgb, c.y);"""

  /** HSV → RGB with cubic smoothstep on the rgb ramp (`t·t·(3 − 2·t)`). Removes
    * the slope discontinuities of [[hsv2rgb]] for ~free.
    */
  val hsv2rgbSmooth: WgslFn[(c: Vec3), Vec3] =
    WgslFn.raw("hsv2rgb_smooth"):
      """  let t = clamp(abs(((c.x * 6.0 + vec3<f32>(0.0, 4.0, 2.0)) % 6.0) - 3.0) - 1.0, vec3<f32>(0.0), vec3<f32>(1.0));
  let rgb = t * t * (vec3<f32>(3.0) - 2.0 * t);
  return c.z * mix(vec3<f32>(1.0), rgb, c.y);"""

  /** HSV → RGB with quintic smootherstep (`t³·(t·(t·6 − 15) + 10)`). Smoother
    * than [[hsv2rgbSmooth]] at second-derivative-continuous cost.
    */
  val hsv2rgbSmoother: WgslFn[(c: Vec3), Vec3] =
    WgslFn.raw("hsv2rgb_smoother"):
      """  let t = clamp(abs(((c.x * 6.0 + vec3<f32>(0.0, 4.0, 2.0)) % 6.0) - 3.0) - 1.0, vec3<f32>(0.0), vec3<f32>(1.0));
  let rgb = t * t * t * (t * (t * 6.0 - 15.0) + 10.0);
  return c.z * mix(vec3<f32>(1.0), rgb, c.y);"""

  /** HSV → RGB with cosine-based smoothing. Experimental — visually almost
    * identical to [[hsv2rgbSmooth]] but evaluates three `cos`es per fragment.
    * Prefer [[hsv2rgbSmooth]] in practice;
    */
  val hsv2rgbSmoothest: WgslFn[(c: Vec3), Vec3] =
    WgslFn.raw("hsv2rgb_smoothest"):
      """  let t = clamp(abs(((c.x * 6.0 + vec3<f32>(0.0, 4.0, 2.0)) % 6.0) - 3.0) - 1.0, vec3<f32>(0.0), vec3<f32>(1.0));
  let pi = 3.14159265358979;
  let rgb = cos((t + vec3<f32>(1.0)) * pi) * 0.5 + 0.5;
  return c.z * mix(vec3<f32>(1.0), rgb, c.y);"""

  // ---------------------------------------------------------------------------
  // HSL → RGB
  // ---------------------------------------------------------------------------

  /** HSL → RGB. Input `(hue, saturation, lightness)` in [0, 1]; output RGB in
    * [0, 1]. The natural inverse for [[rgb2hsl]].
    *
    * Lightness scales symmetrically: `L = 0` is black, `L = 1` is white,
    * `L = 0.5` is the fully-saturated hue. This differs from HSV `value`, where
    * the fully-saturated hue is at `V = 1`.
    */
  val hsl2rgb: WgslFn[(c: Vec3), Vec3] =
    WgslFn.raw("hsl2rgb"):
      """  let rgb = clamp(abs(((c.x * 6.0 + vec3<f32>(0.0, 4.0, 2.0)) % 6.0) - 3.0) - 1.0, vec3<f32>(0.0), vec3<f32>(1.0));
  return c.z + c.y * (rgb - 0.5) * (1.0 - abs(2.0 * c.z - 1.0));"""
