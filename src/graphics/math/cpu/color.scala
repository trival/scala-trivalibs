package trivalibs.graphics.math.cpu

// CPU counterparts of the WGSL conversions in
// `trivalibs.graphics.shader.lib.color` — same Iñigo Quilez formulation
// (https://www.shadertoy.com/view/MsS3Wc), so a color computed on the CPU and
// uploaded as a uniform matches one computed shader-side from the same HSV.
//
// Both sides are postfix ops on their vector type (`c.hsv2rgb`), so the two
// imports coexist in one file and the same call reads the same in CPU code and
// in a shader body.
//
// HSL vs HSV — both share hue and saturation axes but differ in the third
// channel:
//   - HSV "value" V = max(R, G, B). Pure red is (0, 1, 1), white is (0, 0, 1),
//     black is (0, 0, 0).
//   - HSL "lightness" L = (max + min) / 2. Pure red is (0, 1, 0.5), white is
//     (0, 0, 1), black is (0, 0, 0).
// They are NOT round-trip-equivalent: rgb2hsl(hsv2rgb(c)) ≠ c, and likewise.
// Hue is in [0, 1] (1.0 == 360°) throughout, never degrees.

import trivalibs.graphics.math.Vec3Base
import trivalibs.graphics.math.Vec3ImmutableOps
import trivalibs.utils.numbers.NumExt.given

/** The IQ hue ramp: one channel of
  * `clamp(abs(((h·6 + k) mod 6) − 3) − 1, 0, 1)`, offset `k` being 0 / 4 / 2
  * for R / G / B.
  */
private inline def hueRamp(h6: Double, k: Double): Double =
  ((((h6 + k) % 6.0) - 3.0).abs - 1.0).clamp01

extension [Vec](c: Vec)(using base: Vec3Base[Vec], ops: Vec3ImmutableOps[Vec])

  /** HSV → RGB (piecewise-linear). Reads `c` as `(hue, saturation, value)` in
    * [0, 1]; returns RGB in [0, 1]. Cheapest of the four hsv→rgb variants —
    * visible color-band edges where the ramp changes slope.
    */
  def hsv2rgb: Vec =
    val h6 = c.x * 6.0
    val s = c.y
    val v = c.z
    ops.create(
      v * (1.0 + (hueRamp(h6, 0.0) - 1.0) * s),
      v * (1.0 + (hueRamp(h6, 4.0) - 1.0) * s),
      v * (1.0 + (hueRamp(h6, 2.0) - 1.0) * s),
    )

  /** HSV → RGB with cubic smoothstep on the rgb ramp (`t·t·(3 − 2·t)`). Removes
    * the slope discontinuities of [[hsv2rgb]].
    */
  def hsv2rgbSmooth: Vec =
    inline def smooth(t: Double) = t * t * (3.0 - 2.0 * t)
    val h6 = c.x * 6.0
    val s = c.y
    val v = c.z
    ops.create(
      v * (1.0 + (smooth(hueRamp(h6, 0.0)) - 1.0) * s),
      v * (1.0 + (smooth(hueRamp(h6, 4.0)) - 1.0) * s),
      v * (1.0 + (smooth(hueRamp(h6, 2.0)) - 1.0) * s),
    )

  /** HSV → RGB with quintic smootherstep (`t³·(t·(t·6 − 15) + 10)`). Smoother
    * than [[hsv2rgbSmooth]].
    */
  def hsv2rgbSmoother: Vec =
    inline def smoother(t: Double) = t * t * t * (t * (t * 6.0 - 15.0) + 10.0)
    val h6 = c.x * 6.0
    val s = c.y
    val v = c.z
    ops.create(
      v * (1.0 + (smoother(hueRamp(h6, 0.0)) - 1.0) * s),
      v * (1.0 + (smoother(hueRamp(h6, 4.0)) - 1.0) * s),
      v * (1.0 + (smoother(hueRamp(h6, 2.0)) - 1.0) * s),
    )

  /** HSL → RGB. Reads `c` as `(hue, saturation, lightness)` in [0, 1]; returns
    * RGB in [0, 1]. The natural inverse of [[rgb2hsl]].
    *
    * Lightness scales symmetrically: `L = 0` is black, `L = 1` is white,
    * `L = 0.5` is the fully-saturated hue — unlike HSV `value`, where the
    * fully-saturated hue sits at `V = 1`.
    */
  def hsl2rgb: Vec =
    val h6 = c.x * 6.0
    val s = c.y
    val l = c.z
    val chroma = 1.0 - (2.0 * l - 1.0).abs
    ops.create(
      l + s * (hueRamp(h6, 0.0) - 0.5) * chroma,
      l + s * (hueRamp(h6, 4.0) - 0.5) * chroma,
      l + s * (hueRamp(h6, 2.0) - 0.5) * chroma,
    )

  /** RGB → HSV. Reads `c` as `(r, g, b)` in [0, 1]; returns
    * `(hue, saturation, value)` with `value = max(r, g, b)`. The natural
    * inverse of [[hsv2rgb]].
    */
  def rgb2hsv: Vec =
    // The WGSL version encodes the two orderings as `mix`/`step` over a vec4;
    // on the CPU the same two branches are cheaper and clearer.
    val (px, py, pz, pw) =
      if c.y >= c.z then (c.y, c.z, 0.0, -1.0 / 3.0)
      else (c.z, c.y, -1.0, 2.0 / 3.0)
    val (qx, qy, qz, qw) =
      if c.x >= px then (c.x, py, pz, px)
      else (px, py, pw, c.x)
    val d = qx - qy.min(qw)
    val e = 1.0e-10
    ops.create((qz + (qw - qy) / (6.0 * d + e)).abs, d / (qx + e), qx)

  /** RGB → HSL. Reads `c` as `(r, g, b)` in [0, 1]; returns
    * `(hue, saturation, lightness)` with `lightness = (max + min) / 2`. The
    * natural inverse of [[hsl2rgb]] — saturation uses the HSL formula
    * `chroma / (1 − |2L − 1|)`, which differs from HSV saturation.
    */
  def rgb2hsl: Vec =
    val (px, py, pz, pw) =
      if c.y >= c.z then (c.y, c.z, 0.0, -1.0 / 3.0)
      else (c.z, c.y, -1.0, 2.0 / 3.0)
    val (qx, qy, qz, qw) =
      if c.x >= px then (c.x, py, pz, px)
      else (px, py, pw, c.x)
    val d = qx - qy.min(qw)
    val l = qx - d * 0.5
    val e = 1.0e-10
    ops.create(
      (qz + (qw - qy) / (6.0 * d + e)).abs,
      d / (1.0 - (2.0 * l - 1.0).abs + e),
      l,
    )
