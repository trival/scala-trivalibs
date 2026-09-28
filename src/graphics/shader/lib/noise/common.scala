package trivalibs.graphics.shader.lib.noise

// Shared WGSL helpers of the noise family, as in stegu/webgl-noise
// (https://github.com/stegu/webgl-noise, MIT): the floor-based mod289 /
// mod7 (WGSL `%` is a remainder, not a modulo), the (34x² + 10x) mod 289
// permutation polynomial and the Taylor inverse square root. Plus the seed
// hash that turns a scalar seed into the two permute offsets of the seeded
// variants.

import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.Vec4
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given
import trivalibs.graphics.shader.lib.random.Hash

private[lib] object NoiseCommon:

  lazy val mod289v1: WgslFn[(x: Float), Float] =
    WgslFn.raw("noise_mod289_1")("  return x - floor(x * (1.0 / 289.0)) * 289.0;")

  lazy val mod289v2: WgslFn[(x: Vec2), Vec2] =
    WgslFn.raw("noise_mod289_2")("  return x - floor(x * (1.0 / 289.0)) * 289.0;")

  lazy val mod289v3: WgslFn[(x: Vec3), Vec3] =
    WgslFn.raw("noise_mod289_3")("  return x - floor(x * (1.0 / 289.0)) * 289.0;")

  lazy val mod289v4: WgslFn[(x: Vec4), Vec4] =
    WgslFn.raw("noise_mod289_4")("  return x - floor(x * (1.0 / 289.0)) * 289.0;")

  lazy val mod7v3: WgslFn[(x: Vec3), Vec3] =
    WgslFn.raw("noise_mod7_3")("  return x - floor(x * (1.0 / 7.0)) * 7.0;")

  lazy val permute1: WgslFn[(x: Float), Float] =
    WgslFn
      .raw("noise_permute_1")("  return noise_mod289_1(((x * 34.0) + 10.0) * x);")
      .withDeps(mod289v1)

  lazy val permute3: WgslFn[(x: Vec3), Vec3] =
    WgslFn
      .raw("noise_permute_3")("  return noise_mod289_3(((x * 34.0) + 10.0) * x);")
      .withDeps(mod289v3)

  lazy val permute4: WgslFn[(x: Vec4), Vec4] =
    WgslFn
      .raw("noise_permute_4")("  return noise_mod289_4(((x * 34.0) + 10.0) * x);")
      .withDeps(mod289v4)

  lazy val taylorInvSqrt1: WgslFn[(r: Float), Float] =
    WgslFn.raw("noise_taylor_inv_sqrt_1")(
      "  return 1.79284291400159 - 0.85373472095314 * r;",
    )

  lazy val taylorInvSqrt4: WgslFn[(r: Vec4), Vec4] =
    WgslFn.raw("noise_taylor_inv_sqrt_4")(
      "  return 1.79284291400159 - 0.85373472095314 * r;",
    )

  /** Two permute offsets in `[0, 289)` from a scalar seed, hashed from its f32
    * bit pattern, so any change of seed gives an unrelated field. Integer
    * offsets keep every permute input exact in f32 (and so identical to the
    * CPU tables).
    */
  lazy val seedOffsets: WgslFn[(seed: Float), Vec2] =
    WgslFn
      .raw("noise_seed_offsets")("""  let h = hash1i(bitcast<u32>(seed));
  return vec2<f32>(f32(h % 289u), f32((h / 289u) % 289u));""")
      .withDeps(Hash.wgsl.hash1i)
