package trivalibs.graphics.lib.noise

// CPU simplex noise. The field is stegu/webgl-noise's (Ashima / McEwan,
// Gustavson; MIT) — the same skewing, hash and gradients as the GPU
// `trivalibs.graphics.shader.lib.noise.Simplex`, via `NoiseTables`. The
// execution follows jwagner/simplex-noise.js (MIT): scalar locals, table
// lookups, and an early out per corner where the WGSL computes `max(0, t)`
// for all corners. Where the two disagree, Ashima wins (for the field).

import scala.scalajs.js.typedarray.Float64Array
import scala.scalajs.js.typedarray.Int32Array
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.Vec4
import trivalibs.utils.numbers.NumExt.given

/** Simplex noise (value only), 2D / 3D / 4D, on the CPU — same names,
  * parameters, defaults and ranges as the GPU `Simplex`.
  *
  * Everything here is `inline` and erases to one direct [[kernel]] call. Hot
  * loops over raw components can call the kernels directly.
  */
object Simplex:

  /** Noise value at `pos`, in about `[-1, 1]`. */
  inline def noise2d(pos: Vec2, inline seed: Double | Null = null): Double =
    inline seed match
      case null => kernel.noise2d(pos.x, pos.y)
      case s: Double => kernel.noise2dSeeded(pos.x, pos.y, s)

  /** Noise value at `pos`, in about `[-1, 1]`. */
  inline def noise3d(pos: Vec3, inline seed: Double | Null = null): Double =
    inline seed match
      case null => kernel.noise3d(pos.x, pos.y, pos.z)
      case s: Double => kernel.noise3dSeeded(pos.x, pos.y, pos.z, s)

  /** Noise value at `pos`, in about `[-1, 1]`. */
  inline def noise4d(pos: Vec4, inline seed: Double | Null = null): Double =
    inline seed match
      case null => kernel.noise4d(pos.x, pos.y, pos.z, pos.w)
      case s: Double => kernel.noise4dSeeded(pos.x, pos.y, pos.z, pos.w, s)

  /** Fractal Brownian motion, the amplitude-weighted average of `octaves`
    * layers (strictly `[-1, 1]`).
    */
  inline def fbm2d(
      pos: Vec2,
      octaves: Int = 4,
      lacunarity: Double = 2.0,
      gain: Double = 0.5,
      inline seed: Double | Null = null,
  ): Double =
    inline seed match
      case null => kernel.fbm2d(pos.x, pos.y, octaves, lacunarity, gain)
      case s: Double => kernel.fbm2dSeeded(pos.x, pos.y, octaves, lacunarity, gain, s)

  inline def fbm3d(
      pos: Vec3,
      octaves: Int = 4,
      lacunarity: Double = 2.0,
      gain: Double = 0.5,
      inline seed: Double | Null = null,
  ): Double =
    inline seed match
      case null => kernel.fbm3d(pos.x, pos.y, pos.z, octaves, lacunarity, gain)
      case s: Double =>
        kernel.fbm3dSeeded(pos.x, pos.y, pos.z, octaves, lacunarity, gain, s)

  inline def fbm4d(
      pos: Vec4,
      octaves: Int = 4,
      lacunarity: Double = 2.0,
      gain: Double = 0.5,
      inline seed: Double | Null = null,
  ): Double =
    inline seed match
      case null =>
        kernel.fbm4d(pos.x, pos.y, pos.z, pos.w, octaves, lacunarity, gain)
      case s: Double =>
        kernel.fbm4dSeeded(pos.x, pos.y, pos.z, pos.w, octaves, lacunarity, gain, s)

  /** Seamlessly tiling 2D noise: 4D simplex on a torus, `pos` in `[0, 1]`. */
  inline def torusNoise2d(
      pos: Vec2,
      scale: Double,
      inline seed: Double | Null = null,
  ): Double =
    inline seed match
      case null => kernel.torusNoise2d(pos.x, pos.y, scale)
      case s: Double => kernel.torusNoise2dSeeded(pos.x, pos.y, scale, s)

  /** Scalar kernels, one per code path. */
  object kernel:
    import NoiseTables.*

    private inline val F2c = 0.366025403784439
    private inline val G2c = 0.211324865405187
    private inline val G2z = -0.577350269189626

    /** The seed stages on a hashed corner `p`, stopping before the last
      * permute: the composite `permGrad*` tables apply it.
      */
    private inline def seededIndex(P: Int32Array, p: Int, s1: Int, s2: Int): Int =
      P(p + s1) + s2

    // ---- 2D ----

    private inline def impl2(
        x: Double,
        y: Double,
        inline isSeeded: Boolean,
        s1: Int,
        s2: Int,
    ): Double =
      val P = perm
      val gx = permGrad2x
      val gy = permGrad2y
      val s = (x + y) * F2c
      val i = (x + s).floor
      val j = (y + s).floor
      val t = (i + j) * G2c
      val x0 = x - i + t
      val y0 = y - j + t
      val i1 = if x0 > y0 then 1 else 0
      val j1 = 1 - i1
      val x1 = x0 + G2c - i1
      val y1 = y0 + G2c - j1
      val x2 = x0 + G2z
      val y2 = y0 + G2z
      val ii = mod289(i)
      val jj = mod289(j)
      // p* index the composite tables: the corner hash is permGrad(p*).
      var p0 = P(jj) + ii
      var p1 = P(jj + j1) + ii + i1
      var p2 = P(jj + 1) + ii + 1
      inline if isSeeded then
        p0 = seededIndex(P, P(p0), s1, s2)
        p1 = seededIndex(P, P(p1), s1, s2)
        p2 = seededIndex(P, P(p2), s1, s2)
      var n = 0.0
      var t0 = 0.5 - x0 * x0 - y0 * y0
      if t0 > 0.0 then
        t0 *= t0
        n += t0 * t0 * (gx(p0) * x0 + gy(p0) * y0)
      var t1 = 0.5 - x1 * x1 - y1 * y1
      if t1 > 0.0 then
        t1 *= t1
        n += t1 * t1 * (gx(p1) * x1 + gy(p1) * y1)
      var t2 = 0.5 - x2 * x2 - y2 * y2
      if t2 > 0.0 then
        t2 *= t2
        n += t2 * t2 * (gx(p2) * x2 + gy(p2) * y2)
      130.0 * n

    def noise2d(x: Double, y: Double): Double = impl2(x, y, false, 0, 0)

    private def noise2dSo(x: Double, y: Double, s1: Int, s2: Int): Double =
      impl2(x, y, true, s1, s2)

    def noise2dSeeded(x: Double, y: Double, seed: Double): Double =
      val so = seedOffsets(seed)
      noise2dSo(x, y, so % 289, so / 289)

    // ---- 3D ----

    private inline def contrib3(
        gx: Float64Array,
        gy: Float64Array,
        gz: Float64Array,
        p: Int,
        x: Double,
        y: Double,
        z: Double,
    ): Double =
      var t = 0.5 - x * x - y * y - z * z
      if t > 0.0 then
        t *= t
        t * t * (gx(p) * x + gy(p) * y + gz(p) * z)
      else 0.0

    private inline def impl3(
        x: Double,
        y: Double,
        z: Double,
        inline isSeeded: Boolean,
        s1: Int,
        s2: Int,
    ): Double =
      val P = perm
      val g3x = permGrad3x
      val g3y = permGrad3y
      val g3z = permGrad3z
      val s = (x + y + z) * (1.0 / 3.0)
      val i = (x + s).floor
      val j = (y + s).floor
      val k = (z + s).floor
      val t = (i + j + k) * (1.0 / 6.0)
      val x0 = x - i + t
      val y0 = y - j + t
      val z0 = z - k + t
      // webgl-noise: g = step(x0.yzx, x0.xyz), l = 1 - g,
      // i1 = min(g.xyz, l.zxy), i2 = max(g.xyz, l.zxy)
      val gx = if x0 >= y0 then 1 else 0
      val gy = if y0 >= z0 then 1 else 0
      val gz = if z0 >= x0 then 1 else 0
      val i1 = Math.min(gx, 1 - gz)
      val j1 = Math.min(gy, 1 - gx)
      val k1 = Math.min(gz, 1 - gy)
      val i2 = Math.max(gx, 1 - gz)
      val j2 = Math.max(gy, 1 - gx)
      val k2 = Math.max(gz, 1 - gy)
      val x1 = x0 - i1 + (1.0 / 6.0)
      val y1 = y0 - j1 + (1.0 / 6.0)
      val z1 = z0 - k1 + (1.0 / 6.0)
      val x2 = x0 - i2 + (1.0 / 3.0)
      val y2 = y0 - j2 + (1.0 / 3.0)
      val z2 = z0 - k2 + (1.0 / 3.0)
      val x3 = x0 - 0.5
      val y3 = y0 - 0.5
      val z3 = z0 - 0.5
      val ii = mod289(i)
      val jj = mod289(j)
      val kk = mod289(k)
      var p0 = P(P(kk) + jj) + ii
      var p1 = P(P(kk + k1) + jj + j1) + ii + i1
      var p2 = P(P(kk + k2) + jj + j2) + ii + i2
      var p3 = P(P(kk + 1) + jj + 1) + ii + 1
      inline if isSeeded then
        p0 = seededIndex(P, P(p0), s1, s2)
        p1 = seededIndex(P, P(p1), s1, s2)
        p2 = seededIndex(P, P(p2), s1, s2)
        p3 = seededIndex(P, P(p3), s1, s2)
      105.0 * (contrib3(g3x, g3y, g3z, p0, x0, y0, z0) +
        contrib3(g3x, g3y, g3z, p1, x1, y1, z1) +
        contrib3(g3x, g3y, g3z, p2, x2, y2, z2) +
        contrib3(g3x, g3y, g3z, p3, x3, y3, z3))

    def noise3d(x: Double, y: Double, z: Double): Double =
      impl3(x, y, z, false, 0, 0)

    private def noise3dSo(x: Double, y: Double, z: Double, s1: Int, s2: Int): Double =
      impl3(x, y, z, true, s1, s2)

    def noise3dSeeded(x: Double, y: Double, z: Double, seed: Double): Double =
      val so = seedOffsets(seed)
      noise3dSo(x, y, z, so % 289, so / 289)

    // ---- 4D ----

    private inline val G4 = 0.138196601125011
    private inline val F4 = 0.309016994374947451

    private inline def contrib4(
        gx: Float64Array,
        gy: Float64Array,
        gz: Float64Array,
        gw: Float64Array,
        p: Int,
        x: Double,
        y: Double,
        z: Double,
        w: Double,
    ): Double =
      var t = 0.57 - x * x - y * y - z * z - w * w
      if t > 0.0 then
        t *= t
        t * t * (gx(p) * x + gy(p) * y + gz(p) * z + gw(p) * w)
      else 0.0

    private inline def impl4(
        x: Double,
        y: Double,
        z: Double,
        w: Double,
        inline isSeeded: Boolean,
        s1: Int,
        s2: Int,
    ): Double =
      val P = perm
      val g4x = permGrad4x
      val g4y = permGrad4y
      val g4z = permGrad4z
      val g4w = permGrad4w
      val s = (x + y + z + w) * F4
      val i = (x + s).floor
      val j = (y + s).floor
      val k = (z + s).floor
      val l = (w + s).floor
      val t = (i + j + k + l) * G4
      val x0 = x - i + t
      val y0 = y - j + t
      val z0 = z - k + t
      val w0 = w - l + t
      // webgl-noise rank sorting: isX = step(x0.yzw, x0.xxx),
      // isYZ = step(x0.zww, x0.yyz)
      val isXx = if x0 >= y0 then 1 else 0
      val isXy = if x0 >= z0 then 1 else 0
      val isXz = if x0 >= w0 then 1 else 0
      val isYZx = if y0 >= z0 then 1 else 0
      val isYZy = if y0 >= w0 then 1 else 0
      val isYZz = if z0 >= w0 then 1 else 0
      val rx = isXx + isXy + isXz
      val ry = 1 - isXx + isYZx + isYZy
      val rz = 1 - isXy + 1 - isYZx + isYZz
      val rw = 1 - isXz + 1 - isYZy + 1 - isYZz
      val i3 = if rx >= 1 then 1 else 0
      val j3 = if ry >= 1 then 1 else 0
      val k3 = if rz >= 1 then 1 else 0
      val l3 = if rw >= 1 then 1 else 0
      val i2 = if rx >= 2 then 1 else 0
      val j2 = if ry >= 2 then 1 else 0
      val k2 = if rz >= 2 then 1 else 0
      val l2 = if rw >= 2 then 1 else 0
      val i1 = if rx >= 3 then 1 else 0
      val j1 = if ry >= 3 then 1 else 0
      val k1 = if rz >= 3 then 1 else 0
      val l1 = if rw >= 3 then 1 else 0
      val x1 = x0 - i1 + G4
      val y1 = y0 - j1 + G4
      val z1 = z0 - k1 + G4
      val w1 = w0 - l1 + G4
      val x2 = x0 - i2 + 2.0 * G4
      val y2 = y0 - j2 + 2.0 * G4
      val z2 = z0 - k2 + 2.0 * G4
      val w2 = w0 - l2 + 2.0 * G4
      val x3 = x0 - i3 + 3.0 * G4
      val y3 = y0 - j3 + 3.0 * G4
      val z3 = z0 - k3 + 3.0 * G4
      val w3 = w0 - l3 + 3.0 * G4
      val x4 = x0 - 1.0 + 4.0 * G4
      val y4 = y0 - 1.0 + 4.0 * G4
      val z4 = z0 - 1.0 + 4.0 * G4
      val w4 = w0 - 1.0 + 4.0 * G4
      val ii = mod289(i)
      val jj = mod289(j)
      val kk = mod289(k)
      val ll = mod289(l)
      var p0 = P(P(P(ll) + kk) + jj) + ii
      var p1 = P(P(P(ll + l1) + kk + k1) + jj + j1) + ii + i1
      var p2 = P(P(P(ll + l2) + kk + k2) + jj + j2) + ii + i2
      var p3 = P(P(P(ll + l3) + kk + k3) + jj + j3) + ii + i3
      var p4 = P(P(P(ll + 1) + kk + 1) + jj + 1) + ii + 1
      inline if isSeeded then
        p0 = seededIndex(P, P(p0), s1, s2)
        p1 = seededIndex(P, P(p1), s1, s2)
        p2 = seededIndex(P, P(p2), s1, s2)
        p3 = seededIndex(P, P(p3), s1, s2)
        p4 = seededIndex(P, P(p4), s1, s2)
      60.1 * (contrib4(g4x, g4y, g4z, g4w, p0, x0, y0, z0, w0) +
        contrib4(g4x, g4y, g4z, g4w, p1, x1, y1, z1, w1) +
        contrib4(g4x, g4y, g4z, g4w, p2, x2, y2, z2, w2) +
        contrib4(g4x, g4y, g4z, g4w, p3, x3, y3, z3, w3) +
        contrib4(g4x, g4y, g4z, g4w, p4, x4, y4, z4, w4))

    def noise4d(x: Double, y: Double, z: Double, w: Double): Double =
      impl4(x, y, z, w, false, 0, 0)

    private def noise4dSo(
        x: Double,
        y: Double,
        z: Double,
        w: Double,
        s1: Int,
        s2: Int,
    ): Double = impl4(x, y, z, w, true, s1, s2)

    def noise4dSeeded(x: Double, y: Double, z: Double, w: Double, seed: Double): Double =
      val so = seedOffsets(seed)
      noise4dSo(x, y, z, w, so % 289, so / 289)

    // ---- fbm ----

    def fbm2d(x: Double, y: Double, octaves: Int, lacunarity: Double, gain: Double): Double =
      var sum = 0.0
      var amplitude = 1.0
      var total = 0.0
      var frequency = 1.0
      var i = 0
      while i < octaves do
        sum += noise2d(x * frequency, y * frequency) * amplitude
        total += amplitude
        amplitude *= gain
        frequency *= lacunarity
        i += 1
      sum / total

    def fbm2dSeeded(
        x: Double,
        y: Double,
        octaves: Int,
        lacunarity: Double,
        gain: Double,
        seed: Double,
    ): Double =
      val so = seedOffsets(seed)
      val s1 = so % 289
      val s2 = so / 289
      var sum = 0.0
      var amplitude = 1.0
      var total = 0.0
      var frequency = 1.0
      var i = 0
      while i < octaves do
        sum += noise2dSo(x * frequency, y * frequency, (s1 + i) % 289, s2) * amplitude
        total += amplitude
        amplitude *= gain
        frequency *= lacunarity
        i += 1
      sum / total

    def fbm3d(
        x: Double,
        y: Double,
        z: Double,
        octaves: Int,
        lacunarity: Double,
        gain: Double,
    ): Double =
      var sum = 0.0
      var amplitude = 1.0
      var total = 0.0
      var frequency = 1.0
      var i = 0
      while i < octaves do
        sum += noise3d(x * frequency, y * frequency, z * frequency) * amplitude
        total += amplitude
        amplitude *= gain
        frequency *= lacunarity
        i += 1
      sum / total

    def fbm3dSeeded(
        x: Double,
        y: Double,
        z: Double,
        octaves: Int,
        lacunarity: Double,
        gain: Double,
        seed: Double,
    ): Double =
      val so = seedOffsets(seed)
      val s1 = so % 289
      val s2 = so / 289
      var sum = 0.0
      var amplitude = 1.0
      var total = 0.0
      var frequency = 1.0
      var i = 0
      while i < octaves do
        sum += noise3dSo(x * frequency, y * frequency, z * frequency, (s1 + i) % 289, s2) *
          amplitude
        total += amplitude
        amplitude *= gain
        frequency *= lacunarity
        i += 1
      sum / total

    def fbm4d(
        x: Double,
        y: Double,
        z: Double,
        w: Double,
        octaves: Int,
        lacunarity: Double,
        gain: Double,
    ): Double =
      var sum = 0.0
      var amplitude = 1.0
      var total = 0.0
      var frequency = 1.0
      var i = 0
      while i < octaves do
        sum += noise4d(x * frequency, y * frequency, z * frequency, w * frequency) *
          amplitude
        total += amplitude
        amplitude *= gain
        frequency *= lacunarity
        i += 1
      sum / total

    def fbm4dSeeded(
        x: Double,
        y: Double,
        z: Double,
        w: Double,
        octaves: Int,
        lacunarity: Double,
        gain: Double,
        seed: Double,
    ): Double =
      val so = seedOffsets(seed)
      val s1 = so % 289
      val s2 = so / 289
      var sum = 0.0
      var amplitude = 1.0
      var total = 0.0
      var frequency = 1.0
      var i = 0
      while i < octaves do
        sum += noise4dSo(
          x * frequency,
          y * frequency,
          z * frequency,
          w * frequency,
          (s1 + i) % 289,
          s2,
        ) * amplitude
        total += amplitude
        amplitude *= gain
        frequency *= lacunarity
        i += 1
      sum / total

    // ---- torus ----

    def torusNoise2d(x: Double, y: Double, scale: Double): Double =
      val ax = x * 6.28318530718
      val ay = y * 6.28318530718
      noise4d(ax.cos * scale, ax.sin * scale, ay.cos * scale, ay.sin * scale)

    def torusNoise2dSeeded(x: Double, y: Double, scale: Double, seed: Double): Double =
      val ax = x * 6.28318530718
      val ay = y * 6.28318530718
      noise4dSeeded(ax.cos * scale, ax.sin * scale, ay.cos * scale, ay.sin * scale, seed)
