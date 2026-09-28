package trivalibs.graphics.lib.noise

// CPU cellular ("Worley") noise. The field is stegu/webgl-noise's
// src/cellular2D.glsl / cellular3D.glsl (Stefan Gustavson; MIT) — same hash
// and feature-point offsets as the GPU `Worley`, via `NoiseTables`. Upstream
// sorts F1/F2 with vectorized swizzle sequences; the natural CPU form of the
// same computation is a loop over the 3×3 (×3) cells tracking F1 and F2 as
// scalars.

import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.utils.numbers.NumExt.given

/** Cellular noise on the CPU: `Vec2(F1, F2)` — same names, parameters and
  * defaults as the GPU `Worley`.
  */
object Worley:

  inline def noise2d(
      pos: Vec2,
      jitter: Double = 1.0,
      inline seed: Double | Null = null,
  ): Vec2 =
    val out = new Vec2()
    inline seed match
      case null => kernel.noise2dInto(pos.x, pos.y, jitter, out)
      case s: Double => kernel.noise2dSeededInto(pos.x, pos.y, jitter, s, out)

  inline def noise3d(
      pos: Vec3,
      jitter: Double = 1.0,
      inline seed: Double | Null = null,
  ): Vec2 =
    val out = new Vec2()
    inline seed match
      case null => kernel.noise3dInto(pos.x, pos.y, pos.z, jitter, out)
      case s: Double => kernel.noise3dSeededInto(pos.x, pos.y, pos.z, jitter, s, out)

  /** Scalar kernels writing `(F1, F2)` into `out`. */
  object kernel:
    import NoiseTables.*

    /** The cell offset can take a lattice index to `-1`. The permutation of
      * an exact integer depends only on it mod 289, so `-1` reads entry 288.
      */
    private inline def wrapLow(i: Int): Int = if i < 0 then i + 289 else i

    private inline def impl2(
        x: Double,
        y: Double,
        jitter: Double,
        inline isSeeded: Boolean,
        s1: Int,
        s2: Int,
        out: Vec2,
    ): Vec2 =
      val P = perm
      val cox = cellOx
      val coy = cellOy
      val fx = x.floor
      val fy = y.floor
      val pix = mod289(fx)
      val piy = mod289(fy)
      val pfx = x - fx
      val pfy = y - fy
      var f1 = 1e10
      var f2 = 1e10
      var di = -1
      while di <= 1 do
        var px = P(wrapLow(pix + di))
        inline if isSeeded then px = P(P(px + s1) + s2)
        var dj = -1
        while dj <= 1 do
          val p = P(wrapLow(px + piy + dj))
          val dx = pfx - (di + 0.5) + jitter * cox(p)
          val dy = pfy - (dj + 0.5) + jitter * coy(p)
          val d = dx * dx + dy * dy
          if d < f1 then
            f2 = f1
            f1 = d
          else if d < f2 then f2 = d
          dj += 1
        di += 1
      out.x = f1.sqrt
      out.y = f2.sqrt
      out

    def noise2dInto(x: Double, y: Double, jitter: Double, out: Vec2): Vec2 =
      impl2(x, y, jitter, false, 0, 0, out)

    def noise2dSeededInto(x: Double, y: Double, jitter: Double, seed: Double, out: Vec2): Vec2 =
      val so = seedOffsets(seed)
      impl2(x, y, jitter, true, so % 289, so / 289, out)

    private inline def impl3(
        x: Double,
        y: Double,
        z: Double,
        jitter: Double,
        inline isSeeded: Boolean,
        s1: Int,
        s2: Int,
        out: Vec2,
    ): Vec2 =
      val P = perm
      val cox = cellOx
      val coy = cellOy
      val coz = cellOz
      val fx = x.floor
      val fy = y.floor
      val fz = z.floor
      val pix = mod289(fx)
      val piy = mod289(fy)
      val piz = mod289(fz)
      val pfx = x - fx - 0.5
      val pfy = y - fy - 0.5
      val pfz = z - fz - 0.5
      var f1 = 1e10
      var f2 = 1e10
      var di = -1
      while di <= 1 do
        var p = P(wrapLow(pix + di))
        inline if isSeeded then p = P(P(p + s1) + s2)
        var dj = -1
        while dj <= 1 do
          val pj = P(wrapLow(p + piy + dj))
          var dk = -1
          while dk <= 1 do
            val pk = P(wrapLow(pj + piz + dk))
            val dx = pfx - di + jitter * cox(pk)
            val dy = pfy - dj + jitter * coy(pk)
            val dz = pfz - dk + jitter * coz(pk)
            val d = dx * dx + dy * dy + dz * dz
            if d < f1 then
              f2 = f1
              f1 = d
            else if d < f2 then f2 = d
            dk += 1
          dj += 1
        di += 1
      out.x = f1.sqrt
      out.y = f2.sqrt
      out

    def noise3dInto(x: Double, y: Double, z: Double, jitter: Double, out: Vec2): Vec2 =
      impl3(x, y, z, jitter, false, 0, 0, out)

    def noise3dSeededInto(
        x: Double,
        y: Double,
        z: Double,
        jitter: Double,
        seed: Double,
        out: Vec2,
    ): Vec2 =
      val so = seedOffsets(seed)
      impl3(x, y, z, jitter, true, so % 289, so / 289, out)
