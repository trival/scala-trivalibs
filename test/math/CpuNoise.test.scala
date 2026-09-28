package trivalibs.graphics.lib.noise

import munit.FunSuite
import trivalibs.graphics.math.cpu.{*, given}

// CPU noise: table construction against a table-free port of the upstream
// GLSL, ranges, fbm normalization, seeds, tiling, analytic gradients, and the
// CPU branch of the shared extensions.
class CpuNoiseTest extends FunSuite:

  // ---- deterministic sample points ----

  private val rng = new scala.util.Random(1234)
  private val points2 = Seq.fill(500)((rng.nextDouble() * 40 - 20, rng.nextDouble() * 40 - 20))
  private val points3 =
    Seq.fill(500)((rng.nextDouble() * 40 - 20, rng.nextDouble() * 40 - 20, rng.nextDouble() * 40 - 20))

  // ---- straight port of webgl-noise noise2D.glsl / noise3D.glsl, f64 ----

  private def mod289(x: Double) = x - Math.floor(x * (1.0 / 289.0)) * 289.0
  private def permute(x: Double) = mod289(((x * 34.0) + 10.0) * x)
  private def fract(x: Double) = x - Math.floor(x)

  private def ref2(vx: Double, vy: Double): Double =
    val (cx, cy, cz, cw) =
      (0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439)
    var ix = Math.floor(vx + (vx + vy) * cy)
    var iy = Math.floor(vy + (vx + vy) * cy)
    val x0 = Array(vx - ix + (ix + iy) * cx, vy - iy + (ix + iy) * cx)
    val i1 = if x0(0) > x0(1) then Array(1.0, 0.0) else Array(0.0, 1.0)
    val x12 = Array(x0(0) + cx - i1(0), x0(1) + cx - i1(1), x0(0) + cz, x0(1) + cz)
    ix = mod289(ix)
    iy = mod289(iy)
    val offY = Array(0.0, i1(1), 1.0)
    val offX = Array(0.0, i1(0), 1.0)
    val p = Array.tabulate(3)(k => permute(permute(iy + offY(k)) + ix + offX(k)))
    val xs = Array((x0(0), x0(1)), (x12(0), x12(1)), (x12(2), x12(3)))
    var sum = 0.0
    for k <- 0 until 3 do
      val (px, py) = xs(k)
      var m = Math.max(0.5 - (px * px + py * py), 0.0)
      m = m * m
      m = m * m
      val x = 2.0 * fract(p(k) * cw) - 1.0
      val h = Math.abs(x) - 0.5
      val a0 = x - Math.floor(x + 0.5)
      m *= 1.79284291400159 - 0.85373472095314 * (a0 * a0 + h * h)
      sum += m * (a0 * px + h * py)
    130.0 * sum

  private def ref3(vx: Double, vy: Double, vz: Double): Double =
    val s = (vx + vy + vz) / 3.0
    val i = Array(Math.floor(vx + s), Math.floor(vy + s), Math.floor(vz + s))
    val t = (i(0) + i(1) + i(2)) / 6.0
    val x0 = Array(vx - i(0) + t, vy - i(1) + t, vz - i(2) + t)
    val g = Array(
      if x0(0) >= x0(1) then 1.0 else 0.0,
      if x0(1) >= x0(2) then 1.0 else 0.0,
      if x0(2) >= x0(0) then 1.0 else 0.0,
    )
    val l = g.map(1.0 - _)
    val i1 = Array(Math.min(g(0), l(2)), Math.min(g(1), l(0)), Math.min(g(2), l(1)))
    val i2 = Array(Math.max(g(0), l(2)), Math.max(g(1), l(0)), Math.max(g(2), l(1)))
    val xs = Array(
      x0,
      Array.tabulate(3)(c => x0(c) - i1(c) + 1.0 / 6.0),
      Array.tabulate(3)(c => x0(c) - i2(c) + 1.0 / 3.0),
      Array.tabulate(3)(c => x0(c) - 0.5),
    )
    val ii = i.map(mod289)
    val offs = Array(Array(0.0, 0.0, 0.0), i1, i2, Array(1.0, 1.0, 1.0))
    // The gradient derivation runs in f32, as on the GPU: upstream's
    // n_ = 0.142857142857 is below 1/7 in f64 but above it in f32, which moves
    // floor(p · n_²) for hashes that are multiples of 49.
    def f32(v: Double) = v.toFloat.toDouble
    val n_ = f32(0.142857142857)
    val nsx = f32(n_ * 2.0)
    val nsy = f32(f32(n_ * 0.5) - 1.0)
    val nsz = n_
    var sum = 0.0
    for k <- 0 until 4 do
      val p = permute(permute(permute(ii(2) + offs(k)(2)) + ii(1) + offs(k)(1)) + ii(0) + offs(k)(0))
      val j = f32(p - f32(49.0 * Math.floor(f32(f32(p * nsz) * nsz))))
      val x_ = Math.floor(f32(j * nsz))
      val y_ = Math.floor(f32(j - f32(7.0 * x_)))
      val gx0 = f32(f32(x_ * nsx) + nsy)
      val gy0 = f32(f32(y_ * nsx) + nsy)
      val gz = 1.0 - Math.abs(gx0) - Math.abs(gy0)
      val sh = if gz <= 0.0 then -1.0 else 0.0
      val gx = gx0 + (Math.floor(gx0) * 2.0 + 1.0) * sh
      val gy = gy0 + (Math.floor(gy0) * 2.0 + 1.0) * sh
      val norm = 1.79284291400159 - 0.85373472095314 * (gx * gx + gy * gy + gz * gz)
      val x = xs(k)
      var m = Math.max(0.5 - (x(0) * x(0) + x(1) * x(1) + x(2) * x(2)), 0.0)
      m = m * m
      sum += m * m * norm * (gx * x(0) + gy * x(1) + gz * x(2))
    105.0 * sum

  // ---- table-free ports of psrdnoise2.glsl / psrdnoise3.glsl (value) ----

  private def gmod(x: Double, y: Double) = x - y * Math.floor(x / y)
  private def ppermute(x: Double) =
    val xm = gmod(x, 289.0)
    gmod((xm * 34.0 + 10.0) * xm, 289.0)

  private def refExt2(x: Double, y: Double, px: Double, py: Double, alpha: Double): Double =
    val uvx = x + y * 0.5
    val i0x = Math.floor(uvx)
    val i0y = Math.floor(y)
    val cmp = if (uvx - i0x) >= (y - i0y) then 1.0 else 0.0
    val o1 = (cmp, 1.0 - cmp)
    val v = Seq(
      (i0x - i0y * 0.5, i0y),
      (i0x - i0y * 0.5 + o1._1 - o1._2 * 0.5, i0y + o1._2),
      (i0x - i0y * 0.5 + 0.5, i0y + 1.0),
    )
    val i = Seq((i0x, i0y), (i0x + o1._1, i0y + o1._2), (i0x + 1.0, i0y + 1.0))
    var n = 0.0
    for k <- 0 until 3 do
      val (vx, vy) = v(k)
      val (iu, iv) =
        if px > 0.0 || py > 0.0 then
          val xw = if px > 0.0 then gmod(vx, px) else vx
          val yw = if py > 0.0 then gmod(vy, py) else vy
          (Math.floor(xw + 0.5 * yw + 0.5), Math.floor(yw + 0.5))
        else i(k)
      var hash = gmod(iu, 289.0)
      hash = gmod((hash * 51.0 + 2.0) * hash + iv, 289.0)
      hash = gmod((hash * 34.0 + 10.0) * hash, 289.0)
      val psi = hash * 0.07482 + alpha
      val dx = x - vx
      val dy = y - vy
      val w = Math.max(0.8 - (dx * dx + dy * dy), 0.0)
      n += w * w * w * w * (Math.cos(psi) * dx + Math.sin(psi) * dy)
    10.9 * n

  private def refExt3(x: Double, y: Double, z: Double, alpha: Double): Double =
    val uvw = Array(y + z, x + z, x + y)
    val i0 = uvw.map(Math.floor)
    val f0 = Array.tabulate(3)(c => uvw(c) - i0(c))
    val g_ = Array(
      if f0(1) >= f0(0) then 1.0 else 0.0,
      if f0(2) >= f0(1) then 1.0 else 0.0,
      if f0(2) >= f0(0) then 1.0 else 0.0,
    )
    val l_ = g_.map(1.0 - _)
    val g = Array(l_(2), g_(0), g_(1))
    val l = Array(l_(0), l_(1), g_(2))
    val o1 = Array.tabulate(3)(c => Math.min(g(c), l(c)))
    val o2 = Array.tabulate(3)(c => Math.max(g(c), l(c)))
    val is = Seq(i0, Array.tabulate(3)(c => i0(c) + o1(c)), Array.tabulate(3)(c => i0(c) + o2(c)), i0.map(_ + 1.0))
    var n = 0.0
    for i <- is do
      val v = Array(0.5 * (-i(0) + i(1) + i(2)), 0.5 * (i(0) - i(1) + i(2)), 0.5 * (i(0) + i(1) - i(2)))
      val d = Array(x - v(0), y - v(1), z - v(2))
      val hash = ppermute(ppermute(ppermute(i(2)) + i(1)) + i(0))
      val theta = hash * 3.883222077
      val sz = hash * -0.006920415 + 0.996539792
      val psi = hash * 0.108705628
      val ct = Math.cos(theta)
      val st = Math.sin(theta)
      val szp = Math.sqrt(1.0 - sz * sz)
      val (gx, gy, gz) =
        if alpha != 0.0 then
          val sp = Math.sin(psi)
          val cp = Math.cos(psi)
          val px = ct * szp
          val py = st * szp
          val ctp = st * sp - ct * cp
          val qx = ctp * st + (sp - ctp * st) * sz
          val qy = -ctp * ct + (cp + ctp * ct) * sz
          val qz = -(py * cp + px * sp)
          val sa = Math.sin(alpha)
          val ca = Math.cos(alpha)
          (ca * px + sa * qx, ca * py + sa * qy, ca * sz + sa * qz)
        else (ct * szp, st * szp, sz)
      val w = Math.max(0.5 - (d(0) * d(0) + d(1) * d(1) + d(2) * d(2)), 0.0)
      n += w * w * w * (gx * d(0) + gy * d(1) + gz * d(2))
    39.5 * n

  // ---- table-free port of cellular2D.glsl / cellular3D.glsl (F1) ----

  // The feature-point offsets run in f32, as on the GPU: upstream's
  // K = 0.142857142857 is below 1/7 in f64 but above it in f32, which flips
  // fract(p · K) between ~1 and ~0 for hashes that are multiples of 7.
  private def cellOffsets(p: Double): (Double, Double, Double) =
    def f32(v: Double) = v.toFloat.toDouble
    val k = f32(0.142857142857)
    val pk = f32(p * k)
    val ox = f32(f32(pk - Math.floor(pk)) - f32(0.428571428571))
    val fk = Math.floor(pk)
    val oy = f32(f32(f32(fk - f32(Math.floor(f32(fk * f32(1.0 / 7.0))) * 7.0)) * k) - f32(0.428571428571))
    val oz = f32(f32(Math.floor(f32(p * f32(0.020408163265306))) * f32(0.166666666667)) - f32(0.416666666667))
    (ox, oy, oz)

  private def refWorley2F1(x: Double, y: Double): Double =
    val pix = mod289(Math.floor(x))
    val piy = mod289(Math.floor(y))
    val pfx = x - Math.floor(x)
    val pfy = y - Math.floor(y)
    var f1 = 1e10
    for di <- -1 to 1; dj <- -1 to 1 do
      val p = permute(permute(pix + di) + piy + dj)
      val (ox, oy, _) = cellOffsets(p)
      val dx = pfx - (di + 0.5) + ox
      val dy = pfy - (dj + 0.5) + oy
      f1 = Math.min(f1, dx * dx + dy * dy)
    Math.sqrt(f1)

  private def refWorley3F1(x: Double, y: Double, z: Double): Double =
    val pi = Array(x, y, z).map(c => mod289(Math.floor(c)))
    val pf = Array(x, y, z).map(c => c - Math.floor(c) - 0.5)
    var f1 = 1e10
    for di <- -1 to 1; dj <- -1 to 1; dk <- -1 to 1 do
      val p = permute(permute(permute(pi(0) + di) + pi(1) + dj) + pi(2) + dk)
      val (ox, oy, oz) = cellOffsets(p)
      val dx = pf(0) - di + ox
      val dy = pf(1) - dj + oy
      val dz = pf(2) - dk + oz
      f1 = Math.min(f1, dx * dx + dy * dy + dz * dz)
    Math.sqrt(f1)

  // ---------------------------------------------------------------------------
  // Tables
  // ---------------------------------------------------------------------------

  test("extended 2D tables match the table-free psrdnoise2 port"):
    for (x, y) <- points2 do
      for (px, py, a) <- Seq((0.0, 0.0, 0.0), (0.0, 0.0, 0.9), (6.0, 4.0, 0.0), (5.0, 8.0, 2.1)) do
        assertEqualsDouble(
          Extended.kernel.noiseValue2d(x, y, px, py, a),
          refExt2(x, y, px, py, a),
          1e-5,
          s"($x, $y) p=($px, $py) a=$a",
        )

  test("extended 3D tables match the table-free psrdnoise3 port"):
    for (x, y, z) <- points3 do
      for a <- Seq(0.0, 0.9) do
        assertEqualsDouble(
          Extended.kernel.noiseValue3d(x, y, z, 0.0, 0.0, 0.0, a),
          refExt3(x, y, z, a),
          1e-5,
          s"($x, $y, $z) a=$a",
        )

  test("worley tables match the table-free cellular2D / cellular3D port"):
    val out = new Vec2()
    for (x, y, z) <- points3 do
      Worley.kernel.noise2dInto(x, y, 1.0, out)
      assertEqualsDouble(out.x, refWorley2F1(x, y), 1e-5, s"2D ($x, $y)")
      Worley.kernel.noise3dInto(x, y, z, 1.0, out)
      assertEqualsDouble(out.x, refWorley3F1(x, y, z), 1e-5, s"3D ($x, $y, $z)")

  test("permutation table equals Ashima's polynomial on integers"):
    for i <- 0 until 580 do assertEquals(NoiseTables.perm(i), permute(i.toDouble).toInt, s"i=$i")

  test("simplex 2D matches the table-free upstream port"):
    for (x, y) <- points2 do
      assertEqualsDouble(Simplex.kernel.noise2d(x, y), ref2(x, y), 1e-5, s"($x, $y)")

  test("simplex 3D matches the table-free upstream port"):
    for (x, y, z) <- points3 do
      assertEqualsDouble(Simplex.kernel.noise3d(x, y, z), ref3(x, y, z), 1e-5, s"($x, $y, $z)")

  // ---------------------------------------------------------------------------
  // Ranges and normalization
  // ---------------------------------------------------------------------------

  test("simplex 2D / 3D / 4D stay in about [-1, 1] and use the range"):
    val v2 = points2.map((x, y) => Simplex.kernel.noise2d(x, y))
    val v3 = points3.map((x, y, z) => Simplex.kernel.noise3d(x, y, z))
    val v4 = points3.map((x, y, z) => Simplex.kernel.noise4d(x, y, z, x - y))
    for vs <- Seq(v2, v3, v4) do
      assert(vs.forall(v => v >= -1.05 && v <= 1.05), vs.minBy(math.abs(_) * -1).toString)
      assert(vs.max - vs.min > 0.8, s"${vs.min} .. ${vs.max}")

  test("fbm is normalized: strictly inside [-1, 1] for any octaves / gain"):
    for
      octaves <- Seq(1, 3, 6)
      gain <- Seq(0.3, 0.5, 0.8, 1.0)
      (x, y) <- points2.take(100)
    do
      val v = Simplex.kernel.fbm2d(x, y, octaves, 2.0, gain)
      assert(v > -1.05 && v < 1.05, s"octaves=$octaves gain=$gain v=$v")

  // ---------------------------------------------------------------------------
  // Seeds
  // ---------------------------------------------------------------------------

  private def correlation(a: Seq[Double], b: Seq[Double]): Double =
    val ma = a.sum / a.size
    val mb = b.sum / b.size
    val cov = a.zip(b).map((x, y) => (x - ma) * (y - mb)).sum
    val va = a.map(x => (x - ma) * (x - ma)).sum
    val vb = b.map(y => (y - mb) * (y - mb)).sum
    cov / Math.sqrt(va * vb)

  test("seeded noise is deterministic"):
    for (x, y) <- points2.take(50) do
      assertEquals(Simplex.kernel.noise2dSeeded(x, y, 3.5), Simplex.kernel.noise2dSeeded(x, y, 3.5))

  test("nearby seeds give unrelated fields"):
    val a = points3.map((x, y, z) => Simplex.kernel.noise3dSeeded(x, y, z, 0.5))
    val b = points3.map((x, y, z) => Simplex.kernel.noise3dSeeded(x, y, z, 0.5000001))
    val plain = points3.map((x, y, z) => Simplex.kernel.noise3d(x, y, z))
    assert(Math.abs(correlation(a, b)) < 0.25, correlation(a, b).toString)
    assert(Math.abs(correlation(a, plain)) < 0.25, correlation(a, plain).toString)

  test("seed offsets are in [0, 289) and differ for close seeds"):
    val so = NoiseTables.seedOffsets(1.0)
    assert(so >= 0 && so < 289 * 289)
    assertNotEquals(NoiseTables.seedOffsets(1.0), NoiseTables.seedOffsets(1.0000001))

  // ---------------------------------------------------------------------------
  // Extended (psrdnoise)
  // ---------------------------------------------------------------------------

  test("extended 2D tiles with an integer period, seeded too"):
    val out = new Vec3()
    for (x, y) <- points2.take(100) do
      val a = Extended.kernel.noiseValue2d(x, y, 4.0, 6.0, 0.3)
      val b = Extended.kernel.noiseValue2d(x + 4.0, y + 6.0, 4.0, 6.0, 0.3)
      assertEqualsDouble(a, b, 1e-9)
      val sa = Extended.kernel.noiseValue2dSeeded(x, y, 4.0, 6.0, 0.0, 9.0)
      val sb = Extended.kernel.noiseValue2dSeeded(x + 8.0, y, 4.0, 6.0, 0.0, 9.0)
      assertEqualsDouble(sa, sb, 1e-9)

  test("extended 3D tiles with an integer period"):
    for (x, y, z) <- points3.take(100) do
      val a = Extended.kernel.noiseValue3d(x, y, z, 5.0, 0.0, 7.0, 0.0)
      val b = Extended.kernel.noiseValue3d(x + 5.0, y, z - 7.0, 5.0, 0.0, 7.0, 0.0)
      assertEqualsDouble(a, b, 1e-9)

  test("extended value kernels equal the gradient kernels' value"):
    val out2 = new Vec3()
    val out3 = new Vec4()
    for (x, y, z) <- points3.take(100) do
      Extended.kernel.noise2dInto(x, y, 0.0, 0.0, 1.1, out2)
      assertEqualsDouble(out2.x, Extended.kernel.noiseValue2d(x, y, 0.0, 0.0, 1.1), 1e-12)
      Extended.kernel.noise3dInto(x, y, z, 0.0, 0.0, 0.0, 1.1, out3)
      assertEqualsDouble(out3.x, Extended.kernel.noiseValue3d(x, y, z, 0.0, 0.0, 0.0, 1.1), 1e-12)

  test("extended analytic gradients match finite differences"):
    val e = 1e-5
    val out = new Vec4()
    for (x, y, z) <- points3.take(60) do
      Extended.kernel.noise3dInto(x, y, z, 0.0, 0.0, 0.0, 0.7, out)
      def f(a: Double, b: Double, c: Double) =
        Extended.kernel.noiseValue3d(a, b, c, 0.0, 0.0, 0.0, 0.7)
      val dx = (f(x + e, y, z) - f(x - e, y, z)) / (2 * e)
      val dy = (f(x, y + e, z) - f(x, y - e, z)) / (2 * e)
      val dz = (f(x, y, z + e) - f(x, y, z - e)) / (2 * e)
      assertEqualsDouble(out.y, dx, 1e-4)
      assertEqualsDouble(out.z, dy, 1e-4)
      assertEqualsDouble(out.w, dz, 1e-4)

  test("extended fbm: value normalized, gradient is the derivative, tiling kept"):
    val out = new Vec3()
    val e = 1e-5
    for (x, y) <- points2.take(60) do
      Extended.kernel.fbm2dInto(x, y, 4, 2.0, 0.5, 3.0, 4.0, 0.0, -1, out)
      assert(out.x > -1.0 && out.x < 1.0)
      def f(a: Double, b: Double) = Extended.kernel.fbmValue2d(a, b, 4, 2.0, 0.5, 3.0, 4.0, 0.0, -1)
      assertEqualsDouble(out.x, f(x, y), 1e-12)
      assertEqualsDouble(out.y, (f(x + e, y) - f(x - e, y)) / (2 * e), 1e-3)
      assertEqualsDouble(f(x + 3.0, y - 4.0), f(x, y), 1e-9)

  // ---------------------------------------------------------------------------
  // Worley
  // ---------------------------------------------------------------------------

  test("worley F1 <= F2, both non-negative"):
    val out = new Vec2()
    for (x, y, z) <- points3.take(200) do
      Worley.kernel.noise2dInto(x, y, 1.0, out)
      assert(out.x >= 0.0 && out.x <= out.y, out.toString)
      Worley.kernel.noise3dInto(x, y, z, 1.0, out)
      assert(out.x >= 0.0 && out.x <= out.y, out.toString)

  test("worley with zero jitter is the regular cell grid"):
    val out = new Vec2()
    Worley.kernel.noise2dInto(10.5, 3.5, 0.0, out)
    assertEqualsDouble(out.x, 0.0, 1e-9)
    assertEqualsDouble(out.y, 1.0, 1e-9)

  // ---------------------------------------------------------------------------
  // Torus and shared extensions
  // ---------------------------------------------------------------------------

  test("torus noise tiles the unit square"):
    for (x, y) <- points2.take(50) do
      assertEqualsDouble(
        Simplex.kernel.torusNoise2d(x, y, 2.0),
        Simplex.kernel.torusNoise2d(x + 1.0, y - 1.0, 2.0),
        1e-9,
      )

  test("CPU branch of the shared extensions equals the object API"):
    val p = Vec2(1.3, -2.7)
    val q = Vec3(0.3, 4.1, -1.2)
    assertEquals(p.simplexNoise(), Simplex.noise2d(p))
    assertEquals(p.simplexFbm(gain = 0.8, seed = 2.0), Simplex.fbm2d(p, gain = 0.8, seed = 2.0))
    assertEquals(q.simplexFbm(octaves = 3), Simplex.fbm3d(q, octaves = 3))
    val v: Double = q.extendedNoiseValue(tilingPeriod = Vec3(4.0, 4.0, 4.0), rot = 0.1)
    assertEquals(v, Extended.noiseValue3d(q, tilingPeriod = Vec3(4.0, 4.0, 4.0), rot = 0.1))
    val g: Vec4 = q.extendedNoise()
    assertEqualsDouble(g.x, Extended.noiseValue3d(q), 1e-12)
    val w: Vec2 = p.worleyNoise(jitter = 0.5)
    assertEquals(w.x, Worley.noise2d(p, jitter = 0.5).x)
