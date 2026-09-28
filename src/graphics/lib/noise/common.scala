package trivalibs.graphics.lib.noise

import scala.scalajs.js.typedarray.Float32Array
import scala.scalajs.js.typedarray.Float64Array
import scala.scalajs.js.typedarray.Int32Array
import trivalibs.utils.numbers.NumExt.given

/** Precomputed hash and gradient tables for the CPU simplex and worley
  * kernels — stegu/webgl-noise's math (the GPU's), executed the way
  * jwagner/simplex-noise.js executes simplex noise in JS: table lookups
  * instead of per-call arithmetic.
  *
  * On integer input Ashima's permutation `mod289((34x + 10)·x)` is a pure
  * function of `x`, so `perm(x)` gives exactly the GPU's values. The table
  * extends past 289 so chained lookups (`perm(perm(i) + j)`) need no `%`.
  * Gradient tables hold, per hash value, the gradient the GPU derives from it,
  * with every step rounded to f32 as the WGSL computes it (`f32` below), so CPU
  * and GPU pick the same gradients. Values are stored f32-rounded in
  * `Float64Array`s, which read faster than `Float32Array`s in an all-double JS
  * engine.
  */
object NoiseTables:

  private inline def f32(x: Double): Double = x.toFloat.toDouble

  /** `mod289` exactly as the GPU computes it in f32. */
  private def mod289F32(x: Double): Double =
    f32(x - f32((f32(x * f32(1.0 / 289.0))).floor * 289.0))

  private def permuteF32(x: Double): Double =
    mod289F32(f32(f32(f32(x * 34.0) + 10.0) * x))

  private inline val PermSize = 640
  private inline val HashCount = 290

  /** Ashima permutation per integer input `0 until 640`. */
  val perm: Int32Array =
    val t = new Int32Array(PermSize)
    var i = 0
    while i < PermSize do
      t(i) = permuteF32(i.toDouble).toInt
      i += 1
    t

  // ---- simplex 2D: 41 points on a line mapped onto a diamond, with the
  // Taylor normalization folded in ----

  val grad2x: Float64Array = new Float64Array(HashCount)
  val grad2y: Float64Array = new Float64Array(HashCount)

  // ---- simplex 3D: 7×7 points on a square mapped onto an octahedron ----

  val grad3x: Float64Array = new Float64Array(HashCount)
  val grad3y: Float64Array = new Float64Array(HashCount)
  val grad3z: Float64Array = new Float64Array(HashCount)

  // ---- simplex 4D: 7×7×6 points mapped onto a 4-cross polytope ----

  val grad4x: Float64Array = new Float64Array(HashCount)
  val grad4y: Float64Array = new Float64Array(HashCount)
  val grad4z: Float64Array = new Float64Array(HashCount)
  val grad4w: Float64Array = new Float64Array(HashCount)

  // ---- worley: feature point offsets per hash value ----

  val cellOx: Float64Array = new Float64Array(HashCount)
  val cellOy: Float64Array = new Float64Array(HashCount)
  val cellOz: Float64Array = new Float64Array(HashCount)

  private inline def taylorInvSqrt(r: Double): Double =
    f32(1.79284291400159 - f32(0.85373472095314 * r))

  private def fillTables(): Unit =
    val cw = f32(0.024390243902439)
    val n_ = f32(0.142857142857)
    val nsx = f32(n_ * 2.0)
    val nsy = f32(f32(n_ * 0.5) - 1.0)
    val nsz = n_
    val k = f32(0.142857142857)
    val ko = f32(0.428571428571)
    val k2 = f32(0.020408163265306)
    val kz = f32(0.166666666667)
    val kzo = f32(0.416666666667)
    val ip0 = f32(1.0 / 294.0)
    val ip1 = f32(1.0 / 49.0)
    val ip2 = f32(1.0 / 7.0)
    var h = 0
    while h < HashCount do
      val p = h.toDouble

      // 2D
      val x = f32(f32(2.0 * f32(f32(p * cw) - f32(p * cw).floor)) - 1.0)
      val hh = f32(x.abs - 0.5)
      val a0 = f32(x - f32(x + 0.5).floor)
      val norm2 = taylorInvSqrt(f32(f32(a0 * a0) + f32(hh * hh)))
      grad2x(h) = f32(a0 * norm2)
      grad2y(h) = f32(hh * norm2)

      // 3D
      val j = f32(p - f32(49.0 * f32(f32(p * nsz) * nsz).floor))
      val x_ = f32(j * nsz).floor
      val y_ = f32(j - f32(7.0 * x_)).floor
      val gx0 = f32(f32(x_ * nsx) + nsy)
      val gy0 = f32(f32(y_ * nsx) + nsy)
      val gz = f32(f32(1.0 - gx0.abs) - gy0.abs)
      val sh = if gz <= 0.0 then -1.0 else 0.0
      val gx = f32(gx0 + f32(f32(gx0.floor * 2.0 + 1.0) * sh))
      val gy = f32(gy0 + f32(f32(gy0.floor * 2.0 + 1.0) * sh))
      val norm3 = taylorInvSqrt(f32(f32(f32(gx * gx) + f32(gy * gy)) + f32(gz * gz)))
      grad3x(h) = f32(gx * norm3)
      grad3y(h) = f32(gy * norm3)
      grad3z(h) = f32(gz * norm3)

      // 4D (grad4 of webgl-noise noise4D.glsl)
      def component(ipc: Double): Double =
        val v = f32(p * ipc)
        f32(f32(f32(f32(v - v.floor) * 7.0).floor * ip2) - 1.0)
      var px = component(ip0)
      var py = component(ip1)
      var pz = component(ip2)
      val pw = f32(1.5 - f32(f32(px.abs + py.abs) + pz.abs))
      val sw = if pw < 0.0 then 1.0 else 0.0
      if sw != 0.0 then
        px = f32(px + (if px < 0.0 then 1.0 else -1.0))
        py = f32(py + (if py < 0.0 then 1.0 else -1.0))
        pz = f32(pz + (if pz < 0.0 then 1.0 else -1.0))
      val norm4 = taylorInvSqrt(
        f32(f32(f32(f32(px * px) + f32(py * py)) + f32(pz * pz)) + f32(pw * pw)),
      )
      grad4x(h) = f32(px * norm4)
      grad4y(h) = f32(py * norm4)
      grad4z(h) = f32(pz * norm4)
      grad4w(h) = f32(pw * norm4)

      // worley
      val pk = f32(p * k)
      cellOx(h) = f32(f32(pk - pk.floor) - ko)
      val fk = pk.floor
      cellOy(h) = f32(f32(f32(fk - f32(f32(fk * f32(1.0 / 7.0)).floor * 7.0)) * k) - ko)
      cellOz(h) = f32(f32(f32(p * k2).floor * kz) - kzo)
      h += 1

  fillTables()

  // ---- composite tables: gradient of `perm(k)`, indexed by the pre-permute
  // value `k` — simplex-noise.js's `permGrad2x` idiom, saving the last lookup
  // per corner ----

  private def composite(grad: Float64Array): Float64Array =
    val t = new Float64Array(PermSize)
    var k = 0
    while k < PermSize do
      t(k) = grad(perm(k))
      k += 1
    t

  val permGrad2x: Float64Array = composite(grad2x)
  val permGrad2y: Float64Array = composite(grad2y)
  val permGrad3x: Float64Array = composite(grad3x)
  val permGrad3y: Float64Array = composite(grad3y)
  val permGrad3z: Float64Array = composite(grad3z)
  val permGrad4x: Float64Array = composite(grad4x)
  val permGrad4y: Float64Array = composite(grad4y)
  val permGrad4z: Float64Array = composite(grad4z)
  val permGrad4w: Float64Array = composite(grad4w)

  // ---- extended (psrdnoise) ----
  // psrdnoise's hash and gradients are functions of an integer hash in
  // [0, 289) only, so they tabulate like simplex's. Its `mod289` divides
  // (`x - floor(x / 289) * 289`), which is exact on these integers in f32 as
  // well, so the hash tables are plain integer arithmetic and match the GPU.

  /** psrdnoise's `permute`: `((34m + 10)·m) mod 289` of `m = x mod 289`, for
    * `x` in `0 until 640` (the input mod lets chained lookups skip a `%`).
    */
  val extPerm: Int32Array =
    val t = new Int32Array(PermSize)
    var k = 0
    while k < PermSize do
      val m = k % 289
      t(k) = ((34 * m + 10) * m) % 289
      k += 1
    t

  /** psrdnoise 2D's first hash stage `(51h + 2)·h mod 289`, per `h`. */
  val ext2Mix: Int32Array =
    val t = new Int32Array(HashCount)
    var h = 0
    while h < HashCount do
      val m = h % 289
      t(h) = ((51 * m + 2) * m) % 289
      h += 1
    t

  /** psrdnoise 2D gradient per hash: the angle `ψ = hash · 0.07482` on the
    * unit circle. Rotation adds `α` by the angle-addition identity.
    */
  val ext2Cos: Float64Array = new Float64Array(HashCount)
  val ext2Sin: Float64Array = new Float64Array(HashCount)

  /** psrdnoise 3D per hash: `p`, the gradient on a Fibonacci sphere, and `q`,
    * the orthogonal direction it rotates towards — the rotated gradient is
    * `cos α · p + sin α · q`, exactly as upstream composes it.
    */
  val ext3Px: Float64Array = new Float64Array(HashCount)
  val ext3Py: Float64Array = new Float64Array(HashCount)
  val ext3Pz: Float64Array = new Float64Array(HashCount)
  val ext3Qx: Float64Array = new Float64Array(HashCount)
  val ext3Qy: Float64Array = new Float64Array(HashCount)
  val ext3Qz: Float64Array = new Float64Array(HashCount)

  private def fillExtendedTables(): Unit =
    var h = 0
    while h < HashCount do
      val hash = (h % 289).toDouble
      val psi2 = hash * 0.07482
      ext2Cos(h) = f32(psi2.cos)
      ext2Sin(h) = f32(psi2.sin)
      val theta = hash * 3.883222077
      val sz = hash * -0.006920415 + 0.996539792
      val psi = hash * 0.108705628
      val ct = theta.cos
      val st = theta.sin
      val szPrime = (1.0 - sz * sz).sqrt
      val px = ct * szPrime
      val py = st * szPrime
      val sp = psi.sin
      val cp = psi.cos
      val ctp = st * sp - ct * cp
      ext3Px(h) = f32(px)
      ext3Py(h) = f32(py)
      ext3Pz(h) = f32(sz)
      ext3Qx(h) = f32(ctp * st + (sp - ctp * st) * sz)
      ext3Qy(h) = f32(-ctp * ct + (cp + ctp * ct) * sz)
      ext3Qz(h) = f32(-(py * cp + px * sp))
      h += 1

  fillExtendedTables()

  // ---- seed ----

  private val seedF32 = new Float32Array(1)
  private val seedBits = new Int32Array(seedF32.buffer)

  /** Chris Wellons' `hash1i` (as in `Hash.wgsl.hash1i`), in u32 arithmetic. */
  private def hash1i(x: Int): Int =
    var v = x
    v ^= v >>> 16
    v = v * 0x21f0aaad
    v ^= v >>> 15
    v = v * -0x2ca5d269 // 0xd35a2d97 as a signed Int
    v ^ (v >>> 15)

  /** The two seed permute offsets in `[0, 289)`, packed as `s1 + 289·s2` — the
    * same values as `noise_seed_offsets` on the GPU (f32 bits of the seed,
    * `hash1i`, then `% 289` and `/ 289 % 289` in u32).
    */
  def seedOffsets(seed: Double): Int =
    seedF32(0) = seed.toFloat
    val h = hash1i(seedBits(0))
    val u = if h < 0 then h + 4294967296.0 else h.toDouble
    val s1 = u % 289.0
    val s2 = (u / 289.0).floor % 289.0
    (s1 + 289.0 * s2).toInt

  /** Floor-based `mod 289` of an integer lattice coordinate. The result is an
    * exact integer in `[0, 289]`, so the int coercion skips Scala.js's
    * checked `Double.toInt` (a helper call) for a plain `| 0`.
    */
  inline def mod289(i: Double): Int =
    (i - (i * (1.0 / 289.0)).floor * 289.0).asInstanceOf[scala.scalajs.js.Any].asInstanceOf[Int]
