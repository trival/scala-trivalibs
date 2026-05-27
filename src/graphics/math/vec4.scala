package trivalibs.graphics.math

import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// Generic variants — purely abstract shared contract between CPU and GPU.
// ---------------------------------------------------------------------------

trait Vec4BaseG[Num, Vec]:
  extension (v: Vec)
    def x: Num
    def y: Num
    def z: Num
    def w: Num
    inline def r: Num = x
    inline def g: Num = y
    inline def b: Num = z
    inline def a: Num = w
    def dot(other: Vec): Num
    def length_squared: Num
    def length: Num
    /** Euclidean distance between `v` and `other` — `length(v - other)`. */
    def distance(other: Vec): Num

trait Vec4ImmutableOpsG[Num, Vec]:
  def create(x: Num, y: Num, z: Num, w: Num): Vec

  extension (v: Vec)(using Vec4BaseG[Num, Vec])
    @scala.annotation.targetName("addVecG")
    def +(other: Vec): Vec
    @scala.annotation.targetName("addScalarG")
    def +(scalar: Num): Vec
    @scala.annotation.targetName("negateVecG")
    def unary_- : Vec
    @scala.annotation.targetName("subVecG")
    def -(other: Vec): Vec
    @scala.annotation.targetName("subScalarG")
    def -(scalar: Num): Vec
    @scala.annotation.targetName("mulVecG")
    def *(other: Vec): Vec
    @scala.annotation.targetName("mulScalarG")
    def *(scalar: Num): Vec
    @scala.annotation.targetName("divVecG")
    def /(other: Vec): Vec
    @scala.annotation.targetName("divScalarG")
    def /(scalar: Num): Vec
    def normalize: Vec

    def abs: Vec
    def sign: Vec
    def floor: Vec
    def ceil: Vec
    def round: Vec
    def fract: Vec
    def exp: Vec
    def log: Vec
    def log2: Vec
    def sqrt: Vec
    def inverseSqrt: Vec
    def trunc: Vec
    def exp2: Vec
    @scala.annotation.targetName("powVecG")
    def pow(e: Vec): Vec
    @scala.annotation.targetName("powScalarG")
    def pow(e: Num): Vec

    def min(other: Vec): Vec
    def max(other: Vec): Vec
    def clamp(lo: Num, hi: Num): Vec
    def clamp01: Vec
    def fit0111: Vec
    def fit1101: Vec
    @scala.annotation.targetName("mixVecG")
    def mix(b: Vec, t: Vec): Vec
    @scala.annotation.targetName("mixScalarG")
    def mix(b: Vec, t: Num): Vec
    @scala.annotation.targetName("lerpVecG")
    inline def lerp(b: Vec, t: Vec): Vec = v.mix(b, t)
    @scala.annotation.targetName("lerpScalarG")
    inline def lerp(b: Vec, t: Num): Vec = v.mix(b, t)
    @scala.annotation.targetName("ltVecG")
    def <(other: Vec): Vec
    @scala.annotation.targetName("lteVecG")
    def <=(other: Vec): Vec
    @scala.annotation.targetName("gtVecG")
    def >(other: Vec): Vec
    @scala.annotation.targetName("gteVecG")
    def >=(other: Vec): Vec
    @scala.annotation.targetName("stepVecG")
    def step(edge: Vec): Vec
    @scala.annotation.targetName("stepScalarG")
    def step(edge: Num): Vec
    @scala.annotation.targetName("smoothstepVecG")
    def smoothstep(edge0: Vec, edge1: Vec): Vec
    @scala.annotation.targetName("smoothstepScalarG")
    def smoothstep(edge0: Num, edge1: Num): Vec

    /** Reflects incident vector `v` about the surface normal `n`. `n` must be
      * unit length. Computed as `v - 2 * dot(n, v) * n`.
      */
    def reflect(n: Vec): Vec
    /** Refracts incident vector `v` through a surface with normal `n` and
      * ratio of indices of refraction `eta` (source / destination). Returns a
      * zero vector on total internal reflection. `v` and `n` must be unit
      * length.
      */
    def refract(n: Vec, eta: Num): Vec

// ---------------------------------------------------------------------------
// CPU-specific variants — concrete Double implementations + CPU-only ops.
// ---------------------------------------------------------------------------

trait Vec4Base[Vec] extends Vec4BaseG[Double, Vec]:
  extension (v: Vec)
    def dot(other: Vec): Double =
      v.x * other.x + v.y * other.y + v.z * other.z + v.w * other.w
    def length_squared: Double = v.dot(v)
    def length: Double = v.length_squared.sqrt
    def distance(other: Vec): Double =
      val dx = v.x - other.x
      val dy = v.y - other.y
      val dz = v.z - other.z
      val dw = v.w - other.w
      (dx * dx + dy * dy + dz * dz + dw * dw).sqrt

// format: off
trait Vec4ImmutableOps[Vec]:

  def create(x: Double, y: Double, z: Double, w: Double): Vec

  def from[Vec4_](other: Vec4_)(using Vec4Base[Vec4_]): Vec =
    create(other.x, other.y, other.z, other.w)

  extension (v: Vec)(using Vec4Base[Vec])
    @scala.annotation.targetName("addVec")
    def +(other: Vec): Vec =
      create(v.x + other.x, v.y + other.y, v.z + other.z, v.w + other.w)
    @scala.annotation.targetName("addScalar")
    def +(scalar: Double): Vec =
      create(v.x + scalar, v.y + scalar, v.z + scalar, v.w + scalar)
    @scala.annotation.targetName("negateVec")
    def unary_- : Vec = create(-v.x, -v.y, -v.z, -v.w)
    @scala.annotation.targetName("subVec")
    def -(other: Vec): Vec =
      create(v.x - other.x, v.y - other.y, v.z - other.z, v.w - other.w)
    @scala.annotation.targetName("subScalar")
    def -(scalar: Double): Vec =
      create(v.x - scalar, v.y - scalar, v.z - scalar, v.w - scalar)
    @scala.annotation.targetName("mulVec")
    def *(other: Vec): Vec =
      create(v.x * other.x, v.y * other.y, v.z * other.z, v.w * other.w)
    @scala.annotation.targetName("mulScalar")
    def *(scalar: Double): Vec =
      create(v.x * scalar, v.y * scalar, v.z * scalar, v.w * scalar)
    @scala.annotation.targetName("divVec")
    def /(other: Vec): Vec =
      create(v.x / other.x, v.y / other.y, v.z / other.z, v.w / other.w)
    @scala.annotation.targetName("divScalar")
    def /(scalar: Double): Vec =
      create(v.x / scalar, v.y / scalar, v.z / scalar, v.w / scalar)
    def normalize: Vec = v / v.length

    def abs: Vec = create(v.x.abs, v.y.abs, v.z.abs, v.w.abs)
    def sign: Vec = create(v.x.sign, v.y.sign, v.z.sign, v.w.sign)
    def floor: Vec = create(v.x.floor, v.y.floor, v.z.floor, v.w.floor)
    def ceil: Vec = create(v.x.ceil, v.y.ceil, v.z.ceil, v.w.ceil)
    def round: Vec = create(v.x.round, v.y.round, v.z.round, v.w.round)
    def fract: Vec = create(v.x.fract, v.y.fract, v.z.fract, v.w.fract)
    def exp: Vec = create(v.x.exp, v.y.exp, v.z.exp, v.w.exp)
    def log: Vec = create(v.x.log, v.y.log, v.z.log, v.w.log)
    def log2: Vec = create(v.x.log2, v.y.log2, v.z.log2, v.w.log2)
    def sqrt: Vec = create(v.x.sqrt, v.y.sqrt, v.z.sqrt, v.w.sqrt)
    def inverseSqrt: Vec = create(v.x.inverseSqrt, v.y.inverseSqrt, v.z.inverseSqrt, v.w.inverseSqrt)
    def trunc: Vec = create(v.x.trunc, v.y.trunc, v.z.trunc, v.w.trunc)
    def exp2: Vec = create(v.x.exp2, v.y.exp2, v.z.exp2, v.w.exp2)
    @scala.annotation.targetName("powVec")
    def pow(e: Vec): Vec = create(v.x.pow(e.x), v.y.pow(e.y), v.z.pow(e.z), v.w.pow(e.w))
    @scala.annotation.targetName("powScalar")
    def pow(e: Double): Vec = create(v.x.pow(e), v.y.pow(e), v.z.pow(e), v.w.pow(e))

    def min(other: Vec): Vec =
      create(v.x.min(other.x), v.y.min(other.y), v.z.min(other.z), v.w.min(other.w))
    def max(other: Vec): Vec =
      create(v.x.max(other.x), v.y.max(other.y), v.z.max(other.z), v.w.max(other.w))
    def clamp01: Vec = create(v.x.clamp01, v.y.clamp01, v.z.clamp01, v.w.clamp01)
    def clamp(lo: Double, hi: Double): Vec =
      create(v.x.clamp(lo, hi), v.y.clamp(lo, hi), v.z.clamp(lo, hi), v.w.clamp(lo, hi))
    def fit0111: Vec = create(v.x.fit0111, v.y.fit0111, v.z.fit0111, v.w.fit0111)
    def fit1101: Vec = create(v.x.fit1101, v.y.fit1101, v.z.fit1101, v.w.fit1101)
    @scala.annotation.targetName("mixVec")
    def mix(b: Vec, t: Vec): Vec =
      create(v.x.mix(b.x, t.x), v.y.mix(b.y, t.y), v.z.mix(b.z, t.z), v.w.mix(b.w, t.w))
    @scala.annotation.targetName("mixScalar")
    def mix(b: Vec, t: Double): Vec =
      create(v.x.mix(b.x, t), v.y.mix(b.y, t), v.z.mix(b.z, t), v.w.mix(b.w, t))
    @scala.annotation.targetName("lerpVec")
    inline def lerp(b: Vec, t: Vec): Vec = v.mix(b, t)
    @scala.annotation.targetName("lerpScalar")
    inline def lerp(b: Vec, t: Double): Vec = v.mix(b, t)
    @scala.annotation.targetName("ltVec")
    def <(other: Vec): Vec =
      create(v.x.lt(other.x), v.y.lt(other.y), v.z.lt(other.z), v.w.lt(other.w))
    @scala.annotation.targetName("lteVec")
    def <=(other: Vec): Vec =
      create(v.x.lte(other.x), v.y.lte(other.y), v.z.lte(other.z), v.w.lte(other.w))
    @scala.annotation.targetName("gtVec")
    def >(other: Vec): Vec =
      create(v.x.gt(other.x), v.y.gt(other.y), v.z.gt(other.z), v.w.gt(other.w))
    @scala.annotation.targetName("gteVec")
    def >=(other: Vec): Vec =
      create(v.x.gte(other.x), v.y.gte(other.y), v.z.gte(other.z), v.w.gte(other.w))
    @scala.annotation.targetName("stepVec")
    def step(edge: Vec): Vec =
      create(v.x.step(edge.x), v.y.step(edge.y), v.z.step(edge.z), v.w.step(edge.w))
    @scala.annotation.targetName("stepScalar")
    def step(edge: Double): Vec =
      create(v.x.step(edge), v.y.step(edge), v.z.step(edge), v.w.step(edge))
    @scala.annotation.targetName("smoothstepVec")
    def smoothstep(edge0: Vec, edge1: Vec): Vec =
      create(
        v.x.smoothstep(edge0.x, edge1.x),
        v.y.smoothstep(edge0.y, edge1.y),
        v.z.smoothstep(edge0.z, edge1.z),
        v.w.smoothstep(edge0.w, edge1.w),
      )
    @scala.annotation.targetName("smoothstepScalar")
    def smoothstep(edge0: Double, edge1: Double): Vec =
      create(
        v.x.smoothstep(edge0, edge1),
        v.y.smoothstep(edge0, edge1),
        v.z.smoothstep(edge0, edge1),
        v.w.smoothstep(edge0, edge1),
      )

    def reflect(n: Vec): Vec =
      val d = v.dot(n) * 2.0
      create(v.x - n.x * d, v.y - n.y * d, v.z - n.z * d, v.w - n.w * d)
    def refract(n: Vec, eta: Double): Vec =
      val dotNI = n.dot(v)
      val k = 1.0 - eta * eta * (1.0 - dotNI * dotNI)
      if k < 0.0 then create(0.0, 0.0, 0.0, 0.0)
      else
        val s = eta * dotNI + k.sqrt
        create(
          v.x * eta - n.x * s,
          v.y * eta - n.y * s,
          v.z * eta - n.z * s,
          v.w * eta - n.w * s,
        )
// format: on

trait Vec4Mutable[Vec] extends Vec4Base[Vec]:
  extension (v: Vec)
    def x_=(value: Double): Unit
    def y_=(value: Double): Unit
    def z_=(value: Double): Unit
    def w_=(value: Double): Unit
    inline def r_=(value: Double): Unit = x_=(value)
    inline def g_=(value: Double): Unit = y_=(value)
    inline def b_=(value: Double): Unit = z_=(value)
    inline def a_=(value: Double): Unit = w_=(value)

trait Vec4MutableOps[Vec]:

  extension (v: Vec)(using Vec4Mutable[Vec])
    def set[Vec4_](other: Vec4_)(using Vec4Base[Vec4_]): Unit =
      v.x = other.x
      v.y = other.y
      v.z = other.z
      v.w = other.w
    def :=[Vec4_](other: Vec4_)(using Vec4Base[Vec4_]): Unit =
      v.set(other)

    def add(other: Vec, out: Vec = v): Vec =
      out.x = v.x + other.x
      out.y = v.y + other.y
      out.z = v.z + other.z
      out.w = v.w + other.w
      out
    def sub(other: Vec, out: Vec = v): Vec =
      out.x = v.x - other.x
      out.y = v.y - other.y
      out.z = v.z - other.z
      out.w = v.w - other.w
      out
    def mul(other: Vec, out: Vec = v): Vec =
      out.x = v.x * other.x
      out.y = v.y * other.y
      out.z = v.z * other.z
      out.w = v.w * other.w
      out
    def div(other: Vec, out: Vec = v): Vec =
      out.x = v.x / other.x
      out.y = v.y / other.y
      out.z = v.z / other.z
      out.w = v.w / other.w
      out

    def addS(scalar: Double, out: Vec = v): Vec =
      out.x = v.x + scalar
      out.y = v.y + scalar
      out.z = v.z + scalar
      out.w = v.w + scalar
      out
    def subS(scalar: Double, out: Vec = v): Vec =
      out.x = v.x - scalar
      out.y = v.y - scalar
      out.z = v.z - scalar
      out.w = v.w - scalar
      out
    def mulS(scalar: Double, out: Vec = v): Vec =
      out.x = v.x * scalar
      out.y = v.y * scalar
      out.z = v.z * scalar
      out.w = v.w * scalar
      out
    def divS(scalar: Double, out: Vec = v): Vec =
      out.x = v.x / scalar
      out.y = v.y / scalar
      out.z = v.z / scalar
      out.w = v.w / scalar
      out

    @scala.annotation.targetName("addVecAssign")
    def +=(other: Vec): Unit =
      v.add(other)
    @scala.annotation.targetName("addScalarAssign")
    def +=(scalar: Double): Unit =
      v.addS(scalar)
    @scala.annotation.targetName("subVecAssign")
    def -=(other: Vec): Unit =
      v.sub(other)
    @scala.annotation.targetName("subScalarAssign")
    def -=(scalar: Double): Unit =
      v.subS(scalar)
    @scala.annotation.targetName("mulScalarAssign")
    def *=(scalar: Double): Unit =
      v.mulS(scalar)
    @scala.annotation.targetName("divScalarAssign")
    def /=(scalar: Double): Unit =
      v.divS(scalar)
    @scala.annotation.targetName("mulComponentwiseAssign")
    def *=(other: Vec): Unit =
      v.mul(other)
    @scala.annotation.targetName("divComponentwiseAssign")
    def /=(other: Vec): Unit =
      v.div(other)

    def normalizeTo(out: Vec): Vec = v.divS(v.length, out)
    inline def normalizeSelf: Vec = v.normalizeTo(v)
