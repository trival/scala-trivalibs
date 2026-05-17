package trivalibs.graphics.math

import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// Generic variants — purely abstract shared contract between CPU and GPU.
// ---------------------------------------------------------------------------

trait Vec2BaseG[Num, Vec]:
  extension (v: Vec)
    def x: Num
    def y: Num
    inline def u: Num = x
    inline def v: Num = y
    def dot(other: Vec): Num
    def length_squared: Num
    def length: Num
    /** Euclidean distance between `v` and `other` — `length(v - other)`. */
    def distance(other: Vec): Num

trait Vec2ImmutableOpsG[Num, Vec]:
  def create(x: Num, y: Num): Vec

  extension (v: Vec)(using Vec2BaseG[Num, Vec])
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

    def min(other: Vec): Vec
    def max(other: Vec): Vec
    def clamp(lo: Num, hi: Num): Vec
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
    def smoothstep(edge0: Vec, edge1: Vec): Vec

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

trait Vec2Base[Vec] extends Vec2BaseG[Double, Vec]:
  extension (v: Vec)
    def dot(other: Vec): Double =
      v.x * other.x + v.y * other.y
    def length_squared: Double = v.dot(v)
    def length: Double = v.length_squared.sqrt
    def distance(other: Vec): Double =
      val dx = v.x - other.x
      val dy = v.y - other.y
      (dx * dx + dy * dy).sqrt

// format: off
trait Vec2ImmutableOps[Vec]:

  def create(x: Double, y: Double): Vec

  def from[Vec2_](other: Vec2_)(using Vec2Base[Vec2_]): Vec =
    create(other.x, other.y)

  extension (v: Vec)(using Vec2Base[Vec])
    @scala.annotation.targetName("addVec")
    def +(other: Vec): Vec = create(v.x + other.x, v.y + other.y)
    @scala.annotation.targetName("addScalar")
    def +(scalar: Double): Vec = create(v.x + scalar, v.y + scalar)
    @scala.annotation.targetName("negateVec")
    def unary_- : Vec = create(-v.x, -v.y)
    @scala.annotation.targetName("subVec")
    def -(other: Vec): Vec = create(v.x - other.x, v.y - other.y)
    @scala.annotation.targetName("subScalar")
    def -(scalar: Double): Vec = create(v.x - scalar, v.y - scalar)
    @scala.annotation.targetName("mulVec")
    def *(other: Vec): Vec = create(v.x * other.x, v.y * other.y)
    @scala.annotation.targetName("mulScalar")
    def *(scalar: Double): Vec = create(v.x * scalar, v.y * scalar)
    @scala.annotation.targetName("divVec")
    def /(other: Vec): Vec = create(v.x / other.x, v.y / other.y)
    @scala.annotation.targetName("divScalar")
    def /(scalar: Double): Vec = create(v.x / scalar, v.y / scalar)
    def normalize: Vec = v / v.length

    def abs: Vec = create(v.x.abs, v.y.abs)
    def sign: Vec = create(v.x.sign, v.y.sign)
    def floor: Vec = create(v.x.floor, v.y.floor)
    def ceil: Vec = create(v.x.ceil, v.y.ceil)
    def round: Vec = create(v.x.round, v.y.round)
    def fract: Vec = create(v.x.fract, v.y.fract)
    def exp: Vec = create(v.x.exp, v.y.exp)
    def log: Vec = create(v.x.log, v.y.log)
    def log2: Vec = create(v.x.log2, v.y.log2)
    def sqrt: Vec = create(v.x.sqrt, v.y.sqrt)

    def min(other: Vec): Vec = create(v.x.min(other.x), v.y.min(other.y))
    def max(other: Vec): Vec = create(v.x.max(other.x), v.y.max(other.y))
    def clamp(lo: Double, hi: Double): Vec =
      create(v.x.clamp(lo, hi), v.y.clamp(lo, hi))
    def fit0111: Vec = create(v.x.fit0111, v.y.fit0111)
    def fit1101: Vec = create(v.x.fit1101, v.y.fit1101)
    @scala.annotation.targetName("mixVec")
    def mix(b: Vec, t: Vec): Vec =
      create(v.x.mix(b.x, t.x), v.y.mix(b.y, t.y))
    @scala.annotation.targetName("mixScalar")
    def mix(b: Vec, t: Double): Vec =
      create(v.x.mix(b.x, t), v.y.mix(b.y, t))
    @scala.annotation.targetName("lerpVec")
    inline def lerp(b: Vec, t: Vec): Vec = v.mix(b, t)
    @scala.annotation.targetName("lerpScalar")
    inline def lerp(b: Vec, t: Double): Vec = v.mix(b, t)
    @scala.annotation.targetName("ltVec")
    def <(other: Vec): Vec = create(v.x.lt(other.x), v.y.lt(other.y))
    @scala.annotation.targetName("lteVec")
    def <=(other: Vec): Vec = create(v.x.lte(other.x), v.y.lte(other.y))
    @scala.annotation.targetName("gtVec")
    def >(other: Vec): Vec = create(v.x.gt(other.x), v.y.gt(other.y))
    @scala.annotation.targetName("gteVec")
    def >=(other: Vec): Vec = create(v.x.gte(other.x), v.y.gte(other.y))
    @scala.annotation.targetName("stepVec")
    def step(edge: Vec): Vec = create(v.x.step(edge.x), v.y.step(edge.y))
    @scala.annotation.targetName("stepScalar")
    def step(edge: Double): Vec = create(v.x.step(edge), v.y.step(edge))
    def smoothstep(edge0: Vec, edge1: Vec): Vec =
      create(v.x.smoothstep(edge0.x, edge1.x), v.y.smoothstep(edge0.y, edge1.y))

    def reflect(n: Vec): Vec =
      val d = v.dot(n) * 2.0
      create(v.x - n.x * d, v.y - n.y * d)
    def refract(n: Vec, eta: Double): Vec =
      val dotNI = n.dot(v)
      val k = 1.0 - eta * eta * (1.0 - dotNI * dotNI)
      if k < 0.0 then create(0.0, 0.0)
      else
        val s = eta * dotNI + k.sqrt
        create(v.x * eta - n.x * s, v.y * eta - n.y * s)
// format: on

trait Vec2Mutable[Vec] extends Vec2Base[Vec]:
  extension (v: Vec)
    def x_=(value: Double): Unit
    def y_=(value: Double): Unit
    inline def u_=(value: Double): Unit = x_=(value)
    inline def v_=(value: Double): Unit = y_=(value)

trait Vec2MutableOps[Vec]:

  extension (v: Vec)(using Vec2Mutable[Vec])
    def set[Vec2_](other: Vec2_)(using Vec2Base[Vec2_]): Unit =
      v.x = other.x
      v.y = other.y
    def :=[Vec2_](other: Vec2_)(using Vec2Base[Vec2_]): Unit =
      v.set(other)

    def add(other: Vec, out: Vec = v): Vec =
      out.x = v.x + other.x
      out.y = v.y + other.y
      out
    def sub(other: Vec, out: Vec = v): Vec =
      out.x = v.x - other.x
      out.y = v.y - other.y
      out
    def mul(other: Vec, out: Vec = v): Vec =
      out.x = v.x * other.x
      out.y = v.y * other.y
      out
    def div(other: Vec, out: Vec = v): Vec =
      out.x = v.x / other.x
      out.y = v.y / other.y
      out

    def addS(scalar: Double, out: Vec = v): Vec =
      out.x = v.x + scalar
      out.y = v.y + scalar
      out
    def subS(scalar: Double, out: Vec = v): Vec =
      out.x = v.x - scalar
      out.y = v.y - scalar
      out
    def mulS(scalar: Double, out: Vec = v): Vec =
      out.x = v.x * scalar
      out.y = v.y * scalar
      out
    def divS(scalar: Double, out: Vec = v): Vec =
      out.x = v.x / scalar
      out.y = v.y / scalar
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
