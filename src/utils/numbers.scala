package trivalibs.utils.numbers

inline given Conversion[Float, Float] = identity
inline given Conversion[Double, Double] = identity
inline given Conversion[Float, Double] = identity
inline given Conversion[Double, Float]:
  inline def apply(x: Double): Float = x.toFloat

trait NumOps[P]:
  extension (p: P)
    def +(other: P): P
    def -(other: P): P
    def *(other: P): P
    def /(other: P): P
    def unary_- : P
  def zero: P
  def one: P

object NumOps:
  given NumOps[Double]:
    extension (p: Double)
      inline def +(other: Double) = p + other
      inline def -(other: Double) = p - other
      inline def *(other: Double) = p * other
      inline def /(other: Double) = p / other
      inline def unary_- = -p
    val zero = 0.0
    val one = 1.0

  given NumOps[Float]:
    extension (p: Float)
      inline def +(other: Float) = p + other
      inline def -(other: Float) = p - other
      inline def *(other: Float) = p * other
      inline def /(other: Float) = p / other
      inline def unary_- = -p
    val zero = 0.0f
    val one = 1.0f

trait NumExt[P]:
  extension (p: P)
    def sqrt: P
    def pow(exp: P): P
    def abs: P
    def sign: P
    def floor: P
    def ceil: P
    def round: P
    def fract: P
    def exp: P
    def log: P
    def log2: P

    def sin: P
    def cos: P
    def tan: P
    def asin: P
    def acos: P
    def atan: P
    def atan2(other: P): P

    def min(other: P): P
    def max(other: P): P
    def clamp(min: P, max: P): P
    def clamp01: P
    def fit0111: P
    def fit1101: P
    def mix(b: P, t: P): P
    inline def lerp(b: P, t: P): P = mix(b, t)

    def gte(edge: P): P // 1 if self >= edge, else 0
    def gt(edge: P): P // 1 if self >  edge, else 0
    def lte(edge: P): P // 1 if self <= edge, else 0
    def lt(edge: P): P // 1 if self <  edge, else 0

    inline def step(edge: P): P = gte(edge) // alias for gte
    def smoothstep(edge0: P, edge1: P): P

object NumExt:
  given NumExt[Double]:
    extension (p: Double)
      inline def sqrt = Math.sqrt(p)
      inline def pow(e: Double) = Math.pow(p, e)
      inline def abs = Math.abs(p)
      inline def sign = Math.signum(p)
      inline def floor = Math.floor(p)
      inline def ceil = Math.ceil(p)
      inline def round = Math.round(p).toDouble
      inline def fract = p - Math.floor(p)
      inline def exp = Math.exp(p)
      inline def log = Math.log(p)
      inline def log2 = Math.log(p) / Math.log(2.0)

      inline def sin = Math.sin(p)
      inline def cos = Math.cos(p)
      inline def tan = Math.tan(p)
      inline def asin = Math.asin(p)
      inline def acos = Math.acos(p)
      inline def atan = Math.atan(p)
      inline def atan2(other: Double) = Math.atan2(p, other)

      inline def min(other: Double) = Math.min(p, other)
      inline def max(other: Double) = Math.max(p, other)
      inline def clamp(min: Double, max: Double) =
        if p < min then min else if p > max then max else p
      inline def clamp01 = clamp(0.0, 1.0)
      inline def fit0111 = p * 2.0 - 1.0
      inline def fit1101 = p * 0.5 + 0.5
      inline def mix(b: Double, t: Double) = p * (1.0 - t) + b * t
      inline def gte(edge: Double) = if p >= edge then 1.0 else 0.0
      inline def gt(edge: Double) = if p > edge then 1.0 else 0.0
      inline def lte(edge: Double) = if p <= edge then 1.0 else 0.0
      inline def lt(edge: Double) = if p < edge then 1.0 else 0.0
      inline def smoothstep(edge0: Double, edge1: Double) =
        val t = ((p - edge0) / (edge1 - edge0)).clamp01
        t * t * (3.0 - 2.0 * t)

  given NumExt[Float]:
    extension (p: Float)
      inline def sqrt = Math.sqrt(p).toFloat
      inline def pow(e: Float) = Math.pow(p, e).toFloat
      inline def abs = Math.abs(p)
      inline def sign = Math.signum(p)
      inline def floor = Math.floor(p).toFloat
      inline def ceil = Math.ceil(p).toFloat
      inline def round = Math.round(p).toFloat
      inline def fract = p - Math.floor(p).toFloat
      inline def exp = Math.exp(p).toFloat
      inline def log = Math.log(p).toFloat
      inline def log2 = (Math.log(p) / Math.log(2.0)).toFloat

      inline def sin = Math.sin(p).toFloat
      inline def cos = Math.cos(p).toFloat
      inline def tan = Math.tan(p).toFloat
      inline def asin = Math.asin(p).toFloat
      inline def acos = Math.acos(p).toFloat
      inline def atan = Math.atan(p).toFloat
      inline def atan2(other: Float) = Math.atan2(p, other).toFloat

      inline def min(other: Float) = Math.min(p, other)
      inline def max(other: Float) = Math.max(p, other)
      inline def clamp(min: Float, max: Float) =
        if p < min then min else if p > max then max else p
      inline def clamp01 = clamp(0f, 1f)
      inline def fit0111 = (p * 2.0f - 1.0f)
      inline def fit1101 = (p * 0.5f + 0.5f)
      inline def mix(b: Float, t: Float) = p * (1.0f - t) + b * t
      inline def gte(edge: Float) = if p >= edge then 1f else 0f
      inline def gt(edge: Float) = if p > edge then 1f else 0f
      inline def lte(edge: Float) = if p <= edge then 1f else 0f
      inline def lt(edge: Float) = if p < edge then 1f else 0f
      inline def smoothstep(edge0: Float, edge1: Float) =
        val t = ((p - edge0) / (edge1 - edge0)).clamp01
        t * t * (3.0f - 2.0f * t)
