package trivalibs.utils.numbers

given Conversion[Float, Float] = identity
given Conversion[Double, Double] = identity
given Conversion[Float, Double] = identity
given Conversion[Double, Float] = _.toFloat

trait NumExt[P]:
  extension (p: P)
    def sqrt: P
    def pow(exp: P): P
    def abs: P
    def floor: P
    def ceil: P

    def sin: P
    def cos: P
    def tan: P
    def asin: P
    def acos: P
    def atan: P
    def atan2(other: P): P

    def clamp(min: P, max: P): P
    def clamp01: P
    def fit0111: P
    def fit1101: P

object NumExt:
  given NumExt[Double]:
    extension (p: Double)
      inline def sqrt = Math.sqrt(p)
      inline def pow(e: Double) = Math.pow(p, e)
      inline def abs = Math.abs(p)
      inline def floor = Math.floor(p)
      inline def ceil = Math.ceil(p)

      inline def sin = Math.sin(p)
      inline def cos = Math.cos(p)
      inline def tan = Math.tan(p)
      inline def asin = Math.asin(p)
      inline def acos = Math.acos(p)
      inline def atan = Math.atan(p)
      inline def atan2(other: Double) = Math.atan2(p, other)

      inline def clamp(min: Double, max: Double) =
        if p < min then min else if p > max then max else p
      inline def clamp01 = clamp(0.0, 1.0)
      inline def fit0111 = p * 2.0 - 1.0
      inline def fit1101 = p * 0.5 + 0.5

  given NumExt[Float]:
    extension (p: Float)
      inline def sqrt = Math.sqrt(p).toFloat
      inline def pow(e: Float) = Math.pow(p, e).toFloat
      inline def abs = Math.abs(p)
      inline def floor = Math.floor(p).toFloat
      inline def ceil = Math.ceil(p).toFloat

      inline def sin = Math.sin(p).toFloat
      inline def cos = Math.cos(p).toFloat
      inline def tan = Math.tan(p).toFloat
      inline def asin = Math.asin(p).toFloat
      inline def acos = Math.acos(p).toFloat
      inline def atan = Math.atan(p).toFloat
      inline def atan2(other: Float) = Math.atan2(p, other).toFloat

      inline def clamp(min: Float, max: Float) =
        if p < min then min else if p > max then max else p
      inline def clamp01 = clamp(0f, 1f)
      inline def fit0111 = (p * 2.0f - 1.0f)
      inline def fit1101 = (p * 0.5f + 0.5f)
