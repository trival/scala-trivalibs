package trivalibs.utils.numbers

given Conversion[Float, Float] = identity
given Conversion[Double, Double] = identity
given Conversion[Float, Double] = identity
given Conversion[Double, Float] = _.toFloat

trait NumExt[P]:
  extension (p: P)
    def clamp(min: P, max: P): P
    def clamp01: P
    def sqrt: P
    def pow(exp: P): P
    def fit0111: P
    def fit1101: P

object NumExt:
  given NumExt[Double]:
    extension (p: Double)
      inline def clamp(min: Double, max: Double) =
        if p < min then min else if p > max then max else p
      inline def clamp01 = clamp(0.0, 1.0)
      inline def sqrt = Math.sqrt(p)
      inline def pow(e: Double) = Math.pow(p, e)
      inline def fit0111 = p * 2.0 - 1.0
      inline def fit1101 = p * 0.5 + 0.5

  given NumExt[Float]:
    extension (p: Float)
      inline def clamp(min: Float, max: Float) =
        if p < min then min else if p > max then max else p
      inline def clamp01 = clamp(0f, 1f)
      inline def sqrt = Math.sqrt(p).toFloat
      inline def pow(e: Float) = Math.pow(p, e).toFloat
      inline def fit0111 = (p * 2.0f - 1.0f)
      inline def fit1101 = (p * 0.5f + 0.5f)
