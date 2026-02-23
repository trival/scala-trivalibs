package trivalibs.utils.numbers

trait Number[P]:
  given numeric: Numeric[P]
  extension (p: P)
    inline def clamp(min: P, max: P): P =
      if numeric.lt(p, min) then min
      else if numeric.gt(p, max) then max
      else p
    inline def clamp01: P = p.clamp(numeric.zero, numeric.one)

object Number:
  given Number[Float]:
    val numeric = summon[Numeric[Float]]
  given Number[Double]:
    val numeric = summon[Numeric[Double]]

  given [P](using n: Number[P]): Numeric[P] = n.numeric

trait Floating[P] extends Number[P]:
  extension (p: P)
    def sqrt: Double
    def pow(exp: P): Double
    def fit0111: P
    def fit1101: P

object Floating:
  given Floating[Double]:
    val numeric = summon[Numeric[Double]]
    extension (p: Double)
      inline def sqrt         = Math.sqrt(p)
      inline def pow(e: Double) = Math.pow(p, e)
      inline def fit0111      = p * 2.0 - 1.0
      inline def fit1101      = p * 0.5 + 0.5

  given Floating[Float]:
    val numeric = summon[Numeric[Float]]
    extension (p: Float)
      inline def sqrt         = Math.sqrt(p)
      inline def pow(e: Float)  = Math.pow(p, e)
      inline def fit0111      = (p * 2.0 - 1.0).toFloat
      inline def fit1101      = (p * 0.5 + 0.5).toFloat
