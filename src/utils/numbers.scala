package trivalibs.utils.numbers

extension (d: Double)
  inline def fit0111 = d * 2.0 - 1.0
  inline def fit1101 = d * 0.5 + 0.5
  inline def clamp(min: Double, max: Double): Double =
    if d < min then min
    else if d > max then max
    else d
  inline def clamp01: Double =
    d.clamp(0.0, 1.0)

extension (d: Float)
  inline def fit0111 = d * 2.0 - 1.0
  inline def fit1101 = d * 0.5 + 0.5
  inline def clamp(min: Float, max: Float): Float =
    if d < min then min
    else if d > max then max
    else d
  inline def clamp01: Float =
    d.clamp(0.0, 1.0)
