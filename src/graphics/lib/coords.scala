package trivalibs.graphics.lib.coords

import scala.compiletime.erasedValue
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.gpu.Vec2Expr
import trivalibs.graphics.shader.lib.coords.Polar as GpuPolar
import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// Shared extensions — one definition for CPU `Vec2` and GPU `Vec2Expr`
// receivers, branching at compile time.
// ---------------------------------------------------------------------------

extension [P <: Vec2 | Vec2Expr](p: P)
  /** Polar → Cartesian: `p.x` radius, `p.y` angle in radians. */
  transparent inline def polarToCart: Vec2 | Vec2Expr = inline erasedValue[P] match
    case _: Vec2 => Polar.polarToCart(p.asInstanceOf[Vec2])
    case _: Vec2Expr => GpuPolar.polarToCart(p.asInstanceOf[Vec2Expr])

  /** Cartesian → polar: `(length, atan2(y, x))`, angle in `(-π, π]`. */
  transparent inline def cartToPolar: Vec2 | Vec2Expr = inline erasedValue[P] match
    case _: Vec2 => Polar.cartToPolar(p.asInstanceOf[Vec2])
    case _: Vec2Expr => GpuPolar.cartToPolar(p.asInstanceOf[Vec2Expr])

/** Coordinate conversions on the CPU — same names as the GPU `Polar`. */
object Polar:

  /** Polar → Cartesian: `pos.x` radius, `pos.y` angle in radians. */
  inline def polarToCart(pos: Vec2): Vec2 =
    kernel.polarToCartInto(pos.x, pos.y, new Vec2())

  /** Cartesian → polar: `(length(pos), atan2(pos.y, pos.x))`. */
  inline def cartToPolar(pos: Vec2): Vec2 =
    kernel.cartToPolarInto(pos.x, pos.y, new Vec2())

  object kernel:

    def polarToCartInto(radius: Double, angle: Double, out: Vec2): Vec2 =
      out.x = radius * angle.cos
      out.y = radius * angle.sin
      out

    def cartToPolarInto(x: Double, y: Double, out: Vec2): Vec2 =
      out.x = (x * x + y * y).sqrt
      out.y = y.atan2(x)
      out
