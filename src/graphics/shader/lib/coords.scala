package trivalibs.graphics.shader.lib.coords

import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.gpu.Vec2Expr
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given

/** Coordinate conversions on the GPU.
  *
  * The CPU mirror is `trivalibs.graphics.lib.coords.Polar`; the shared
  * extensions (`p.polarToCart`, `p.cartToPolar`) cover both.
  */
object Polar:

  /** Polar → Cartesian. `pos.x` is the radius, `pos.y` the angle in radians;
    * returns `(radius·cos(angle), radius·sin(angle))`.
    */
  inline def polarToCart(pos: Vec2Expr): Vec2Expr = wgsl.polarToCart(pos)

  /** Cartesian → polar: `(length(pos), atan2(pos.y, pos.x))`, radius in `.x`,
    * angle (radians, range `(-π, π]`) in `.y`.
    */
  inline def cartToPolar(pos: Vec2Expr): Vec2Expr = wgsl.cartToPolar(pos)

  /** The WgslFn definitions — the layer for `.withDeps` and raw WGSL
    * composition (`polarToCart` emits `polar_polar_to_cart`).
    */
  object wgsl:

    lazy val polarToCart: WgslFn[(pos: Vec2), Vec2] =
      WgslFn.raw("polar_polar_to_cart"):
        "  return vec2<f32>(pos.x * cos(pos.y), pos.x * sin(pos.y));"

    lazy val cartToPolar: WgslFn[(pos: Vec2), Vec2] =
      WgslFn.raw("polar_cart_to_polar"):
        "  return vec2<f32>(length(pos), atan2(pos.y, pos.x));"
