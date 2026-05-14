package trivalibs.graphics.shader.lib.coords

import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given

object Polar:

  /** Polar → Cartesian. `p.x` is radius, `p.y` is angle in radians.
    *
    * Returns `(radius·cos(angle), radius·sin(angle))`.
    */
  val polarToCart: WgslFn[(p: Vec2), Vec2] =
    WgslFn.raw("polar_to_cart"):
      "  return vec2<f32>(p.x * cos(p.y), p.x * sin(p.y));"

  /** Cartesian → polar. Returns `(length(v), atan2(v.y, v.x))` — i.e. radius in
    * `.x`, angle (radians, range `(-π, π]`) in `.y`.
    */
  val cartToPolar: WgslFn[(v: Vec2), Vec2] =
    WgslFn.raw("cart_to_polar"):
      "  return vec2<f32>(length(v), atan2(v.y, v.x));"
