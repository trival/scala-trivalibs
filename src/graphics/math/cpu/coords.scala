package trivalibs.graphics.math.cpu

// CPU counterparts of the WGSL conversions in
// `trivalibs.graphics.shader.lib.coords`. Both sides are postfix ops on their
// vector type (`p.polarToCart`), so the two imports coexist in one file and the
// same call reads the same in CPU code and in a shader body.

import trivalibs.graphics.math.Vec2Base
import trivalibs.graphics.math.Vec2ImmutableOps
import trivalibs.utils.numbers.NumExt.given

extension [Vec](p: Vec)(using base: Vec2Base[Vec], ops: Vec2ImmutableOps[Vec])

  /** Polar → Cartesian. Reads `p.x` as radius and `p.y` as angle in radians;
    * returns `(radius·cos(angle), radius·sin(angle))`.
    */
  def polarToCart: Vec =
    ops.create(p.x * p.y.cos, p.x * p.y.sin)

  /** Cartesian → polar. Returns `(length(p), atan2(p.y, p.x))` — radius in
    * `.x`, angle (radians, range `(-π, π]`) in `.y`.
    */
  def cartToPolar: Vec =
    ops.create(p.length, p.y.atan2(p.x))
