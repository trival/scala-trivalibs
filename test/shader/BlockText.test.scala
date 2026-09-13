package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.gpu.Block

/** `Block` is opaque, so the emitted WGSL is only reachable through
  * `Block.unwrap`. The shader suites compare it as text constantly, and
  * `stmt: String` reads better than wrapping every assertion. Test scope only —
  * library users go through `Block.unwrap`.
  */
given Conversion[Block, String] = Block.unwrap(_)
