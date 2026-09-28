package trivalibs.graphics.shader.lib.coords

import munit.FunSuite
import trivalibs.graphics.shader.dsl.WgslFnData

class CoordsFnsTest extends FunSuite:

  test("polarToCart emits correct WGSL"):
    val data = Polar.wgsl.polarToCart.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn polar_polar_to_cart(pos: vec2<f32>)"), data.src)
    assert(data.src.contains("-> vec2<f32>"), data.src)
    assert(data.src.contains("pos.x * cos(pos.y)"), data.src)
    assert(data.src.contains("pos.x * sin(pos.y)"), data.src)

  test("cartToPolar emits correct WGSL"):
    val data = Polar.wgsl.cartToPolar.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn polar_cart_to_polar(pos: vec2<f32>)"), data.src)
    assert(data.src.contains("-> vec2<f32>"), data.src)
    assert(data.src.contains("length(pos)"), data.src)
    assert(data.src.contains("atan2(pos.y, pos.x)"), data.src)
