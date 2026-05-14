package trivalibs.graphics.shader.lib.coords

import trivalibs.graphics.shader.dsl.WgslFnData
import munit.FunSuite

class CoordsFnsTest extends FunSuite:

  test("polarToCart emits correct WGSL"):
    val data = Polar.polarToCart.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn polar_to_cart(p: vec2<f32>)"), data.src)
    assert(data.src.contains("-> vec2<f32>"), data.src)
    assert(data.src.contains("p.x * cos(p.y)"), data.src)
    assert(data.src.contains("p.x * sin(p.y)"), data.src)

  test("cartToPolar emits correct WGSL"):
    val data = Polar.cartToPolar.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn cart_to_polar(v: vec2<f32>)"), data.src)
    assert(data.src.contains("-> vec2<f32>"), data.src)
    assert(data.src.contains("length(v)"), data.src)
    assert(data.src.contains("atan2(v.y, v.x)"), data.src)
