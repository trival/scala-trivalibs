package trivalibs.graphics.shader

import trivalibs.graphics.math.gpu.*
import trivalibs.graphics.math.gpu.Expr.{Texture2D, DepthTexture2D}
import munit.FunSuite

class DepthTextureTest extends FunSuite:

  test("panel decls pick texture_depth_2d for depth panel fields"):
    type Panels = (color: FragmentPanel, depth: FragmentDepthPanel)
    val decls = derive.generatePanelDeclarations[Panels]
    assert(
      decls.contains("@group(1) @binding(0) var color: texture_2d<f32>;"),
      s"Missing color texture decl:\n$decls",
    )
    assert(
      decls.contains("@group(1) @binding(1) var depth: texture_depth_2d;"),
      s"Missing depth texture decl:\n$decls",
    )

  test("depth texture load / sample emit correct WGSL (scalar result)"):
    val d = DepthTexture2D("depthTex")
    val s = Sampler("s")
    assertEquals(
      d.load(ivec2(2.i, 3.i), 0.i).wgsl,
      "textureLoad(depthTex, vec2<i32>(2, 3), 0)",
    )
    assertEquals(
      d.load(ivec2(2.i, 3.i)).wgsl,
      "textureLoad(depthTex, vec2<i32>(2, 3), 0)",
    )
    assertEquals(
      d.sample(Vec2Expr("uv"), s).wgsl,
      "textureSample(depthTex, s, uv)",
    )

  test(
    "texture2d load emits correct WGSL (vec4 result), incl. float-vec coord",
  ):
    val t = Texture2D("tex")
    assertEquals(
      t.load(ivec2(0.i, 0.i)).wgsl,
      "textureLoad(tex, vec2<i32>(0, 0), 0)",
    )
    assertEquals(
      ivec2(Vec2Expr("fragCoord")).wgsl,
      "vec2<i32>(fragCoord)",
    )

  test("dimensions + vec2(uvec2) for res-free texel math"):
    val t = Texture2D("tex")
    assertEquals(t.dimensions.wgsl, "textureDimensions(tex)")
    assertEquals(
      vec2(t.dimensions).wgsl,
      "vec2<f32>(textureDimensions(tex))",
    )
    assertEquals(
      ivec2(vec2(t.dimensions)).wgsl,
      "vec2<i32>(vec2<f32>(textureDimensions(tex)))",
    )
