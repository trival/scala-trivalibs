package trivalibs.graphics.shader

import trivalibs.graphics.math.gpu.*
import munit.FunSuite

class ShaderTest extends FunSuite:

  test("minimal shader with defaults generates correct WGSL"):
    type SimpleAttribs = (position: Vec3, color: Vec4)
    type SimpleVaryings = (color: Vec4)

    val shader = Shader[SimpleAttribs, SimpleVaryings, None](
      vertexBody = """
  out.position = vec4(in.position, 1.0);
  out.color = in.color;""",
      fragmentBody = """
  out.color = in.color;""",
    )

    val wgsl = shader.generateWGSL

    // Check VertexInput struct
    assert(
      wgsl.contains("struct VertexInput {"),
      s"Missing VertexInput struct:\n$wgsl",
    )
    assert(
      wgsl.contains("@location(0) position: vec3<f32>"),
      s"Missing position attribute:\n$wgsl",
    )
    assert(
      wgsl.contains("@location(1) color: vec4<f32>"),
      s"Missing color attribute:\n$wgsl",
    )

    // Check VertexOutput struct (varyings + builtin position)
    assert(
      wgsl.contains("struct VertexOutput {"),
      s"Missing VertexOutput struct:\n$wgsl",
    )
    assert(
      wgsl.contains("@location(0) color: vec4<f32>"),
      s"Missing varying color:\n$wgsl",
    )
    assert(
      wgsl.contains("@builtin(position) position: vec4<f32>"),
      s"Missing builtin position:\n$wgsl",
    )

    // Check FragmentOutput struct
    assert(
      wgsl.contains("struct FragmentOutput {"),
      s"Missing FragmentOutput struct:\n$wgsl",
    )

    // Check vertex main
    assert(wgsl.contains("@vertex"), s"Missing @vertex annotation:\n$wgsl")
    assert(
      wgsl.contains("fn vs_main(in: VertexInput) -> VertexOutput"),
      s"Missing vs_main signature:\n$wgsl",
    )
    assert(
      wgsl.contains("var out: VertexOutput"),
      s"Missing out variable in vertex:\n$wgsl",
    )
    assert(
      wgsl.contains("return out;"),
      s"Missing return out in vertex:\n$wgsl",
    )

    // Check fragment main
    assert(wgsl.contains("@fragment"), s"Missing @fragment annotation:\n$wgsl")
    assert(
      wgsl.contains("fn fs_main(in: VertexOutput) -> FragmentOutput"),
      s"Missing fs_main signature:\n$wgsl",
    )
    assert(
      wgsl.contains("var out: FragmentOutput"),
      s"Missing out variable in fragment:\n$wgsl",
    )

  test("shader with uniforms generates correct binding declarations"):
    type Attribs = (position: Vec3)
    type Varyings = (dummy: Vec4)
    type Uniforms = (
        camera: (viewProj: Mat4, time: Float),
        material: (albedo: Vec4),
    )

    val shader = Shader[Attribs, Varyings, Uniforms](
      vertexBody = "",
      fragmentBody = "",
    )

    val wgsl = shader.generateWGSL

    // Check uniform declarations
    assert(
      wgsl.contains(
        "@group(0) @binding(0) var<uniform> viewProj: mat4x4<f32>;",
      ),
      s"Missing viewProj uniform:\n$wgsl",
    )
    assert(
      wgsl.contains("@group(0) @binding(1) var<uniform> time: f32;"),
      s"Missing time uniform:\n$wgsl",
    )
    assert(
      wgsl.contains("@group(1) @binding(0) var<uniform> albedo: vec4<f32>;"),
      s"Missing albedo uniform:\n$wgsl",
    )

  test("shader with custom vertex builtin input"):
    type Attribs = (position: Vec3)
    type Varyings = (color: Vec4)
    type VertBI = (vertexIndex: BuiltinVertexIndex)

    val shader = Shader.full[
      Attribs,
      Varyings,
      None,
      VertBI, // custom vertex builtin input
      VertOut, // default position output
      None, // no fragment builtin input
      FragOut, // default color output
    ](
      vertexBody = "",
      fragmentBody = "",
    )

    val wgsl = shader.generateWGSL

    // Check VertexInput has both location and builtin
    assert(
      wgsl.contains("@location(0) position: vec3<f32>"),
      s"Missing position attribute:\n$wgsl",
    )
    assert(
      wgsl.contains("@builtin(vertex_index) vertexIndex: u32"),
      s"Missing vertex_index builtin:\n$wgsl",
    )

  test("empty tuples generate no struct content"):
    type Attribs = (position: Vec3)
    type Varyings = (color: Vec4)

    val shader = Shader[Attribs, Varyings, None](
      vertexBody = "",
      fragmentBody = "",
    )

    val wgsl = shader.generateWGSL

    // Should not have uniform declarations when Uniforms is None
    assert(!wgsl.contains("@group"), s"Should not have uniform groups:\n$wgsl")

  test("generated WGSL compiles expected structure"):
    type Attribs = (position: Vec3, color: Vec4, uv: Vec2)
    type Varyings = (color: Vec4, uv: Vec2)
    type Uniforms = (
        scene: (mvp: Mat4),
    )

    val shader = Shader[Attribs, Varyings, Uniforms](
      vertexBody = """
  out.position = mvp * vec4(in.position, 1.0);
  out.color = in.color;
  out.uv = in.uv;""",
      fragmentBody = """
  out.color = in.color;""",
    )

    val wgsl = shader.generateWGSL

    // Print for visual inspection during development
    println("=== Generated WGSL ===")
    println(wgsl)
    println("======================")

    // Verify structure order
    val vertexInputIdx = wgsl.indexOf("struct VertexInput")
    val vertexOutputIdx = wgsl.indexOf("struct VertexOutput")
    val fragmentOutputIdx = wgsl.indexOf("struct FragmentOutput")
    val uniformIdx = wgsl.indexOf("@group(0)")
    val vsMainIdx = wgsl.indexOf("@vertex")
    val fsMainIdx = wgsl.indexOf("@fragment")

    assert(
      vertexInputIdx < vertexOutputIdx,
      "VertexInput should come before VertexOutput",
    )
    assert(
      vertexOutputIdx < fragmentOutputIdx,
      "VertexOutput should come before FragmentOutput",
    )
    assert(
      fragmentOutputIdx < uniformIdx,
      "Structs should come before uniforms",
    )
    assert(uniformIdx < vsMainIdx, "Uniforms should come before vs_main")
    assert(vsMainIdx < fsMainIdx, "vs_main should come before fs_main")

  test("shader with visibility-wrapped uniforms generates correct WGSL"):
    type Attribs = (position: Vec3)
    type Varyings = (dummy: Vec4)
    type Uniforms = (
        scene: (viewProj: SharedUniform[Mat4], time: VertexUniform[Float]),
        material: (albedo: FragmentUniform[Vec4]),
    )

    val shader = Shader[Attribs, Varyings, Uniforms](
      vertexBody = "",
      fragmentBody = "",
    )

    val wgsl = shader.generateWGSL

    // Visibility wrappers should be transparent in WGSL output
    assert(
      wgsl.contains(
        "@group(0) @binding(0) var<uniform> viewProj: mat4x4<f32>;",
      ),
      s"Missing viewProj uniform:\n$wgsl",
    )
    assert(
      wgsl.contains("@group(0) @binding(1) var<uniform> time: f32;"),
      s"Missing time uniform:\n$wgsl",
    )
    assert(
      wgsl.contains("@group(1) @binding(0) var<uniform> albedo: vec4<f32>;"),
      s"Missing albedo uniform:\n$wgsl",
    )
