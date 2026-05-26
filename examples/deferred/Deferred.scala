package examples.deferred

import trivalibs.graphics.buffers.*
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.bufferdata.*
import trivalibs.utils.js.*

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("main", moduleID = "deferred")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    // -----------------------------------------------------------------------
    // G-buffer shade — writes albedo + encoded normal to two render targets
    // -----------------------------------------------------------------------

    type GAttribs = (position: Vec2, color: Vec3, normal: Vec3)
    type GVaryings = (color: Vec3, normal: Vec3)
    type GBufferOut = (color: Vec4, normal: Vec4)

    val gBufferShade =
      painter.shade[GAttribs, GVaryings, EmptyTuple, EmptyTuple, GBufferOut]:
        program =>
          program.vert: ctx =>
            Block(
              ctx.out.color := ctx.in.color,
              ctx.out.normal := ctx.in.normal,
              ctx.out.position := vec4(ctx.in.position, 0.0, 1.0),
            )
          program.frag: ctx =>
            Block(
              ctx.out.color := vec4(ctx.in.color, 1.0),
              ctx.out.normal := vec4(ctx.in.normal * 0.5 + 0.5, 1.0),
            )

    // -----------------------------------------------------------------------
    // Scene geometry — several triangles with different colors and normals
    // -----------------------------------------------------------------------

    val verts = allocateAttribs[GAttribs](12)
    // Triangle 1 — red, facing up-right
    verts(0).set0(0.0, 0.8)
    verts(0).set1(1.0, 0.2, 0.1)
    verts(0).set2(0.3, 0.7, 0.6)
    verts(1).set0(-0.6, -0.3)
    verts(1).set1(0.9, 0.3, 0.1)
    verts(1).set2(0.3, 0.7, 0.6)
    verts(2).set0(0.6, -0.3)
    verts(2).set1(1.0, 0.5, 0.2)
    verts(2).set2(0.3, 0.7, 0.6)
    // Triangle 2 — green, facing left
    verts(3).set0(-0.8, 0.6)
    verts(3).set1(0.1, 0.9, 0.2)
    verts(3).set2(-0.8, 0.2, 0.5)
    verts(4).set0(-0.9, -0.5)
    verts(4).set1(0.0, 0.7, 0.4)
    verts(4).set2(-0.8, 0.2, 0.5)
    verts(5).set0(-0.1, -0.1)
    verts(5).set1(0.2, 1.0, 0.1)
    verts(5).set2(-0.8, 0.2, 0.5)
    // Triangle 3 — blue, facing camera
    verts(6).set0(0.8, 0.7)
    verts(6).set1(0.1, 0.3, 1.0)
    verts(6).set2(0.0, 0.0, 1.0)
    verts(7).set0(0.2, -0.2)
    verts(7).set1(0.2, 0.5, 0.9)
    verts(7).set2(0.0, 0.0, 1.0)
    verts(8).set0(0.9, -0.6)
    verts(8).set1(0.0, 0.2, 0.8)
    verts(8).set2(0.0, 0.0, 1.0)
    // Triangle 4 — yellow, facing down
    verts(9).set0(-0.3, 0.4)
    verts(9).set1(1.0, 1.0, 0.1)
    verts(9).set2(0.1, -0.9, 0.4)
    verts(10).set0(0.3, 0.4)
    verts(10).set1(0.9, 0.9, 0.3)
    verts(10).set2(0.1, -0.9, 0.4)
    verts(11).set0(0.0, -0.8)
    verts(11).set1(1.0, 0.8, 0.0)
    verts(11).set2(0.1, -0.9, 0.4)

    val sceneForm = painter.form(vertices = verts)
    val sceneShape = painter.shape(sceneForm, gBufferShade)

    // -----------------------------------------------------------------------
    // G-buffer panel — 2 render targets: albedo (rgba8unorm) + normals (rgba16float)
    // -----------------------------------------------------------------------

    val gBuffer = painter.panel(
      clearColor = (0.0, 0.0, 0.0, 1.0),
      formats = Arr(TextureFormat.Rgba8Unorm, TextureFormat.Rgba16Float),
      shape = sceneShape,
    )

    // -----------------------------------------------------------------------
    // Lighting layer — reads both G-buffer textures, applies directional light
    // -----------------------------------------------------------------------

    type LightUniforms = (
        lightDir: FragmentUniform[Vec3],
        texSampler: FragmentUniform[Sampler],
    )
    type LightPanels = (
        albedo: FragmentPanel,
        normals: FragmentPanel,
    )

    val lightShade =
      painter.layerShade[LightUniforms, LightPanels]: program =>
        program.frag[(
            albedo: Vec4,
            normalEncoded: Vec4,
            normal: Vec3,
            nDotL: Float,
        )]: ctx =>
          val albedo = ctx.locals.albedo
          val normalEncoded = ctx.locals.normalEncoded
          val normal = ctx.locals.normal
          val nDotL = ctx.locals.nDotL
          Block(
            albedo := ctx.textures.albedo.sample(
              ctx.in.uv,
              ctx.bindings.texSampler,
            ),
            normalEncoded := ctx.textures.normals.sample(
              ctx.in.uv,
              ctx.bindings.texSampler,
            ),
            normal := vec3(
              normalEncoded.x,
              normalEncoded.y,
              normalEncoded.z,
            ) * 2.0 - 1.0,
            nDotL := normal
              .dot(ctx.bindings.lightDir.normalize)
              .max(0.0),
            ctx.out.color := vec4(
              albedo.x * (nDotL + 0.15),
              albedo.y * (nDotL + 0.15),
              albedo.z * (nDotL + 0.15),
              1.0,
            ),
          )

    val nearestSampler = painter.sampler(
      magFilter = FilterMode.Nearest,
      minFilter = FilterMode.Nearest,
    )

    // -----------------------------------------------------------------------
    // Canvas panel — lighting pass composites the G-buffer
    // -----------------------------------------------------------------------

    val lightLayer = painter
      .layer(lightShade, mipSource = -1, mipTarget = -1)
      .bind(
        "lightDir" := Vec3(0.5, 0.7, 1.0),
        "texSampler" := nearestSampler,
        "albedo" := gBuffer.binding(index = 0),
        "normals" := gBuffer.binding(index = 1),
      )

    val canvasPanel = painter.panel(
      clearColor = (0.02, 0.02, 0.06, 1.0),
      layer = lightLayer,
    )

    // -----------------------------------------------------------------------
    // Render loop — animate light direction
    // -----------------------------------------------------------------------

    var time = 0.0

    animate: tpf =>
      time += tpf

      val lx = Math.cos(time * 0.5)
      val lz = Math.sin(time * 0.5)
      lightLayer.bind("lightDir" := Vec3(lx, 0.7, lz))

      painter.paint(gBuffer)
      painter.paint(canvasPanel)
      painter.show(canvasPanel)
