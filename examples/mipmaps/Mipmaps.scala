package examples.mipmaps

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

@JSExportTopLevel("main", moduleID = "mipmaps")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    // -----------------------------------------------------------------------
    // Scene shade — colored triangles rendered to the mip source panel
    // -----------------------------------------------------------------------

    type ColorAttribs = (position: Vec2, color: Vec3)
    type ColorVaryings = (color: Vec3)

    val colorShade =
      painter.shade[ColorAttribs, ColorVaryings, EmptyTuple]: program =>
        program.vert: ctx =>
          Block(
            ctx.out.color := ctx.in.color,
            ctx.out.position := vec4(ctx.in.position, 0.0, 1.0),
          )
        program.frag: ctx =>
          ctx.out.color := vec4(ctx.in.color, 1.0)

    // Several colored triangles to create a busy pattern
    val verts = allocateAttribs[ColorAttribs](12)
    // Triangle 1 — red/orange
    verts(0).set0(0.0, 0.8)
    verts(0).set1(1.0, 0.2, 0.1)
    verts(1).set0(-0.6, -0.3)
    verts(1).set1(1.0, 0.6, 0.0)
    verts(2).set0(0.6, -0.3)
    verts(2).set1(0.9, 0.3, 0.1)
    // Triangle 2 — green
    verts(3).set0(-0.8, 0.6)
    verts(3).set1(0.1, 0.9, 0.2)
    verts(4).set0(-0.9, -0.5)
    verts(4).set1(0.0, 0.7, 0.4)
    verts(5).set0(-0.1, -0.1)
    verts(5).set1(0.2, 1.0, 0.1)
    // Triangle 3 — blue
    verts(6).set0(0.8, 0.7)
    verts(6).set1(0.1, 0.3, 1.0)
    verts(7).set0(0.2, -0.2)
    verts(7).set1(0.2, 0.5, 0.9)
    verts(8).set0(0.9, -0.6)
    verts(8).set1(0.0, 0.2, 0.8)
    // Triangle 4 — yellow
    verts(9).set0(-0.3, 0.4)
    verts(9).set1(1.0, 1.0, 0.1)
    verts(10).set0(0.3, 0.4)
    verts(10).set1(0.9, 0.9, 0.3)
    verts(11).set0(0.0, -0.8)
    verts(11).set1(1.0, 0.8, 0.0)

    val sceneForm = painter.form(vertices = verts)
    val sceneShape = painter.shape(sceneForm, colorShade)

    // -----------------------------------------------------------------------
    // Source panel — renders scene with mipmap generation
    // -----------------------------------------------------------------------

    val sourcePanel = painter.panel(
      width = 512,
      height = 512,
      clearColor = (0.05, 0.05, 0.15, 1.0),
      mips = true, // full mip chain
      shape = sceneShape,
    )

    // -----------------------------------------------------------------------
    // Display shade — samples a panel texture at an explicit mip level
    // -----------------------------------------------------------------------

    type QuadAttribs = (position: Vec2, uv: Vec2)
    type QuadVaryings = (uv: Vec2)
    type QuadUniforms = (
        mipLevel: FragmentUniform[Float],
        texSampler: FragmentUniform[Sampler],
    )
    type QuadPanels = (tex: FragmentPanel)

    val quadShade =
      painter.shade[QuadAttribs, QuadVaryings, QuadUniforms, QuadPanels]:
        program =>
          program.vert: ctx =>
            Block(
              ctx.out.uv := ctx.in.uv,
              ctx.out.position := vec4(ctx.in.position, 0.0, 1.0),
            )
          program.frag: ctx =>
            ctx.out.color := ctx.textures.tex.sampleLevel(
              ctx.in.uv,
              ctx.bindings.texSampler,
              ctx.bindings.mipLevel,
            )

    // -----------------------------------------------------------------------
    // Quad geometry helper — positioned quad in clip space
    // -----------------------------------------------------------------------

    def makeQuad(x0: Double, y0: Double, x1: Double, y1: Double): Form =
      val v = allocateAttribs[QuadAttribs](6)
      v(0).set0(x0, y0)
      v(0).set1(0.0, 1.0)
      v(1).set0(x1, y0)
      v(1).set1(1.0, 1.0)
      v(2).set0(x1, y1)
      v(2).set1(1.0, 0.0)
      v(3).set0(x0, y0)
      v(3).set1(0.0, 1.0)
      v(4).set0(x1, y1)
      v(4).set1(1.0, 0.0)
      v(5).set0(x0, y1)
      v(5).set1(0.0, 0.0)
      painter.form(vertices = v)

    // -----------------------------------------------------------------------
    // 4 quads showing mip levels 0, 2, 4, 6 in a 2x2 grid
    // -----------------------------------------------------------------------

    val mipSampler = painter.sampler(
      magFilter = FilterMode.Linear,
      minFilter = FilterMode.Linear,
      mipmapFilter = FilterMode.Nearest,
    )

    val mipLevelsToShow = Arr(0.0f, 2.0f, 4.0f, 6.0f)
    val positions = Arr(
      (-0.98, -0.98, -0.02, -0.02), // bottom-left: mip 0
      (0.02, -0.98, 0.98, -0.02), // bottom-right: mip 2
      (-0.98, 0.02, -0.02, 0.98), // top-left: mip 4
      (0.02, 0.02, 0.98, 0.98), // top-right: mip 6
    )

    val displayShapes = Arr[AnyShape]()
    var i = 0
    while i < 4 do
      val (x0, y0, x1, y1) = positions(i)
      val form = makeQuad(x0, y0, x1, y1)
      val shape = painter
        .shape(form, quadShade)
        .bind(
          "mipLevel" := mipLevelsToShow(i),
          "texSampler" := mipSampler,
          "tex" := sourcePanel,
        )
      displayShapes.push(shape)
      i += 1

    // -----------------------------------------------------------------------
    // Canvas panel — displays the 4 mip-level quads
    // -----------------------------------------------------------------------

    val canvasPanel = painter.panel(
      clearColor = (0.02, 0.02, 0.06, 1.0),
      shapes = displayShapes,
    )

    // -----------------------------------------------------------------------
    // Render loop
    // -----------------------------------------------------------------------

    animate: tpf =>
      painter.paint(sourcePanel, canvasPanel)
      painter.show(canvasPanel)
