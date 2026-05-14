package examples.blur

import trivalibs.graphics.buffers.*
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.lib.blur.Blur
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import org.scalajs.dom
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.bufferdata.*
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("main", moduleID = "blur")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    // -----------------------------------------------------------------------
    // Shape shade — UV-colored triangle
    // -----------------------------------------------------------------------

    type Attribs = (position: Vec2, color: Vec3)
    type Varyings = (color: Vec3)

    val shapeShade = painter.shade[Attribs, Varyings, EmptyTuple]: program =>
      program.vert: ctx =>
        Block(
          ctx.out.color := ctx.in.color,
          ctx.out.position := vec4(ctx.in.position, 0.0, 1.0),
        )
      program.frag: ctx =>
        ctx.out.color := vec4(ctx.in.color, 1.0)

    // -----------------------------------------------------------------------
    // Triangle geometry
    // -----------------------------------------------------------------------

    val verts = allocateAttribs[Attribs](3)
    verts(0).set0(0.0, 0.7)
    verts(0).set1(1.0, 0.2, 0.2)
    verts(1).set0(-0.7, -0.5)
    verts(1).set1(0.2, 1.0, 0.2)
    verts(2).set0(0.7, -0.5)
    verts(2).set1(0.2, 0.2, 1.0)
    val form = painter.form(vertices = verts)
    val shape = painter.shape(form, shapeShade)

    // -----------------------------------------------------------------------
    // Blur layer shades
    // -----------------------------------------------------------------------

    type BlurUniforms = (
        diameter: Float,
        resolution: Vec2,
        dir: Vec2,
        blurSampler: Sampler,
    )
    type FixedBlurUniforms = (
        resolution: Vec2,
        dir: Vec2,
        blurSampler: Sampler,
    )
    type BlurPanels = (source: FragmentPanel)

    // Logarithmic-chain shade (dir is pre-scaled by diameter).
    val blur9Shade = painter.layerShade[BlurUniforms, BlurPanels]: program =>
      program.fn(Blur.gaussianBlur9)
      program.frag: ctx =>
        ctx.out.color := Blur.gaussianBlur9(
          ctx.textures.source,
          ctx.bindings.blurSampler,
          ctx.in.uv,
          ctx.bindings.resolution,
          ctx.bindings.dir * ctx.bindings.diameter,
        )

    val blur5Shade = painter.layerShade[FixedBlurUniforms, BlurPanels]:
      program =>
        program.fn(Blur.gaussianBlur5)
        program.frag: ctx =>
          ctx.out.color := Blur.gaussianBlur5(
            ctx.textures.source,
            ctx.bindings.blurSampler,
            ctx.in.uv,
            ctx.bindings.resolution,
            ctx.bindings.dir,
          )

    val boxBlurShade = painter.layerShade[BlurUniforms, BlurPanels]: program =>
      program.fn(Blur.boxBlur)
      program.frag: ctx =>
        ctx.out.color := Blur.boxBlur(
          ctx.textures.source,
          ctx.bindings.blurSampler,
          ctx.bindings.diameter,
          ctx.in.uv,
          ctx.bindings.resolution,
          ctx.bindings.dir,
        )

    val gaussShade = painter.layerShade[BlurUniforms, BlurPanels]: program =>
      program.fn(Blur.gaussianBlur)
      program.frag: ctx =>
        ctx.out.color := Blur.gaussianBlur(
          ctx.textures.source,
          ctx.bindings.blurSampler,
          ctx.bindings.diameter,
          ctx.in.uv,
          ctx.bindings.resolution,
          ctx.bindings.dir,
        )

    // -----------------------------------------------------------------------
    // Shared bindings
    // -----------------------------------------------------------------------

    val res = painter.binding[Vec2]
    val dirH = Vec2(1.0, 0.0)
    val dirV = Vec2(0.0, 1.0)
    val sampler = painter.samplerLinear

    // -----------------------------------------------------------------------
    // Panel 1: heavy logarithmic gaussianBlur9
    // -----------------------------------------------------------------------

    val heavyLayers = Arr[AnyLayer]()
    var d = 32.0
    while d > 1.0 do
      heavyLayers.push(
        painter
          .layer(blur9Shade)
          .bind(
            "diameter" := d,
            "resolution" := res,
            "dir" := dirH,
            "blurSampler" := sampler,
          ),
      )
      heavyLayers.push(
        painter
          .layer(blur9Shade)
          .bind(
            "diameter" := d,
            "resolution" := res,
            "dir" := dirV,
            "blurSampler" := sampler,
          ),
      )
      d = d / 2.0

    // -----------------------------------------------------------------------
    // Panel 2: small gaussianBlur5 (single H + V, fixed 5px)
    // -----------------------------------------------------------------------

    val blur5Layers = Arr(
      painter
        .layer(blur5Shade)
        .bind(
          "resolution" := res,
          "dir" := dirH,
          "blurSampler" := sampler,
        ),
      painter
        .layer(blur5Shade)
        .bind(
          "resolution" := res,
          "dir" := dirV,
          "blurSampler" := sampler,
        ),
    )

    // -----------------------------------------------------------------------
    // Panel 3: box blur (single H + V, animated diameter on a sine curve)
    // -----------------------------------------------------------------------

    val animDiameter = painter.binding(1.0)

    val boxLayers = Arr(
      painter
        .layer(boxBlurShade)
        .bind(
          "diameter" := animDiameter,
          "resolution" := res,
          "dir" := dirH,
          "blurSampler" := sampler,
        ),
      painter
        .layer(boxBlurShade)
        .bind(
          "diameter" := animDiameter,
          "resolution" := res,
          "dir" := dirV,
          "blurSampler" := sampler,
        ),
    )

    // -----------------------------------------------------------------------
    // Panel 4: animated generic gaussian blur — diameter on a sine curve
    // -----------------------------------------------------------------------

    val animLayers = Arr(
      painter
        .layer(gaussShade)
        .bind(
          "diameter" := animDiameter,
          "resolution" := res,
          "dir" := dirH,
          "blurSampler" := sampler,
        ),
      painter
        .layer(gaussShade)
        .bind(
          "diameter" := animDiameter,
          "resolution" := res,
          "dir" := dirV,
          "blurSampler" := sampler,
        ),
    )

    // -----------------------------------------------------------------------
    // Panels
    // -----------------------------------------------------------------------

    def makePanel[L <: AnyLayer](layers: Arr[L]): Panel =
      painter.panel(
        clearColor = (0.04, 0.04, 0.15, 1.0),
        shape = shape,
        layers = layers,
        multisample = true,
      )

    val panels = js.Array(
      makePanel(heavyLayers),
      makePanel(blur5Layers),
      makePanel(boxLayers),
      makePanel(animLayers),
    )

    var currentPanel = 0
    canvas.addEventListener(
      "pointerdown",
      (_: dom.Event) => currentPanel = (currentPanel + 1) % panels.length,
    )

    painter.onResize: (w, h) =>
      res.set(Vec2(w, h))

    var time = 0.0
    animate: tpf =>
      time = time + tpf * 0.001
      // Sweep diameters on sine curves.
      animDiameter.set(60.0 + 58.0 * time.sin)
      painter.paint(panels(currentPanel))
      painter.show(panels(currentPanel))
