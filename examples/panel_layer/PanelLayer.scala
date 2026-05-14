package examples.panel_layer

import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("main", moduleID = "panel_layer")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type Uniforms = (time: Float, res: Vec2)

    val shade = painter.layerShade[Uniforms]: program =>
      program.frag: ctx =>
        val aspect = LetFloat("aspect")
        val p = LetVec2("p")
        val d = LetFloat("d")
        val t = ctx.bindings.time
        val res = ctx.bindings.res
        Block(
          aspect := res.x / res.y,
          p := (ctx.in.uv - 0.5) * vec2(aspect * 2.0, 2.0),
          d := p.length,
          ctx.out.color := vec4(
            (p.x * 4.0 + (p.y * 3.0 + t).sin + t).sin * 0.5 + 0.5,
            (p.y * 4.0 + (p.x * 3.0 - t * 0.7).sin - t * 0.5).sin * 0.5 + 0.5,
            (d * 5.0 - t * 1.5).sin * 0.5 + 0.5,
            1.0,
          ),
        )

    val time = painter.binding(0.0f)
    val res = painter.binding[Vec2]

    val layer = painter.layer(shade).bind("time" := time, "res" := res)
    val panel = painter.panel(layer = layer)

    painter.onResize: (w, h) =>
      res.set(Vec2(w, h))

    animate: tpf =>
      time.set(time.get + tpf * 0.0005f)

      painter.paint(panel)
      painter.show(panel)
