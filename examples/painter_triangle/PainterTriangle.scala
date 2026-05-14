package examples.painter_triangle

import trivalibs.graphics.buffers.*
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.None as GPUNone
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import org.scalajs.dom
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("main", moduleID = "painter_triangle")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type Attribs = (position: Vec2, color: Vec3)
    type Varyings = (color: Vec3)
    type Uniforms = (tintColor: Vec3)

    val shade = painter.shade[Attribs, Varyings, Uniforms](
      vertWgsl = """
        out.position = vec4<f32>(in.position, 0.0, 1.0);
        out.color = in.color;
        """,
      fragWgsl = """
        out.color = vec4<f32>(in.color * tintColor, 1.0);
        """,
    )

    val vertices = allocateAttribs[Attribs](3)

    // Vertex 0: top (red)
    vertices(0).set0(0.0, 0.5)
    vertices(0).set1(1.0, 0.0, 0.0)

    // Vertex 1: bottom-left (green)
    vertices(1).set0(-0.5, -0.5)
    vertices(1).set1(0.0, 1.0, 0.0)

    // Vertex 2: bottom-right (blue)
    vertices(2).set0(0.5, -0.5)
    vertices(2).set1(0.0, 0.0, 1.0)

    val form = painter.form(vertices = vertices)
    val tintColor = painter.binding[Vec3]
    val shape = painter.shape(form, shade).bind("tintColor" := tintColor)

    var time = 0.0

    animate: tpf =>
      time += tpf
      val elapsed = time / 2000.0

      tintColor.update: c =>
        c.r = (elapsed * 2.0).sin * 0.5 + 0.5
        c.g = (elapsed * 2.0 + 2.0).sin * 0.5 + 0.5
        c.b = (elapsed * 2.0 + 4.0).sin * 0.5 + 0.5

      painter.draw(shape, clearColor = (0.1, 0.1, 0.1, 1.0))
