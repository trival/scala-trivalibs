package examples.panel_triangle

import trivalibs.graphics.buffers.*
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.None as GPUNone
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("main", moduleID = "panel_triangle")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type Attribs = (position: Vec2, color: Vec3)
    type Varyings = (color: Vec3)
    type Uniforms = (
        tintColor: FragmentUniform[Vec3],
        translation: VertexUniform[Vec2],
    )

    val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
      program.vert: ctx =>
        val pos = LetVec2("pos")
        Block(
          pos := ctx.in.position + ctx.bindings.translation,
          ctx.out.position := vec4(pos, 0.0, 1.0),
          ctx.out.color := ctx.in.color,
        )
      program.frag: ctx =>
        ctx.out.color := vec4(ctx.in.color * ctx.bindings.tintColor, 1.0)

    // Small triangle centered at origin, shared across all three shapes
    val vertices = allocateAttribs[Attribs](3)
    vertices(0).set0(0.0, 0.3)
    vertices(0).set1(1.0, 0.2, 0.2) // warm red
    vertices(1).set0(-0.25, -0.15)
    vertices(1).set1(0.2, 1.0, 0.2) // warm green
    vertices(2).set0(0.25, -0.15)
    vertices(2).set1(0.2, 0.2, 1.0) // warm blue

    val form = painter.form(vertices = vertices)

    // Three shapes at different positions with independent tint animations
    val shape1 = painter
      .shape(form, shade)
      .bind(
        "translation" := Vec2(0.0, 0.55),
      )
    val shape2 = painter
      .shape(form, shade)
      .bind(
        "translation" := Vec2(-0.55, -0.4),
      )
    val shape3 = painter
      .shape(form, shade)
      .bind(
        "translation" := Vec2(0.55, -0.4),
      )

    val panel = painter.panel(
      clearColor = (0.08, 0.08, 0.12, 1.0),
      shapes = Arr(shape1, shape2, shape3),
    )

    var time = 0.0

    animate: tpf =>
      time += tpf
      val t = time / 1500.0

      shape1.bind(
        "tintColor" := Vec3(
          (t * 1.5).sin * 0.5 + 0.5,
          (t * 1.5 + 2.0).sin * 0.5 + 0.5,
          (t * 1.5 + 4.0).sin * 0.5 + 0.5,
        ),
      )
      shape2.bind(
        "tintColor" := Vec3(
          (t * 1.0 + 2.0).sin * 0.5 + 0.5,
          (t * 1.0 + 4.0).sin * 0.5 + 0.5,
          (t * 1.0).sin * 0.5 + 0.5,
        ),
      )
      shape3.bind(
        "tintColor" := Vec3(
          (t * 2.0 + 4.0).sin * 0.5 + 0.5,
          (t * 2.0).sin * 0.5 + 0.5,
          (t * 2.0 + 2.0).sin * 0.5 + 0.5,
        ),
      )

      painter.paint(panel)
      painter.show(panel)
