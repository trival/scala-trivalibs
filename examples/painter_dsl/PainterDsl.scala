package examples.painter_dsl

import trivalibs.graphics.buffers.*
import trivalibs.graphics.math.cpu.Mat2Buffer.given
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.None as GPUNone
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import org.scalajs.dom
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("main", moduleID = "painter_dsl")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type Attribs = (position: Vec2)
    type Varyings = EmptyTuple
    type Uniforms = (
        color: FragmentUniform[Vec3],
        rotation: VertexUniform[Mat2],
        translation: VertexUniform[Vec2],
    )

    // Helper function defined with raw WGSL and applies mat2 rotation + translation
    val applyTransform =
      WgslFn.dsl[(pos: Vec2, mat: Mat2, offset: Vec2), Vec2](
        "apply_transform",
      ): (p, ret) =>
        ret(p.offset + p.mat * p.pos)

    val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
      program.vert[(t: Vec2)]: ctx =>
        val t = ctx.locals.t
        Block(
          t := applyTransform(
            ctx.in.position,
            ctx.bindings.rotation,
            ctx.bindings.translation,
          ),
          ctx.out.position := vec4(t.x, t.y, 0.0, 1.0),
        )
      program.frag[EmptyTuple]: ctx =>
        ctx.out.color := vec4(ctx.bindings.color, 1.0)

    // Simple triangle centered at origin
    val vertices = allocateAttribs[Attribs](3)
    vertices(0).set0(0.0, 0.3)
    vertices(1).set0(-0.25, -0.15)
    vertices(2).set0(0.25, -0.15)

    val form = painter.form(vertices = vertices)

    // Triangle 1: red, fast rotation, oscillates up/down
    val shape1 = painter
      .shape(form, shade)
      .bind("color" := Vec3(1.0, 0.2, 0.2))

    val mat1 = painter.binding(Mat2.identity)

    // Triangle 2: blue, slow rotation, oscillates left/right
    val shape2 = painter
      .shape(form, shade)
      .bind("color" := Vec3(0.2, 0.5, 1.0), "rotation" := mat1)

    var time = 0.0

    animate: tpf =>
      time += tpf
      val t = time / 1000.0

      shape1.bind(
        "rotation" := Mat2.fromRotation(t * 2.0),
        "translation" := Vec2(0.0, t.sin * 0.7),
      )
      shape2.bind(
        "translation" := Vec2((t * 0.7).cos * 0.7, 0.0),
      )

      mat1.update:
        _.rotateSelf(tpf / 1000 * 0.5)

      painter.draw(shape1, clearColor = (0.1, 0.1, 0.1, 1.0))
      painter.draw(shape2)
