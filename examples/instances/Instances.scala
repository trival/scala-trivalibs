package examples.instances

import trivalibs.graphics.buffers.*
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.None as GPUNone
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.utils.js.*

import trivalibs.utils.random.*

import scala.scalajs.js.annotation.*

val NUM_TRIANGLES = 100

@JSExportTopLevel("main", moduleID = "instances")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type Attribs = (position: Vec3, color: Vec3)
    type Varyings = (color: Vec3)
    type Uniforms = (
        viewProj: VertexUniform[Mat4],
        model: VertexUniform[Mat4],
        tint: FragmentUniform[Vec3],
    )

    val shade = painter.shade[Attribs, Varyings, Uniforms](
      vertWgsl = """
        out.position = viewProj * model * vec4<f32>(in.position, 1.0);
        out.color = in.color;
        """,
      fragWgsl = """
        out.color = vec4<f32>(in.color * tint, 1.0);
        """,
    )

    // Unit triangle in XY plane
    val vertices = allocateAttribs[Attribs](3)
    vertices(0).set0(0.0, 0.4, 0.0)
    vertices(0).set1(1.0, 0.8, 0.8)
    vertices(1).set0(-0.3, -0.2, 0.0)
    vertices(1).set1(0.8, 1.0, 0.8)
    vertices(2).set0(0.3, -0.2, 0.0)
    vertices(2).set1(0.8, 0.8, 1.0)

    val form = painter.form(vertices = vertices)

    val shape = painter.shape(form, shade)

    // Seed per-triangle parameters
    val positions = Arr[(Double, Double, Double)]()
    val colors = Arr[Vec3]()
    val axes = Arr[(Double, Double)]() // (axisChoice, speed)
    var i = 0
    while i < NUM_TRIANGLES do
      val x = randInRange(-8.0, 8.0)
      val y = randInRange(-8.0, 8.0)
      val z = randInRange(-8.0, 8.0)
      positions.push((x, y, z))
      colors.push(
        Vec3(
          randInRange(0.4, 1.0),
          randInRange(0.4, 1.0),
          randInRange(0.4, 1.0),
        ),
      )
      val axis = rand() * 3.0 // 0-1: X, 1-2: Y, 2-3: Z
      val speed = randInRange(-2.0, 2.0)
      axes.push((axis, speed))
      i += 1

    // Pre-create instances with initial bindings
    i = 0
    while i < NUM_TRIANGLES do
      val (x, y, z) = positions(i)
      shape.instances.add(
        "model" := Mat4.fromTranslation(x, y, z),
        "tint" := colors(i),
      )
      i += 1

    // Camera: viewProj shared via panel binding
    val viewProj = painter.binding(Mat4.identity)

    val panel = painter
      .panel(
        clearColor = (0.06, 0.06, 0.12, 1.0),
        shape = shape,
      )
      .bind("viewProj" := viewProj)

    val aspect = canvas.width.toDouble / canvas.height.toDouble
    val proj = Mat4.perspective(math.Pi / 3.0, aspect, 0.1, 100.0)
    val view = Mat4.lookAt(0.0, 5.0, 20.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
    viewProj.set(proj * view)

    painter.onResize: (w, h) =>
      val a = w / h
      val p = Mat4.perspective(math.Pi / 3.0, a, 0.1, 100.0)
      viewProj.set(p * view)

    var time = 0.0

    animate: tpf =>
      time += tpf
      val t = time / 1000.0

      // Update each instance's model matrix with rotation
      i = 0
      while i < NUM_TRIANGLES do
        val (x, y, z) = positions(i)
        val (axisF, speed) = axes(i)
        val angle = t * speed
        val rot =
          if axisF < 1.0 then Mat4.fromRotationX(angle)
          else if axisF < 2.0 then Mat4.fromRotationY(angle)
          else Mat4.fromRotationZ(angle)
        val model = Mat4.fromTranslation(x, y, z) * rot
        shape.instances(i).bind("model" := model)
        i += 1

      painter.paint(panel)
      painter.show(panel)
