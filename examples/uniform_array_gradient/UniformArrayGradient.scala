package examples.uniform_array_gradient

import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.graphics.buffers.*
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given
import trivalibs.utils.random.*

import scala.scalajs.js.annotation.*

// ---------------------------------------------------------------------------
// Multi-step gradients from uniform arrays.
//
// One shade draws every band. Each band binds its own `array<vec4, MaxStops>`
// of color stops and its own array of per-segment exponents, so the number of
// steps and the interpolation between them vary per draw with nothing
// recompiled. Everything is randomised once per page load — reload for a new
// set.
// ---------------------------------------------------------------------------

/** Capacity of the uniform arrays — the WGSL `array<vec4<f32>, N>` length.
  * A band uses between 2 and this many stops; `count` says how many are live.
  */
type MaxStops = 8
val MaxStops: Int = valueOf[MaxStops]

val BandCount = 6
val Gap = 0.02

@JSExportTopLevel("main", moduleID = "uniform_array_gradient")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type Attribs = (position: Vec2)
    type Varyings = (uv: Vec2)
    type Uniforms = (
        // xy = lower-left corner in clip space, zw = size
        rect: VertexUniform[Vec4],
        // xyz = color, w = position along the band in [0,1]
        stops: FragmentUniform[UniformArray[Vec4, MaxStops]],
        // x = exponent for the segment starting at the stop of the same index
        curves: FragmentUniform[UniformArray[Vec4, MaxStops]],
        count: FragmentUniform[Double],
    )

    val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
      program.vert[(p: Vec2)]: ctx =>
        val rect = ctx.bindings.rect
        val p = ctx.locals.p
        Block(
          p := rect.xy + ctx.in.position * rect.zw,
          ctx.out.uv := ctx.in.position,
          ctx.out.position := vec4(p.x, p.y, 0.0, 1.0),
        )

      program.frag[(col: Var[Vec3])]: ctx =>
        val stops = ctx.bindings.stops
        val curves = ctx.bindings.curves
        val count = ctx.bindings.count
        val x = ctx.in.uv.x
        val col = ctx.locals.col

        // A loop over the LIVE stop count, not over the capacity. The band with
        // 2 stops runs one iteration; only the array's length is fixed at
        // `MaxStops`.
        //
        // Each step mixes the whole color so far toward the next stop by the
        // segment's own eased parameter. The mix factor saturates at 1 before
        // the next segment starts, so the chain reproduces the piecewise
        // gradient exactly and holds the final color past the last stop.
        Block(
          col := stops(0).rgb,
          loop(1, count.toI32)(i =>
            val prev = LetVec4("prev")
            val cur = LetVec4("cur")
            val t = LetFloat("t")
            Block(
              prev := stops(i - 1),
              cur := stops(i),
              t := ((x - prev.w) / (cur.w - prev.w)).clamp01,
              col := col.mix(cur.rgb, t.pow(curves(i - 1).x)),
            ),
          ),
          ctx.out.color := vec4(col, 1.0),
        )

    val vertices = allocateAttribs[Attribs](6)
    vertices(0).set0(0.0, 0.0)
    vertices(1).set0(1.0, 0.0)
    vertices(2).set0(1.0, 1.0)
    vertices(3).set0(0.0, 0.0)
    vertices(4).set0(1.0, 1.0)
    vertices(5).set0(0.0, 1.0)

    val form = painter.form(vertices = vertices)

    // -----------------------------------------------------------------------
    // One random gradient per band
    // -----------------------------------------------------------------------

    /** Stop positions: evenly spaced, then jittered by less than half a spacing
      * so they stay strictly increasing. First and last stay pinned to the ends.
      */
    def stopPositions(count: Int): Arr[Double] =
      val spacing = 1.0 / (count - 1)
      val out = Arr[Double]()
      for i <- 0 until count do
        val even = i * spacing
        val jitter =
          if i == 0 || i == count - 1 then 0.0
          else randInRange(-0.4, 0.4) * spacing
        out.push(even + jitter)
      out

    def randomStops(count: Int): Arr[Vec4] =
      val positions = stopPositions(count)
      val out = Arr[Vec4]()
      for i <- 0 until count do
        val c = Vec3(rand(), randInRange(0.45, 0.95), randInRange(0.35, 1.0)).hsv2rgb
        out.push(Vec4(c.x, c.y, c.z, positions(i)))
      out

    /** One exponent per segment, log-uniform in [1/8, 8] so ease-in and
      * ease-out are equally likely and 1.0 (linear) sits in the middle.
      */
    def randomCurves(count: Int): Arr[Vec4] =
      val out = Arr[Vec4]()
      for _ <- 0 until count do
        out.push(Vec4(2.0.pow(randInRange(-3.0, 3.0)), 0.0, 0.0, 0.0))
      out

    val bandHeight = (2.0 - Gap * (BandCount + 1)) / BandCount

    val bands = Arr[AnyShape]()
    for i <- 0 until BandCount do
      val count = randIntInRange(2, MaxStops + 1)
      bands.push(
        painter
          .shape(form, shade)
          .bind(
            "rect" := Vec4(
              -1.0 + Gap,
              -1.0 + Gap + i * (bandHeight + Gap),
              2.0 - 2.0 * Gap,
              bandHeight,
            ),
            "stops" := UniformArray[Vec4, MaxStops](randomStops(count)),
            "curves" := UniformArray[Vec4, MaxStops](randomCurves(count)),
            "count" := count.toDouble,
          ),
      )

    animate: _ =>
      painter.draw(bands(0), clearColor = (0.06, 0.06, 0.08, 1.0))
      var i = 1
      while i < bands.length do
        painter.draw(bands(i))
        i += 1
