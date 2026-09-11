package examples.sdf_trace

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

import scala.scalajs.js.annotation.*

// ---------------------------------------------------------------------------
// 2D sphere tracing — the shape a runtime loop is for.
//
// Every fragment marches a ray toward the moving light, stepping by the scene's
// signed distance and stopping as soon as it hits something. The number of
// steps is per-fragment and unknowable at build time, so this cannot be
// unrolled: it needs `loopIf` + `break`.
//
// The same march also yields a soft shadow for free — tracking the closest
// approach along the ray widens the penumbra with distance.
// ---------------------------------------------------------------------------

/** March cap. A `while` needs a bound the GPU can rely on; this is it. */
val MaxSteps = 64

@JSExportTopLevel("main", moduleID = "sdf_trace")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type Attribs = (position: Vec2)
    type Varyings = (uv: Vec2)
    type Uniforms = (
        res: FragmentUniform[Vec2],
        time: FragmentUniform[Double],
    )

    // ---- scene ------------------------------------------------------------

    val sdCircle =
      WgslFn.dsl[(p: Vec2, center: Vec2, r: Float), Float]("sd_circle"):
        (p, ret) => ret((p.p - p.center).length - p.r)

    val sdBox =
      WgslFn.dsl[(p: Vec2, center: Vec2, half: Vec2), Float]("sd_box"):
        (p, ret) =>
          val q = LetVec2("q")
          Block(
            q := (p.p - p.center).abs - p.half,
            ret(q.max(0.0).length + q.x.max(q.y).min(0.0)),
          )

    val sceneDist = WgslFn.dsl[(p: Vec2, time: Float), Float]("scene_dist"):
      (p, ret) =>
        val orbit = LetVec2("orbit")
        Block(
          orbit := vec2((p.time * 0.7).cos * 0.45, (p.time * 0.9).sin * 0.3),
          ret(
            sdCircle(p.p, vec2(-0.55, -0.15), 0.18)
              .min(sdCircle(p.p, orbit, 0.12))
              .min(sdBox(p.p, vec2(0.45, 0.25), vec2(0.22, 0.07)))
              .min(sdBox(p.p, vec2(0.1, -0.55), vec2(0.5, 0.05))),
          ),
        )

    // Surfaces read from the distance field; lit air reads from the march.
    def shadedColor(
        d0: FloatExpr,
        lit: FloatExpr,
        distToLight: FloatExpr,
    ): Vec3Expr =
      val falloff = (1.0 - distToLight * 0.55).clamp01
      val inside = (-d0 * 30.0).clamp01
      val glow = lit.clamp01 * falloff
      val air = vec3(0.16, 0.18, 0.28) + vec3(1.0, 0.86, 0.62) * glow * 0.85
      val solid = vec3(0.05, 0.05, 0.09)
      air.mix(solid, inside)

    val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
      program.vert: ctx =>
        Block(
          ctx.out.uv := ctx.in.position,
          ctx.out.position := vec4(ctx.in.position, 0.0, 1.0),
        )

      program.frag: ctx =>
        val time = ctx.bindings.time
        val res = ctx.bindings.res

        val p = LetVec2("p")
        val light = LetVec2("light")
        val toLight = LetVec2("toLight")
        val dist = LetFloat("dist")
        val dir = LetVec2("dir")
        val d0 = LetFloat("d0")

        // Marched state. All four are declared (and seeded) out here: a `var`
        // first assigned inside the loop would be scoped to the loop body.
        val t = VarFloat("t")
        val shade_ = VarFloat("shade")
        val steps = VarInt("steps")
        val d = VarFloat("d")

        Block(
          p := ctx.in.uv * vec2(res.x / res.y, 1.0),
          light := vec2((time * 0.45).sin * 0.75, 0.62),
          toLight := light - p,
          dist := toLight.length,
          dir := toLight.normalize,
          d0 := sceneDist(p, time),

          t := 0.03,
          shade_ := 1.0,
          steps := 0.i,
          d := 0.0,

          // The march. Two exit routes: the condition (reached the light, or
          // ran out of the step budget) and the `break` on a hit.
          loopIf((t < dist) && (steps < MaxSteps))(
            Block(
              d := sceneDist(p + dir * t, time),
              when(d < 0.001)(Block(shade_ := 0.0, break)),
              // Closest approach so far, widened by distance travelled — the
              // standard cheap penumbra.
              shade_ := shade_.min(d * 12.0 / t),
              t := t + d.max(0.01),
              steps := steps + 1,
            ),
          ),

          ctx.out.color := vec4(shadedColor(d0, shade_, dist), 1.0),
        )

    val vertices = allocateAttribs[Attribs](6)
    vertices(0).set0(-1.0, -1.0)
    vertices(1).set0(1.0, -1.0)
    vertices(2).set0(1.0, 1.0)
    vertices(3).set0(-1.0, -1.0)
    vertices(4).set0(1.0, 1.0)
    vertices(5).set0(-1.0, 1.0)

    val uTime = painter.binding(0.0)
    val uRes = painter.binding[Vec2]

    val quad = painter
      .shape(painter.form(vertices = vertices), shade)
      .bind("time" := uTime, "res" := uRes)

    painter.onResize: (w, h) =>
      uRes.set(Vec2(w.toDouble, h.toDouble))

    var elapsed = 0.0
    animate: tpf =>
      elapsed += tpf
      uTime.set(elapsed * 0.001)
      painter.draw(quad, clearColor = (0.06, 0.06, 0.09, 1.0))
