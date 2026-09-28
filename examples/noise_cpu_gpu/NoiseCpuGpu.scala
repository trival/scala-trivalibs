package examples.noise_cpu_gpu

import org.scalajs.dom
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document
import trivalibs.prelude.core.{*, given}
import trivalibs.prelude.painter.{*, given}

import scala.scalajs.js.annotation.*

// CPU / GPU noise parity. Each panel shows one lib noise call, written once
// per side with the same shared extension and the same arguments:
//   - left half: the GPU evaluates it per pixel (shader DSL → WGSL),
//   - right half: the CPU evaluates it per grid cell (inline → scalar kernel),
//     uploaded as a per-vertex gray value.
// Where the preferred parity holds, both halves show the identical field
// (the CPU side is just coarser); at minimum they show the same
// characteristics. Click to cycle.

// One comparison: a name, the GPU call and the CPU call — the same extension
// with the same arguments, mapped to [0, 1] for display.
class Comparison(
    val name: String,
    val gpu: Vec2Expr => FloatExpr,
    val cpu: Vec2 => Double,
)

@JSExportTopLevel("main", moduleID = "noise_cpu_gpu")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]
  val label = document.getElementById("label").asInstanceOf[HTMLElement]

  Painter.init(canvas): painter =>
    type GpuAttribs = (position: Vec2, domain: Vec2)
    type GpuVaryings = (domain: Vec2)
    type CpuAttribs = (position: Vec2, gray: Vec3)
    type CpuVaryings = (gray: Vec3)

    val domainSize = 6.0
    val cells = 120

    val comparisons = Arr(
      Comparison(
        "p.simplexNoise()",
        p => p.simplexNoise().fit1101,
        p => p.simplexNoise().fit1101,
      ),
      Comparison(
        "p.simplexFbm(octaves = 5, gain = 0.6, seed = 7.0)",
        p => p.simplexFbm(octaves = 5, gain = 0.6, seed = 7.0).fit1101,
        p => p.simplexFbm(octaves = 5, gain = 0.6, seed = 7.0).fit1101,
      ),
      Comparison(
        "vec3(p, 1.5).simplexNoise()",
        p => vec3(p.x, p.y, 1.5).simplexNoise().fit1101,
        p => Vec3(p.x, p.y, 1.5).simplexNoise().fit1101,
      ),
      Comparison(
        "vec4(p, 0.3, 0.7).simplexFbm(octaves = 3)",
        p => vec4(p.x, p.y, 0.3, 0.7).simplexFbm(octaves = 3).fit1101,
        p => Vec4(p.x, p.y, 0.3, 0.7).simplexFbm(octaves = 3).fit1101,
      ),
      Comparison(
        "(p / 6).simplexTorusNoise(3.0)",
        p => (p / domainSize).simplexTorusNoise(3.0).fit1101,
        p => (p / domainSize).simplexTorusNoise(3.0).fit1101,
      ),
      Comparison(
        "p.extendedNoiseValue(tilingPeriod = (3, 4), rot = 0.2)",
        p => p.extendedNoiseValue(tilingPeriod = vec2(3.0, 4.0), rot = 0.2).fit1101,
        p => p.extendedNoiseValue(tilingPeriod = Vec2(3.0, 4.0), rot = 0.2).fit1101,
      ),
      Comparison(
        "p.extendedNoise().y  (analytic d/dx)",
        p => p.extendedNoise().y * 0.15 + 0.5,
        p => p.extendedNoise().y * 0.15 + 0.5,
      ),
      Comparison(
        "vec3(p, 0.5).extendedFbmValue(octaves = 4, seed = 2.0)",
        p => vec3(p.x, p.y, 0.5).extendedFbmValue(octaves = 4, seed = 2.0).fit1101,
        p => Vec3(p.x, p.y, 0.5).extendedFbmValue(octaves = 4, seed = 2.0).fit1101,
      ),
      Comparison(
        "p.worleyNoise().x  (F1)",
        p => p.worleyNoise().x,
        p => p.worleyNoise().x,
      ),
      Comparison(
        "vec3(p, 0.5).worleyNoise(jitter = 0.8, seed = 3.0).y  (F2)",
        p => vec3(p.x, p.y, 0.5).worleyNoise(jitter = 0.8, seed = 3.0).y * 0.7,
        p => Vec3(p.x, p.y, 0.5).worleyNoise(jitter = 0.8, seed = 3.0).y * 0.7,
      ),
    )

    // ---- GPU half: one quad over the left half, noise per pixel ----

    val gpuQuad = allocateAttribs[GpuAttribs](6)
    val quadCorners = Arr((0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 0.0), (1.0, 1.0), (0.0, 1.0))
    for i <- 0 until 6 do
      val (u, v) = quadCorners(i)
      gpuQuad(i).set0(-1.0 + u * 0.99, -1.0 + v * 2.0)
      gpuQuad(i).set1(u * domainSize, v * domainSize)
    val gpuForm = painter.form(vertices = gpuQuad)

    def gpuShade(f: Vec2Expr => FloatExpr) =
      painter.shade[GpuAttribs, GpuVaryings, EmptyTuple]: program =>
        program.vert: ctx =>
          Block(
            ctx.out.position := vec4(ctx.in.position, 0.0, 1.0),
            ctx.out.domain := ctx.in.domain,
          )
        program.frag: ctx =>
          val v = LetFloat("v")
          Block(
            v := f(ctx.in.domain),
            ctx.out.color := vec4(v, v, v, 1.0),
          )

    // ---- CPU half: a grid of cells over the right half, value per cell ----

    val cpuShade = painter.shade[CpuAttribs, CpuVaryings, EmptyTuple]: program =>
      program.vert: ctx =>
        Block(
          ctx.out.position := vec4(ctx.in.position, 0.0, 1.0),
          ctx.out.gray := ctx.in.gray,
        )
      program.frag: ctx =>
        ctx.out.color := vec4(ctx.in.gray, 1.0)

    def cpuForm(f: Vec2 => Double) =
      val verts = allocateAttribs[CpuAttribs](cells * cells * 6)
      val cellSize = 1.0 / cells
      var n = 0
      for
        j <- 0 until cells
        i <- 0 until cells
      do
        val value = f(Vec2((i + 0.5) * cellSize * domainSize, (j + 0.5) * cellSize * domainSize))
        for k <- 0 until 6 do
          val (u, v) = quadCorners(k)
          verts(n).set0(0.01 + (i + u) * cellSize * 0.99, -1.0 + (j + v) * cellSize * 2.0)
          verts(n).set1(value, value, value)
          n += 1
      painter.form(vertices = verts)

    val panels = comparisons.map: c =>
      painter.panel(
        clearColor = (0.1, 0.1, 0.14, 1.0),
        shapes = Arr(
          painter.shape(gpuForm, gpuShade(c.gpu)),
          painter.shape(cpuForm(c.cpu), cpuShade),
        ),
      )

    var current = 0
    label.textContent = comparisons(current).name

    canvas.addEventListener(
      "pointerdown",
      (_: dom.Event) =>
        current = (current + 1) % panels.length
        label.textContent = comparisons(current).name,
    )

    animate: _ =>
      painter.paint(panels(current))
      painter.show(panels(current))
