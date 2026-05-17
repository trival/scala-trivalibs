package examples.noise_tests

import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.lib.random.Hash
import trivalibs.graphics.shader.lib.random.Psrdnoise
import trivalibs.graphics.shader.lib.random.Simplex
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate
import org.scalajs.dom
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

import scala.scalajs.js
import scala.scalajs.js.annotation.*

// 4×2 grid showing 8 float-output hash functions using DSL if/else.
// Top row: scalar hashes. Bottom row: vec hashes.
private val hashDisplay: WgslFn[(uv: Vec2, time: Float), Vec4] =
  WgslFn
    .dsl[(uv: Vec2, time: Float), Vec4]("hash_display"): (p, ret) =>
      val N = LetFloat("N")
      val M = LetFloat("M")
      val q = LetVec2("q")
      val qi = LetUVec2("qi")
      val border = LetFloat("border")
      val borderHi = LetFloat("borderHi")
      val color = VarVec3("color")
      val qa = LetVec2("qa")
      val qa3 = vec3(qa.x, qa.y, p.time * 0.05)

      Block(
        N := 4.0,
        M := 2.0,
        q := (p.uv * vec2(N, M)).fract,
        qi := uvec2((p.uv.x * N).toU32, (p.uv.y * M).toU32),
        border := 0.025,
        borderHi := FloatExpr("1.0") - border,
        when(
          q.x < border || q.x > borderHi || q.y < border || q.y > borderHi,
          ret(vec4(0.0, 0.0, 0.0, 1.0)),
        ),
        color := vec3(0.0),
        qa := q + vec2(p.time * 0.1, 0.0),
        ifElse(
          qi.y === 0.u,
          ifChain(qi.x === 0.u, color := vec3(Hash.hash1(qa.x.bitsToU32)))
            .elseIf(qi.x === 1.u, color := vec3(Hash.hash1f(qa.x)))
            .elseIf(qi.x === 2.u, color := vec3(Hash.hash21(qa.bitsToU32)))
            .elseDo(color := vec3(Hash.u32ToF32(Hash.hash21i(qa.bitsToU32)))),
          ifChain(qi.x === 0.u, color := vec3(Hash.hash2(qa.bitsToU32), 0.0))
            .elseIf(qi.x === 1.u, color := vec3(Hash.hash2f(qa), 0.0))
            .elseIf(qi.x === 2.u, color := Hash.hash3(qa3.bitsToU32))
            .elseDo(color := Hash.hash3f(qa3)),
        ),
        ret(vec4(color, 1.0)),
      )
    .withDeps(
      Hash.hash1f,
      Hash.hash21,
      Hash.u32ToF32,
      Hash.hash2f,
      Hash.hash3f,
    )

@JSExportTopLevel("main", moduleID = "noise_tests")
def main(): Unit =
  val canvas =
    document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type Uniforms = (time: Float, res: Vec2)

    val aspectUv = WgslFn.dsl[(uv: Vec2, res: Vec2), Vec2]("aspect_uv"):
      (p, ret) =>
        val aspect = LetFloat("aspect")
        val aspectInv = LetFloat("aspectInv")
        Block(
          aspect := p.res.x / p.res.y,
          aspectInv := FloatExpr("1.0") / aspect,
          ret(p.uv * vec2(aspect.min(1.0), aspectInv.min(1.0))),
        )

    def noiseShade(
        body: (
            uv: Vec2Expr,
            t: FloatExpr,
            r: Vec2Expr,
            out: AssignTarget,
        ) => Block,
    ): Shade[Uniforms, EmptyTuple] =
      painter.layerShade[Uniforms]: program =>
        program.frag: ctx =>
          body(ctx.in.uv, ctx.bindings.time, ctx.bindings.res, ctx.out.color)

    // -------------------------------------------------------------------------
    // Simplex noise shaders
    // -------------------------------------------------------------------------

    val simplex2dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * (t.sin * 5.0 + 6.0),
        n := Simplex.simplexNoise2d(uv).fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val simplex3dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 5.0,
        n := Simplex.simplexNoise3d(vec3(uv.x, uv.y, t * 0.3)).fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val simplex2dSeededShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 5.0,
        n := Simplex.simplexNoise2dSeeded(uv, t * 0.2).fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val simplex3dSeededShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 5.0,
        n := Simplex
          .simplexNoise3dSeeded(
            vec3(uv.x, uv.y, 0.0),
            vec3(t * 0.1, t * 0.07, 0.0),
          )
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    // -------------------------------------------------------------------------
    // fBm shaders
    // -------------------------------------------------------------------------

    val fbm2dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 3.0 + vec2(t * 0.2, 0.0),
        n := Simplex.fbmSimplex2d(uv, 5.i, 2.0, 0.5).fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val fbm2dSeededShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 3.0,
        n := Simplex.fbmSimplex2dSeeded(uv, 5.i, 2.0, 0.5, t * 0.3).fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val fbm3dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 3.0,
        n := Simplex
          .fbmSimplex3d(vec3(uv.x, uv.y, t * 0.2), 5.i, 2.0, 0.5)
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val fbm3dSeededShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 3.0,
        n := Simplex
          .fbmSimplex3dSeeded(
            vec3(uv.x, uv.y, 0.0),
            5.i,
            2.0,
            0.5,
            vec3(t * 0.1, 0.0, 0.0),
          )
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    // -------------------------------------------------------------------------
    // Worley noise
    // -------------------------------------------------------------------------

    val worley2dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 5.0 + vec2(t * 0.2, 0.0),
        n := Simplex.worley2d(uv, 1.0).x,
        out := vec4(n, n, n, 1.0),
      )

    // -------------------------------------------------------------------------
    // 4D simplex noise shaders
    // -------------------------------------------------------------------------

    val simplex4dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 4.0,
        n := Simplex
          .simplexNoise4d(vec4(uv.x, uv.y, t * 0.2, t * 0.13))
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val tilingSimplexNoise2dShade = painter.layerShade[Uniforms]: program =>
      program.frag: ctx =>
        val n = LetFloat("n")
        val t = ctx.bindings.time
        Block(
          n := Simplex
            .tilingSimplexNoise2d(
              ctx.in.uv + vec2(t * 0.05, 0.0).fract,
              4.0,
            )
            .fit1101,
          ctx.out.color := vec4(n, n, n, 1.0),
        )

    // -------------------------------------------------------------------------
    // psrdnoise 2D shaders
    // -------------------------------------------------------------------------

    val tilingRotNoise2dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 2.5,
        n := Psrdnoise
          .tilingRotNoise2d(uv.fract * 4.0 + 0.5, vec2(4.0, 4.0), t * 0.1)
          .x
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val tilingNoise2dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 2.5,
        n := Psrdnoise
          .tilingNoise2d(uv.fract * 4.0 + 0.5, vec2(4.0, 4.0))
          .x
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val rotNoise2dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 5.0,
        n := Psrdnoise.rotNoise2d(uv, t * 0.05).x.fit1101,
        out := vec4(n, n, n, 1.0),
      )

    // -------------------------------------------------------------------------
    // psrdnoise 3D shaders
    // -------------------------------------------------------------------------

    val tilingRotNoise3dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 4.0,
        n := Psrdnoise
          .tilingRotNoise3d(
            vec3(uv.x, uv.y, t * 0.1),
            vec3(4.0, 4.0, 4.0),
            t * 0.05,
          )
          .x
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val tilingNoise3dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 4.0,
        n := Psrdnoise
          .tilingNoise3d(vec3(uv.x, uv.y, t * 0.2), vec3(4.0, 4.0, 4.0))
          .x
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    val rotNoise3dShade = noiseShade: (inUv, t, r, out) =>
      val uv = LetVec2("uv")
      val n = LetFloat("n")
      Block(
        uv := aspectUv(inUv, r) * 5.0,
        n := Psrdnoise
          .rotNoise3d(vec3(uv.x, uv.y, t * 0.15), t * 0.03)
          .x
          .fit1101,
        out := vec4(n, n, n, 1.0),
      )

    // -------------------------------------------------------------------------
    // Hash display shader
    // -------------------------------------------------------------------------

    val hashShade = painter.layerShade[Uniforms]: program =>
      program.frag: ctx =>
        val t = ctx.bindings.time
        Block(
          ctx.out.color := hashDisplay(ctx.in.uv, t),
        )

    // -------------------------------------------------------------------------
    // Bindings and panels
    // -------------------------------------------------------------------------

    val time = painter.binding(0.0f)
    val res = painter.binding[Vec2]

    def makePanel(shade: Shade[Uniforms, EmptyTuple]): Panel =
      val layer = painter.layer(shade).bind("time" := time, "res" := res)
      painter.panel(layer = layer)

    val panels = js.Array(
      makePanel(hashShade),
      makePanel(simplex2dShade),
      makePanel(simplex3dShade),
      makePanel(simplex4dShade),
      makePanel(tilingSimplexNoise2dShade),
      makePanel(simplex2dSeededShade),
      makePanel(simplex3dSeededShade),
      makePanel(fbm2dShade),
      makePanel(fbm2dSeededShade),
      makePanel(fbm3dShade),
      makePanel(fbm3dSeededShade),
      makePanel(worley2dShade),
      makePanel(tilingRotNoise2dShade),
      makePanel(tilingNoise2dShade),
      makePanel(rotNoise2dShade),
      makePanel(tilingRotNoise3dShade),
      makePanel(tilingNoise3dShade),
      makePanel(rotNoise3dShade),
    )

    var currentPanel = 0

    canvas.addEventListener(
      "pointerdown",
      (_: dom.Event) => currentPanel = (currentPanel + 1) % panels.length,
    )

    painter.onResize: (w, h) =>
      res.set(Vec2(w, h))

    animate: tpf =>
      time.set(time.get + tpf * 0.001f)
      painter.paint(panels(currentPanel))
      painter.show(panels(currentPanel))
