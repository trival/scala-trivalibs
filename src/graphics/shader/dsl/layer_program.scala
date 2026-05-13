package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.gpu.*
import trivalibs.graphics.shader.FragOut
import trivalibs.utils.js.Arr
import trivalibs.utils.js.Dict
import scala.NamedTuple
import scala.NamedTuple.AnyNamedTuple
import scala.scalajs.js

/** DSL program builder for Layer shaders.
  *
  * The vertex stage is fixed (fullscreen triangle from vertex_index) with UV
  * passed as a varying. Users only define the fragment shader:
  *
  * {{{
  * val myShade = painter.layerShade[Uniforms]: program =>
  *   program.frag: ctx =>
  *     ctx.out.color := vec4(ctx.in.uv, 0.0, 1.0)
  * }}}
  */
class LayerProgram[U, P, FO]:
  var fragBody: Block = Block.empty
  private val fnSrcs = Arr[String]()
  private val fnNames = Dict[Boolean]()

  /** Register a helper function. Idempotent — same name registered twice is a
    * no-op. Recursively registers any deps declared via WgslFn.withDeps.
    */
  def fn[FP, R](f: WgslFn[FP, R]): Unit =
    fnRec(f.asInstanceOf[WgslFnData])

  private def fnRec(data: WgslFnData): Unit =
    if !js.DynamicImplicits.truthValue(
        fnNames.asInstanceOf[js.Dynamic].hasOwnProperty(data.name),
      )
    then
      fnNames(data.name) = true
      data.deps.foreach(fnRec)
      fnSrcs.push(data.src)

  def helperFnsStr: String = fnSrcs.join("\n\n")

  /** Fragment shader with no typed locals. `ctx.in.uv` provides screen UV
    * coords (0..1).
    */
  inline def frag(
      body: FragmentCtx[(uv: Vec2), U, EmptyTuple, P, FO] => Block,
  ): Unit = frag[EmptyTuple](body)

  /** Fragment shader with typed locals. */
  inline def frag[L](
      body: FragmentCtx[(uv: Vec2), U, L, P, FO] => Block,
  ): Unit =
    val kinds = buildLocalKinds[L]
    val ctx = FragmentCtx[(uv: Vec2), U, L, P, FO](
      in = TypedExprAccessor[NamedTuple.Map[(uv: Vec2) & AnyNamedTuple, ToExpr]]("in"),
      out = TypedAssignAccessor[
        NamedTuple.Map[FO & AnyNamedTuple, ToAssign],
      ]("out"),
      bindings =
        TypedExprAccessor[NamedTuple.Map[U & AnyNamedTuple, UniformToExpr]](""),
      locals =
        TypedLocalAccessor[NamedTuple.Map[L & AnyNamedTuple, ToLocal]](kinds),
      textures = TypedPanelAccessor[P](),
    )
    fragBody = body(ctx)

  def fragBodyStr: String = Block.unwrap(fragBody)
