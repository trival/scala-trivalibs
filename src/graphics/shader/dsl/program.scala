package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.gpu.*
import trivalibs.graphics.shader.FragOut
import trivalibs.utils.js.Arr
import trivalibs.utils.js.Dict
import scala.NamedTuple
import scala.NamedTuple.AnyNamedTuple
import scala.scalajs.js

// ---------------------------------------------------------------------------
// Program[A, V, U, P, FO] — DSL program builder with fully typed contexts
//
// Usage:
//   val program = Program[Attribs, Varyings, Uniforms]()
//   program.vert[(rotated: Vec2)]: ctx =>
//     Block(
//       ctx.locals.rotated := ctx.bindings.rotation * ctx.in.position,
//       ctx.out.position   := vec4(ctx.locals.rotated + ctx.bindings.translation, 0.0, 1.0),
//     )
//   program.frag: ctx =>
//     Block(
//       ctx.out.color := vec4(ctx.bindings.color, 1.0),
//     )
// ---------------------------------------------------------------------------

/** DSL builder for a vertex+fragment shader, passed to the `build` lambda of
  * [[trivalibs.graphics.painter.Painter.shade]]. Call [[vert]] and [[frag]] with
  * a function of the typed context `ctx`, which exposes:
  *   - `ctx.in` — inputs (`A` attributes in vert, `V` varyings in frag), read-only
  *   - `ctx.out` — outputs (`V` varyings + `out.position` in vert; `FO` color in
  *     frag); assign with `:=`
  *   - `ctx.bindings` — the `U` uniforms, by field name
  *   - `ctx.textures` — the `P` panel textures, by field name (`.sample(...)`)
  *   - `ctx.locals` — typed local `var`/`let` declared via the `[L]` type param
  *
  * Each `vert`/`frag` body returns a [[trivalibs.graphics.math.gpu.Block]] of
  * statements. Register reusable helpers with [[fn]] (also auto-collected when a
  * `WgslFn` is referenced in a body). The type params `A,V,U,P,FO` are the same
  * named-tuple schemas described on `Painter.shade`.
  */
class Program[A, V, U, P, FO]:
  var vertBody: Block = Block.empty
  var fragBody: Block = Block.empty
  private val fnSrcs = Arr[String]()
  private val fnNames = Dict[Boolean]()

  /** Register a helper function to be emitted before vs_main/fs_main.
    * Idempotent — registering the same name twice has no effect.
    * Recursively registers any deps declared via WgslFn.withDeps.
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

  /** Vertex shader with no typed locals. */
  inline def vert(body: VertexCtx[A, V, U, EmptyTuple, P] => Block): Unit =
    vert[EmptyTuple](body)

  /** Vertex shader with optional typed locals. */
  inline def vert[L](
      body: VertexCtx[A, V, U, L, P] => Block,
  ): Unit =
    val kinds = buildLocalKinds[L]
    val ctx = VertexCtx[A, V, U, L, P](
      in = TypedExprAccessor[NamedTuple.Map[A & AnyNamedTuple, ToExpr]]("in"),
      out = VertexOut[V]("out"),
      bindings =
        TypedExprAccessor[NamedTuple.Map[U & AnyNamedTuple, UniformToExpr]](""),
      locals =
        TypedLocalAccessor[NamedTuple.Map[L & AnyNamedTuple, ToLocal]](kinds),
      textures = TypedPanelAccessor[P](),
    )
    val reg = FnRegistry()
    vertBody = FnRegistry.withActive(reg)(body(ctx))
    reg.items.foreach(fnRec)

  /** Fragment shader with no typed locals. */
  inline def frag(
      body: FragmentCtx[V, U, EmptyTuple, P, FO] => Block,
  ): Unit =
    frag[EmptyTuple](body)

  /** Fragment shader with optional typed locals. */
  inline def frag[L](
      body: FragmentCtx[V, U, L, P, FO] => Block,
  ): Unit =
    val kinds = buildLocalKinds[L]
    val ctx = FragmentCtx[V, U, L, P, FO](
      in = TypedExprAccessor[
        NamedTuple.Map[V & AnyNamedTuple, ToExpr],
      ]("in"),
      out = TypedAssignAccessor[
        NamedTuple.Map[FO & AnyNamedTuple, ToAssign],
      ]("out"),
      bindings =
        TypedExprAccessor[NamedTuple.Map[U & AnyNamedTuple, UniformToExpr]](""),
      locals =
        TypedLocalAccessor[NamedTuple.Map[L & AnyNamedTuple, ToLocal]](kinds),
      textures = TypedPanelAccessor[P](),
    )
    val reg = FnRegistry()
    fragBody = FnRegistry.withActive(reg)(body(ctx))
    reg.items.foreach(fnRec)

  def vertBodyStr: String = Block.unwrap(vertBody)
  def fragBodyStr: String = Block.unwrap(fragBody)
