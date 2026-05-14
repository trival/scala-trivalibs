package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.gpu.*
import trivalibs.graphics.shader.WGSLType
import trivalibs.utils.js.Arr

import scala.NamedTuple.AnyNamedTuple
import scala.compiletime.*

// ---------------------------------------------------------------------------
// WgslFnData — runtime carrier (zero Scala boxing)
// ---------------------------------------------------------------------------

class WgslFnData(
    val name: String,
    val src: String,
    val deps: Arr[WgslFnData] = Arr(),
) extends scala.scalajs.js.Object

// ---------------------------------------------------------------------------
// WgslFn[P, R] — typed opaque wrapper
//
// P = named tuple of parameter types, e.g. (v: Vec2, angle: Float)
// R = return type, e.g. Vec2 (or Unit for void)
// ---------------------------------------------------------------------------

opaque type WgslFn[P, R] = WgslFnData

object WgslFn:

  // -------------------------------------------------------------------------
  // Raw WGSL body constructor
  // -------------------------------------------------------------------------

  /** Define a WGSL function with a raw string body.
    *
    * Example:
    * {{{
    * val rotate: WgslFn[(v: Vec2, angle: Float), Vec2] =
    *   WgslFn.raw("rotate"):
    *     "return vec2<f32>(v.x * cos(angle) - v.y * sin(angle), ...);"
    * }}}
    */
  inline def raw[P, R](name: String)(body: String): WgslFn[P, R] =
    val paramList = buildParamList[P]
    val retType = wgslReturnType[R]
    val src = s"fn $name($paramList) -> $retType {\n$body\n}"
    WgslFnData(name, src)

  /** Build a WgslFn from a pre-built full WGSL source string. Use when the
    * source includes struct declarations or helper functions that cannot be
    * expressed as individual WgslFns (e.g. psrdnoise's `NG2` struct return).
    * The caller must ensure `src` contains a function named `name` whose
    * parameter types and return type match `P` and `R`.
    */
  def fromSrc[P, R](name: String)(src: String): WgslFn[P, R] =
    WgslFnData(name, src)

  // -------------------------------------------------------------------------
  // DSL body constructor
  // -------------------------------------------------------------------------

  /** Define a WGSL function using the Scala shader DSL.
    *
    * Example:
    * {{{
    * val rotate: WgslFn[(v: Vec2, angle: Float), Vec2] =
    *   WgslFn.dsl("rotate"): (p, ret) =>
    *     ret(vec2(
    *       p.v.x * p.angle.cos - p.v.y * p.angle.sin,
    *       p.v.x * p.angle.sin + p.v.y * p.angle.cos,
    *     ))
    * }}}
    */
  inline def dsl[P, R](name: String)(
      body: (
          TypedExprAccessor[NamedTuple.Map[P & AnyNamedTuple, ToExpr]],
          ReturnEmitter[R],
      ) => Block,
  ): WgslFn[P, R] =
    val p = TypedExprAccessor[NamedTuple.Map[P & AnyNamedTuple, ToExpr]]("")
    val ret = ReturnEmitter[R]()
    val block = body(p, ret)
    val paramList = buildParamList[P]
    val retType = wgslReturnType[R]
    val src = s"fn $name($paramList) -> $retType {\n${Block.unwrap(block)}\n}"
    WgslFnData(name, src)

  // -------------------------------------------------------------------------
  // Internal: compile-time param list + return type
  // -------------------------------------------------------------------------

  private inline def buildParamList[P]: String =
    inline erasedValue[P] match
      case _: EmptyTuple    => ""
      case _: AnyNamedTuple =>
        val names = paramNames[NamedTuple.Names[P & AnyNamedTuple]]
        val types = paramWgslTypes[NamedTuple.DropNames[P & AnyNamedTuple]]
        val parts = Arr[String]()
        var i = 0
        while i < names.length do
          parts.push(s"${names(i)}: ${types(i)}")
          i += 1
        parts.join(", ")
      case _: Tuple =>
        val types = paramWgslTypes[P & Tuple]
        val parts = Arr[String]()
        var i = 0
        while i < types.length do
          parts.push(s"p$i: ${types(i)}")
          i += 1
        parts.join(", ")

  private inline def paramNames[N <: Tuple]: Arr[String] =
    inline erasedValue[N] match
      case _: EmptyTuple  => Arr()
      case _: (n *: rest) =>
        Arr(constValue[n].asInstanceOf[String]) ++ paramNames[rest]

  private inline def paramWgslTypes[T <: Tuple]: Arr[String] =
    inline erasedValue[T] match
      case _: EmptyTuple  => Arr()
      case _: (h *: rest) =>
        Arr(summonInline[WGSLType[h]].wgslName) ++ paramWgslTypes[rest]

  inline def wgslReturnType[R]: String =
    inline erasedValue[R] match
      case _: Unit => "void"
      case _       => summonInline[WGSLType[R]].wgslName

  inline def callExpr[R](s: String): ToExpr[R] =
    inline erasedValue[R] match
      case _: Float   => FloatExpr(s).asInstanceOf[ToExpr[R]]
      case _: Double  => FloatExpr(s).asInstanceOf[ToExpr[R]]
      case _: Int     => IntExpr(s).asInstanceOf[ToExpr[R]]
      case _: UInt    => UIntExpr(s).asInstanceOf[ToExpr[R]]
      case _: Vec2    => Vec2Expr(s).asInstanceOf[ToExpr[R]]
      case _: Vec3    => Vec3Expr(s).asInstanceOf[ToExpr[R]]
      case _: Vec4    => Vec4Expr(s).asInstanceOf[ToExpr[R]]
      case _: IVec2   => IVec2Expr(s).asInstanceOf[ToExpr[R]]
      case _: IVec3   => IVec3Expr(s).asInstanceOf[ToExpr[R]]
      case _: IVec4   => IVec4Expr(s).asInstanceOf[ToExpr[R]]
      case _: UVec2   => UVec2Expr(s).asInstanceOf[ToExpr[R]]
      case _: UVec3   => UVec3Expr(s).asInstanceOf[ToExpr[R]]
      case _: UVec4   => UVec4Expr(s).asInstanceOf[ToExpr[R]]
      case _: Mat2    => Mat2Expr(s).asInstanceOf[ToExpr[R]]
      case _: Mat3    => Mat3Expr(s).asInstanceOf[ToExpr[R]]
      case _: Mat4    => Mat4Expr(s).asInstanceOf[ToExpr[R]]
      case _: Boolean => BoolExpr(s).asInstanceOf[ToExpr[R]]

  // Inside the companion, WgslFn[P,R] is transparent (= WgslFnData),
  // so we can access .name and .src directly.
  private[dsl] def nameOf[P, R](fn: WgslFn[P, R]): String = fn.name
  private[dsl] def srcOf[P, R](fn: WgslFn[P, R]): String = fn.src

  // -------------------------------------------------------------------------
  // Dependency declaration — withDeps chains deps onto a WgslFn so that
  // Program.fn / LayerProgram.fn registers them automatically before self.
  // -------------------------------------------------------------------------

  // Implicit coercion: any WgslFn[P,R] → WgslFnData.
  // Inside the companion WgslFn[P,R] is transparent (= WgslFnData), so identity
  // is valid. The given lives in implicit scope (companion of WgslFn) and is
  // found automatically at call sites without explicit import.
  given [P, R]: Conversion[WgslFn[P, R], WgslFnData] = identity

  extension [P, R](fn: WgslFn[P, R])
    def withDeps(ds: WgslFnData*): WgslFn[P, R] =
      val depsArr = Arr[WgslFnData]()
      for d <- ds do depsArr.push(d)
      WgslFnData(fn.name, fn.src, depsArr)

  // -------------------------------------------------------------------------
  // apply extensions — per-arity, enabling myFn(arg1, arg2) call syntax
  //
  // Defined inside the companion so they are in the opaque type's implicit
  // scope and resolved automatically at call sites without explicit import.
  // -------------------------------------------------------------------------

  // Arity 1 — unnamed
  extension [N1, R](fn: WgslFn[N1 *: EmptyTuple, R])
    inline def apply(a1: ToExpr[N1]): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1)")

  // Arity 1 — named tuple
  extension [K1 <: String, N1, R](
      fn: WgslFn[NamedTuple.NamedTuple[K1 *: EmptyTuple, N1 *: EmptyTuple], R]
  )
    inline def apply(a1: ToExpr[N1]): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1)")

  // Arity 2 — unnamed
  extension [N1, N2, R](fn: WgslFn[N1 *: N2 *: EmptyTuple, R])
    inline def apply(a1: ToExpr[N1], a2: ToExpr[N2]): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2)")

  // Arity 2 — named tuple
  extension [K1 <: String, K2 <: String, N1, N2, R](
      fn: WgslFn[
        NamedTuple.NamedTuple[K1 *: K2 *: EmptyTuple, N1 *: N2 *: EmptyTuple],
        R,
      ]
  )
    inline def apply(a1: ToExpr[N1], a2: ToExpr[N2]): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2)")

  // Arity 3 — unnamed
  extension [N1, N2, N3, R](fn: WgslFn[N1 *: N2 *: N3 *: EmptyTuple, R])
    inline def apply(
        a1: ToExpr[N1],
        a2: ToExpr[N2],
        a3: ToExpr[N3],
    ): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2, $a3)")

  // Arity 3 — named tuple
  extension [K1 <: String, K2 <: String, K3 <: String, N1, N2, N3, R](
      fn: WgslFn[NamedTuple.NamedTuple[
        K1 *: K2 *: K3 *: EmptyTuple,
        N1 *: N2 *: N3 *: EmptyTuple,
      ], R]
  )
    inline def apply(
        a1: ToExpr[N1],
        a2: ToExpr[N2],
        a3: ToExpr[N3],
    ): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2, $a3)")

  // Arity 4 — unnamed
  extension [N1, N2, N3, N4, R](
      fn: WgslFn[N1 *: N2 *: N3 *: N4 *: EmptyTuple, R]
  )
    inline def apply(
        a1: ToExpr[N1],
        a2: ToExpr[N2],
        a3: ToExpr[N3],
        a4: ToExpr[N4],
    ): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2, $a3, $a4)")

  // Arity 4 — named tuple
  extension [
      K1 <: String,
      K2 <: String,
      K3 <: String,
      K4 <: String,
      N1,
      N2,
      N3,
      N4,
      R,
  ](
      fn: WgslFn[NamedTuple.NamedTuple[
        K1 *: K2 *: K3 *: K4 *: EmptyTuple,
        N1 *: N2 *: N3 *: N4 *: EmptyTuple,
      ], R]
  )
    inline def apply(
        a1: ToExpr[N1],
        a2: ToExpr[N2],
        a3: ToExpr[N3],
        a4: ToExpr[N4],
    ): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2, $a3, $a4)")

  // Arity 5 — unnamed
  extension [N1, N2, N3, N4, N5, R](
      fn: WgslFn[N1 *: N2 *: N3 *: N4 *: N5 *: EmptyTuple, R]
  )
    inline def apply(
        a1: ToExpr[N1],
        a2: ToExpr[N2],
        a3: ToExpr[N3],
        a4: ToExpr[N4],
        a5: ToExpr[N5],
    ): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2, $a3, $a4, $a5)")

  // Arity 5 — named tuple
  extension [
      K1 <: String,
      K2 <: String,
      K3 <: String,
      K4 <: String,
      K5 <: String,
      N1,
      N2,
      N3,
      N4,
      N5,
      R,
  ](
      fn: WgslFn[NamedTuple.NamedTuple[
        K1 *: K2 *: K3 *: K4 *: K5 *: EmptyTuple,
        N1 *: N2 *: N3 *: N4 *: N5 *: EmptyTuple,
      ], R]
  )
    inline def apply(
        a1: ToExpr[N1],
        a2: ToExpr[N2],
        a3: ToExpr[N3],
        a4: ToExpr[N4],
        a5: ToExpr[N5],
    ): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2, $a3, $a4, $a5)")

  // Arity 6 — unnamed
  extension [N1, N2, N3, N4, N5, N6, R](
      fn: WgslFn[N1 *: N2 *: N3 *: N4 *: N5 *: N6 *: EmptyTuple, R]
  )
    inline def apply(
        a1: ToExpr[N1],
        a2: ToExpr[N2],
        a3: ToExpr[N3],
        a4: ToExpr[N4],
        a5: ToExpr[N5],
        a6: ToExpr[N6],
    ): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2, $a3, $a4, $a5, $a6)")

  // Arity 6 — named tuple
  extension [
      K1 <: String,
      K2 <: String,
      K3 <: String,
      K4 <: String,
      K5 <: String,
      K6 <: String,
      N1,
      N2,
      N3,
      N4,
      N5,
      N6,
      R,
  ](
      fn: WgslFn[NamedTuple.NamedTuple[
        K1 *: K2 *: K3 *: K4 *: K5 *: K6 *: EmptyTuple,
        N1 *: N2 *: N3 *: N4 *: N5 *: N6 *: EmptyTuple,
      ], R]
  )
    inline def apply(
        a1: ToExpr[N1],
        a2: ToExpr[N2],
        a3: ToExpr[N3],
        a4: ToExpr[N4],
        a5: ToExpr[N5],
        a6: ToExpr[N6],
    ): ToExpr[R] =
      callExpr[R](s"${nameOf(fn)}($a1, $a2, $a3, $a4, $a5, $a6)")

  // -------------------------------------------------------------------------
  // DSL body constructor with typed locals
  // -------------------------------------------------------------------------

  /** Define a WGSL function using the Scala shader DSL with typed locals.
    *
    * Type parameters: [L, P, R] — locals, params, return type.
    *
    * Example:
    * {{{
    * val accumulate: WgslFn[(v: Vec2, delta: Vec2), Vec2] =
    *   WgslFn.dsl[(acc: Var[Vec2]), (v: Vec2, delta: Vec2), Vec2]("accumulate"): ctx =>
    *     Block(
    *       ctx.locals.acc := ctx.params.v,
    *       ctx.locals.acc := ctx.locals.acc + ctx.params.delta,
    *       ctx.ret(ctx.locals.acc),
    *     )
    * }}}
    */
  inline def dsl[L, P, R](name: String)(
      body: WgslFnCtx[P, L, R] => Block,
  ): WgslFn[P, R] =
    val kinds = buildLocalKinds[L]
    val ctx = WgslFnCtx[P, L, R](
      params = TypedExprAccessor[NamedTuple.Map[P & AnyNamedTuple, ToExpr]](""),
      locals =
        TypedLocalAccessor[NamedTuple.Map[L & AnyNamedTuple, ToLocal]](kinds),
      ret = ReturnEmitter[R](),
    )
    val block = body(ctx)
    val paramList = buildParamList[P]
    val retType = wgslReturnType[R]
    val src = s"fn $name($paramList) -> $retType {\n${Block.unwrap(block)}\n}"
    WgslFnData(name, src)

// ---------------------------------------------------------------------------
// WgslFnCtx[P, L, R] — context for WgslFn.dsl with typed locals
// ---------------------------------------------------------------------------

class WgslFnCtx[P, L, R](
    val params: TypedExprAccessor[
      NamedTuple.Map[P & AnyNamedTuple, ToExpr],
    ],
    val locals: TypedLocalAccessor[
      NamedTuple.Map[L & AnyNamedTuple, ToLocal],
    ],
    val ret: ReturnEmitter[R],
)

// ---------------------------------------------------------------------------
// ReturnEmitter[R] — typed return statement builder for WgslFn.dsl
// ---------------------------------------------------------------------------

class ReturnEmitter[R]:
  def apply(v: Expr): Stmt =
    Stmt.raw(s"  return ${v.wgsl};")
