package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.gpu.*
import trivalibs.utils.js.*

import scala.NamedTuple
import scala.NamedTuple.AnyNamedTuple

// ---------------------------------------------------------------------------
// TypedPanelAccessor[P] — returns Texture2D for every panel field
// ---------------------------------------------------------------------------

/** Maps each panel wrapper type to its DSL texture expression — color panels to
  * `Texture2D`, depth panels to `DepthTexture2D`.
  */
type PanelToTexture[T] = T match
  case trivalibs.graphics.shader.FragmentPanel      => Texture2D
  case trivalibs.graphics.shader.VertexPanel        => Texture2D
  case trivalibs.graphics.shader.SharedPanel        => Texture2D
  case trivalibs.graphics.shader.FragmentDepthPanel => DepthTexture2D
  case trivalibs.graphics.shader.VertexDepthPanel   => DepthTexture2D
  case trivalibs.graphics.shader.SharedDepthPanel   => DepthTexture2D

/** Read-only accessor for panel texture bindings (group 1). Each field returns
  * a `Texture2D` (color panel) or `DepthTexture2D` (depth panel) expression.
  * The runtime value is the field name; the static type comes from `Fields`.
  */
class TypedPanelAccessor[P] extends Selectable:
  type Fields = NamedTuple.Map[P & AnyNamedTuple, PanelToTexture]
  def selectDynamic(name: String): Expr = Expr.raw(name)

// ---------------------------------------------------------------------------
// Typed Selectable Accessors — compile-time field checking via named tuples
// ---------------------------------------------------------------------------

/** Typed read-only accessor for input fields and uniforms.
  *
  * Fields maps to Expr subtypes (Vec2Expr, Mat2Expr, etc.), giving typed field
  * access like `ctx.in.position` → `Vec2Expr`.
  */
class TypedExprAccessor[F <: AnyNamedTuple](prefix: String) extends Selectable:
  type Fields = F

  def selectDynamic(name: String): Expr =
    if prefix.isEmpty then Expr.raw(name)
    else Expr.raw(s"$prefix.$name")

/** Typed write-only accessor for output fields.
  *
  * Fields maps to AssignTarget for all field types, preserving the `:=`
  * operator for assignment statements.
  */
class TypedAssignAccessor[F <: AnyNamedTuple](prefix: String)
    extends Selectable:
  type Fields = F

  def selectDynamic(name: String): AssignTarget =
    AssignTarget(if prefix.isEmpty then name else s"$prefix.$name")

class AssignTarget(val target: String):
  inline def :=(value: Expr): Stmt = Stmt.assign(target, value)

/** Typed read+write accessor for local variables.
  *
  * Fields maps to Local* types (e.g., LocalVec2). Each is an opaque type <:
  * Vec*Expr & LetExpr, so math operations (via Vec*Expr) and `:=` (via LetExpr)
  * are both available. At runtime all are LetExpr.
  */
class TypedLocalAccessor[F <: AnyNamedTuple](
    kinds: Dict[String] = Dict[String](),
) extends Selectable:
  type Fields = F

  def selectDynamic(name: String): Any =
    if kinds.has(name) then
      kinds.at(name) match
        case "v" => VarExpr(name)
        case _   => ConstExpr(name)
    else LetExpr(name)

// ---------------------------------------------------------------------------
// Stage-Specific Context Types
// ---------------------------------------------------------------------------

/** Vertex output accessor.
  *
  *   - `out.position` — direct val, always available, no Selectable dispatch
  *   - `out.fieldName` — any varying field via Selectable (same mechanism as
  *     `ctx.in` in `FragmentCtx`)
  */
class VertexOut[V](prefix: String) extends Selectable:
  type Fields = NamedTuple.Map[V & AnyNamedTuple, ToAssign]
  val position: AssignTarget = AssignTarget(s"$prefix.position")
  def selectDynamic(name: String): AssignTarget =
    AssignTarget(s"$prefix.$name")

/** Vertex shader context.
  *
  *   - `out.position` — built-in clip-space position output
  *   - `out.fieldName` — write a named varying passed to the fragment stage
  *   - `in.fieldName` — read a vertex attribute
  *   - `bindings.name` — read a uniform binding
  *   - `locals.name` — read/write a typed local variable
  *   - `textures.name` — read a panel texture binding (group 1)
  */
class VertexCtx[A, V, U, L, P](
    val in: TypedExprAccessor[NamedTuple.Map[A & AnyNamedTuple, ToExpr]],
    val out: VertexOut[V],
    val bindings: TypedExprAccessor[
      NamedTuple.Map[U & AnyNamedTuple, UniformToExpr],
    ],
    val locals: TypedLocalAccessor[NamedTuple.Map[L & AnyNamedTuple, ToLocal]],
    val textures: TypedPanelAccessor[P],
):
  /** `@builtin(vertex_index)` — index of the current vertex (`u32`). */
  val vertexIndex: UIntExpr = UIntExpr("in.vertexIndex")

  /** `@builtin(instance_index)` — index of the current instance (`u32`). */
  val instanceIndex: UIntExpr = UIntExpr("in.instanceIndex")

/** Fragment shader context.
  *
  * FO is the fragment output named tuple (default: `(color: Vec4)`). Each field
  * becomes an AssignTarget via `ToAssign`.
  */
class FragmentCtx[V, U, L, P, FO](
    val in: TypedExprAccessor[NamedTuple.Map[V & AnyNamedTuple, ToExpr]],
    val out: TypedAssignAccessor[NamedTuple.Map[FO & AnyNamedTuple, ToAssign]],
    val bindings: TypedExprAccessor[
      NamedTuple.Map[U & AnyNamedTuple, UniformToExpr],
    ],
    val locals: TypedLocalAccessor[NamedTuple.Map[L & AnyNamedTuple, ToLocal]],
    val textures: TypedPanelAccessor[P],
):
  /** The `@builtin(position)` fragment input — framebuffer pixel coordinates
    * ("fragCoord"). `.xy` are pixel coords (pixel centers, origin top-left),
    * `.z` is the fragment's depth, `.w` is `1/clip.w`. Handy for a 1:1
    * `tex.load(ivec2(ctx.fragCoord.xy))` read of a same-resolution panel.
    */
  val fragCoord: Vec4Expr = Vec4Expr("in.position")

  /** `@builtin(front_facing)` — `true` if the fragment is on a front-facing
    * primitive (winding + cull state). Useful for two-sided shading.
    */
  val frontFacing: BoolExpr = BoolExpr("frontFacing")
