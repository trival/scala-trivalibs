package trivalibs.graphics.math.gpu

import scala.scalajs.js
import trivalibs.utils.js.Arr

// ---------------------------------------------------------------------------
// Expression base class — wraps a WGSL string, toString = wgsl so string
// interpolation works naturally.
// ---------------------------------------------------------------------------

/** A typed GPU shader expression — a thin wrapper over the WGSL string it
  * generates. The DSL builds a tree of these and emits WGSL at shade-build time.
  * Subtypes carry the element type (`FloatExpr`, `Vec3Expr`, `Mat4Expr`, …) so
  * operators and methods (`+`, `.dot`, `.normalize`, `.sin`, swizzles, …) are
  * type-checked in Scala. You rarely construct these directly — they come from
  * `ctx.in`/`bindings`/`locals`, the `vec2/3/4` constructors, and the math ops.
  *
  * A numeric literal works on either side of an arithmetic operator
  * (`0.5 * vExpr` and `vExpr * 0.5`) and of a scalar `FloatExpr` comparison
  * (`0.5 < x`); a bare `Int` counts as a float. (Vector comparison is
  * component-wise and vector-only — no scalar-literal form on either side.)
  */
class Expr(val wgsl: String):
  override def toString: String = wgsl

// ---------------------------------------------------------------------------
// LetExpr — base class for local variable expressions.
// Extends Expr so runtime values created by selectDynamic are compatible.
// ---------------------------------------------------------------------------

/** A named local backed by a WGSL `let` (immutable). `name := expr` emits the
  * declaration. Declared in a `vert`/`frag` body via the `[L]` locals schema, or
  * ad-hoc with `LetFloat("n")`, `LetVec2("p")`, etc. */
class LetExpr(val name: String) extends Expr(name):
  def :=(value: Expr): Stmt = Stmt.let(name, value)

/** A mutable WGSL `var` local: the first `:=` declares it, later `:=` reassign.
  * Use for accumulation (e.g. `VarVec3("col")`). The compound forms `+= -= *=
  * /=` emit WGSL compound assignment (`col += …;`) and require the `var` to be
  * already declared (i.e. used after the initial `:=`). */
class VarExpr(name: String) extends LetExpr(name):
  private var declared = false
  override def :=(value: Expr): Stmt =
    if !declared then
      declared = true
      Stmt.varDecl(name, value)
    else Stmt.varAssign(name, value)

  def +=(value: Expr): Stmt = Stmt.compound(name, "+", value)
  def -=(value: Expr): Stmt = Stmt.compound(name, "-", value)
  def *=(value: Expr): Stmt = Stmt.compound(name, "*", value)
  def /=(value: Expr): Stmt = Stmt.compound(name, "/", value)

/** A WGSL `const` local (compile-time constant). */
class ConstExpr(name: String) extends LetExpr(name):
  override def :=(value: Expr): Stmt = Stmt.constDecl(name, value)

// ---------------------------------------------------------------------------
// All opaque types in one object so the compiler sees through them and can
// validate bounds like `LocalVec2 <: Vec2Expr & LetExpr`.
// Inside this object: Vec2Expr = Expr, and LetExpr <: Expr = Vec2Expr.
// Outside: LocalVec2 <: Vec2Expr (gets all Vec2 ops) & LetExpr (gets :=).
// ---------------------------------------------------------------------------

object Expr:
  def raw(s: String): Expr = new Expr(s)
  def apply(s: String): Expr = new Expr(s)

  opaque type FloatExpr <: Expr = Expr
  object FloatExpr { def apply(s: String): FloatExpr = new Expr(s) }

  opaque type Vec2Expr <: Expr = Expr
  object Vec2Expr { def apply(s: String): Vec2Expr = new Expr(s) }

  opaque type Vec3Expr <: Expr = Expr
  object Vec3Expr { def apply(s: String): Vec3Expr = new Expr(s) }

  opaque type Vec4Expr <: Expr = Expr
  object Vec4Expr { def apply(s: String): Vec4Expr = new Expr(s) }

  opaque type Mat2Expr <: Expr = Expr
  object Mat2Expr { def apply(s: String): Mat2Expr = new Expr(s) }

  opaque type Mat3Expr <: Expr = Expr
  object Mat3Expr { def apply(s: String): Mat3Expr = new Expr(s) }

  opaque type Mat4Expr <: Expr = Expr
  object Mat4Expr { def apply(s: String): Mat4Expr = new Expr(s) }

  opaque type BoolExpr <: Expr = Expr
  object BoolExpr { def apply(s: String): BoolExpr = new Expr(s) }

  // GPU resource expression types — opaque wrappers used in shader DSL
  // for texture and sampler bindings. No CPU-side representation.

  /** A 2D texture in the shader DSL, obtained from `ctx.textures.<name>`. Sample
    * it with `.sample(uv, sampler)` / `(uv, sampler)` / `.sampleLevel(...)`. As a
    * uniform/panel field type it's written via the panel markers (`FragmentPanel`). */
  opaque type Texture2D <: Expr = Expr
  object Texture2D { def apply(s: String): Texture2D = new Expr(s) }

  /** A texture sampler in the shader DSL. Declare as a uniform field of type
    * `Sampler` (e.g. `samp: Sampler`) and bind a `GPUSampler`
    * (`painter.samplerLinear`); pass it to `texture.sample(uv, sampler)`. */
  opaque type Sampler <: Expr = Expr
  object Sampler { def apply(s: String): Sampler = new Expr(s) }

  // Local types — each <: its Expr type (for math ops) & LetExpr (for :=)
  // At runtime all are LetExpr instances, so selectDynamic returning
  // LetExpr(name) + asInstanceOf cast works safely.

  opaque type LetFloat <: FloatExpr & LetExpr = LetExpr
  object LetFloat { def apply(s: String): LetFloat = new LetExpr(s) }

  opaque type LetVec2 <: Vec2Expr & LetExpr = LetExpr
  object LetVec2 { def apply(s: String): LetVec2 = new LetExpr(s) }

  opaque type LetVec3 <: Vec3Expr & LetExpr = LetExpr
  object LetVec3 { def apply(s: String): LetVec3 = new LetExpr(s) }

  opaque type LetVec4 <: Vec4Expr & LetExpr = LetExpr
  object LetVec4 { def apply(s: String): LetVec4 = new LetExpr(s) }

  opaque type LetMat2 <: Mat2Expr & LetExpr = LetExpr
  object LetMat2 { def apply(s: String): LetMat2 = new LetExpr(s) }

  opaque type LetMat3 <: Mat3Expr & LetExpr = LetExpr
  object LetMat3 { def apply(s: String): LetMat3 = new LetExpr(s) }

  opaque type LetMat4 <: Mat4Expr & LetExpr = LetExpr
  object LetMat4 { def apply(s: String): LetMat4 = new LetExpr(s) }

  opaque type LetBool <: BoolExpr & LetExpr = LetExpr
  object LetBool { def apply(s: String): LetBool = new LetExpr(s) }

  // Var types — mutable locals (var on first :=, reassignment after)
  opaque type VarFloat <: FloatExpr & VarExpr = VarExpr
  object VarFloat { def apply(s: String): VarFloat = new VarExpr(s) }

  opaque type VarVec2 <: Vec2Expr & VarExpr = VarExpr
  object VarVec2 { def apply(s: String): VarVec2 = new VarExpr(s) }

  opaque type VarVec3 <: Vec3Expr & VarExpr = VarExpr
  object VarVec3 { def apply(s: String): VarVec3 = new VarExpr(s) }

  opaque type VarVec4 <: Vec4Expr & VarExpr = VarExpr
  object VarVec4 { def apply(s: String): VarVec4 = new VarExpr(s) }

  // Const types — WGSL compile-time constants
  opaque type ConstFloat <: FloatExpr & ConstExpr = ConstExpr
  object ConstFloat { def apply(s: String): ConstFloat = new ConstExpr(s) }

  opaque type ConstVec2 <: Vec2Expr & ConstExpr = ConstExpr
  object ConstVec2 { def apply(s: String): ConstVec2 = new ConstExpr(s) }

  opaque type ConstVec3 <: Vec3Expr & ConstExpr = ConstExpr
  object ConstVec3 { def apply(s: String): ConstVec3 = new ConstExpr(s) }

  opaque type ConstVec4 <: Vec4Expr & ConstExpr = ConstExpr
  object ConstVec4 { def apply(s: String): ConstVec4 = new ConstExpr(s) }

  // ---------------------------------------------------------------------------
  // Integer scalar expression types
  // ---------------------------------------------------------------------------

  opaque type IntExpr <: Expr = Expr
  object IntExpr:
    def apply(s: String): IntExpr = new Expr(s)
    def apply(v: Int): IntExpr = new Expr(v.toString)

  opaque type UIntExpr <: Expr = Expr
  object UIntExpr:
    def apply(s: String): UIntExpr = new Expr(s)
    def apply(v: Int): UIntExpr = new Expr(s"${v}u")

  // ---------------------------------------------------------------------------
  // Integer vector expression types (GPU-only phantoms)
  // ---------------------------------------------------------------------------

  opaque type IVec2Expr <: Expr = Expr
  object IVec2Expr { def apply(s: String): IVec2Expr = new Expr(s) }

  opaque type IVec3Expr <: Expr = Expr
  object IVec3Expr { def apply(s: String): IVec3Expr = new Expr(s) }

  opaque type IVec4Expr <: Expr = Expr
  object IVec4Expr { def apply(s: String): IVec4Expr = new Expr(s) }

  opaque type UVec2Expr <: Expr = Expr
  object UVec2Expr { def apply(s: String): UVec2Expr = new Expr(s) }

  opaque type UVec3Expr <: Expr = Expr
  object UVec3Expr { def apply(s: String): UVec3Expr = new Expr(s) }

  opaque type UVec4Expr <: Expr = Expr
  object UVec4Expr { def apply(s: String): UVec4Expr = new Expr(s) }

  // ---------------------------------------------------------------------------
  // Let/Var/Const variants for integer scalar types
  // ---------------------------------------------------------------------------

  opaque type LetInt <: IntExpr & LetExpr = LetExpr
  object LetInt { def apply(s: String): LetInt = new LetExpr(s) }

  opaque type VarInt <: IntExpr & VarExpr = VarExpr
  object VarInt { def apply(s: String): VarInt = new VarExpr(s) }

  opaque type ConstInt <: IntExpr & ConstExpr = ConstExpr
  object ConstInt { def apply(s: String): ConstInt = new ConstExpr(s) }

  opaque type LetUInt <: UIntExpr & LetExpr = LetExpr
  object LetUInt { def apply(s: String): LetUInt = new LetExpr(s) }

  opaque type VarUInt <: UIntExpr & VarExpr = VarExpr
  object VarUInt { def apply(s: String): VarUInt = new VarExpr(s) }

  opaque type ConstUInt <: UIntExpr & ConstExpr = ConstExpr
  object ConstUInt { def apply(s: String): ConstUInt = new ConstExpr(s) }

  // ---------------------------------------------------------------------------
  // Let variants for integer vector types (Var/Const added as needed)
  // ---------------------------------------------------------------------------

  opaque type LetIVec2 <: IVec2Expr & LetExpr = LetExpr
  object LetIVec2 { def apply(s: String): LetIVec2 = new LetExpr(s) }

  opaque type LetIVec3 <: IVec3Expr & LetExpr = LetExpr
  object LetIVec3 { def apply(s: String): LetIVec3 = new LetExpr(s) }

  opaque type LetIVec4 <: IVec4Expr & LetExpr = LetExpr
  object LetIVec4 { def apply(s: String): LetIVec4 = new LetExpr(s) }

  opaque type LetUVec2 <: UVec2Expr & LetExpr = LetExpr
  object LetUVec2 { def apply(s: String): LetUVec2 = new LetExpr(s) }

  opaque type LetUVec3 <: UVec3Expr & LetExpr = LetExpr
  object LetUVec3 { def apply(s: String): LetUVec3 = new LetExpr(s) }

  opaque type LetUVec4 <: UVec4Expr & LetExpr = LetExpr
  object LetUVec4 { def apply(s: String): LetUVec4 = new LetExpr(s) }

/** Texture sampling ops on a panel texture (`ctx.textures.<name>`). */
extension (tex: Expr.Texture2D)
  /** Sample at `uv` with `sampler` (auto LOD): `tex.sample(ctx.in.uv, samp)`.
    * `tex(uv, samp)` is shorthand for the same. */
  def sample(uv: Expr.Vec2Expr, sampler: Expr.Sampler): Expr.Vec4Expr =
    Expr.Vec4Expr(s"textureSample(${tex.wgsl}, ${sampler.wgsl}, ${uv.wgsl})")
  /** Shorthand for [[sample]]: `tex(uv, sampler)`. */
  def apply(uv: Expr.Vec2Expr, sampler: Expr.Sampler): Expr.Vec4Expr =
    Expr.Vec4Expr(s"textureSample(${tex.wgsl}, ${sampler.wgsl}, ${uv.wgsl})")
  /** Sample an explicit mip `level` (e.g. to show a chosen mip, or read a
    * mip-mapped panel at a fixed LOD). */
  def sampleLevel(
      uv: Expr.Vec2Expr,
      sampler: Expr.Sampler,
      level: Expr.FloatExpr,
  ): Expr.Vec4Expr =
    Expr.Vec4Expr(
      s"textureSampleLevel(${tex.wgsl}, ${sampler.wgsl}, ${uv.wgsl}, ${level.wgsl})",
    )
  /** Number of mip levels in the bound texture, as a `Float`. */
  def numLevels: Expr.FloatExpr =
    Expr.FloatExpr(s"f32(textureNumLevels(${tex.wgsl}))")

export Expr.{
  FloatExpr,
  Vec2Expr,
  Vec3Expr,
  Vec4Expr,
  Mat2Expr,
  Mat3Expr,
  Mat4Expr,
  BoolExpr,
  Texture2D,
  Sampler,
  IntExpr,
  UIntExpr,
  IVec2Expr,
  IVec3Expr,
  IVec4Expr,
  UVec2Expr,
  UVec3Expr,
  UVec4Expr,
  LetFloat,
  LetVec2,
  LetVec3,
  LetVec4,
  LetMat2,
  LetMat3,
  LetMat4,
  LetBool,
  VarFloat,
  VarVec2,
  VarVec3,
  VarVec4,
  ConstFloat,
  ConstVec2,
  ConstVec3,
  ConstVec4,
  LetInt,
  VarInt,
  ConstInt,
  LetUInt,
  VarUInt,
  ConstUInt,
  LetIVec2,
  LetIVec3,
  LetIVec4,
  LetUVec2,
  LetUVec3,
  LetUVec4,
}

// ---------------------------------------------------------------------------
// Stmt and Block opaque types
// ---------------------------------------------------------------------------

/** A single WGSL statement (an assignment, declaration, or `if`). Produced by
  * `:=`, the control-flow helpers, or `Stmt.raw`. */
opaque type Stmt = String

/** A sequence of [[Stmt]]s — the body returned by a `vert`/`frag` block. Build
  * with `Block(stmt1, stmt2, …)`; a single `Stmt` also converts to a `Block`. */
opaque type Block = String

object Stmt:
  inline def assign(target: String, value: Expr): Stmt =
    s"  $target = ${value.wgsl};"
  inline def let(name: String, value: Expr): Stmt =
    s"  let $name = ${value.wgsl};"
  inline def constDecl(name: String, value: Expr): Stmt =
    s"  const $name = ${value.wgsl};"
  inline def varDecl(name: String, value: Expr): Stmt =
    s"  var $name = ${value.wgsl};"
  inline def varDeclTyped(name: String, wgslType: String, value: Expr): Stmt =
    s"  var $name: $wgslType = ${value.wgsl};"
  inline def varAssign(name: String, value: Expr): Stmt =
    s"  $name = ${value.wgsl};"
  inline def compound(name: String, op: String, value: Expr): Stmt =
    s"  $name $op= ${value.wgsl};"
  inline def raw(s: String): Stmt = s

  def ifBlock(cond: BoolExpr, body: Block): Stmt =
    s"  if (${cond.wgsl}) {\n${indentBlock(body)}\n  }"
  def ifElseBlock(cond: BoolExpr, thenBody: Block, elseBody: Block): Stmt =
    s"  if (${cond.wgsl}) {\n${indentBlock(thenBody)}\n  } else {\n${indentBlock(elseBody)}\n  }"

given Conversion[Stmt, Block] = s => s

object Block:
  /** Combine statements into a shader body: `Block(out.color := …, …)`. */
  def apply(stmts: Stmt*): Block = stmts.mkString("\n")
  def empty: Block = ""
  def unwrap(b: Block): String = b.asInstanceOf[String]

// ---------------------------------------------------------------------------
// Control flow — if / if-else statement constructors and helpers.
// Re-indents nested blocks by adding two spaces to every line, so nesting
// works recursively without explicit indentation tracking.
// ---------------------------------------------------------------------------

private def indentBlock(body: Block): String =
  val lines = Block.unwrap(body).asInstanceOf[js.Dynamic].split("\n")
    .asInstanceOf[js.Array[String]]
  val out = Arr[String]()
  var i = 0
  while i < lines.length do
    out.push("  " + lines(i))
    i += 1
  out.join("\n")

/** Branchless conditional. WGSL signature:
  * `select(falseValue, trueValue, cond)`.
  */
def select[T <: Expr](onFalse: T, onTrue: T, cond: BoolExpr): T =
  Expr
    .raw(s"select(${onFalse.wgsl}, ${onTrue.wgsl}, ${cond.wgsl})")
    .asInstanceOf[T]

/** Single-branch `if (cond) { ... }`. */
def when(cond: BoolExpr, body: Block): Stmt = Stmt.ifBlock(cond, body)

/** Two-branch `if (cond) { ... } else { ... }`. */
def ifElse(cond: BoolExpr, thenBody: Block, elseBody: Block): Stmt =
  Stmt.ifElseBlock(cond, thenBody, elseBody)

/** Multi-branch `if / else if / ... [/ else]` chain. Start with `ifChain`,
  * append `.elseIf(...)` for each additional branch, terminate with
  * `.orElse(...)` for a final else, or use the chain directly as a `Stmt`
  * for an open-ended chain.
  */
opaque type IfChain = String

def ifChain(cond: BoolExpr, body: Block): IfChain =
  s"  if (${cond.wgsl}) {\n${indentBlock(body)}\n  }"

extension (chain: IfChain)
  def elseIf(cond: BoolExpr, body: Block): IfChain =
    s"$chain else if (${cond.wgsl}) {\n${indentBlock(body)}\n  }"
  def elseDo(body: Block): Stmt =
    s"$chain else {\n${indentBlock(body)}\n  }"

given Conversion[IfChain, Stmt] = c => c
given Conversion[IfChain, Block] = c => c

extension (cond: BoolExpr)
  /** Branchless conditional: `cond.select(onTrue, onFalse)`. */
  @annotation.targetName("boolSelect")
  def select[T <: Expr](onTrue: T, onFalse: T): T =
    Expr
      .raw(s"select(${onFalse.wgsl}, ${onTrue.wgsl}, ${cond.wgsl})")
      .asInstanceOf[T]

  /** `cond.thenDo(body)` — single-branch `if`. */
  def thenDo(body: Block): Stmt = Stmt.ifBlock(cond, body)

  /** `cond.thenElse(thenBody, elseBody)` — `if`/`else` chain. */
  def thenElse(thenBody: Block, elseBody: Block): Stmt =
    Stmt.ifElseBlock(cond, thenBody, elseBody)

  @annotation.targetName("boolAnd")
  def &&(other: BoolExpr): BoolExpr =
    BoolExpr(s"(${cond.wgsl} && ${other.wgsl})")
  @annotation.targetName("boolOr")
  def ||(other: BoolExpr): BoolExpr =
    BoolExpr(s"(${cond.wgsl} || ${other.wgsl})")
  @annotation.targetName("boolNot")
  def unary_! : BoolExpr = BoolExpr(s"!(${cond.wgsl})")

// ---------------------------------------------------------------------------
// Numeric comparison operators (return BoolExpr).
// Symbolic comparisons live here together because Scala 3 disallows
// overloaded top-level methods spread across files. Numeric step-based
// helpers .gt/.lt/.gte/.lte stay with their respective numeric files.
// ---------------------------------------------------------------------------

extension (a: FloatExpr)
  @annotation.targetName("floatLt")
  def <(b: FloatExpr): BoolExpr = BoolExpr(s"(${a.wgsl} < ${b.wgsl})")
  @annotation.targetName("floatLte")
  def <=(b: FloatExpr): BoolExpr = BoolExpr(s"(${a.wgsl} <= ${b.wgsl})")
  @annotation.targetName("floatGt")
  def >(b: FloatExpr): BoolExpr = BoolExpr(s"(${a.wgsl} > ${b.wgsl})")
  @annotation.targetName("floatGte")
  def >=(b: FloatExpr): BoolExpr = BoolExpr(s"(${a.wgsl} >= ${b.wgsl})")
  @annotation.targetName("floatEq")
  def ===(b: FloatExpr): BoolExpr = BoolExpr(s"(${a.wgsl} == ${b.wgsl})")
  @annotation.targetName("floatNe")
  def !==(b: FloatExpr): BoolExpr = BoolExpr(s"(${a.wgsl} != ${b.wgsl})")

extension (a: IntExpr)
  @annotation.targetName("intLt")
  def <(b: IntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} < ${b.wgsl})")
  @annotation.targetName("intLte")
  def <=(b: IntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} <= ${b.wgsl})")
  @annotation.targetName("intGt")
  def >(b: IntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} > ${b.wgsl})")
  @annotation.targetName("intGte")
  def >=(b: IntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} >= ${b.wgsl})")
  @annotation.targetName("intEq")
  def ===(b: IntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} == ${b.wgsl})")
  @annotation.targetName("intNe")
  def !==(b: IntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} != ${b.wgsl})")

extension (a: UIntExpr)
  @annotation.targetName("uintLt")
  def <(b: UIntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} < ${b.wgsl})")
  @annotation.targetName("uintLte")
  def <=(b: UIntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} <= ${b.wgsl})")
  @annotation.targetName("uintGt")
  def >(b: UIntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} > ${b.wgsl})")
  @annotation.targetName("uintGte")
  def >=(b: UIntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} >= ${b.wgsl})")
  @annotation.targetName("uintEq")
  def ===(b: UIntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} == ${b.wgsl})")
  @annotation.targetName("uintNe")
  def !==(b: UIntExpr): BoolExpr = BoolExpr(s"(${a.wgsl} != ${b.wgsl})")

// Numeric literal on the **left** of a scalar comparison against a `FloatExpr`
// (mirrors the left-operand arithmetic support). A bare `Int` counts as a float.
// Each block compares against a single arg type, so there is no erasure-overload
// ambiguity (unlike the vector arithmetic case).
extension (d: Double)
  @annotation.targetName("dLtF") inline def <(e: FloatExpr): BoolExpr = (d: FloatExpr) < e
  @annotation.targetName("dLteF") inline def <=(e: FloatExpr): BoolExpr = (d: FloatExpr) <= e
  @annotation.targetName("dGtF") inline def >(e: FloatExpr): BoolExpr = (d: FloatExpr) > e
  @annotation.targetName("dGteF") inline def >=(e: FloatExpr): BoolExpr = (d: FloatExpr) >= e
  @annotation.targetName("dEqF") inline def ===(e: FloatExpr): BoolExpr = (d: FloatExpr) === e
  @annotation.targetName("dNeF") inline def !==(e: FloatExpr): BoolExpr = (d: FloatExpr) !== e

extension (n: Int)
  @annotation.targetName("iLtF") inline def <(e: FloatExpr): BoolExpr = (n: FloatExpr) < e
  @annotation.targetName("iLteF") inline def <=(e: FloatExpr): BoolExpr = (n: FloatExpr) <= e
  @annotation.targetName("iGtF") inline def >(e: FloatExpr): BoolExpr = (n: FloatExpr) > e
  @annotation.targetName("iGteF") inline def >=(e: FloatExpr): BoolExpr = (n: FloatExpr) >= e
  @annotation.targetName("iEqF") inline def ===(e: FloatExpr): BoolExpr = (n: FloatExpr) === e
  @annotation.targetName("iNeF") inline def !==(e: FloatExpr): BoolExpr = (n: FloatExpr) !== e

// ---------------------------------------------------------------------------
// Numeric arithmetic operators (+, -, *, /).
// Top-level operator extensions for `Double`, `Int`, `IVec*Expr`, `UVec*Expr`
// receivers live here together for the same reason as the comparison
// operators above — Scala 3 disallows overloaded top-level methods spread
// across files. FloatExpr / Vec*Expr arithmetic lives inside NumOps /
// Vec*ImmutableOpsG given instances in float_expr.scala — those don't
// conflict because they're nested inside givens, not at the top level.
// ---------------------------------------------------------------------------

private[gpu] def floatToWgsl(v: Double): String =
  val s = v.toString
  if s.indexOf('.') >= 0 || s.indexOf('E') >= 0 || s.indexOf('e') >= 0 then s
  else s + ".0"

// Left-operand arithmetic for numeric literals. A literal converts to a
// `FloatExpr` only as the *right* operand (via the `Conversion`s in
// float_expr.scala). These extensions cover the left side so `0.5 * expr`,
// `2 - vExpr`, etc. work without reordering or ascription.
//
// A bare `Int` literal is treated as a *float* here, mirroring
// `Conversion[Int, FloatExpr]` (emits `f32(n)`) — int/uint expressions are
// opted into explicitly with `.i` / `.u`, never inferred from a left operand.
//
// `FloatExpr`/`Vec*Expr` are opaque aliases that all erase to `Expr`. Defining
// one operator as several arg-type overloads (e.g. `*(FloatExpr)` and
// `*(Vec2Expr)`) confuses operator resolution for an `Int` receiver — the
// erasure-identical candidates get discarded and member `Int.*` is reported as
// the failure. So instead each operator is a single *generic* method dispatched
// by the `LeftScalar` witness, which both rebuilds the right expr subtype and
// keeps one unambiguous `*`/`+`/… per receiver.

/** Maps a left-scalar operand expr type `E` to the result type `Out` of
  * `literal OP expr`. `Out` is always the base value type (`FloatExpr`,
  * `Vec2Expr`, …), never a `Let`/`Var`/`Const` binding subtype — the result is a
  * computed expression, not a binding. The subtype-bounded givens below let any
  * binding subtype (`LetFloat`, `VarVec3`, …) resolve to its base, so e.g.
  * `0.15 + letFloat` type-checks and yields a `FloatExpr`. */
trait LeftScalar[E <: Expr]:
  type Out <: Expr
  def wrap(s: String): Out

object LeftScalar:
  type Aux[E <: Expr, O <: Expr] = LeftScalar[E] { type Out = O }

  private def inst[E <: Expr, O <: Expr](f: String => O): Aux[E, O] =
    new LeftScalar[E]:
      type Out = O
      def wrap(s: String): O = f(s)

  given [E <: FloatExpr] => Aux[E, FloatExpr] = inst(FloatExpr(_))
  given [E <: Vec2Expr] => Aux[E, Vec2Expr] = inst(Vec2Expr(_))
  given [E <: Vec3Expr] => Aux[E, Vec3Expr] = inst(Vec3Expr(_))
  given [E <: Vec4Expr] => Aux[E, Vec4Expr] = inst(Vec4Expr(_))

extension (d: Double)
  @annotation.targetName("dAdd") inline def +[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(${floatToWgsl(d)} + ${e.wgsl})")
  @annotation.targetName("dSub") inline def -[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(${floatToWgsl(d)} - ${e.wgsl})")
  @annotation.targetName("dMul") inline def *[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(${floatToWgsl(d)} * ${e.wgsl})")
  @annotation.targetName("dDiv") inline def /[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(${floatToWgsl(d)} / ${e.wgsl})")

extension (n: Int)
  @annotation.targetName("iAdd") inline def +[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(f32($n) + ${e.wgsl})")
  @annotation.targetName("iSub") inline def -[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(f32($n) - ${e.wgsl})")
  @annotation.targetName("iMul") inline def *[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(f32($n) * ${e.wgsl})")
  @annotation.targetName("iDiv") inline def /[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(f32($n) / ${e.wgsl})")

// Integer-vector arithmetic. Lives here, not in int_expr.scala, because all
// top-level +/-/*// must share a file (see header).

extension (v: IVec2Expr)
  @annotation.targetName("ivec2AddVec") def +(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("ivec2AddScalar") def +(s: IntExpr): IVec2Expr = IVec2Expr(s"(${v.wgsl} + ${s.wgsl})")
  @annotation.targetName("ivec2SubVec") def -(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("ivec2SubScalar") def -(s: IntExpr): IVec2Expr = IVec2Expr(s"(${v.wgsl} - ${s.wgsl})")
  @annotation.targetName("ivec2MulVec") def *(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("ivec2MulScalar") def *(s: IntExpr): IVec2Expr = IVec2Expr(s"(${v.wgsl} * ${s.wgsl})")
  @annotation.targetName("ivec2DivVec") def /(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} / ${other.wgsl})")

extension (v: IVec3Expr)
  @annotation.targetName("ivec3AddVec") def +(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("ivec3SubVec") def -(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("ivec3MulVec") def *(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("ivec3MulScalar") def *(s: IntExpr): IVec3Expr = IVec3Expr(s"(${v.wgsl} * ${s.wgsl})")

extension (v: IVec4Expr)
  @annotation.targetName("ivec4AddVec") def +(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("ivec4SubVec") def -(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("ivec4MulVec") def *(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} * ${other.wgsl})")

extension (v: UVec2Expr)
  @annotation.targetName("uvec2AddVec") def +(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("uvec2AddScalar") def +(s: UIntExpr): UVec2Expr = UVec2Expr(s"(${v.wgsl} + ${s.wgsl})")
  @annotation.targetName("uvec2SubVec") def -(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("uvec2MulVec") def *(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("uvec2MulScalar") def *(s: UIntExpr): UVec2Expr = UVec2Expr(s"(${v.wgsl} * ${s.wgsl})")
  @annotation.targetName("uvec2DivVec") def /(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} / ${other.wgsl})")

extension (v: UVec3Expr)
  @annotation.targetName("uvec3AddVec") def +(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("uvec3SubVec") def -(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("uvec3MulVec") def *(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("uvec3MulScalar") def *(s: UIntExpr): UVec3Expr = UVec3Expr(s"(${v.wgsl} * ${s.wgsl})")

extension (v: UVec4Expr)
  @annotation.targetName("uvec4AddVec") def +(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("uvec4SubVec") def -(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("uvec4MulVec") def *(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("uvec4MulScalar") def *(s: UIntExpr): UVec4Expr = UVec4Expr(s"(${v.wgsl} * ${s.wgsl})")
