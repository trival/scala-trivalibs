package trivalibs.graphics.math.gpu

import trivalibs.graphics.math.LerpBy
import trivalibs.utils.js.Arr

import scala.annotation.implicitNotFound
import scala.scalajs.js

// ---------------------------------------------------------------------------
// Expression base class — wraps a WGSL string, toString = wgsl so string
// interpolation works naturally.
// ---------------------------------------------------------------------------

/** A typed GPU shader expression — a thin wrapper over the WGSL string it
  * generates. The DSL builds a tree of these and emits WGSL at shade-build
  * time. Subtypes carry the element type (`FloatExpr`, `Vec3Expr`, `Mat4Expr`,
  * …) so operators and methods (`+`, `.dot`, `.normalize`, `.sin`, swizzles, …)
  * are type-checked in Scala. You rarely construct these directly — they come
  * from `ctx.in`/`bindings`/`locals`, the `vec2/3/4` constructors, and the math
  * ops.
  *
  * A numeric literal works on either side of an arithmetic operator (`0.5 *
  * vExpr` and `vExpr * 0.5`) and of a scalar `FloatExpr` comparison (`0.5 <
  * x`); a bare `Int` counts as a float. (Vector comparison is component-wise
  * and vector-only — no scalar-literal form on either side.)
  */
class Expr(val wgsl: String):
  override def toString: String = wgsl

// ---------------------------------------------------------------------------
// LetExpr — base class for local variable expressions.
// Extends Expr so runtime values created by selectDynamic are compatible.
// ---------------------------------------------------------------------------

/** Lifts a CPU value into the shader expression type a slot expects. Resolved
  * by the *target* type `E`, so the same literal lands correctly on either
  * side: `varInt := 0` emits `0`, `varFloat := 0` emits `f32(0)`.
  *
  * This is what carries CPU values across in an assignment. A `Conversion`
  * cannot do the job — `cpu_interop.scala` deliberately refuses
  * `Conversion[Vec3, Vec3Expr]` (it would make every GPU extension applicable
  * to CPU values), and an overload set blocks conversions anyway.
  *
  * Instances live in the *element type's* companion (`object FloatExpr`,
  * `object IntExpr`, …), not here: inside `object Expr` the opaque types are
  * transparent, so instances sharing one owner would collide.
  */
@implicitNotFound(
  "Cannot assign a ${C} to a shader local of type ${E}.",
)
trait Lift[C, E]:
  def lift(value: C): E

/** A named local backed by a WGSL `let` (immutable). `name := expr` emits the
  * declaration. Declared ad-hoc in a `vert`/`frag` body with `LetFloat("n")`,
  * `LetVec2("p")`, etc.
  *
  * `T` is the element type the local holds, so assignment is type-checked:
  * `LetVec3("col") := someFloatExpr` does not compile.
  */
class LetExpr[T <: Expr](val name: String) extends Expr(name):
  def :=(value: T): Stmt = Stmt.let(name, value)

  /** CPU-value assignment — `col := WallTint`, `n := 0.5`. Delegates to the
    * `T` form above, so the `VarExpr` / `ConstExpr` overrides still apply.
    */
  def :=[C](value: C)(using l: Lift[C, T]): Stmt = this := l.lift(value)

/** A mutable WGSL `var` local: the first `:=` declares it, later `:=` reassign.
  * Use for accumulation (e.g. `VarVec3("col")`). The compound forms `+= -= *=
  * /=` emit WGSL compound assignment (`col += …;`) and require the `var` to be
  * already declared (i.e. used after the initial `:=`).
  */
class VarExpr[T <: Expr](name: String) extends LetExpr[T](name):
  private var declared = false
  override def :=(value: T): Stmt =
    if !declared then
      declared = true
      Stmt.varDecl(name, value)
    else Stmt.varAssign(name, value)

  // Compound assignment. Each operator carries the same overload set as `:=`
  // above: the `Expr` form, CPU `Vec*` operands, and the `Double`/`Int` forms
  // needed to keep `col *= 0.5` working once these become overload sets.
  //
  // These stay untyped, unlike `:=`. A compound op cannot *declare* a local, so
  // it can never fix a wrong element type behind a correctly-typed name the way
  // `:=` could — a mismatch here fails in WGSL at the line as written. They are
  // also legitimately mixed-type (`vec3 *= f32` broadcasts, `mat3 *= f32` too
  // but `mat3 += f32` does not), so typing them needs its own operand rules.
  // See documents/typed-shader-assignment.md.
  def +=(value: Expr): Stmt = Stmt.compound(name, "+", value)
  def +=(value: Double): Stmt = this += FloatExpr(floatToWgsl(value))
  def +=(value: Int): Stmt = this += FloatExpr(s"f32($value)")
  def +=(value: Vec2): Stmt = this += value.toExpr
  def +=(value: Vec3): Stmt = this += value.toExpr
  def +=(value: Vec4): Stmt = this += value.toExpr

  def -=(value: Expr): Stmt = Stmt.compound(name, "-", value)
  def -=(value: Double): Stmt = this -= FloatExpr(floatToWgsl(value))
  def -=(value: Int): Stmt = this -= FloatExpr(s"f32($value)")
  def -=(value: Vec2): Stmt = this -= value.toExpr
  def -=(value: Vec3): Stmt = this -= value.toExpr
  def -=(value: Vec4): Stmt = this -= value.toExpr

  def *=(value: Expr): Stmt = Stmt.compound(name, "*", value)
  def *=(value: Double): Stmt = this *= FloatExpr(floatToWgsl(value))
  def *=(value: Int): Stmt = this *= FloatExpr(s"f32($value)")
  def *=(value: Vec2): Stmt = this *= value.toExpr
  def *=(value: Vec3): Stmt = this *= value.toExpr
  def *=(value: Vec4): Stmt = this *= value.toExpr

  def /=(value: Expr): Stmt = Stmt.compound(name, "/", value)
  def /=(value: Double): Stmt = this /= FloatExpr(floatToWgsl(value))
  def /=(value: Int): Stmt = this /= FloatExpr(s"f32($value)")
  def /=(value: Vec2): Stmt = this /= value.toExpr
  def /=(value: Vec3): Stmt = this /= value.toExpr
  def /=(value: Vec4): Stmt = this /= value.toExpr

/** A WGSL `const` local (compile-time constant). */
class ConstExpr[T <: Expr](name: String) extends LetExpr[T](name):
  override def :=(value: T): Stmt = Stmt.constDecl(name, value)

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
  object FloatExpr:
    def apply(s: String): FloatExpr = new Expr(s)

    /** Lets generic `[T: LerpBy[_, FloatExpr]]` code — `t.lerpIn(lo, hi)` with
      * scalar bounds — interpolate a shader scalar. It lives here rather than
      * in `LerpBy`'s companion so `graphics.math` keeps knowing nothing about
      * `graphics.math.gpu`; the implicit scope of `LerpBy[FloatExpr,
      * FloatExpr]` includes this companion either way.
      */
    given floatExprLerp: LerpBy[FloatExpr, FloatExpr]:
      extension (a: FloatExpr)
        inline def lerp(b: FloatExpr, t: FloatExpr): FloatExpr = a.mix(b, t)

    /** `n := 0.5` — a CPU scalar into a float slot. */
    given liftDouble: Lift[Double, FloatExpr] = v => FloatExpr(floatToWgsl(v))

    /** `n := 1` — a bare `Int` is an f32 literal, matching
      * `Conversion[Int, FloatExpr]`. The int-typed slots lift it as an `i32`
      * instead; the target decides.
      */
    given liftInt: Lift[Int, FloatExpr] = v => FloatExpr(s"f32($v)")

  opaque type Vec2Expr <: Expr = Expr
  object Vec2Expr:
    def apply(s: String): Vec2Expr = new Expr(s)

    /** Concrete instance in the companion, so generic `[T: LerpBy[_,
      * FloatExpr]]` code resolves to a module rather than a parameterized given
      * that materialises a class per call site. See
      * `documents/lerp-typeclass-refactor.md`.
      *
      * The WGSL is built here rather than delegating to `.mix`: inside `object
      * Expr` the opaque types are transparent, so `Vec2Expr` and `FloatExpr`
      * are the same type and the `mix` overloads are ambiguous. Emits exactly
      * what `Vec2ImmutableOpsG.mix` emits — keep the two in step.
      */
    given lerpInstance: LerpBy[Vec2Expr, FloatExpr]:
      extension (a: Vec2Expr)
        def lerp(b: Vec2Expr, t: FloatExpr): Vec2Expr =
          Vec2Expr(s"mix(${a.wgsl}, ${b.wgsl}, ${t.wgsl})")

    /** `p := SomeVec2` — a CPU vector into a vec2 slot. */
    given liftCpu: Lift[Vec2, Vec2Expr] = _.toExpr

  opaque type Vec3Expr <: Expr = Expr
  object Vec3Expr:
    def apply(s: String): Vec3Expr = new Expr(s)

    /** Concrete instance in the companion, so generic `[T: LerpBy[_,
      * FloatExpr]]` code resolves to a module rather than a parameterized given
      * that materialises a class per call site. See
      * `documents/lerp-typeclass-refactor.md`.
      *
      * The WGSL is built here rather than delegating to `.mix`: inside `object
      * Expr` the opaque types are transparent, so `Vec3Expr` and `FloatExpr`
      * are the same type and the `mix` overloads are ambiguous. Emits exactly
      * what `Vec3ImmutableOpsG.mix` emits — keep the two in step.
      */
    given lerpInstance: LerpBy[Vec3Expr, FloatExpr]:
      extension (a: Vec3Expr)
        def lerp(b: Vec3Expr, t: FloatExpr): Vec3Expr =
          Vec3Expr(s"mix(${a.wgsl}, ${b.wgsl}, ${t.wgsl})")

    /** `p := SomeVec3` — a CPU vector into a vec3 slot. */
    given liftCpu: Lift[Vec3, Vec3Expr] = _.toExpr

  opaque type Vec4Expr <: Expr = Expr
  object Vec4Expr:
    def apply(s: String): Vec4Expr = new Expr(s)

    /** Concrete instance in the companion, so generic `[T: LerpBy[_,
      * FloatExpr]]` code resolves to a module rather than a parameterized given
      * that materialises a class per call site. See
      * `documents/lerp-typeclass-refactor.md`.
      *
      * The WGSL is built here rather than delegating to `.mix`: inside `object
      * Expr` the opaque types are transparent, so `Vec4Expr` and `FloatExpr`
      * are the same type and the `mix` overloads are ambiguous. Emits exactly
      * what `Vec4ImmutableOpsG.mix` emits — keep the two in step.
      */
    given lerpInstance: LerpBy[Vec4Expr, FloatExpr]:
      extension (a: Vec4Expr)
        def lerp(b: Vec4Expr, t: FloatExpr): Vec4Expr =
          Vec4Expr(s"mix(${a.wgsl}, ${b.wgsl}, ${t.wgsl})")

    /** `p := SomeVec4` — a CPU vector into a vec4 slot. */
    given liftCpu: Lift[Vec4, Vec4Expr] = _.toExpr

  opaque type Mat2Expr <: Expr = Expr
  object Mat2Expr:
    def apply(s: String): Mat2Expr = new Expr(s)

    /** `m := SomeMat2` — a CPU matrix into a mat2x2 slot. */
    given liftCpu: Lift[Mat2, Mat2Expr] = _.toExpr

  opaque type Mat3Expr <: Expr = Expr
  object Mat3Expr:
    def apply(s: String): Mat3Expr = new Expr(s)

    /** `m := SomeMat3` — a CPU matrix into a mat3x3 slot. */
    given liftCpu: Lift[Mat3, Mat3Expr] = _.toExpr

  opaque type Mat4Expr <: Expr = Expr
  object Mat4Expr:
    def apply(s: String): Mat4Expr = new Expr(s)

    /** `m := SomeMat4` — a CPU matrix into a mat4x4 slot. */
    given liftCpu: Lift[Mat4, Mat4Expr] = _.toExpr

  opaque type BoolExpr <: Expr = Expr
  object BoolExpr:
    def apply(s: String): BoolExpr = new Expr(s)

    /** `flag := true` — a CPU boolean into a bool slot. */
    given liftCpu: Lift[Boolean, BoolExpr] = v => BoolExpr(v.toString)

  /** How an element is reached inside a uniform array, given the element's
    * expression type. Scalars and `Vec2`s are lane-packed into `vec4` rows on
    * the GPU (four and two per row), so reaching element `i` means addressing a
    * row and then a part of it; everything else is one element per row and
    * indexes directly.
    *
    * Resolved on the element expression type because that determines the packing
    * uniquely — a `FloatExpr` element is always a 4-lane array, a `Vec2Expr`
    * element always a 2-lane one.
    */
  opaque type ArrayAccess[E] = Int

  trait ArrayAccessLow:
    /** One element per row — the unpacked case. */
    given [E] => ArrayAccess[E] = 1

  object ArrayAccess extends ArrayAccessLow:
    /** Four scalars per `vec4` row. */
    given ArrayAccess[FloatExpr] = 4

    /** Two `Vec2`s per `vec4` row — `.xy` then `.zw`. */
    given ArrayAccess[Vec2Expr] = 2

    extension [E](lanes: ArrayAccess[E])
      def const(base: String, i: Int): String =
        if lanes == 1 then s"$base[$i]"
        else if lanes == 2 then
          s"$base[${i / 2}]${if i % 2 == 0 then ".xy" else ".zw"}"
        else s"$base[${i / 4}][${i % 4}]"

      // The 2-lane form repeats the index expression to pick a half. WGSL
      // expressions are pure, so this is a shader-source size cost only.
      def dyn(base: String, i: String): String =
        if lanes == 1 then s"$base[$i]"
        else if lanes == 2 then
          s"select($base[$i / 2].xy, $base[$i / 2].zw, ($i % 2) == 1)"
        else s"$base[$i / 4][$i % 4]"

  /** A WGSL array binding, obtained from `ctx.bindings.<name>` for a
    * `UniformArray[T, N]` uniform. Index it with a constant or an `IntExpr` —
    * `stops(0)`, `stops(i)` — to get the element expression `E`.
    *
    * Indices are always element indices, `0` to `N-1`, whether or not the
    * elements are lane-packed on the GPU ([[ArrayAccess]]).
    */
  opaque type ArrayExpr[E] <: Expr = Expr
  object ArrayExpr:
    def apply[E](s: String): ArrayExpr[E] = new Expr(s)

    import ArrayAccess.{const, dyn}

    extension [E](a: ArrayExpr[E])
      /** Element at a build-time constant index. */
      inline def apply(i: Int)(using acc: ArrayAccess[E]): E =
        new Expr(acc.const(a.wgsl, i)).asInstanceOf[E]

      /** Element at an index computed in the shader. */
      inline def apply(i: IntExpr)(using acc: ArrayAccess[E]): E =
        new Expr(acc.dyn(a.wgsl, i.wgsl)).asInstanceOf[E]

  // GPU resource expression types — opaque wrappers used in shader DSL
  // for texture and sampler bindings. No CPU-side representation.

  /** A 2D texture in the shader DSL, obtained from `ctx.textures.<name>`.
    * Sample it with `.sample(uv, sampler)` / `(uv, sampler)` /
    * `.sampleLevel(...)`. As a uniform/panel field type it's written via the
    * panel markers (`FragmentPanel`).
    */
  opaque type Texture2D <: Expr = Expr
  object Texture2D { def apply(s: String): Texture2D = new Expr(s) }

  /** A texture sampler in the shader DSL. Declare as a uniform field of type
    * `Sampler` (e.g. `samp: Sampler`) and bind a `GPUSampler`
    * (`painter.samplerLinear`); pass it to `texture.sample(uv, sampler)`.
    */
  opaque type Sampler <: Expr = Expr
  object Sampler { def apply(s: String): Sampler = new Expr(s) }

  /** A 2D depth texture in the shader DSL (WGSL `texture_depth_2d`), obtained
    * from `ctx.textures.<name>` for a depth panel field (declared with a
    * `*DepthPanel` marker). Read it with `.load(coord, level)` (no sampler,
    * point-exact) — depth reads yield a single channel, so the result is a
    * scalar `FloatExpr`, not a `Vec4`.
    */
  opaque type DepthTexture2D <: Expr = Expr
  object DepthTexture2D { def apply(s: String): DepthTexture2D = new Expr(s) }

  // Local types — each <: its Expr type (for math ops) & LetExpr (for :=)
  // At runtime all are LetExpr instances, so selectDynamic returning
  // LetExpr(name) + asInstanceOf cast works safely.

  opaque type LetFloat <: FloatExpr & LetExpr[FloatExpr] = LetExpr[FloatExpr]
  object LetFloat { def apply(s: String): LetFloat = new LetExpr[FloatExpr](s) }

  opaque type LetVec2 <: Vec2Expr & LetExpr[Vec2Expr] = LetExpr[Vec2Expr]
  object LetVec2 { def apply(s: String): LetVec2 = new LetExpr[Vec2Expr](s) }

  opaque type LetVec3 <: Vec3Expr & LetExpr[Vec3Expr] = LetExpr[Vec3Expr]
  object LetVec3 { def apply(s: String): LetVec3 = new LetExpr[Vec3Expr](s) }

  opaque type LetVec4 <: Vec4Expr & LetExpr[Vec4Expr] = LetExpr[Vec4Expr]
  object LetVec4 { def apply(s: String): LetVec4 = new LetExpr[Vec4Expr](s) }

  opaque type LetMat2 <: Mat2Expr & LetExpr[Mat2Expr] = LetExpr[Mat2Expr]
  object LetMat2 { def apply(s: String): LetMat2 = new LetExpr[Mat2Expr](s) }

  opaque type LetMat3 <: Mat3Expr & LetExpr[Mat3Expr] = LetExpr[Mat3Expr]
  object LetMat3 { def apply(s: String): LetMat3 = new LetExpr[Mat3Expr](s) }

  opaque type LetMat4 <: Mat4Expr & LetExpr[Mat4Expr] = LetExpr[Mat4Expr]
  object LetMat4 { def apply(s: String): LetMat4 = new LetExpr[Mat4Expr](s) }

  opaque type LetBool <: BoolExpr & LetExpr[BoolExpr] = LetExpr[BoolExpr]
  object LetBool { def apply(s: String): LetBool = new LetExpr[BoolExpr](s) }

  // Var types — mutable locals (var on first :=, reassignment after)
  opaque type VarFloat <: FloatExpr & VarExpr[FloatExpr] = VarExpr[FloatExpr]
  object VarFloat { def apply(s: String): VarFloat = new VarExpr[FloatExpr](s) }

  opaque type VarVec2 <: Vec2Expr & VarExpr[Vec2Expr] = VarExpr[Vec2Expr]
  object VarVec2 { def apply(s: String): VarVec2 = new VarExpr[Vec2Expr](s) }

  opaque type VarVec3 <: Vec3Expr & VarExpr[Vec3Expr] = VarExpr[Vec3Expr]
  object VarVec3 { def apply(s: String): VarVec3 = new VarExpr[Vec3Expr](s) }

  opaque type VarVec4 <: Vec4Expr & VarExpr[Vec4Expr] = VarExpr[Vec4Expr]
  object VarVec4 { def apply(s: String): VarVec4 = new VarExpr[Vec4Expr](s) }

  opaque type VarMat2 <: Mat2Expr & VarExpr[Mat2Expr] = VarExpr[Mat2Expr]
  object VarMat2 { def apply(s: String): VarMat2 = new VarExpr[Mat2Expr](s) }

  opaque type VarMat3 <: Mat3Expr & VarExpr[Mat3Expr] = VarExpr[Mat3Expr]
  object VarMat3 { def apply(s: String): VarMat3 = new VarExpr[Mat3Expr](s) }

  opaque type VarMat4 <: Mat4Expr & VarExpr[Mat4Expr] = VarExpr[Mat4Expr]
  object VarMat4 { def apply(s: String): VarMat4 = new VarExpr[Mat4Expr](s) }

  opaque type VarBool <: BoolExpr & VarExpr[BoolExpr] = VarExpr[BoolExpr]
  object VarBool { def apply(s: String): VarBool = new VarExpr[BoolExpr](s) }

  // Const types — WGSL compile-time constants
  opaque type ConstFloat <: FloatExpr & ConstExpr[FloatExpr] = ConstExpr[FloatExpr]
  object ConstFloat { def apply(s: String): ConstFloat = new ConstExpr[FloatExpr](s) }

  opaque type ConstVec2 <: Vec2Expr & ConstExpr[Vec2Expr] = ConstExpr[Vec2Expr]
  object ConstVec2 { def apply(s: String): ConstVec2 = new ConstExpr[Vec2Expr](s) }

  opaque type ConstVec3 <: Vec3Expr & ConstExpr[Vec3Expr] = ConstExpr[Vec3Expr]
  object ConstVec3 { def apply(s: String): ConstVec3 = new ConstExpr[Vec3Expr](s) }

  opaque type ConstVec4 <: Vec4Expr & ConstExpr[Vec4Expr] = ConstExpr[Vec4Expr]
  object ConstVec4 { def apply(s: String): ConstVec4 = new ConstExpr[Vec4Expr](s) }

  opaque type ConstMat2 <: Mat2Expr & ConstExpr[Mat2Expr] = ConstExpr[Mat2Expr]
  object ConstMat2 { def apply(s: String): ConstMat2 = new ConstExpr[Mat2Expr](s) }

  opaque type ConstMat3 <: Mat3Expr & ConstExpr[Mat3Expr] = ConstExpr[Mat3Expr]
  object ConstMat3 { def apply(s: String): ConstMat3 = new ConstExpr[Mat3Expr](s) }

  opaque type ConstMat4 <: Mat4Expr & ConstExpr[Mat4Expr] = ConstExpr[Mat4Expr]
  object ConstMat4 { def apply(s: String): ConstMat4 = new ConstExpr[Mat4Expr](s) }

  opaque type ConstBool <: BoolExpr & ConstExpr[BoolExpr] = ConstExpr[BoolExpr]
  object ConstBool { def apply(s: String): ConstBool = new ConstExpr[BoolExpr](s) }

  // ---------------------------------------------------------------------------
  // Integer scalar expression types
  // ---------------------------------------------------------------------------

  opaque type IntExpr <: Expr = Expr
  object IntExpr:
    def apply(s: String): IntExpr = new Expr(s)
    def apply(v: Int): IntExpr = new Expr(v.toString)

    /** `i := 0` — an `Int` into an int slot stays an `i32`. Everywhere a float
      * is expected a bare `Int` still means `f32` (`Conversion[Int,
      * FloatExpr]`); assignment resolves on the target, so `.i` is not needed
      * to declare an int local.
      */
    given liftInt: Lift[Int, IntExpr] = IntExpr(_)

  opaque type UIntExpr <: Expr = Expr
  object UIntExpr:
    def apply(s: String): UIntExpr = new Expr(s)
    def apply(v: Int): UIntExpr = new Expr(s"${v}u")

    /** `n := 0` — an `Int` into a uint slot, emitted as `0u`. */
    given liftInt: Lift[Int, UIntExpr] = UIntExpr(_)

    /** `n := 3.u` — an already-`UInt` CPU value. */
    given liftUInt: Lift[UInt, UIntExpr] = v => UIntExpr(v.toInt)

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

  opaque type LetInt <: IntExpr & LetExpr[IntExpr] = LetExpr[IntExpr]
  object LetInt { def apply(s: String): LetInt = new LetExpr[IntExpr](s) }

  opaque type VarInt <: IntExpr & VarExpr[IntExpr] = VarExpr[IntExpr]
  object VarInt { def apply(s: String): VarInt = new VarExpr[IntExpr](s) }

  opaque type ConstInt <: IntExpr & ConstExpr[IntExpr] = ConstExpr[IntExpr]
  object ConstInt { def apply(s: String): ConstInt = new ConstExpr[IntExpr](s) }

  opaque type LetUInt <: UIntExpr & LetExpr[UIntExpr] = LetExpr[UIntExpr]
  object LetUInt { def apply(s: String): LetUInt = new LetExpr[UIntExpr](s) }

  opaque type VarUInt <: UIntExpr & VarExpr[UIntExpr] = VarExpr[UIntExpr]
  object VarUInt { def apply(s: String): VarUInt = new VarExpr[UIntExpr](s) }

  opaque type ConstUInt <: UIntExpr & ConstExpr[UIntExpr] = ConstExpr[UIntExpr]
  object ConstUInt { def apply(s: String): ConstUInt = new ConstExpr[UIntExpr](s) }

  // ---------------------------------------------------------------------------
  // Let variants for integer vector types (Var/Const added as needed)
  // ---------------------------------------------------------------------------

  opaque type LetIVec2 <: IVec2Expr & LetExpr[IVec2Expr] = LetExpr[IVec2Expr]
  object LetIVec2 { def apply(s: String): LetIVec2 = new LetExpr[IVec2Expr](s) }

  opaque type LetIVec3 <: IVec3Expr & LetExpr[IVec3Expr] = LetExpr[IVec3Expr]
  object LetIVec3 { def apply(s: String): LetIVec3 = new LetExpr[IVec3Expr](s) }

  opaque type LetIVec4 <: IVec4Expr & LetExpr[IVec4Expr] = LetExpr[IVec4Expr]
  object LetIVec4 { def apply(s: String): LetIVec4 = new LetExpr[IVec4Expr](s) }

  opaque type LetUVec2 <: UVec2Expr & LetExpr[UVec2Expr] = LetExpr[UVec2Expr]
  object LetUVec2 { def apply(s: String): LetUVec2 = new LetExpr[UVec2Expr](s) }

  opaque type LetUVec3 <: UVec3Expr & LetExpr[UVec3Expr] = LetExpr[UVec3Expr]
  object LetUVec3 { def apply(s: String): LetUVec3 = new LetExpr[UVec3Expr](s) }

  opaque type LetUVec4 <: UVec4Expr & LetExpr[UVec4Expr] = LetExpr[UVec4Expr]
  object LetUVec4 { def apply(s: String): LetUVec4 = new LetExpr[UVec4Expr](s) }

  // Var / Const variants for the integer vector types
  opaque type VarIVec2 <: IVec2Expr & VarExpr[IVec2Expr] = VarExpr[IVec2Expr]
  object VarIVec2 { def apply(s: String): VarIVec2 = new VarExpr[IVec2Expr](s) }

  opaque type VarIVec3 <: IVec3Expr & VarExpr[IVec3Expr] = VarExpr[IVec3Expr]
  object VarIVec3 { def apply(s: String): VarIVec3 = new VarExpr[IVec3Expr](s) }

  opaque type VarIVec4 <: IVec4Expr & VarExpr[IVec4Expr] = VarExpr[IVec4Expr]
  object VarIVec4 { def apply(s: String): VarIVec4 = new VarExpr[IVec4Expr](s) }

  opaque type VarUVec2 <: UVec2Expr & VarExpr[UVec2Expr] = VarExpr[UVec2Expr]
  object VarUVec2 { def apply(s: String): VarUVec2 = new VarExpr[UVec2Expr](s) }

  opaque type VarUVec3 <: UVec3Expr & VarExpr[UVec3Expr] = VarExpr[UVec3Expr]
  object VarUVec3 { def apply(s: String): VarUVec3 = new VarExpr[UVec3Expr](s) }

  opaque type VarUVec4 <: UVec4Expr & VarExpr[UVec4Expr] = VarExpr[UVec4Expr]
  object VarUVec4 { def apply(s: String): VarUVec4 = new VarExpr[UVec4Expr](s) }

  opaque type ConstIVec2 <: IVec2Expr & ConstExpr[IVec2Expr] = ConstExpr[IVec2Expr]
  object ConstIVec2 { def apply(s: String): ConstIVec2 = new ConstExpr[IVec2Expr](s) }

  opaque type ConstIVec3 <: IVec3Expr & ConstExpr[IVec3Expr] = ConstExpr[IVec3Expr]
  object ConstIVec3 { def apply(s: String): ConstIVec3 = new ConstExpr[IVec3Expr](s) }

  opaque type ConstIVec4 <: IVec4Expr & ConstExpr[IVec4Expr] = ConstExpr[IVec4Expr]
  object ConstIVec4 { def apply(s: String): ConstIVec4 = new ConstExpr[IVec4Expr](s) }

  opaque type ConstUVec2 <: UVec2Expr & ConstExpr[UVec2Expr] = ConstExpr[UVec2Expr]
  object ConstUVec2 { def apply(s: String): ConstUVec2 = new ConstExpr[UVec2Expr](s) }

  opaque type ConstUVec3 <: UVec3Expr & ConstExpr[UVec3Expr] = ConstExpr[UVec3Expr]
  object ConstUVec3 { def apply(s: String): ConstUVec3 = new ConstExpr[UVec3Expr](s) }

  opaque type ConstUVec4 <: UVec4Expr & ConstExpr[UVec4Expr] = ConstExpr[UVec4Expr]
  object ConstUVec4 { def apply(s: String): ConstUVec4 = new ConstExpr[UVec4Expr](s) }

/** Texture sampling ops on a panel texture (`ctx.textures.<name>`). */
extension (tex: Expr.Texture2D)
  /** Sample at `uv` with `sampler` (auto LOD): `tex.sample(ctx.in.uv, samp)`.
    * `tex(uv, samp)` is shorthand for the same.
    */
  def sample(uv: Expr.Vec2Expr, sampler: Expr.Sampler): Expr.Vec4Expr =
    Expr.Vec4Expr(s"textureSample(${tex.wgsl}, ${sampler.wgsl}, ${uv.wgsl})")

  /** Shorthand for [[sample]]: `tex(uv, sampler)`. */
  def apply(uv: Expr.Vec2Expr, sampler: Expr.Sampler): Expr.Vec4Expr =
    Expr.Vec4Expr(s"textureSample(${tex.wgsl}, ${sampler.wgsl}, ${uv.wgsl})")

  /** Sample an explicit mip `level` (e.g. to show a chosen mip, or read a
    * mip-mapped panel at a fixed LOD).
    */
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

  /** Point-read a texel by integer coordinate at an explicit mip `level`, with
    * no sampler and no filtering (nearest). Coords are texel indices (not
    * normalized UV); out-of-bounds reads return 0. Ideal for fullscreen / layer
    * passes that read a texture 1:1 with their target (e.g.
    * `tex.load(ivec2(ctx.in.fragCoord.xy), 0.i)`).
    */
  def load(coord: Expr.IVec2Expr, level: Expr.IntExpr): Expr.Vec4Expr =
    Expr.Vec4Expr(s"textureLoad(${tex.wgsl}, ${coord.wgsl}, ${level.wgsl})")

  /** [[load]] at mip 0. */
  def load(coord: Expr.IVec2Expr): Expr.Vec4Expr =
    Expr.Vec4Expr(s"textureLoad(${tex.wgsl}, ${coord.wgsl}, 0)")

  /** Size in texels of the bound texture's mip 0, as `vec2<u32>`. Convert to
    * float for UV→texel math: `ivec2(uv * vec2(tex.dimensions))`.
    */
  def dimensions: Expr.UVec2Expr =
    Expr.UVec2Expr(s"textureDimensions(${tex.wgsl})")

/** Read ops on a panel depth texture (`ctx.textures.<name>` for a depth panel
  * field). Depth reads yield a single channel, so each returns a scalar
  * `FloatExpr`.
  */
extension (tex: Expr.DepthTexture2D)
  /** Point-read the depth texel by integer coordinate at mip `level`, no
    * sampler. Coords are texel indices (not UV); out-of-bounds reads return 0.
    * Primary way to read a depth attachment in a 1:1 fullscreen / layer pass:
    * `depthTex.load(ivec2(ctx.in.fragCoord.xy), 0.i)`.
    */
  @annotation.targetName("depthTexLoad")
  def load(coord: Expr.IVec2Expr, level: Expr.IntExpr): Expr.FloatExpr =
    Expr.FloatExpr(s"textureLoad(${tex.wgsl}, ${coord.wgsl}, ${level.wgsl})")

  /** [[load]] at mip 0. */
  @annotation.targetName("depthTexLoad0")
  def load(coord: Expr.IVec2Expr): Expr.FloatExpr =
    Expr.FloatExpr(s"textureLoad(${tex.wgsl}, ${coord.wgsl}, 0)")

  /** Sample the depth at `uv` with a **non-comparison** `sampler`. Returns the
    * stored depth as a scalar. (No `sampleLevel` variant: depth attachments are
    * single-level — the mip pyramid is built on the color texture, not depth.
    * For shadow-map PCF use a comparison sampler + `textureSampleCompare`, not
    * yet wrapped.)
    */
  @annotation.targetName("depthTexSample")
  def sample(uv: Expr.Vec2Expr, sampler: Expr.Sampler): Expr.FloatExpr =
    Expr.FloatExpr(s"textureSample(${tex.wgsl}, ${sampler.wgsl}, ${uv.wgsl})")

  /** Size in texels of the depth texture's mip 0, as `vec2<u32>`. */
  @annotation.targetName("depthTexDimensions")
  def dimensions: Expr.UVec2Expr =
    Expr.UVec2Expr(s"textureDimensions(${tex.wgsl})")

export Expr.{
  ArrayExpr,
  FloatExpr,
  Vec2Expr,
  Vec3Expr,
  Vec4Expr,
  Mat2Expr,
  Mat3Expr,
  Mat4Expr,
  BoolExpr,
  Texture2D,
  DepthTexture2D,
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
  VarMat2,
  VarMat3,
  VarMat4,
  VarBool,
  ConstMat2,
  ConstMat3,
  ConstMat4,
  ConstBool,
  VarIVec2,
  VarIVec3,
  VarIVec4,
  VarUVec2,
  VarUVec3,
  VarUVec4,
  ConstIVec2,
  ConstIVec3,
  ConstIVec4,
  ConstUVec2,
  ConstUVec3,
  ConstUVec4,
}

// ---------------------------------------------------------------------------
// Stmt and Block opaque types
// ---------------------------------------------------------------------------

/** A group of WGSL statements — the body returned by a `vert`/`frag` block.
  * Build with `Block(part1, part2, …)`, where each part is itself a `Block`, so
  * groups nest to any depth and flatten into one statement list. Every
  * statement-producing construct in the DSL is a `Block`: `:=`, `when`, `loop`,
  * `unroll`, `scope`, and any helper that returns a group.
  */
opaque type Block = String

/** A single WGSL statement (an assignment, declaration, or `if`). Produced by
  * `:=`, the control-flow helpers, or `Stmt.raw`. A `Stmt` IS a [[Block]] — the
  * narrower type only documents that exactly one statement comes out.
  */
opaque type Stmt <: Block = String

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
  def whileBlock(cond: BoolExpr, body: Block): Stmt =
    s"  while (${cond.wgsl}) {\n${indentBlock(body)}\n  }"
  def forBlock(from: String, until: String, name: String, body: Block): Stmt =
    s"  for (var $name: i32 = $from; $name < $until; $name++) {\n${indentBlock(body)}\n  }"
  def scopeBlock(body: Block): Stmt =
    s"  {\n${indentBlock(body)}\n  }"

object Block:
  /** Combine parts into a shader body: `Block(out.color := …, …)`. Each part is
    * a `Block` itself, so a nested group — an `unroll`, a `when`, a helper that
    * returns several statements — is a legal argument and flattens into the
    * enclosing list. [[empty]] parts contribute no line.
    */
  def apply(parts: Block*): Block =
    val out = Arr[String]()
    var i = 0
    while i < parts.length do
      val p = parts(i)
      if p.length > 0 then out.push(p)
      i += 1
    out.join("\n")

  /** Combine a dynamically-built array of parts into a shader body — the
    * js-native counterpart to the varargs `apply`, for parts accumulated in an
    * `Arr`. Compiles to a `while` + `Array.join`.
    */
  def apply(parts: Arr[Block]): Block =
    val out = Arr[String]()
    var i = 0
    while i < parts.length do
      val p = parts(i)
      if p.length > 0 then out.push(p)
      i += 1
    out.join("\n")

  /** The empty group — emits nothing, and contributes no line to an enclosing
    * `Block`. The "nothing to emit" branch of a build-time conditional.
    */
  def empty: Block = ""
  def unwrap(b: Block): String = b.asInstanceOf[String]

// ---------------------------------------------------------------------------
// Control flow — if / if-else statement constructors and helpers.
// Re-indents nested blocks by adding two spaces to every line, so nesting
// works recursively without explicit indentation tracking.
// ---------------------------------------------------------------------------

private def indentBlock(body: Block): String =
  val lines = Block
    .unwrap(body)
    .asInstanceOf[js.Dynamic]
    .split("\n")
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

/** `if (cond) { ... }`, and the start of any `if` chain.
  *
  * The body is by-name and sits in its own parameter list, so it can be written
  * as an indented block that is also a Scala scope — the place to declare the
  * locals that belong to it:
  *
  * {{{
  * when(uv.x < 0.5):
  *   val tint = LetVec3("tint")
  *   Block(tint := …, col := col * tint)
  * .elseDo:
  *   col := col * 0.5
  * }}}
  *
  * The result is an [[IfChain]]: continue it with `.elseIf` / `.elseDo`, or use
  * it directly where a `Stmt` or `Block` is expected.
  */
def when(cond: BoolExpr)(body: => Block): IfChain = Stmt.ifBlock(cond, body)

/** `while (cond) { ... }` — the condition-driven loop.
  *
  * Named for the DSL's own vocabulary rather than WGSL's: [[loop]] is the
  * looping primitive (it emits a `for`), and `loopIf` is the same primitive
  * driven by a condition instead of a count. The body is by-name, so it is a
  * Scala scope like every other control-flow body.
  *
  * Use [[break]] / [[breakIf]] to leave it early.
  */
def loopIf(cond: BoolExpr)(body: => Block): Stmt = Stmt.whileBlock(cond, body)

/** `{ ... }` — a bare WGSL scope, with no condition and no loop attached.
  *
  * The body is by-name and in its own parameter list, so it is a Scala scope
  * too, and the locals that belong to it are declared inside it. Use it to give
  * repeated code its own WGSL scope: sibling scopes may declare the same names,
  * so an [[unroll]]ed body no longer needs the index in its local names.
  *
  * {{{
  * unroll(sortPairs): (pair, _) =>
  *   scope:
  *     val hi = LetVec4("hi")
  *     Block(hi := …, lines(pair._1) := hi)
  * }}}
  *
  * The accumulator rule is the same as for [[loop]] and [[when]]: a `var` read
  * after the scope must be first assigned OUTSIDE it, or its declaration lands
  * inside the braces and is gone at the closing one. Assigning an outer `var`
  * from within is fine.
  */
def scope(body: => Block): Stmt = Stmt.scopeBlock(body)

// ---------------------------------------------------------------------------
// Counted loop — emits a WGSL `for`. The induction variable is the one
// identifier the DSL invents, so it is derived from lexical nesting depth
// (`i`, `j`, `k`, …): deterministic output, and it reads like hand-written
// shader code. Siblings at the same depth share a name legally — each `for`
// body is its own WGSL scope.
// ---------------------------------------------------------------------------

private val LoopVarNames = Arr("i", "j", "k", "l", "m", "n")
private var loopDepth = 0

private def forLoop(from: String, until: String)(
    body: IntExpr => Block,
): Stmt =
  val name =
    if loopDepth < LoopVarNames.length then LoopVarNames(loopDepth)
    else s"i$loopDepth"
  loopDepth += 1
  val b = body(IntExpr(name))
  loopDepth -= 1
  Stmt.forBlock(from, until, name, b)

/** Repeat a statement group at RUNTIME — emits a WGSL `for` loop over
  * `0 until count`, with the index available to the body as an `IntExpr`.
  *
  * The counterpart of [[unroll]], and deliberately the same shape: the only
  * difference at the call site is whether the bound is a build-time `Int` or a
  * runtime `IntExpr`.
  *
  * {{{
  * Block(
  *   col := stops(0).rgb,
  *   loop(1, ctx.bindings.count.toI32): i =>
  *     val cur = LetVec4("cur")
  *     Block(cur := stops(i), col := col.mix(cur.rgb, w(i))),
  *   ctx.out.color := vec4(col, 1.0),
  * )
  * }}}
  *
  * A constant bound is allowed and is not the same as unrolling: `loop(64)`
  * keeps the body once in the shader source, `unroll(64)` emits 64 copies and
  * constant-folds the index into each. Pick by which of those you want.
  *
  * An accumulator `var` belongs OUTSIDE the loop — assign it before the loop so
  * its declaration lands in the enclosing scope. A `var` first assigned inside
  * the body is declared inside the loop's braces: re-initialised every
  * iteration, and out of scope afterwards.
  */
def loop(count: IntExpr)(body: IntExpr => Block): Stmt =
  forLoop("0", count.wgsl)(body)

/** [[loop]] with a build-time constant bound — `loop(64)`. */
def loop(count: Int)(body: IntExpr => Block): Stmt =
  forLoop("0", count.toString)(body)

/** [[loop]] over `from until until`. */
def loop(from: IntExpr, until: IntExpr)(body: IntExpr => Block): Stmt =
  forLoop(from.wgsl, until.wgsl)(body)

/** [[loop]] over `from until until`, literal start. */
def loop(from: Int, until: IntExpr)(body: IntExpr => Block): Stmt =
  forLoop(from.toString, until.wgsl)(body)

/** [[loop]] over `from until until`, literal end. */
def loop(from: IntExpr, until: Int)(body: IntExpr => Block): Stmt =
  forLoop(from.wgsl, until.toString)(body)

/** [[loop]] over `from until until`, both literal. */
def loop(from: Int, until: Int)(body: IntExpr => Block): Stmt =
  forLoop(from.toString, until.toString)(body)

// ---------------------------------------------------------------------------
// Jumps
// ---------------------------------------------------------------------------

/** `break;` — leave the innermost enclosing loop. */
val break: Stmt = "  break;"

/** `continue;` — skip to the next iteration of the innermost loop. */
val continue: Stmt = "  continue;"

/** `if (cond) { break; }` — the `…If` twin of [[loopIf]]. */
def breakIf(cond: BoolExpr): Stmt = Stmt.ifBlock(cond, break)

/** `if (cond) { continue; }`. */
def continueIf(cond: BoolExpr): Stmt = Stmt.ifBlock(cond, continue)

/** Repeat a statement group once per index, at BUILD TIME — the index is a
  * Scala `Int`, so every use of it constant-folds and the emitted WGSL is
  * straight-line code with no loop in it.
  *
  * This is the shape to reach for when the trip count is a compile-time
  * constant: a fixed [[trivalibs.graphics.buffers.UniformArray]] capacity, a
  * build-time-known edge set, a fixed number of taps. It says "deliberately
  * unrolled" where a WGSL loop would say "runs on the GPU".
  *
  * {{{
  * Block(
  *   col := stops(0).rgb,
  *   unroll(1, MaxStops)(i =>
  *     col := col.mix(stops(i).rgb, weight(i))
  *   ),
  *   ctx.out.color := vec4(col, 1.0),
  * )
  * }}}
  *
  * All iterations are emitted into the ENCLOSING scope, so any local declared
  * inside the body needs the index in its name — `LetVec4(s"cur$i")` — or the
  * second iteration re-declares it. (A `var` accumulated across iterations is
  * fine either way: the first `:=` declares it and the rest assign, all in the
  * one scope.)
  */
def unroll(from: Int, until: Int)(body: Int => Block): Block =
  val parts = Arr[String]()
  var i = from
  while i < until do
    val p = Block.unwrap(body(i))
    if p.length > 0 then parts.push(p)
    i += 1
  parts.join("\n")

/** [[unroll]] over `0 until count`. */
def unroll(count: Int)(body: Int => Block): Block = unroll(0, count)(body)

/** [[unroll]] over the values of `xs`, with the index alongside — for the
  * common case where the iteration is driven by build-time data rather than a
  * range (a set of offsets, a fixed list of index pairs, a tap kernel).
  *
  * {{{
  * unroll(Arr((0, 1), (1, 2), (0, 1))): (pair, i) =>
  *   val a = LetVec4(s"a$i")
  *   Block(a := …)
  * }}}
  */
def unroll[T](xs: Arr[T])(body: (T, Int) => Block): Block =
  val parts = Arr[String]()
  var i = 0
  while i < xs.length do
    val p = Block.unwrap(body(xs(i), i))
    if p.length > 0 then parts.push(p)
    i += 1
  parts.join("\n")

/** An `if` chain in progress, as returned by [[when]] and `BoolExpr.thenDo`.
  * Append `.elseIf(...)` for each additional branch and `.elseDo(...)` for a
  * final else, or use it directly where a `Stmt` / `Block` is expected — it is
  * a complete `if` statement at every step, not a builder awaiting a
  * terminator.
  */
opaque type IfChain <: Stmt = String

extension (chain: IfChain)
  def elseIf(cond: BoolExpr)(body: => Block): IfChain =
    s"$chain else if (${cond.wgsl}) {\n${indentBlock(body)}\n  }"
  def elseDo(body: => Block): Stmt =
    s"$chain else {\n${indentBlock(body)}\n  }"

// ---------------------------------------------------------------------------
// Receiver forms of the loop constructs. Every control-flow construct has both
// a function and an extension spelling; these are the non-BoolExpr receivers.
// A bare `Int` here is a COUNT, not shader math — the `Conversion[Int,
// FloatExpr]` that turns a literal into `f32(n)` applies to operands, never to
// a receiver.
// ---------------------------------------------------------------------------

extension (count: IntExpr)
  /** `count.loop(i => body)` — the receiver form of [[loop]]. */
  @annotation.targetName("intExprLoop")
  def loop(body: IntExpr => Block): Stmt = forLoop("0", count.wgsl)(body)

extension (count: Int)
  /** `64.loop(i => body)` — the receiver form of [[loop]] with a constant
    * bound. Emits a `for` over a literal range; see [[unroll]] for the
    * straight-line alternative.
    */
  @annotation.targetName("intLoop")
  def loop(body: IntExpr => Block): Stmt = forLoop("0", count.toString)(body)

  /** `64.unroll(i => body)` — the receiver form of [[unroll]]. */
  @annotation.targetName("intUnroll")
  def unroll(body: Int => Block): Block =
    trivalibs.graphics.math.gpu.unroll(0, count)(body)

extension [T](xs: Arr[T])
  /** `xs.unroll((v, i) => body)` — the receiver form of [[unroll]] over values.
    * Reads better than the function form: the data comes first.
    */
  @annotation.targetName("arrUnroll")
  def unroll(body: (T, Int) => Block): Block =
    trivalibs.graphics.math.gpu.unroll(xs)(body)

  /** `xs.unroll(v => body)` — [[unroll]] over values when the index is not
    * needed.
    */
  @annotation.targetName("arrUnrollNoIdx")
  def unroll(body: T => Block): Block =
    trivalibs.graphics.math.gpu.unroll(xs)((v, _) => body(v))

extension (cond: BoolExpr)
  /** Branchless conditional: `cond.select(onTrue, onFalse)`. */
  @annotation.targetName("boolSelect")
  def select[T <: Expr](onTrue: T, onFalse: T): T =
    Expr
      .raw(s"select(${onFalse.wgsl}, ${onTrue.wgsl}, ${cond.wgsl})")
      .asInstanceOf[T]

  /** `cond.thenDo(body)` — the receiver form of [[when]]. Continue it with
    * `.elseIf` / `.elseDo`, or use it as a statement.
    */
  def thenDo(body: => Block): IfChain = Stmt.ifBlock(cond, body)

  /** `cond.thenLoop(body)` — the receiver form of [[loopIf]]. */
  def thenLoop(body: => Block): Stmt = Stmt.whileBlock(cond, body)

  /** `cond.thenBreak` — the receiver form of [[breakIf]]. */
  def thenBreak: Stmt = Stmt.ifBlock(cond, break)

  /** `cond.thenContinue` — the receiver form of [[continueIf]]. */
  def thenContinue: Stmt = Stmt.ifBlock(cond, continue)

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

  // Int-literal forms — loop conditions are where literals actually show up,
  // and a bare `Int` cannot reach the overloads above (it would convert to
  // FloatExpr).
  @annotation.targetName("intLtLit")
  def <(b: Int): BoolExpr = BoolExpr(s"(${a.wgsl} < $b)")
  @annotation.targetName("intLteLit")
  def <=(b: Int): BoolExpr = BoolExpr(s"(${a.wgsl} <= $b)")
  @annotation.targetName("intGtLit")
  def >(b: Int): BoolExpr = BoolExpr(s"(${a.wgsl} > $b)")
  @annotation.targetName("intGteLit")
  def >=(b: Int): BoolExpr = BoolExpr(s"(${a.wgsl} >= $b)")
  @annotation.targetName("intEqLit")
  def ===(b: Int): BoolExpr = BoolExpr(s"(${a.wgsl} == $b)")
  @annotation.targetName("intNeLit")
  def !==(b: Int): BoolExpr = BoolExpr(s"(${a.wgsl} != $b)")

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
  @annotation.targetName("dLtF")
  inline def <(e: FloatExpr): BoolExpr = (d: FloatExpr) < e
  @annotation.targetName("dLteF")
  inline def <=(e: FloatExpr): BoolExpr = (d: FloatExpr) <= e
  @annotation.targetName("dGtF")
  inline def >(e: FloatExpr): BoolExpr = (d: FloatExpr) > e
  @annotation.targetName("dGteF")
  inline def >=(e: FloatExpr): BoolExpr = (d: FloatExpr) >= e
  @annotation.targetName("dEqF")
  inline def ===(e: FloatExpr): BoolExpr = (d: FloatExpr) === e
  @annotation.targetName("dNeF")
  inline def !==(e: FloatExpr): BoolExpr = (d: FloatExpr) !== e

extension (n: Int)
  @annotation.targetName("iLtF")
  inline def <(e: FloatExpr): BoolExpr = (n: FloatExpr) < e
  @annotation.targetName("iLteF")
  inline def <=(e: FloatExpr): BoolExpr = (n: FloatExpr) <= e
  @annotation.targetName("iGtF")
  inline def >(e: FloatExpr): BoolExpr = (n: FloatExpr) > e
  @annotation.targetName("iGteF")
  inline def >=(e: FloatExpr): BoolExpr = (n: FloatExpr) >= e
  @annotation.targetName("iEqF")
  inline def ===(e: FloatExpr): BoolExpr = (n: FloatExpr) === e
  @annotation.targetName("iNeF")
  inline def !==(e: FloatExpr): BoolExpr = (n: FloatExpr) !== e

// ---------------------------------------------------------------------------
// Numeric arithmetic operators (+, -, *, /).
// Top-level operator extensions for `Double`, `Int`, `IVec*Expr`, `UVec*Expr`
// receivers live here together for the same reason as the comparison
// operators above — Scala 3 disallows overloaded top-level methods spread
// across files. FloatExpr / Vec*Expr arithmetic lives inside NumOps /
// Vec*ImmutableOpsG given instances in float_expr.scala — those don't
// conflict because they're nested inside givens, not at the top level.
// ---------------------------------------------------------------------------

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
  * `Vec2Expr`, …), never a `Let`/`Var`/`Const` binding subtype — the result is
  * a computed expression, not a binding. The subtype-bounded givens below let
  * any binding subtype (`LetFloat`, `VarVec3`, …) resolve to its base, so e.g.
  * `0.15 + letFloat` type-checks and yields a `FloatExpr`.
  */
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
  @annotation.targetName("dAdd")
  inline def +[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O =
    L.wrap(s"(${floatToWgsl(d)} + ${e.wgsl})")
  @annotation.targetName("dSub")
  inline def -[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O =
    L.wrap(s"(${floatToWgsl(d)} - ${e.wgsl})")
  @annotation.targetName("dMul")
  inline def *[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O =
    L.wrap(s"(${floatToWgsl(d)} * ${e.wgsl})")
  @annotation.targetName("dDiv")
  inline def /[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O =
    L.wrap(s"(${floatToWgsl(d)} / ${e.wgsl})")

extension (n: Int)
  @annotation.targetName("iAdd")
  inline def +[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O =
    L.wrap(s"(f32($n) + ${e.wgsl})")
  @annotation.targetName("iSub")
  inline def -[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O =
    L.wrap(s"(f32($n) - ${e.wgsl})")
  @annotation.targetName("iMul")
  inline def *[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O =
    L.wrap(s"(f32($n) * ${e.wgsl})")
  @annotation.targetName("iDiv")
  inline def /[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O =
    L.wrap(s"(f32($n) / ${e.wgsl})")

// Integer-vector arithmetic. Lives here, not in int_expr.scala, because all
// top-level +/-/*// must share a file (see header).

extension (v: IVec2Expr)
  @annotation.targetName("ivec2AddVec")
  def +(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("ivec2AddScalar")
  def +(s: IntExpr): IVec2Expr = IVec2Expr(s"(${v.wgsl} + ${s.wgsl})")
  @annotation.targetName("ivec2SubVec")
  def -(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("ivec2SubScalar")
  def -(s: IntExpr): IVec2Expr = IVec2Expr(s"(${v.wgsl} - ${s.wgsl})")
  @annotation.targetName("ivec2MulVec")
  def *(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("ivec2MulScalar")
  def *(s: IntExpr): IVec2Expr = IVec2Expr(s"(${v.wgsl} * ${s.wgsl})")
  @annotation.targetName("ivec2DivVec")
  def /(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} / ${other.wgsl})")

extension (v: IVec3Expr)
  @annotation.targetName("ivec3AddVec")
  def +(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("ivec3SubVec")
  def -(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("ivec3MulVec")
  def *(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("ivec3MulScalar")
  def *(s: IntExpr): IVec3Expr = IVec3Expr(s"(${v.wgsl} * ${s.wgsl})")

extension (v: IVec4Expr)
  @annotation.targetName("ivec4AddVec")
  def +(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("ivec4SubVec")
  def -(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("ivec4MulVec")
  def *(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} * ${other.wgsl})")

extension (v: UVec2Expr)
  @annotation.targetName("uvec2AddVec")
  def +(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("uvec2AddScalar")
  def +(s: UIntExpr): UVec2Expr = UVec2Expr(s"(${v.wgsl} + ${s.wgsl})")
  @annotation.targetName("uvec2SubVec")
  def -(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("uvec2MulVec")
  def *(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("uvec2MulScalar")
  def *(s: UIntExpr): UVec2Expr = UVec2Expr(s"(${v.wgsl} * ${s.wgsl})")
  @annotation.targetName("uvec2DivVec")
  def /(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} / ${other.wgsl})")

extension (v: UVec3Expr)
  @annotation.targetName("uvec3AddVec")
  def +(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("uvec3SubVec")
  def -(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("uvec3MulVec")
  def *(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("uvec3MulScalar")
  def *(s: UIntExpr): UVec3Expr = UVec3Expr(s"(${v.wgsl} * ${s.wgsl})")

extension (v: UVec4Expr)
  @annotation.targetName("uvec4AddVec")
  def +(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} + ${other.wgsl})")
  @annotation.targetName("uvec4SubVec")
  def -(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} - ${other.wgsl})")
  @annotation.targetName("uvec4MulVec")
  def *(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} * ${other.wgsl})")
  @annotation.targetName("uvec4MulScalar")
  def *(s: UIntExpr): UVec4Expr = UVec4Expr(s"(${v.wgsl} * ${s.wgsl})")
