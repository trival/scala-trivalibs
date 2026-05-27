package trivalibs.graphics.math.gpu

import trivalibs.graphics.math.*
import trivalibs.utils.numbers.NumExt
import trivalibs.utils.numbers.NumOps

// ---------------------------------------------------------------------------
// Implicit conversions from numeric literals
// ---------------------------------------------------------------------------

private def floatToWgsl(v: Double): String =
  val s = v.toString
  if s.indexOf('.') >= 0 || s.indexOf('E') >= 0 || s.indexOf('e') >= 0 then s
  else s + ".0"

given Conversion[Double, FloatExpr] = v => FloatExpr(floatToWgsl(v))
given Conversion[Float, FloatExpr] = v => FloatExpr(floatToWgsl(v.toDouble))
given Conversion[Int, FloatExpr] = v => FloatExpr(s"f32($v)")

// ---------------------------------------------------------------------------
// Left-operand arithmetic for numeric literals.
//
// A literal converts to a `FloatExpr` only as the *right* operand (via the
// conversions above). These extensions cover the left side so `0.5 * expr`,
// `2 - vExpr`, etc. work without reordering or ascription.
//
// A bare `Int` literal is treated as a *float* here, mirroring
// `Conversion[Int, FloatExpr]` (emits `f32(n)`) — int/uint expressions are
// opted into explicitly with `.i` / `.u`, never inferred from a left operand.
// ---------------------------------------------------------------------------

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

  // Bounds are mutually disjoint (no expr is a subtype of two base value
  // types), so resolution is unambiguous — and each base also covers its own
  // Let/Var/Const subtypes.
  given [E <: FloatExpr] => Aux[E, FloatExpr] = inst(FloatExpr(_))
  given [E <: Vec2Expr] => Aux[E, Vec2Expr] = inst(Vec2Expr(_))
  given [E <: Vec3Expr] => Aux[E, Vec3Expr] = inst(Vec3Expr(_))
  given [E <: Vec4Expr] => Aux[E, Vec4Expr] = inst(Vec4Expr(_))

extension (d: Double)
  inline def +[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(${floatToWgsl(d)} + ${e.wgsl})")
  inline def -[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(${floatToWgsl(d)} - ${e.wgsl})")
  inline def *[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(${floatToWgsl(d)} * ${e.wgsl})")
  inline def /[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(${floatToWgsl(d)} / ${e.wgsl})")

extension (n: Int)
  inline def +[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(f32($n) + ${e.wgsl})")
  inline def -[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(f32($n) - ${e.wgsl})")
  inline def *[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(f32($n) * ${e.wgsl})")
  inline def /[E <: Expr, O <: Expr](e: E)(using L: LeftScalar.Aux[E, O]): O = L.wrap(s"(f32($n) / ${e.wgsl})")

// ---------------------------------------------------------------------------
// NumOps / NumExt for FloatExpr
// LocalFloat <: FloatExpr, so these apply to LocalFloat too.
// ---------------------------------------------------------------------------

/** Arithmetic on `FloatExpr` (`+ - * /`, unary `-`). A `Double`/`Int`/`Float`
  * converts to `FloatExpr` as the **right** operand only (see the `Expr`
  * left-operand gotcha). */
given NumOps[FloatExpr]:
  extension (a: FloatExpr)
    def +(b: FloatExpr): FloatExpr = FloatExpr(s"(${a.wgsl} + ${b.wgsl})")
    def -(b: FloatExpr): FloatExpr = FloatExpr(s"(${a.wgsl} - ${b.wgsl})")
    def *(b: FloatExpr): FloatExpr = FloatExpr(s"(${a.wgsl} * ${b.wgsl})")
    def /(b: FloatExpr): FloatExpr = FloatExpr(s"(${a.wgsl} / ${b.wgsl})")
    def unary_- : FloatExpr = FloatExpr(s"(-${a.wgsl})")
  def zero: FloatExpr = FloatExpr("0.0")
  def one: FloatExpr = FloatExpr("1.0")

/** Scalar math on `FloatExpr` mapping to WGSL builtins: `.sqrt .pow .sin .cos
  * .tan .abs .floor .ceil .fract .exp .log .min .max .clamp .clamp01 .mix
  * .step .smoothstep .fit0111 .fit1101` and comparisons (`<`, `>`, … →
  * `BoolExpr`). Mirrors the CPU `NumExt`, so shader math reads like CPU math. */

given NumExt[FloatExpr]:
  extension (a: FloatExpr)
    def sqrt: FloatExpr = FloatExpr(s"sqrt(${a.wgsl})")
    def inverseSqrt: FloatExpr = FloatExpr(s"inverseSqrt(${a.wgsl})")
    def pow(exp: FloatExpr): FloatExpr = FloatExpr(
      s"pow(${a.wgsl}, ${exp.wgsl})",
    )
    def abs: FloatExpr = FloatExpr(s"abs(${a.wgsl})")
    def sign: FloatExpr = FloatExpr(s"sign(${a.wgsl})")
    def floor: FloatExpr = FloatExpr(s"floor(${a.wgsl})")
    def ceil: FloatExpr = FloatExpr(s"ceil(${a.wgsl})")
    def round: FloatExpr = FloatExpr(s"round(${a.wgsl})")
    def trunc: FloatExpr = FloatExpr(s"trunc(${a.wgsl})")
    def fract: FloatExpr = FloatExpr(s"fract(${a.wgsl})")
    def exp: FloatExpr = FloatExpr(s"exp(${a.wgsl})")
    def exp2: FloatExpr = FloatExpr(s"exp2(${a.wgsl})")
    def log: FloatExpr = FloatExpr(s"log(${a.wgsl})")
    def log2: FloatExpr = FloatExpr(s"log2(${a.wgsl})")
    def degrees: FloatExpr = FloatExpr(s"degrees(${a.wgsl})")
    def radians: FloatExpr = FloatExpr(s"radians(${a.wgsl})")
    def sin: FloatExpr = FloatExpr(s"sin(${a.wgsl})")
    def cos: FloatExpr = FloatExpr(s"cos(${a.wgsl})")
    def tan: FloatExpr = FloatExpr(s"tan(${a.wgsl})")
    def asin: FloatExpr = FloatExpr(s"asin(${a.wgsl})")
    def acos: FloatExpr = FloatExpr(s"acos(${a.wgsl})")
    def atan: FloatExpr = FloatExpr(s"atan(${a.wgsl})")
    def atan2(other: FloatExpr): FloatExpr = FloatExpr(
      s"atan2(${a.wgsl}, ${other.wgsl})",
    )
    def sinh: FloatExpr = FloatExpr(s"sinh(${a.wgsl})")
    def cosh: FloatExpr = FloatExpr(s"cosh(${a.wgsl})")
    def tanh: FloatExpr = FloatExpr(s"tanh(${a.wgsl})")
    def min(other: FloatExpr): FloatExpr = FloatExpr(
      s"min(${a.wgsl}, ${other.wgsl})",
    )
    def max(other: FloatExpr): FloatExpr = FloatExpr(
      s"max(${a.wgsl}, ${other.wgsl})",
    )
    def clamp(min: FloatExpr, max: FloatExpr): FloatExpr =
      FloatExpr(s"clamp(${a.wgsl}, ${min.wgsl}, ${max.wgsl})")
    def clamp01: FloatExpr = FloatExpr(s"saturate(${a.wgsl})")
    def fit0111: FloatExpr = FloatExpr(s"(${a.wgsl} * 2.0 - 1.0)")
    def fit1101: FloatExpr = FloatExpr(s"(${a.wgsl} * 0.5 + 0.5)")
    def mix(b: FloatExpr, t: FloatExpr): FloatExpr =
      FloatExpr(s"mix(${a.wgsl}, ${b.wgsl}, ${t.wgsl})")
    def gte(edge: FloatExpr): FloatExpr = FloatExpr(
      s"step(${edge.wgsl}, ${a.wgsl})",
    )
    def gt(edge: FloatExpr): FloatExpr = FloatExpr(
      s"(1.0 - step(${a.wgsl}, ${edge.wgsl}))",
    )
    def lte(edge: FloatExpr): FloatExpr = FloatExpr(
      s"step(${a.wgsl}, ${edge.wgsl})",
    )
    def lt(edge: FloatExpr): FloatExpr = FloatExpr(
      s"(1.0 - step(${edge.wgsl}, ${a.wgsl}))",
    )
    def smoothstep(edge0: FloatExpr, edge1: FloatExpr): FloatExpr =
      FloatExpr(s"smoothstep(${edge0.wgsl}, ${edge1.wgsl}, ${a.wgsl})")

// ---------------------------------------------------------------------------
// Vec2 — LocalVec2 <: Vec2Expr, so only one Base + one ImmutableOps needed
// ---------------------------------------------------------------------------

given Vec2BaseG[FloatExpr, Vec2Expr] =
  new Vec2BaseG[FloatExpr, Vec2Expr]:
    extension (v: Vec2Expr)
      def x: FloatExpr = FloatExpr(s"${v.wgsl}.x")
      def y: FloatExpr = FloatExpr(s"${v.wgsl}.y")
      def dot(other: Vec2Expr): FloatExpr = FloatExpr(
        s"dot(${v.wgsl}, ${other.wgsl})",
      )
      def length_squared: FloatExpr = FloatExpr(s"dot(${v.wgsl}, ${v.wgsl})")
      def length: FloatExpr = FloatExpr(s"length(${v.wgsl})")
      def distance(other: Vec2Expr): FloatExpr = FloatExpr(
        s"distance(${v.wgsl}, ${other.wgsl})",
      )

/** Vector ops for `Vec2Expr` / `Vec3Expr` / `Vec4Expr` (this and the analogous
  * `Vec3`/`Vec4` givens below): component-wise `+ - * /` (vector or scalar),
  * `.dot`, `.length`, `.distance`, `.normalize`, `.cross` (Vec3), `.mix`,
  * `.clamp`/`.clamp01`, `.min`/`.max`, `.abs`/`.floor`/`.fract`/`.sqrt`/
  * `.inverseSqrt`/`.trunc`/`.exp2`, `.pow` (vector or scalar exponent),
  * `.smoothstep` (vector or scalar edges), `.fit0111`/
  * `.fit1101`, and swizzles (`.xy`, `.xyz`, `.rgb`, `.wzyx`, …). Same surface as
  * the GPU `Expr[T]` so shader code reads like the CPU `Vec*` API. */
given Vec2ImmutableOpsG[FloatExpr, Vec2Expr]:
  def create(x: FloatExpr, y: FloatExpr): Vec2Expr =
    Vec2Expr(s"vec2<f32>(${x.wgsl}, ${y.wgsl})")

  extension (v: Vec2Expr)(using Vec2BaseG[FloatExpr, Vec2Expr])
    @annotation.targetName("addVecG")
    override def +(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"(${v.wgsl} + ${other.wgsl})")
    @annotation.targetName("addScalarG")
    override def +(scalar: FloatExpr): Vec2Expr =
      Vec2Expr(s"(${v.wgsl} + ${scalar.wgsl})")
    def +(scalar: Double): Vec2Expr = v + (scalar: FloatExpr)
    @annotation.targetName("negateVecG")
    override def unary_- : Vec2Expr = Vec2Expr(s"(-${v.wgsl})")
    @annotation.targetName("subVecG")
    override def -(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"(${v.wgsl} - ${other.wgsl})")
    @annotation.targetName("subScalarG")
    override def -(scalar: FloatExpr): Vec2Expr =
      Vec2Expr(s"(${v.wgsl} - ${scalar.wgsl})")
    def -(scalar: Double): Vec2Expr = v - (scalar: FloatExpr)
    @annotation.targetName("mulVecG")
    override def *(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"(${v.wgsl} * ${other.wgsl})")
    @annotation.targetName("mulScalarG")
    override def *(scalar: FloatExpr): Vec2Expr =
      Vec2Expr(s"(${v.wgsl} * ${scalar.wgsl})")
    def *(scalar: Double): Vec2Expr = v * (scalar: FloatExpr)
    @annotation.targetName("divVecG")
    override def /(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"(${v.wgsl} / ${other.wgsl})")
    @annotation.targetName("divScalarG")
    override def /(scalar: FloatExpr): Vec2Expr =
      Vec2Expr(s"(${v.wgsl} / ${scalar.wgsl})")
    def /(scalar: Double): Vec2Expr = v / (scalar: FloatExpr)

    override def normalize: Vec2Expr = Vec2Expr(s"normalize(${v.wgsl})")
    override def abs: Vec2Expr = Vec2Expr(s"abs(${v.wgsl})")
    override def sign: Vec2Expr = Vec2Expr(s"sign(${v.wgsl})")
    override def floor: Vec2Expr = Vec2Expr(s"floor(${v.wgsl})")
    override def ceil: Vec2Expr = Vec2Expr(s"ceil(${v.wgsl})")
    override def round: Vec2Expr = Vec2Expr(s"round(${v.wgsl})")
    override def fract: Vec2Expr = Vec2Expr(s"fract(${v.wgsl})")
    override def exp: Vec2Expr = Vec2Expr(s"exp(${v.wgsl})")
    override def log: Vec2Expr = Vec2Expr(s"log(${v.wgsl})")
    override def log2: Vec2Expr = Vec2Expr(s"log2(${v.wgsl})")
    override def sqrt: Vec2Expr = Vec2Expr(s"sqrt(${v.wgsl})")
    override def inverseSqrt: Vec2Expr = Vec2Expr(s"inverseSqrt(${v.wgsl})")
    override def trunc: Vec2Expr = Vec2Expr(s"trunc(${v.wgsl})")
    override def exp2: Vec2Expr = Vec2Expr(s"exp2(${v.wgsl})")
    @annotation.targetName("powVecG")
    override def pow(e: Vec2Expr): Vec2Expr = Vec2Expr(s"pow(${v.wgsl}, ${e.wgsl})")
    @annotation.targetName("powScalarG")
    override def pow(e: FloatExpr): Vec2Expr =
      Vec2Expr(s"pow(${v.wgsl}, vec2<f32>(${e.wgsl}))")
    def pow(scalar: Double): Vec2Expr = v.pow(scalar: FloatExpr)
    override def min(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"min(${v.wgsl}, ${other.wgsl})")
    override def max(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"max(${v.wgsl}, ${other.wgsl})")
    override def clamp(lo: FloatExpr, hi: FloatExpr): Vec2Expr =
      Vec2Expr(s"clamp(${v.wgsl}, ${lo.wgsl}, ${hi.wgsl})")
    override def clamp01: Vec2Expr = Vec2Expr(s"saturate(${v.wgsl})")
    override def fit0111: Vec2Expr = Vec2Expr(s"(${v.wgsl} * 2.0 - 1.0)")
    override def fit1101: Vec2Expr = Vec2Expr(s"(${v.wgsl} * 0.5 + 0.5)")
    @annotation.targetName("mixVecG")
    override def mix(b: Vec2Expr, t: Vec2Expr): Vec2Expr =
      Vec2Expr(s"mix(${v.wgsl}, ${b.wgsl}, ${t.wgsl})")
    @annotation.targetName("mixScalarG")
    override def mix(b: Vec2Expr, t: FloatExpr): Vec2Expr =
      Vec2Expr(s"mix(${v.wgsl}, ${b.wgsl}, ${t.wgsl})")
    @annotation.targetName("stepVecG")
    override def step(edge: Vec2Expr): Vec2Expr =
      Vec2Expr(s"step(${edge.wgsl}, ${v.wgsl})")
    @annotation.targetName("stepScalarG")
    override def step(edge: FloatExpr): Vec2Expr =
      Vec2Expr(s"step(${edge.wgsl}, ${v.wgsl})")
    @annotation.targetName("smoothstepVecG")
    override def smoothstep(edge0: Vec2Expr, edge1: Vec2Expr): Vec2Expr =
      Vec2Expr(s"smoothstep(${edge0.wgsl}, ${edge1.wgsl}, ${v.wgsl})")
    @annotation.targetName("smoothstepScalarG")
    override def smoothstep(edge0: FloatExpr, edge1: FloatExpr): Vec2Expr =
      Vec2Expr(s"smoothstep(vec2<f32>(${edge0.wgsl}), vec2<f32>(${edge1.wgsl}), ${v.wgsl})")
    def smoothstep(edge0: Double, edge1: Double): Vec2Expr =
      v.smoothstep(edge0: FloatExpr, edge1: FloatExpr)
    @annotation.targetName("ltVecG")
    override def <(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"(1.0 - step(${other.wgsl}, ${v.wgsl}))")
    @annotation.targetName("lteVecG")
    override def <=(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"step(${v.wgsl}, ${other.wgsl})")
    @annotation.targetName("gtVecG")
    override def >(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"(1.0 - step(${v.wgsl}, ${other.wgsl}))")
    @annotation.targetName("gteVecG")
    override def >=(other: Vec2Expr): Vec2Expr =
      Vec2Expr(s"step(${other.wgsl}, ${v.wgsl})")

    override def reflect(n: Vec2Expr): Vec2Expr =
      Vec2Expr(s"reflect(${v.wgsl}, ${n.wgsl})")
    override def refract(n: Vec2Expr, eta: FloatExpr): Vec2Expr =
      Vec2Expr(s"refract(${v.wgsl}, ${n.wgsl}, ${eta.wgsl})")

// ---------------------------------------------------------------------------
// Vec3 — LocalVec3 <: Vec3Expr
// ---------------------------------------------------------------------------

given Vec3BaseG[FloatExpr, Vec3Expr] =
  new Vec3BaseG[FloatExpr, Vec3Expr]:
    extension (v: Vec3Expr)
      def x: FloatExpr = FloatExpr(s"${v.wgsl}.x")
      def y: FloatExpr = FloatExpr(s"${v.wgsl}.y")
      def z: FloatExpr = FloatExpr(s"${v.wgsl}.z")
      def dot(other: Vec3Expr): FloatExpr = FloatExpr(
        s"dot(${v.wgsl}, ${other.wgsl})",
      )
      def length_squared: FloatExpr = FloatExpr(s"dot(${v.wgsl}, ${v.wgsl})")
      def length: FloatExpr = FloatExpr(s"length(${v.wgsl})")
      def distance(other: Vec3Expr): FloatExpr = FloatExpr(
        s"distance(${v.wgsl}, ${other.wgsl})",
      )

given Vec3ImmutableOpsG[FloatExpr, Vec3Expr]:
  def create(x: FloatExpr, y: FloatExpr, z: FloatExpr): Vec3Expr =
    Vec3Expr(s"vec3<f32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl})")

  extension (v: Vec3Expr)(using Vec3BaseG[FloatExpr, Vec3Expr])
    @annotation.targetName("addVecG")
    override def +(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} + ${other.wgsl})")
    @annotation.targetName("addScalarG")
    override def +(scalar: FloatExpr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} + ${scalar.wgsl})")
    def +(scalar: Double): Vec3Expr = v + (scalar: FloatExpr)
    @annotation.targetName("negateVecG")
    override def unary_- : Vec3Expr = Vec3Expr(s"(-${v.wgsl})")
    @annotation.targetName("subVecG")
    override def -(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} - ${other.wgsl})")
    @annotation.targetName("subScalarG")
    override def -(scalar: FloatExpr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} - ${scalar.wgsl})")
    def -(scalar: Double): Vec3Expr = v - (scalar: FloatExpr)
    @annotation.targetName("mulVecG")
    override def *(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} * ${other.wgsl})")
    @annotation.targetName("mulScalarG")
    override def *(scalar: FloatExpr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} * ${scalar.wgsl})")
    def *(scalar: Double): Vec3Expr = v * (scalar: FloatExpr)
    @annotation.targetName("divVecG")
    override def /(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} / ${other.wgsl})")
    @annotation.targetName("divScalarG")
    override def /(scalar: FloatExpr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} / ${scalar.wgsl})")
    def /(scalar: Double): Vec3Expr = v / (scalar: FloatExpr)

    override def cross(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"cross(${v.wgsl}, ${other.wgsl})")
    override def normalize: Vec3Expr = Vec3Expr(s"normalize(${v.wgsl})")
    override def abs: Vec3Expr = Vec3Expr(s"abs(${v.wgsl})")
    override def sign: Vec3Expr = Vec3Expr(s"sign(${v.wgsl})")
    override def floor: Vec3Expr = Vec3Expr(s"floor(${v.wgsl})")
    override def ceil: Vec3Expr = Vec3Expr(s"ceil(${v.wgsl})")
    override def round: Vec3Expr = Vec3Expr(s"round(${v.wgsl})")
    override def fract: Vec3Expr = Vec3Expr(s"fract(${v.wgsl})")
    override def exp: Vec3Expr = Vec3Expr(s"exp(${v.wgsl})")
    override def log: Vec3Expr = Vec3Expr(s"log(${v.wgsl})")
    override def log2: Vec3Expr = Vec3Expr(s"log2(${v.wgsl})")
    override def sqrt: Vec3Expr = Vec3Expr(s"sqrt(${v.wgsl})")
    override def inverseSqrt: Vec3Expr = Vec3Expr(s"inverseSqrt(${v.wgsl})")
    override def trunc: Vec3Expr = Vec3Expr(s"trunc(${v.wgsl})")
    override def exp2: Vec3Expr = Vec3Expr(s"exp2(${v.wgsl})")
    @annotation.targetName("powVecG")
    override def pow(e: Vec3Expr): Vec3Expr = Vec3Expr(s"pow(${v.wgsl}, ${e.wgsl})")
    @annotation.targetName("powScalarG")
    override def pow(e: FloatExpr): Vec3Expr =
      Vec3Expr(s"pow(${v.wgsl}, vec3<f32>(${e.wgsl}))")
    def pow(scalar: Double): Vec3Expr = v.pow(scalar: FloatExpr)
    override def min(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"min(${v.wgsl}, ${other.wgsl})")
    override def max(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"max(${v.wgsl}, ${other.wgsl})")
    override def clamp(lo: FloatExpr, hi: FloatExpr): Vec3Expr =
      Vec3Expr(s"clamp(${v.wgsl}, ${lo.wgsl}, ${hi.wgsl})")
    override def clamp01: Vec3Expr = Vec3Expr(s"saturate(${v.wgsl})")
    override def fit0111: Vec3Expr = Vec3Expr(s"(${v.wgsl} * 2.0 - 1.0)")
    override def fit1101: Vec3Expr = Vec3Expr(s"(${v.wgsl} * 0.5 + 0.5)")
    @annotation.targetName("mixVecG")
    override def mix(b: Vec3Expr, t: Vec3Expr): Vec3Expr =
      Vec3Expr(s"mix(${v.wgsl}, ${b.wgsl}, ${t.wgsl})")
    @annotation.targetName("mixScalarG")
    override def mix(b: Vec3Expr, t: FloatExpr): Vec3Expr =
      Vec3Expr(s"mix(${v.wgsl}, ${b.wgsl}, ${t.wgsl})")
    @annotation.targetName("stepVecG")
    override def step(edge: Vec3Expr): Vec3Expr =
      Vec3Expr(s"step(${edge.wgsl}, ${v.wgsl})")
    @annotation.targetName("stepScalarG")
    override def step(edge: FloatExpr): Vec3Expr =
      Vec3Expr(s"step(${edge.wgsl}, ${v.wgsl})")
    @annotation.targetName("smoothstepVecG")
    override def smoothstep(edge0: Vec3Expr, edge1: Vec3Expr): Vec3Expr =
      Vec3Expr(s"smoothstep(${edge0.wgsl}, ${edge1.wgsl}, ${v.wgsl})")
    @annotation.targetName("smoothstepScalarG")
    override def smoothstep(edge0: FloatExpr, edge1: FloatExpr): Vec3Expr =
      Vec3Expr(s"smoothstep(vec3<f32>(${edge0.wgsl}), vec3<f32>(${edge1.wgsl}), ${v.wgsl})")
    def smoothstep(edge0: Double, edge1: Double): Vec3Expr =
      v.smoothstep(edge0: FloatExpr, edge1: FloatExpr)
    @annotation.targetName("ltVecG")
    override def <(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(1.0 - step(${other.wgsl}, ${v.wgsl}))")
    @annotation.targetName("lteVecG")
    override def <=(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"step(${v.wgsl}, ${other.wgsl})")
    @annotation.targetName("gtVecG")
    override def >(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(1.0 - step(${v.wgsl}, ${other.wgsl}))")
    @annotation.targetName("gteVecG")
    override def >=(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"step(${other.wgsl}, ${v.wgsl})")

    override def reflect(n: Vec3Expr): Vec3Expr =
      Vec3Expr(s"reflect(${v.wgsl}, ${n.wgsl})")
    override def refract(n: Vec3Expr, eta: FloatExpr): Vec3Expr =
      Vec3Expr(s"refract(${v.wgsl}, ${n.wgsl}, ${eta.wgsl})")

// ---------------------------------------------------------------------------
// Vec4 — LocalVec4 <: Vec4Expr
// ---------------------------------------------------------------------------

given Vec4BaseG[FloatExpr, Vec4Expr] =
  new Vec4BaseG[FloatExpr, Vec4Expr]:
    extension (v: Vec4Expr)
      def x: FloatExpr = FloatExpr(s"${v.wgsl}.x")
      def y: FloatExpr = FloatExpr(s"${v.wgsl}.y")
      def z: FloatExpr = FloatExpr(s"${v.wgsl}.z")
      def w: FloatExpr = FloatExpr(s"${v.wgsl}.w")
      def dot(other: Vec4Expr): FloatExpr = FloatExpr(
        s"dot(${v.wgsl}, ${other.wgsl})",
      )
      def length_squared: FloatExpr = FloatExpr(s"dot(${v.wgsl}, ${v.wgsl})")
      def length: FloatExpr = FloatExpr(s"length(${v.wgsl})")
      def distance(other: Vec4Expr): FloatExpr = FloatExpr(
        s"distance(${v.wgsl}, ${other.wgsl})",
      )

given Vec4ImmutableOpsG[FloatExpr, Vec4Expr]:
  def create(x: FloatExpr, y: FloatExpr, z: FloatExpr, w: FloatExpr): Vec4Expr =
    Vec4Expr(s"vec4<f32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl}, ${w.wgsl})")

  extension (v: Vec4Expr)(using Vec4BaseG[FloatExpr, Vec4Expr])
    @annotation.targetName("addVecG")
    override def +(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"(${v.wgsl} + ${other.wgsl})")
    @annotation.targetName("addScalarG")
    override def +(scalar: FloatExpr): Vec4Expr =
      Vec4Expr(s"(${v.wgsl} + ${scalar.wgsl})")
    def +(scalar: Double): Vec4Expr = v + (scalar: FloatExpr)
    @annotation.targetName("negateVecG")
    override def unary_- : Vec4Expr = Vec4Expr(s"(-${v.wgsl})")
    @annotation.targetName("subVecG")
    override def -(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"(${v.wgsl} - ${other.wgsl})")
    @annotation.targetName("subScalarG")
    override def -(scalar: FloatExpr): Vec4Expr =
      Vec4Expr(s"(${v.wgsl} - ${scalar.wgsl})")
    def -(scalar: Double): Vec4Expr = v - (scalar: FloatExpr)
    @annotation.targetName("mulVecG")
    override def *(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"(${v.wgsl} * ${other.wgsl})")
    @annotation.targetName("mulScalarG")
    override def *(scalar: FloatExpr): Vec4Expr =
      Vec4Expr(s"(${v.wgsl} * ${scalar.wgsl})")
    def *(scalar: Double): Vec4Expr = v * (scalar: FloatExpr)
    @annotation.targetName("divVecG")
    override def /(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"(${v.wgsl} / ${other.wgsl})")
    @annotation.targetName("divScalarG")
    override def /(scalar: FloatExpr): Vec4Expr =
      Vec4Expr(s"(${v.wgsl} / ${scalar.wgsl})")
    def /(scalar: Double): Vec4Expr = v / (scalar: FloatExpr)

    override def normalize: Vec4Expr = Vec4Expr(s"normalize(${v.wgsl})")
    override def abs: Vec4Expr = Vec4Expr(s"abs(${v.wgsl})")
    override def sign: Vec4Expr = Vec4Expr(s"sign(${v.wgsl})")
    override def floor: Vec4Expr = Vec4Expr(s"floor(${v.wgsl})")
    override def ceil: Vec4Expr = Vec4Expr(s"ceil(${v.wgsl})")
    override def round: Vec4Expr = Vec4Expr(s"round(${v.wgsl})")
    override def fract: Vec4Expr = Vec4Expr(s"fract(${v.wgsl})")
    override def exp: Vec4Expr = Vec4Expr(s"exp(${v.wgsl})")
    override def log: Vec4Expr = Vec4Expr(s"log(${v.wgsl})")
    override def log2: Vec4Expr = Vec4Expr(s"log2(${v.wgsl})")
    override def sqrt: Vec4Expr = Vec4Expr(s"sqrt(${v.wgsl})")
    override def inverseSqrt: Vec4Expr = Vec4Expr(s"inverseSqrt(${v.wgsl})")
    override def trunc: Vec4Expr = Vec4Expr(s"trunc(${v.wgsl})")
    override def exp2: Vec4Expr = Vec4Expr(s"exp2(${v.wgsl})")
    @annotation.targetName("powVecG")
    override def pow(e: Vec4Expr): Vec4Expr = Vec4Expr(s"pow(${v.wgsl}, ${e.wgsl})")
    @annotation.targetName("powScalarG")
    override def pow(e: FloatExpr): Vec4Expr =
      Vec4Expr(s"pow(${v.wgsl}, vec4<f32>(${e.wgsl}))")
    def pow(scalar: Double): Vec4Expr = v.pow(scalar: FloatExpr)
    override def min(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"min(${v.wgsl}, ${other.wgsl})")
    override def max(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"max(${v.wgsl}, ${other.wgsl})")
    override def clamp(lo: FloatExpr, hi: FloatExpr): Vec4Expr =
      Vec4Expr(s"clamp(${v.wgsl}, ${lo.wgsl}, ${hi.wgsl})")
    override def clamp01: Vec4Expr = Vec4Expr(s"saturate(${v.wgsl})")
    override def fit0111: Vec4Expr = Vec4Expr(s"(${v.wgsl} * 2.0 - 1.0)")
    override def fit1101: Vec4Expr = Vec4Expr(s"(${v.wgsl} * 0.5 + 0.5)")
    @annotation.targetName("mixVecG")
    override def mix(b: Vec4Expr, t: Vec4Expr): Vec4Expr =
      Vec4Expr(s"mix(${v.wgsl}, ${b.wgsl}, ${t.wgsl})")
    @annotation.targetName("mixScalarG")
    override def mix(b: Vec4Expr, t: FloatExpr): Vec4Expr =
      Vec4Expr(s"mix(${v.wgsl}, ${b.wgsl}, ${t.wgsl})")
    @annotation.targetName("stepVecG")
    override def step(edge: Vec4Expr): Vec4Expr =
      Vec4Expr(s"step(${edge.wgsl}, ${v.wgsl})")
    @annotation.targetName("stepScalarG")
    override def step(edge: FloatExpr): Vec4Expr =
      Vec4Expr(s"step(${edge.wgsl}, ${v.wgsl})")
    @annotation.targetName("smoothstepVecG")
    override def smoothstep(edge0: Vec4Expr, edge1: Vec4Expr): Vec4Expr =
      Vec4Expr(s"smoothstep(${edge0.wgsl}, ${edge1.wgsl}, ${v.wgsl})")
    @annotation.targetName("smoothstepScalarG")
    override def smoothstep(edge0: FloatExpr, edge1: FloatExpr): Vec4Expr =
      Vec4Expr(s"smoothstep(vec4<f32>(${edge0.wgsl}), vec4<f32>(${edge1.wgsl}), ${v.wgsl})")
    def smoothstep(edge0: Double, edge1: Double): Vec4Expr =
      v.smoothstep(edge0: FloatExpr, edge1: FloatExpr)
    @annotation.targetName("ltVecG")
    override def <(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"(1.0 - step(${other.wgsl}, ${v.wgsl}))")
    @annotation.targetName("lteVecG")
    override def <=(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"step(${v.wgsl}, ${other.wgsl})")
    @annotation.targetName("gtVecG")
    override def >(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"(1.0 - step(${v.wgsl}, ${other.wgsl}))")
    @annotation.targetName("gteVecG")
    override def >=(other: Vec4Expr): Vec4Expr =
      Vec4Expr(s"step(${other.wgsl}, ${v.wgsl})")

    override def reflect(n: Vec4Expr): Vec4Expr =
      Vec4Expr(s"reflect(${v.wgsl}, ${n.wgsl})")
    override def refract(n: Vec4Expr, eta: FloatExpr): Vec4Expr =
      Vec4Expr(s"refract(${v.wgsl}, ${n.wgsl}, ${eta.wgsl})")

// ---------------------------------------------------------------------------
// Mat2
// ---------------------------------------------------------------------------

given Mat2BaseG[FloatExpr, Mat2Expr] =
  new Mat2BaseG[FloatExpr, Mat2Expr]:
    extension (m: Mat2Expr)
      def m00: FloatExpr = FloatExpr(s"${m.wgsl}[0][0]")
      def m01: FloatExpr = FloatExpr(s"${m.wgsl}[0][1]")
      def m10: FloatExpr = FloatExpr(s"${m.wgsl}[1][0]")
      def m11: FloatExpr = FloatExpr(s"${m.wgsl}[1][1]")
      def determinant: FloatExpr =
        FloatExpr(s"determinant(${m.wgsl})")

given Mat2ImmutableOpsG[FloatExpr, Mat2Expr]:
  def create(
      m00: FloatExpr,
      m01: FloatExpr,
      m10: FloatExpr,
      m11: FloatExpr,
  ): Mat2Expr =
    Mat2Expr(s"mat2x2<f32>(${m00.wgsl}, ${m01.wgsl}, ${m10.wgsl}, ${m11.wgsl})")

  extension (m: Mat2Expr)(using Mat2BaseG[FloatExpr, Mat2Expr])
    @annotation.targetName("matMulG")
    override def *(other: Mat2Expr): Mat2Expr =
      Mat2Expr(s"(${m.wgsl} * ${other.wgsl})")
    @annotation.targetName("vecMulG")
    override def *[Vec](v: Vec)(using
        Vec2BaseG[FloatExpr, Vec],
        Vec2ImmutableOpsG[FloatExpr, Vec],
    ): Vec =
      Vec2Expr(s"(${m.wgsl} * ${v.asInstanceOf[Expr].wgsl})").asInstanceOf[Vec]

// ---------------------------------------------------------------------------
// Mat3
// ---------------------------------------------------------------------------

given Mat3BaseG[FloatExpr, Mat3Expr] =
  new Mat3BaseG[FloatExpr, Mat3Expr]:
    extension (m: Mat3Expr)
      def m00: FloatExpr = FloatExpr(s"${m.wgsl}[0][0]")
      def m01: FloatExpr = FloatExpr(s"${m.wgsl}[0][1]")
      def m02: FloatExpr = FloatExpr(s"${m.wgsl}[0][2]")
      def m10: FloatExpr = FloatExpr(s"${m.wgsl}[1][0]")
      def m11: FloatExpr = FloatExpr(s"${m.wgsl}[1][1]")
      def m12: FloatExpr = FloatExpr(s"${m.wgsl}[1][2]")
      def m20: FloatExpr = FloatExpr(s"${m.wgsl}[2][0]")
      def m21: FloatExpr = FloatExpr(s"${m.wgsl}[2][1]")
      def m22: FloatExpr = FloatExpr(s"${m.wgsl}[2][2]")
      def determinant: FloatExpr =
        FloatExpr(s"determinant(${m.wgsl})")

given Mat3ImmutableOpsG[FloatExpr, Mat3Expr]:
  def create(
      m00: FloatExpr,
      m01: FloatExpr,
      m02: FloatExpr,
      m10: FloatExpr,
      m11: FloatExpr,
      m12: FloatExpr,
      m20: FloatExpr,
      m21: FloatExpr,
      m22: FloatExpr,
  ): Mat3Expr =
    Mat3Expr(
      s"mat3x3<f32>(${m00.wgsl}, ${m01.wgsl}, ${m02.wgsl}, ${m10.wgsl}, ${m11.wgsl}, ${m12.wgsl}, ${m20.wgsl}, ${m21.wgsl}, ${m22.wgsl})",
    )

  extension (m: Mat3Expr)(using Mat3BaseG[FloatExpr, Mat3Expr])
    @annotation.targetName("matMulG")
    override def *(other: Mat3Expr): Mat3Expr =
      Mat3Expr(s"(${m.wgsl} * ${other.wgsl})")
    @annotation.targetName("vecMulG")
    override def *[Vec](v: Vec)(using
        Vec3BaseG[FloatExpr, Vec],
        Vec3ImmutableOpsG[FloatExpr, Vec],
    ): Vec =
      Vec3Expr(s"(${m.wgsl} * ${v.asInstanceOf[Expr].wgsl})").asInstanceOf[Vec]

// ---------------------------------------------------------------------------
// Mat4
// ---------------------------------------------------------------------------

given Mat4BaseG[FloatExpr, Mat4Expr] =
  new Mat4BaseG[FloatExpr, Mat4Expr]:
    extension (m: Mat4Expr)
      def m00: FloatExpr = FloatExpr(s"${m.wgsl}[0][0]")
      def m01: FloatExpr = FloatExpr(s"${m.wgsl}[0][1]")
      def m02: FloatExpr = FloatExpr(s"${m.wgsl}[0][2]")
      def m03: FloatExpr = FloatExpr(s"${m.wgsl}[0][3]")
      def m10: FloatExpr = FloatExpr(s"${m.wgsl}[1][0]")
      def m11: FloatExpr = FloatExpr(s"${m.wgsl}[1][1]")
      def m12: FloatExpr = FloatExpr(s"${m.wgsl}[1][2]")
      def m13: FloatExpr = FloatExpr(s"${m.wgsl}[1][3]")
      def m20: FloatExpr = FloatExpr(s"${m.wgsl}[2][0]")
      def m21: FloatExpr = FloatExpr(s"${m.wgsl}[2][1]")
      def m22: FloatExpr = FloatExpr(s"${m.wgsl}[2][2]")
      def m23: FloatExpr = FloatExpr(s"${m.wgsl}[2][3]")
      def m30: FloatExpr = FloatExpr(s"${m.wgsl}[3][0]")
      def m31: FloatExpr = FloatExpr(s"${m.wgsl}[3][1]")
      def m32: FloatExpr = FloatExpr(s"${m.wgsl}[3][2]")
      def m33: FloatExpr = FloatExpr(s"${m.wgsl}[3][3]")
      def determinant: FloatExpr =
        FloatExpr(s"determinant(${m.wgsl})")

// format: off
given Mat4ImmutableOpsG[FloatExpr, Mat4Expr]:
  def create(
      m00: FloatExpr, m01: FloatExpr, m02: FloatExpr, m03: FloatExpr,
      m10: FloatExpr, m11: FloatExpr, m12: FloatExpr, m13: FloatExpr,
      m20: FloatExpr, m21: FloatExpr, m22: FloatExpr, m23: FloatExpr,
      m30: FloatExpr, m31: FloatExpr, m32: FloatExpr, m33: FloatExpr,
  ): Mat4Expr =
    Mat4Expr(s"mat4x4<f32>(${m00.wgsl}, ${m01.wgsl}, ${m02.wgsl}, ${m03.wgsl}, ${m10.wgsl}, ${m11.wgsl}, ${m12.wgsl}, ${m13.wgsl}, ${m20.wgsl}, ${m21.wgsl}, ${m22.wgsl}, ${m23.wgsl}, ${m30.wgsl}, ${m31.wgsl}, ${m32.wgsl}, ${m33.wgsl})")
  // format: on

  extension (m: Mat4Expr)(using Mat4BaseG[FloatExpr, Mat4Expr])
    @annotation.targetName("matMulG")
    override def *(other: Mat4Expr): Mat4Expr =
      Mat4Expr(s"(${m.wgsl} * ${other.wgsl})")
    @annotation.targetName("vecMulG")
    override def *[Vec](v: Vec)(using
        Vec4BaseG[FloatExpr, Vec],
        Vec4ImmutableOpsG[FloatExpr, Vec],
    ): Vec =
      Vec4Expr(s"(${m.wgsl} * ${v.asInstanceOf[Expr].wgsl})").asInstanceOf[Vec]

// ---------------------------------------------------------------------------
// Swizzles — consecutive sub-vector splits + full-arity reverse.
// ---------------------------------------------------------------------------

extension (v: Vec2Expr)
  def yx: Vec2Expr = Vec2Expr(s"${v.wgsl}.yx")

extension (v: Vec3Expr)
  @annotation.targetName("vec3_xy")
  def xy: Vec2Expr = Vec2Expr(s"${v.wgsl}.xy")
  @annotation.targetName("vec3_yz")
  def yz: Vec2Expr = Vec2Expr(s"${v.wgsl}.yz")
  def zyx: Vec3Expr = Vec3Expr(s"${v.wgsl}.zyx")
  @annotation.targetName("vec3_rg") inline def rg: Vec2Expr = v.xy
  @annotation.targetName("vec3_gb") inline def gb: Vec2Expr = v.yz
  inline def bgr: Vec3Expr = v.zyx

extension (v: Vec4Expr)
  @annotation.targetName("vec4_xy")
  def xy: Vec2Expr = Vec2Expr(s"${v.wgsl}.xy")
  @annotation.targetName("vec4_yz")
  def yz: Vec2Expr = Vec2Expr(s"${v.wgsl}.yz")
  def zw: Vec2Expr = Vec2Expr(s"${v.wgsl}.zw")
  def xyz: Vec3Expr = Vec3Expr(s"${v.wgsl}.xyz")
  def yzw: Vec3Expr = Vec3Expr(s"${v.wgsl}.yzw")
  def wzyx: Vec4Expr = Vec4Expr(s"${v.wgsl}.wzyx")
  @annotation.targetName("vec4_rg") inline def rg: Vec2Expr = v.xy
  @annotation.targetName("vec4_gb") inline def gb: Vec2Expr = v.yz
  inline def ba: Vec2Expr = v.zw
  inline def rgb: Vec3Expr = v.xyz
  inline def gba: Vec3Expr = v.yzw
  inline def abgr: Vec4Expr = v.wzyx

// ---------------------------------------------------------------------------
// Vector constructors (lowercase, matching WGSL syntax)
// ---------------------------------------------------------------------------

/** GPU `vec2<f32>` constructor: `vec2(x, y)` or `vec2(scalar)` (broadcast).
  * Lowercase to match WGSL; the CPU-side counterpart is `Vec2(...)`. */
object vec2:
  def apply(x: FloatExpr, y: FloatExpr): Vec2Expr =
    Vec2Expr(s"vec2<f32>(${x.wgsl}, ${y.wgsl})")
  def apply(scalar: FloatExpr): Vec2Expr = Vec2Expr(
    s"vec2<f32>(${scalar.wgsl})",
  )

/** GPU `vec3<f32>` constructor: `vec3(x, y, z)`, `vec3(xy, z)`, or
  * `vec3(scalar)`. */
object vec3:
  def apply(x: FloatExpr, y: FloatExpr, z: FloatExpr): Vec3Expr =
    Vec3Expr(s"vec3<f32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl})")
  def apply(scalar: FloatExpr): Vec3Expr = Vec3Expr(
    s"vec3<f32>(${scalar.wgsl})",
  )
  def apply(xy: Vec2Expr, z: FloatExpr): Vec3Expr =
    Vec3Expr(s"vec3<f32>(${xy.wgsl}, ${z.wgsl})")

/** GPU `vec4<f32>` constructor: `vec4(x,y,z,w)`, `vec4(xyz, w)`, `vec4(xy, z,
  * w)`, or `vec4(scalar)`. Common for colors: `vec4(rgb, 1.0)`. */
object vec4:
  def apply(x: FloatExpr, y: FloatExpr, z: FloatExpr, w: FloatExpr): Vec4Expr =
    Vec4Expr(s"vec4<f32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl}, ${w.wgsl})")
  def apply(xyz: Vec3Expr, w: FloatExpr): Vec4Expr =
    Vec4Expr(s"vec4<f32>(${xyz.wgsl}, ${w.wgsl})")
  def apply(xy: Vec2Expr, z: FloatExpr, w: FloatExpr): Vec4Expr =
    Vec4Expr(s"vec4<f32>(${xy.wgsl}, ${z.wgsl}, ${w.wgsl})")
  def apply(scalar: FloatExpr): Vec4Expr = Vec4Expr(
    s"vec4<f32>(${scalar.wgsl})",
  )
