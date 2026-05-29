package trivalibs.graphics.math.gpu

import trivalibs.utils.numbers.IntExt
import trivalibs.utils.numbers.NumOps

// ---------------------------------------------------------------------------
// Zero-cost unsigned 32-bit type for the shader DSL.
// Bit-identical to Int at runtime; distinct in the type system so that
// WgslFn parameter / return types and ToExpr match cases stay unambiguous.
// U32 is a GPU / WGSL concept with no general Scala utility.
// ---------------------------------------------------------------------------

/** A 32-bit unsigned value for the shader DSL (WGSL `u32`). Bit-identical to
  * `Int` at runtime but distinct in the type system so DSL `u32` expressions and
  * `WgslFn` signatures stay unambiguous. Make one with `UInt(v)` or `v.u`. */
final class UInt(val toInt: Int) extends AnyVal

object UInt:
  inline def apply(v: Int): UInt = new UInt(v)

/** `v.u` — lift an `Int` literal to a `UInt` (WGSL `u32`). */
extension (v: Int) inline def u: UInt = UInt(v)

// ---------------------------------------------------------------------------
// GPU-only phantom markers for integer vector types.
// No CPU-side value representation — these exist only to carry WGSLType
// instances and serve as WgslFn parameter / return types.
// ---------------------------------------------------------------------------

sealed trait IVec2
sealed trait IVec3
sealed trait IVec4
sealed trait UVec2
sealed trait UVec3
sealed trait UVec4

// ---------------------------------------------------------------------------
// Integer literal helpers
// ---------------------------------------------------------------------------

given Conversion[UInt, UIntExpr] = v => UIntExpr(s"${v.toInt}u")

extension (v: Int) inline def i: IntExpr = IntExpr(v)

// ---------------------------------------------------------------------------
// NumOps for IntExpr (signed i32 arithmetic)
// ---------------------------------------------------------------------------

given NumOps[IntExpr]:
  extension (a: IntExpr)
    def +(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} + ${b.wgsl})")
    def -(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} - ${b.wgsl})")
    def *(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} * ${b.wgsl})")
    def /(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} / ${b.wgsl})")
    def unary_- : IntExpr = IntExpr(s"(-${a.wgsl})")
  def zero: IntExpr = IntExpr("0")
  def one: IntExpr = IntExpr("1")

// ---------------------------------------------------------------------------
// NumOps for UIntExpr (unsigned u32 arithmetic; unary_- wraps modulo 2^32)
// ---------------------------------------------------------------------------

given NumOps[UIntExpr]:
  extension (a: UIntExpr)
    def +(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} + ${b.wgsl})")
    def -(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} - ${b.wgsl})")
    def *(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} * ${b.wgsl})")
    def /(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} / ${b.wgsl})")
    def unary_- : UIntExpr = UIntExpr(s"(0u - ${a.wgsl})")
  def zero: UIntExpr = UIntExpr("0u")
  def one: UIntExpr = UIntExpr("1u")

// ---------------------------------------------------------------------------
// IntExt for IntExpr — min, max, clamp, comparison predicates
// ---------------------------------------------------------------------------

given IntExt[IntExpr]:
  extension (a: IntExpr)
    def min(b: IntExpr): IntExpr = IntExpr(s"min(${a.wgsl}, ${b.wgsl})")
    def max(b: IntExpr): IntExpr = IntExpr(s"max(${a.wgsl}, ${b.wgsl})")
    def clamp(lo: IntExpr, hi: IntExpr): IntExpr =
      IntExpr(s"clamp(${a.wgsl}, ${lo.wgsl}, ${hi.wgsl})")
    def gte(edge: IntExpr): IntExpr =
      IntExpr(s"select(0, 1, ${a.wgsl} >= ${edge.wgsl})")
    def gt(edge: IntExpr): IntExpr =
      IntExpr(s"select(0, 1, ${a.wgsl} > ${edge.wgsl})")
    def lte(edge: IntExpr): IntExpr =
      IntExpr(s"select(0, 1, ${a.wgsl} <= ${edge.wgsl})")
    def lt(edge: IntExpr): IntExpr =
      IntExpr(s"select(0, 1, ${a.wgsl} < ${edge.wgsl})")

// ---------------------------------------------------------------------------
// IntExt for UIntExpr
// ---------------------------------------------------------------------------

given IntExt[UIntExpr]:
  extension (a: UIntExpr)
    def min(b: UIntExpr): UIntExpr = UIntExpr(s"min(${a.wgsl}, ${b.wgsl})")
    def max(b: UIntExpr): UIntExpr = UIntExpr(s"max(${a.wgsl}, ${b.wgsl})")
    def clamp(lo: UIntExpr, hi: UIntExpr): UIntExpr =
      UIntExpr(s"clamp(${a.wgsl}, ${lo.wgsl}, ${hi.wgsl})")
    def gte(edge: UIntExpr): UIntExpr =
      UIntExpr(s"select(0u, 1u, ${a.wgsl} >= ${edge.wgsl})")
    def gt(edge: UIntExpr): UIntExpr =
      UIntExpr(s"select(0u, 1u, ${a.wgsl} > ${edge.wgsl})")
    def lte(edge: UIntExpr): UIntExpr =
      UIntExpr(s"select(0u, 1u, ${a.wgsl} <= ${edge.wgsl})")
    def lt(edge: UIntExpr): UIntExpr =
      UIntExpr(s"select(0u, 1u, ${a.wgsl} < ${edge.wgsl})")

// ---------------------------------------------------------------------------
// IntExpr — abs, sign, and bitwise operators
// ---------------------------------------------------------------------------

extension (a: IntExpr)
  def abs: IntExpr = IntExpr(s"abs(${a.wgsl})")
  def sign: IntExpr = IntExpr(s"sign(${a.wgsl})")

  /** Bitwise OR — output bit is 1 if either operand has that bit set. Used to
    * combine flag sets. Alias: [[or]].
    */
  @annotation.targetName("intOrExpr")
  def |(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} | ${b.wgsl})")

  /** Bitwise OR with a compile-time constant — useful for setting known flag
    * bits.
    */
  @annotation.targetName("intOrInt")
  def |(b: Int): IntExpr = IntExpr(s"(${a.wgsl} | $b)")

  /** Bitwise AND — output bit is 1 only when both operands have that bit set.
    * Standard masking tool: `x & 0xff` keeps the low 8 bits. Alias: [[and]].
    */
  @annotation.targetName("intAndExpr")
  def &(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} & ${b.wgsl})")

  /** Bitwise AND with a compile-time constant — useful for hex masks. */
  @annotation.targetName("intAndInt")
  def &(b: Int): IntExpr = IntExpr(s"(${a.wgsl} & $b)")

  /** Bitwise XOR — output bit is 1 when exactly one operand has that bit set.
    * Core hash-mixing primitive (`x ^ (x >> 16)`). Alias: [[xor]].
    */
  @annotation.targetName("intXorExpr")
  def ^(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} ^ ${b.wgsl})")

  /** Bitwise XOR with a compile-time constant. */
  @annotation.targetName("intXorInt")
  def ^(b: Int): IntExpr = IntExpr(s"(${a.wgsl} ^ $b)")

  /** Shift left by b positions — multiplies by 2^b, zero-fills low bits. Alias:
    * [[shl]].
    */
  @annotation.targetName("intShlExpr")
  def <<(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} << ${b.wgsl})")

  /** Shift left by a compile-time count. */
  @annotation.targetName("intShlInt")
  def <<(b: Int): IntExpr = IntExpr(s"(${a.wgsl} << $b)")

  /** Arithmetic shift right — sign bit is preserved. Equivalent to division by
    * 2^b rounding toward negative infinity. Alias: [[shr]].
    */
  @annotation.targetName("intShrExpr")
  def >>(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} >> ${b.wgsl})")

  /** Arithmetic shift right by a compile-time count. */
  @annotation.targetName("intShrInt")
  def >>(b: Int): IntExpr = IntExpr(s"(${a.wgsl} >> $b)")

  /** Bitwise NOT — flips every bit. For i32: `~x == -x - 1`. Alias: [[not]]. */
  @annotation.targetName("intBitNot")
  def unary_~ : IntExpr = IntExpr(s"(~${a.wgsl})")

  @annotation.targetName("intOr")
  inline def or(b: IntExpr): IntExpr = a | b
  @annotation.targetName("intAnd")
  inline def and(b: IntExpr): IntExpr = a & b
  @annotation.targetName("intXor")
  inline def xor(b: IntExpr): IntExpr = a ^ b
  @annotation.targetName("intShl")
  inline def shl(b: IntExpr): IntExpr = a << b
  @annotation.targetName("intShr")
  inline def shr(b: IntExpr): IntExpr = a >> b
  @annotation.targetName("intNot")
  inline def not: IntExpr = ~a

// ---------------------------------------------------------------------------
// UIntExpr — bitwise operators (logical right shift, u suffix on Int constants)
// ---------------------------------------------------------------------------

extension (a: UIntExpr)
  /** Bitwise OR — combines bits from both operands. Alias: [[or]]. */
  @annotation.targetName("uintOrExpr")
  def |(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} | ${b.wgsl})")

  /** Bitwise OR with a compile-time constant (emits `u` suffix). */
  @annotation.targetName("uintOrInt")
  def |(b: Int): UIntExpr = UIntExpr(s"(${a.wgsl} | ${b}u)")

  /** Bitwise AND — keeps only bits set in both operands. Alias: [[and]]. */
  @annotation.targetName("uintAndExpr")
  def &(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} & ${b.wgsl})")

  /** Bitwise AND with a compile-time constant (emits `u` suffix). */
  @annotation.targetName("uintAndInt")
  def &(b: Int): UIntExpr = UIntExpr(s"(${a.wgsl} & ${b}u)")

  /** Bitwise XOR — flips bits where the other operand has 1s. The hashing
    * workhorse. Alias: [[xor]].
    */
  @annotation.targetName("uintXorExpr")
  def ^(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} ^ ${b.wgsl})")

  /** Bitwise XOR with a compile-time constant (emits `u` suffix). */
  @annotation.targetName("uintXorInt")
  def ^(b: Int): UIntExpr = UIntExpr(s"(${a.wgsl} ^ ${b}u)")

  /** Shift left — zero-fills low bits. Alias: [[shl]]. */
  @annotation.targetName("uintShlExpr")
  def <<(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} << ${b.wgsl})")

  /** Shift left by a compile-time count (emits `u` suffix). */
  @annotation.targetName("uintShlInt")
  def <<(b: Int): UIntExpr = UIntExpr(s"(${a.wgsl} << ${b}u)")

  /** Logical (zero-fill) shift right — high bits become 0, unlike `IntExpr.>>`
    * which preserves the sign bit. Alias: [[shr]].
    */
  @annotation.targetName("uintShrExpr")
  def >>(b: UIntExpr): UIntExpr = UIntExpr(s"(${a.wgsl} >> ${b.wgsl})")

  /** Logical shift right by a compile-time count (emits `u` suffix). */
  @annotation.targetName("uintShrInt")
  def >>(b: Int): UIntExpr = UIntExpr(s"(${a.wgsl} >> ${b}u)")

  /** Bitwise NOT — flips every bit. Alias: [[not]]. */
  @annotation.targetName("uintBitNot")
  def unary_~ : UIntExpr = UIntExpr(s"(~${a.wgsl})")

  @annotation.targetName("uintOr")
  inline def or(b: UIntExpr): UIntExpr = a | b
  @annotation.targetName("uintAnd")
  inline def and(b: UIntExpr): UIntExpr = a & b
  @annotation.targetName("uintXor")
  inline def xor(b: UIntExpr): UIntExpr = a ^ b
  @annotation.targetName("uintShl")
  inline def shl(b: UIntExpr): UIntExpr = a << b
  @annotation.targetName("uintShr")
  inline def shr(b: UIntExpr): UIntExpr = a >> b
  @annotation.targetName("uintNot")
  inline def not: UIntExpr = ~a

// ---------------------------------------------------------------------------
// Type conversion extensions (i32/u32/f32 casts)
// ---------------------------------------------------------------------------

extension (a: FloatExpr)
  def toI32: IntExpr = IntExpr(s"i32(${a.wgsl})")
  def toU32: UIntExpr = UIntExpr(s"u32(${a.wgsl})")
  def bitsToU32: UIntExpr = UIntExpr(s"bitcast<u32>(${a.wgsl})")

extension (a: IntExpr)
  @annotation.targetName("intToF32")
  def toF32: FloatExpr = FloatExpr(s"f32(${a.wgsl})")
  @annotation.targetName("intToU32")
  def toU32: UIntExpr = UIntExpr(s"u32(${a.wgsl})")

extension (a: UIntExpr)
  @annotation.targetName("uintToF32")
  def toF32: FloatExpr = FloatExpr(s"f32(${a.wgsl})")
  @annotation.targetName("uintToI32")
  def toI32: IntExpr = IntExpr(s"i32(${a.wgsl})")
  def bitsToF32: FloatExpr = FloatExpr(s"bitcast<f32>(${a.wgsl})")

// ---------------------------------------------------------------------------
// Bitcast extensions for vectors
// ---------------------------------------------------------------------------

extension (v: Vec2Expr)
  @annotation.targetName("vec2BitsToU32")
  def bitsToU32: UVec2Expr = UVec2Expr(s"bitcast<vec2<u32>>(${v.wgsl})")

extension (v: UVec2Expr)
  @annotation.targetName("uvec2BitsToF32")
  def bitsToF32: Vec2Expr = Vec2Expr(s"bitcast<vec2<f32>>(${v.wgsl})")

extension (v: Vec3Expr)
  @annotation.targetName("vec3BitsToU32")
  def bitsToU32: UVec3Expr = UVec3Expr(s"bitcast<vec3<u32>>(${v.wgsl})")

extension (v: UVec3Expr)
  @annotation.targetName("uvec3BitsToF32")
  def bitsToF32: Vec3Expr = Vec3Expr(s"bitcast<vec3<f32>>(${v.wgsl})")

extension (v: Vec4Expr)
  @annotation.targetName("vec4BitsToU32")
  def bitsToU32: UVec4Expr = UVec4Expr(s"bitcast<vec4<u32>>(${v.wgsl})")

extension (v: UVec4Expr)
  @annotation.targetName("uvec4BitsToF32")
  def bitsToF32: Vec4Expr = Vec4Expr(s"bitcast<vec4<f32>>(${v.wgsl})")

// ---------------------------------------------------------------------------
// IVec2Expr — signed integer 2-vector operations
// ---------------------------------------------------------------------------

// +/-/*// arithmetic for IVec2Expr lives in expr.scala (Scala 3 cross-file
// overloading constraint — see the operator section there).
extension (v: IVec2Expr)
  @annotation.targetName("ivec2X")
  def x: IntExpr = IntExpr(s"${v.wgsl}.x")
  @annotation.targetName("ivec2Y")
  def y: IntExpr = IntExpr(s"${v.wgsl}.y")
  @annotation.targetName("ivec2Negate")
  def unary_- : IVec2Expr = IVec2Expr(s"(-${v.wgsl})")
  @annotation.targetName("ivec2Abs")
  def abs: IVec2Expr = IVec2Expr(s"abs(${v.wgsl})")
  @annotation.targetName("ivec2Min")
  def min(other: IVec2Expr): IVec2Expr = IVec2Expr(
    s"min(${v.wgsl}, ${other.wgsl})",
  )
  @annotation.targetName("ivec2Max")
  def max(other: IVec2Expr): IVec2Expr = IVec2Expr(
    s"max(${v.wgsl}, ${other.wgsl})",
  )
  @annotation.targetName("ivec2Or")
  def |(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} | ${other.wgsl})")
  @annotation.targetName("ivec2And")
  def &(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} & ${other.wgsl})")
  @annotation.targetName("ivec2Xor")
  def ^(other: IVec2Expr): IVec2Expr = IVec2Expr(s"(${v.wgsl} ^ ${other.wgsl})")
  @annotation.targetName("ivec2Not")
  def unary_~ : IVec2Expr = IVec2Expr(s"(~${v.wgsl})")

// ---------------------------------------------------------------------------
// UVec2Expr — unsigned integer 2-vector operations
// ---------------------------------------------------------------------------

// +/-/*// arithmetic for UVec2Expr lives in expr.scala.
extension (v: UVec2Expr)
  @annotation.targetName("uvec2X")
  def x: UIntExpr = UIntExpr(s"${v.wgsl}.x")
  @annotation.targetName("uvec2Y")
  def y: UIntExpr = UIntExpr(s"${v.wgsl}.y")
  @annotation.targetName("uvec2Min")
  def min(other: UVec2Expr): UVec2Expr = UVec2Expr(
    s"min(${v.wgsl}, ${other.wgsl})",
  )
  @annotation.targetName("uvec2Max")
  def max(other: UVec2Expr): UVec2Expr = UVec2Expr(
    s"max(${v.wgsl}, ${other.wgsl})",
  )
  @annotation.targetName("uvec2Or")
  def |(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} | ${other.wgsl})")
  @annotation.targetName("uvec2And")
  def &(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} & ${other.wgsl})")
  @annotation.targetName("uvec2Xor")
  def ^(other: UVec2Expr): UVec2Expr = UVec2Expr(s"(${v.wgsl} ^ ${other.wgsl})")
  @annotation.targetName("uvec2Not")
  def unary_~ : UVec2Expr = UVec2Expr(s"(~${v.wgsl})")

// ---------------------------------------------------------------------------
// IVec3Expr — signed integer 3-vector operations
// ---------------------------------------------------------------------------

// +/-/*// arithmetic for IVec3Expr lives in expr.scala.
extension (v: IVec3Expr)
  @annotation.targetName("ivec3X")
  def x: IntExpr = IntExpr(s"${v.wgsl}.x")
  @annotation.targetName("ivec3Y")
  def y: IntExpr = IntExpr(s"${v.wgsl}.y")
  @annotation.targetName("ivec3Z")
  def z: IntExpr = IntExpr(s"${v.wgsl}.z")
  @annotation.targetName("ivec3Negate")
  def unary_- : IVec3Expr = IVec3Expr(s"(-${v.wgsl})")
  @annotation.targetName("ivec3Abs")
  def abs: IVec3Expr = IVec3Expr(s"abs(${v.wgsl})")
  @annotation.targetName("ivec3Min")
  def min(other: IVec3Expr): IVec3Expr = IVec3Expr(
    s"min(${v.wgsl}, ${other.wgsl})",
  )
  @annotation.targetName("ivec3Max")
  def max(other: IVec3Expr): IVec3Expr = IVec3Expr(
    s"max(${v.wgsl}, ${other.wgsl})",
  )
  @annotation.targetName("ivec3Or")
  def |(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} | ${other.wgsl})")
  @annotation.targetName("ivec3And")
  def &(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} & ${other.wgsl})")
  @annotation.targetName("ivec3Xor")
  def ^(other: IVec3Expr): IVec3Expr = IVec3Expr(s"(${v.wgsl} ^ ${other.wgsl})")
  @annotation.targetName("ivec3Not")
  def unary_~ : IVec3Expr = IVec3Expr(s"(~${v.wgsl})")

// ---------------------------------------------------------------------------
// UVec3Expr — unsigned integer 3-vector operations
// ---------------------------------------------------------------------------

// +/-/*// arithmetic for UVec3Expr lives in expr.scala.
extension (v: UVec3Expr)
  @annotation.targetName("uvec3X")
  def x: UIntExpr = UIntExpr(s"${v.wgsl}.x")
  @annotation.targetName("uvec3Y")
  def y: UIntExpr = UIntExpr(s"${v.wgsl}.y")
  @annotation.targetName("uvec3Z")
  def z: UIntExpr = UIntExpr(s"${v.wgsl}.z")
  @annotation.targetName("uvec3Min")
  def min(other: UVec3Expr): UVec3Expr = UVec3Expr(
    s"min(${v.wgsl}, ${other.wgsl})",
  )
  @annotation.targetName("uvec3Max")
  def max(other: UVec3Expr): UVec3Expr = UVec3Expr(
    s"max(${v.wgsl}, ${other.wgsl})",
  )
  @annotation.targetName("uvec3Or")
  def |(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} | ${other.wgsl})")
  @annotation.targetName("uvec3And")
  def &(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} & ${other.wgsl})")
  @annotation.targetName("uvec3Xor")
  def ^(other: UVec3Expr): UVec3Expr = UVec3Expr(s"(${v.wgsl} ^ ${other.wgsl})")
  @annotation.targetName("uvec3Not")
  def unary_~ : UVec3Expr = UVec3Expr(s"(~${v.wgsl})")

// ---------------------------------------------------------------------------
// IVec4Expr — signed integer 4-vector operations
// ---------------------------------------------------------------------------

// +/-/*// arithmetic for IVec4Expr lives in expr.scala.
extension (v: IVec4Expr)
  @annotation.targetName("ivec4X")
  def x: IntExpr = IntExpr(s"${v.wgsl}.x")
  @annotation.targetName("ivec4Y")
  def y: IntExpr = IntExpr(s"${v.wgsl}.y")
  @annotation.targetName("ivec4Z")
  def z: IntExpr = IntExpr(s"${v.wgsl}.z")
  @annotation.targetName("ivec4W")
  def w: IntExpr = IntExpr(s"${v.wgsl}.w")
  @annotation.targetName("ivec4Negate")
  def unary_- : IVec4Expr = IVec4Expr(s"(-${v.wgsl})")
  @annotation.targetName("ivec4Or")
  def |(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} | ${other.wgsl})")
  @annotation.targetName("ivec4And")
  def &(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} & ${other.wgsl})")
  @annotation.targetName("ivec4Xor")
  def ^(other: IVec4Expr): IVec4Expr = IVec4Expr(s"(${v.wgsl} ^ ${other.wgsl})")
  @annotation.targetName("ivec4Not")
  def unary_~ : IVec4Expr = IVec4Expr(s"(~${v.wgsl})")

// ---------------------------------------------------------------------------
// UVec4Expr — unsigned integer 4-vector operations
// ---------------------------------------------------------------------------

// +/-/*// arithmetic for UVec4Expr lives in expr.scala.
extension (v: UVec4Expr)
  @annotation.targetName("uvec4X")
  def x: UIntExpr = UIntExpr(s"${v.wgsl}.x")
  @annotation.targetName("uvec4Y")
  def y: UIntExpr = UIntExpr(s"${v.wgsl}.y")
  @annotation.targetName("uvec4Z")
  def z: UIntExpr = UIntExpr(s"${v.wgsl}.z")
  @annotation.targetName("uvec4W")
  def w: UIntExpr = UIntExpr(s"${v.wgsl}.w")
  @annotation.targetName("uvec4Or")
  def |(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} | ${other.wgsl})")
  @annotation.targetName("uvec4And")
  def &(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} & ${other.wgsl})")
  @annotation.targetName("uvec4Xor")
  def ^(other: UVec4Expr): UVec4Expr = UVec4Expr(s"(${v.wgsl} ^ ${other.wgsl})")
  @annotation.targetName("uvec4Not")
  def unary_~ : UVec4Expr = UVec4Expr(s"(~${v.wgsl})")

// ---------------------------------------------------------------------------
// Integer vector constructors (lowercase, matching WGSL syntax)
// ---------------------------------------------------------------------------

object ivec2:
  def apply(x: IntExpr, y: IntExpr): IVec2Expr =
    IVec2Expr(s"vec2<i32>(${x.wgsl}, ${y.wgsl})")

object ivec3:
  def apply(x: IntExpr, y: IntExpr, z: IntExpr): IVec3Expr =
    IVec3Expr(s"vec3<i32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl})")

object ivec4:
  def apply(x: IntExpr, y: IntExpr, z: IntExpr, w: IntExpr): IVec4Expr =
    IVec4Expr(s"vec4<i32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl}, ${w.wgsl})")

object uvec2:
  def apply(x: UIntExpr, y: UIntExpr): UVec2Expr =
    UVec2Expr(s"vec2<u32>(${x.wgsl}, ${y.wgsl})")

object uvec3:
  def apply(x: UIntExpr, y: UIntExpr, z: UIntExpr): UVec3Expr =
    UVec3Expr(s"vec3<u32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl})")

object uvec4:
  def apply(x: UIntExpr, y: UIntExpr, z: UIntExpr, w: UIntExpr): UVec4Expr =
    UVec4Expr(s"vec4<u32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl}, ${w.wgsl})")
