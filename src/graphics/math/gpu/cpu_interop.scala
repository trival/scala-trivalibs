package trivalibs.graphics.math.gpu

// === CPU value → GPU expression ===
//
// Lifts a CPU-side `Vec*` / `Mat*` into the shader DSL as a WGSL literal, so a
// constant can be authored once as a CPU value and used in both domains:
//
//   val CeilTint = Vec3(0.86, 0.86, 0.85)   // authored once, CPU side
//   col := CeilTint.toExpr * roomNoise(wp, normal)
//
// Each conversion is also available implicitly, so a CPU value is accepted
// directly wherever an expression is expected (function arguments, the right
// side of `:=`). Note that implicit conversion does *not* fire in operator
// *receiver* position — `CeilTint * someFloatExpr` will not compile, because
// the compiler tries the CPU `Vec3ImmutableOps` extensions, fails, and does not
// fall back to converting the receiver. Write `CeilTint.toExpr * x` or
// `vec3(CeilTint) * x` there.
//
// These run at shader *build* time (once, at startup) and produce a WGSL
// string, so they are deliberately not `inline` and are not on any hot path.

/** Lift a CPU [[trivalibs.graphics.math.cpu.Vec2]] into a `vec2<f32>` literal.
  */
extension (v: Vec2)
  def toExpr: Vec2Expr =
    Vec2Expr(s"vec2<f32>(${floatToWgsl(v.x)}, ${floatToWgsl(v.y)})")

/** Lift a CPU [[trivalibs.graphics.math.cpu.Vec3]] into a `vec3<f32>` literal.
  */
extension (v: Vec3)
  def toExpr: Vec3Expr = Vec3Expr(
    s"vec3<f32>(${floatToWgsl(v.x)}, ${floatToWgsl(v.y)}, ${floatToWgsl(v.z)})",
  )

/** Lift a CPU [[trivalibs.graphics.math.cpu.Vec4]] into a `vec4<f32>` literal.
  */
extension (v: Vec4)
  def toExpr: Vec4Expr = Vec4Expr(
    s"vec4<f32>(${floatToWgsl(v.x)}, ${floatToWgsl(v.y)}, ${floatToWgsl(v.z)}, ${floatToWgsl(v.w)})",
  )

/** Lift a CPU [[trivalibs.graphics.math.cpu.Mat2]] into a `mat2x2<f32>`
  * literal. Element order matches the existing `Mat2Expr` constructor.
  */
extension (m: Mat2)
  def toExpr: Mat2Expr = Mat2Expr(
    s"mat2x2<f32>(${floatToWgsl(m.m00)}, ${floatToWgsl(m.m01)}, ${floatToWgsl(m.m10)}, ${floatToWgsl(m.m11)})",
  )

/** Lift a CPU [[trivalibs.graphics.math.cpu.Mat3]] into a `mat3x3<f32>`
  * literal. Element order matches the existing `Mat3Expr` constructor.
  */
extension (m: Mat3)
  def toExpr: Mat3Expr = Mat3Expr(
    s"mat3x3<f32>(${floatToWgsl(m.m00)}, ${floatToWgsl(m.m01)}, ${floatToWgsl(m.m02)}, ${floatToWgsl(m.m10)}, ${floatToWgsl(m.m11)}, ${floatToWgsl(m.m12)}, ${floatToWgsl(m.m20)}, ${floatToWgsl(m.m21)}, ${floatToWgsl(m.m22)})",
  )

/** Lift a CPU [[trivalibs.graphics.math.cpu.Mat4]] into a `mat4x4<f32>`
  * literal. Element order matches the existing `Mat4Expr` constructor.
  */
extension (m: Mat4)
  def toExpr: Mat4Expr = Mat4Expr(
    s"mat4x4<f32>(${floatToWgsl(m.m00)}, ${floatToWgsl(m.m01)}, ${floatToWgsl(m.m02)}, ${floatToWgsl(m.m03)}, ${floatToWgsl(m.m10)}, ${floatToWgsl(m.m11)}, ${floatToWgsl(m.m12)}, ${floatToWgsl(m.m13)}, ${floatToWgsl(m.m20)}, ${floatToWgsl(m.m21)}, ${floatToWgsl(m.m22)}, ${floatToWgsl(m.m23)}, ${floatToWgsl(m.m30)}, ${floatToWgsl(m.m31)}, ${floatToWgsl(m.m32)}, ${floatToWgsl(m.m33)})",
  )

// No `given Conversion[Vec*, Vec*Expr]` here — deliberately. Such a conversion
// makes every GPU extension applicable to a CPU value, which collides with the
// identically-named CPU ones: `v3.xy` becomes "ambiguous extension methods"
// between `cpu.xy(v)` and `gpu.xy(conversion(v))` for any file importing both
// namespaces — i.e. the standard sketch preamble. See the
// "CPU + GPU swizzles coexist" case in test/math/Swizzle.test.scala.
//
// Crossing the domain is therefore always explicit, via `.toExpr` above or the
// `vec2`/`vec3`/`vec4` constructors. That matches the `Vec3(…)` / `vec3(…)`
// case convention: the barrier stays visible at the point it is crossed.
