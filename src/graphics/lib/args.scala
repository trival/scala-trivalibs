package trivalibs.graphics.lib

import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.Vec4
import trivalibs.graphics.math.gpu.{*, given}

// ---------------------------------------------------------------------------
// Parameter types that close the gap between CPU and GPU contexts.
//
// A shared lib extension (`p.simplexFbm(gain = 0.8)`) is one definition
// serving CPU receivers (`Vec2`, computing now) and GPU receivers (`Vec2Expr`,
// building WGSL). Scala forbids default arguments on more than one overloaded
// alternative, so the two sides can't be separate overloads, and each
// parameter takes either side's value instead.
//
// They are parameter types only, never stored or returned. The narrowing
// helpers below resolve them to one side's type at compile time: the CPU
// branch requires the CPU value (a shader expression there is a compile
// error), the GPU branch converts CPU values explicitly (`d.toExpr`, `i.i`,
// `v.toExpr`). Nothing of them survives into the linked JS or the WGSL.
// ---------------------------------------------------------------------------

/** A CPU `Double` or a shader `FloatExpr` — see the note in `args.scala`. */
type FloatArg = Double | FloatExpr

/** A CPU `Int` or a shader `IntExpr` — see the note in `args.scala`. */
type IntArg = Int | IntExpr

/** A CPU `Vec2` or a shader `Vec2Expr` — see the note in `args.scala`. */
type Vec2Arg = Vec2 | Vec2Expr

/** A CPU `Vec3` or a shader `Vec3Expr` — see the note in `args.scala`. */
type Vec3Arg = Vec3 | Vec3Expr

/** A CPU `Vec4` or a shader `Vec4Expr` — see the note in `args.scala`. */
type Vec4Arg = Vec4 | Vec4Expr

/** Any vector argument, for parameters of extensions whose receivers span
  * dimensions (`tilingPeriod` on 2D and 3D receivers). Each branch narrows it
  * to its own dimension; a mismatch is a compile error.
  */
type VecArg = Vec2Arg | Vec3Arg | Vec4Arg

// ---- CPU side: require the CPU value ----

transparent inline def cpuD(inline x: FloatArg): Double = inline x match
  case d: Double => d
  case _ => compiletime.error("CPU context takes a Double here, not a FloatExpr")

transparent inline def cpuI(inline x: IntArg): Int = inline x match
  case i: Int => i
  case _ => compiletime.error("CPU context takes an Int here, not an IntExpr")

transparent inline def cpuOptD(inline x: FloatArg | Null): Double | Null = inline x match
  case null => null
  case d: Double => d
  case _ => compiletime.error("CPU context takes a Double here, not a FloatExpr")

transparent inline def cpuOptV2(inline x: VecArg | Null): Vec2 | Null = inline x match
  case null => null
  case v: Vec2 => v
  case _ => compiletime.error("CPU context takes a Vec2 here, not a Vec2Expr")

transparent inline def cpuOptV3(inline x: VecArg | Null): Vec3 | Null = inline x match
  case null => null
  case v: Vec3 => v
  case _ => compiletime.error("CPU context takes a Vec3 here, not a Vec3Expr")

// ---- GPU side: convert CPU values explicitly ----

transparent inline def gpuF(inline x: FloatArg): FloatExpr = inline x match
  case d: Double => d.toExpr
  case e: FloatExpr => e

transparent inline def gpuI(inline x: IntArg): IntExpr = inline x match
  case i: Int => i.i
  case e: IntExpr => e

transparent inline def gpuOptF(inline x: FloatArg | Null): FloatExpr | Null = inline x match
  case null => null
  case d: Double => d.toExpr
  case e: FloatExpr => e

transparent inline def gpuOptV2(inline x: VecArg | Null): Vec2Expr | Null = inline x match
  case null => null
  case v: Vec2 => v.toExpr
  case e: Vec2Expr => e

transparent inline def gpuOptV3(inline x: VecArg | Null): Vec3Expr | Null = inline x match
  case null => null
  case v: Vec3 => v.toExpr
  case e: Vec3Expr => e

/** Runtime narrowing for the GPU object API, whose wrappers are ordinary defs
  * (shader-build time, so a type test is free): a plain `Int` becomes an `i32`
  * literal, an `IntExpr` passes through.
  */
def toIntExpr(x: IntArg): IntExpr = x match
  case i: Int => i.i
  case e => e.asInstanceOf[IntExpr]
