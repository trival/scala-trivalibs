package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.gpu.*
import trivalibs.graphics.math.gpu.{IVec2, IVec3, IVec4, UInt, UVec2, UVec3, UVec4}
import trivalibs.graphics.shader.FragmentUniform
import trivalibs.graphics.shader.SharedUniform
import trivalibs.graphics.shader.VertexUniform
import trivalibs.utils.js.Dict
import scala.NamedTuple
import scala.NamedTuple.AnyNamedTuple
import scala.compiletime.*

/** Marker type for mutable WGSL local variables (`var`). */
class Var[T]

/** Marker type for WGSL compile-time constants (`const`). */
class Const[T]

/** Maps GPU math types to their DSL expression equivalents.
  * Concrete types (Float, Vec2, …) reduce to their Expr wrappers.
  * Opaque GPU resource types (Texture2D, Sampler) fall through to the
  * identity case — they are already expression types (<: Expr).
  */
type ToExpr[T] = T match
  case Float   => FloatExpr
  case Double  => FloatExpr
  case Boolean => BoolExpr
  case Int     => IntExpr
  case UInt    => UIntExpr
  case Vec2    => Vec2Expr
  case Vec3    => Vec3Expr
  case Vec4    => Vec4Expr
  case IVec2   => IVec2Expr
  case IVec3   => IVec3Expr
  case IVec4   => IVec4Expr
  case UVec2   => UVec2Expr
  case UVec3   => UVec3Expr
  case UVec4   => UVec4Expr
  case Mat2    => Mat2Expr
  case Mat3    => Mat3Expr
  case Mat4    => Mat4Expr
  case _       => T

/** Unwraps uniform wrapper types and maps to Expr. */
type UniformToExpr[T] = T match
  case VertexUniform[Sampler]   => Sampler
  case FragmentUniform[Sampler] => Sampler
  case SharedUniform[Sampler]   => Sampler
  case VertexUniform[t]         => ToExpr[t]
  case FragmentUniform[t]       => ToExpr[t]
  case SharedUniform[t]         => ToExpr[t]
  case _                        => ToExpr[T]

/** Maps any type to AssignTarget — used for vertex varying output fields. */
type ToAssign[T] = AssignTarget

/** Maps GPU math types to Local/Var/Const opaque types for typed local
  * variables.
  */
type ToLocal[T] = T match
  case Var[Float]    => VarFloat
  case Var[Double]   => VarFloat
  case Var[Int]      => VarInt
  case Var[UInt]     => VarUInt
  case Var[Vec2]     => VarVec2
  case Var[Vec3]     => VarVec3
  case Var[Vec4]     => VarVec4
  case Const[Float]  => ConstFloat
  case Const[Double] => ConstFloat
  case Const[Int]    => ConstInt
  case Const[UInt]   => ConstUInt
  case Const[Vec2]   => ConstVec2
  case Const[Vec3]   => ConstVec3
  case Const[Vec4]   => ConstVec4
  case Float         => LetFloat
  case Double        => LetFloat
  case Int           => LetInt
  case UInt          => LetUInt
  case Vec2          => LetVec2
  case Vec3          => LetVec3
  case Vec4          => LetVec4
  case IVec2         => LetIVec2
  case IVec3         => LetIVec3
  case IVec4         => LetIVec4
  case UVec2         => LetUVec2
  case UVec3         => LetUVec3
  case UVec4         => LetUVec4

/** Builds a Dict mapping field names to their kind: "v" for Var, "c" for Const.
  * Plain locals are omitted (default). Called at compile time via inline.
  */
inline def buildLocalKinds[L]: Dict[String] =
  val d = Dict[String]()
  inline erasedValue[L] match
    case _: EmptyTuple => ()
    case _             =>
      populateKinds[
        NamedTuple.Names[L & AnyNamedTuple],
        NamedTuple.DropNames[L & AnyNamedTuple],
      ](d)
  d

private inline def populateKinds[Names <: Tuple, Types <: Tuple](
    d: Dict[String],
): Unit =
  inline (erasedValue[Names], erasedValue[Types]) match
    case _: (EmptyTuple, EmptyTuple) => ()
    case _: ((n *: ns), (t *: ts))   =>
      inline erasedValue[t] match
        case _: Var[?]   => d(constValue[n & String]) = "v"
        case _: Const[?] => d(constValue[n & String]) = "c"
        case _           => ()
      populateKinds[ns, ts](d)
