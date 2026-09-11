package trivalibs.graphics.shader.dsl

import trivalibs.graphics.buffers.UniformArray
import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.gpu.*
import trivalibs.graphics.math.gpu.IVec2
import trivalibs.graphics.math.gpu.IVec3
import trivalibs.graphics.math.gpu.IVec4
import trivalibs.graphics.math.gpu.UInt
import trivalibs.graphics.math.gpu.UVec2
import trivalibs.graphics.math.gpu.UVec3
import trivalibs.graphics.math.gpu.UVec4
import trivalibs.graphics.shader.FragmentUniform
import trivalibs.graphics.shader.SharedUniform
import trivalibs.graphics.shader.VertexUniform
import trivalibs.utils.js.Dict

import scala.NamedTuple
import scala.NamedTuple.AnyNamedTuple
import scala.compiletime.*

/** Maps GPU math types to their DSL expression equivalents. Concrete types
  * (Float, Vec2, …) reduce to their Expr wrappers. Opaque GPU resource types
  * (Texture2D, Sampler) fall through to the identity case — they are already
  * expression types (<: Expr).
  */
type ToExpr[T] = T match
  case UniformArray[t, n] => ArrayExpr[ToExpr[t]]
  case Float              => FloatExpr
  case Double             => FloatExpr
  case Boolean            => BoolExpr
  case Int                => IntExpr
  case UInt               => UIntExpr
  case Vec2               => Vec2Expr
  case Vec3               => Vec3Expr
  case Vec4               => Vec4Expr
  case IVec2              => IVec2Expr
  case IVec3              => IVec3Expr
  case IVec4              => IVec4Expr
  case UVec2              => UVec2Expr
  case UVec3              => UVec3Expr
  case UVec4              => UVec4Expr
  case Mat2               => Mat2Expr
  case Mat3               => Mat3Expr
  case Mat4               => Mat4Expr
  case _                  => T

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
