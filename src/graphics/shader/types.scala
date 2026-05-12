package trivalibs.graphics.shader

import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.gpu.{IVec2, IVec3, IVec4, UInt, UVec2, UVec3, UVec4}
import trivalibs.graphics.math.gpu.Expr.{Texture2D, Sampler}
import trivalibs.bufferdata.F32

import scala.compiletime.*

/** Type class for WGSL-compatible types */
trait WGSLType[T]:
  def wgslName: String
  def byteSize: Int
  def alignment: Int
  def vertexFormat: String
  def isSampler: Boolean = false
  type AttribBuffer <: Tuple
  type UniformBuffer <: Tuple

object WGSLType:
  inline def apply[T](using wt: WGSLType[T]): WGSLType[T] = wt

// =============================================================================
// Primitive Types
// =============================================================================

given WGSLType[Float]:
  def wgslName = "f32"
  def byteSize = 4
  def alignment = 4
  def vertexFormat = "float32"
  type AttribBuffer = F32 *: EmptyTuple
  type UniformBuffer = F32 *: EmptyTuple

// JS does only have 64-bit floats, so we map Scala Double to f32 in WGSL for simplicity
given WGSLType[Double]:
  def wgslName = "f32"
  def byteSize = 4
  def alignment = 4
  def vertexFormat = "float32"
  type AttribBuffer = F32 *: EmptyTuple
  type UniformBuffer = F32 *: EmptyTuple

given WGSLType[Vec2]:
  def wgslName = "vec2<f32>"
  def byteSize = 8
  def alignment = 8
  def vertexFormat = "float32x2"
  type AttribBuffer = Vec2Buffer
  type UniformBuffer = Vec2Buffer

given WGSLType[Vec3]:
  def wgslName = "vec3<f32>"
  def byteSize = 12
  def alignment = 16 // WGSL alignment rules
  def vertexFormat = "float32x3"
  type AttribBuffer = Vec3Buffer
  type UniformBuffer = Vec4Buffer // padded to vec4 per WGSL std140

given WGSLType[Vec4]:
  def wgslName = "vec4<f32>"
  def byteSize = 16
  def alignment = 16
  def vertexFormat = "float32x4"
  type AttribBuffer = Vec4Buffer
  type UniformBuffer = Vec4Buffer

given WGSLType[Mat2]:
  def wgslName = "mat2x2<f32>"
  def byteSize = 16
  def alignment = 8
  def vertexFormat = ""
  type AttribBuffer = Mat2Buffer
  type UniformBuffer = Mat2Buffer

given WGSLType[Mat3]:
  def wgslName = "mat3x3<f32>"
  def byteSize = 36
  def alignment = 16
  def vertexFormat = ""
  type AttribBuffer = Mat3Buffer
  type UniformBuffer = Mat3PaddedBuffer

given WGSLType[Mat4]:
  def wgslName = "mat4x4<f32>"
  def byteSize = 64
  def alignment = 16
  def vertexFormat = ""
  type AttribBuffer = Mat4Buffer
  type UniformBuffer = Mat4Buffer

// =============================================================================
// Utility Type Aliases
// =============================================================================

/** Alias for empty named tuple - use instead of EmptyTuple for clarity */
type None = EmptyTuple

/** Default vertex builtin output */
type VertOut = (position: BuiltinPosition)

/** Default fragment output */
type FragOut = (color: Vec4)

// =============================================================================
// Uniform Visibility Wrappers
// =============================================================================

/** Uniform visible only in vertex shader */
sealed trait VertexUniform[T]

object VertexUniform:
  given [T] => (inner: WGSLType[T]) => WGSLType[VertexUniform[T]]:
    def wgslName = inner.wgslName
    def byteSize = inner.byteSize
    def alignment = inner.alignment
    def vertexFormat = inner.vertexFormat
    override def isSampler = inner.isSampler
    type AttribBuffer = inner.AttribBuffer
    type UniformBuffer = inner.UniformBuffer

/** Uniform visible only in fragment shader */
sealed trait FragmentUniform[T]

object FragmentUniform:
  given [T] => (inner: WGSLType[T]) => WGSLType[FragmentUniform[T]]:
    def wgslName = inner.wgslName
    def byteSize = inner.byteSize
    def alignment = inner.alignment
    def vertexFormat = inner.vertexFormat
    override def isSampler = inner.isSampler
    type AttribBuffer = inner.AttribBuffer
    type UniformBuffer = inner.UniformBuffer

/** Uniform visible in both vertex and fragment shaders */
sealed trait SharedUniform[T]

object SharedUniform:
  given [T] => (inner: WGSLType[T]) => WGSLType[SharedUniform[T]]:
    def wgslName = inner.wgslName
    def byteSize = inner.byteSize
    def alignment = inner.alignment
    def vertexFormat = inner.vertexFormat
    override def isSampler = inner.isSampler
    type AttribBuffer = inner.AttribBuffer
    type UniformBuffer = inner.UniformBuffer

// =============================================================================
// GPU Resource Types (Texture2D, Sampler) — used in shader DSL, no CPU side
// =============================================================================

given WGSLType[Texture2D]:
  def wgslName = "texture_2d<f32>"
  def byteSize = 0
  def alignment = 0
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[Sampler]:
  def wgslName = "sampler"
  def byteSize = 0
  def alignment = 0
  def vertexFormat = ""
  override def isSampler = true
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

// =============================================================================
// Panel Visibility Wrappers (Group 1 texture bindings)
// =============================================================================

/** Panel texture visible only in the fragment shader */
sealed trait FragmentPanel

/** Panel texture visible only in the vertex shader */
sealed trait VertexPanel

/** Panel texture visible in both vertex and fragment shaders */
sealed trait SharedPanel

// =============================================================================
// Integer Types (shader-only; no CPU buffer representation)
// =============================================================================

given WGSLType[Int]:
  def wgslName = "i32"
  def byteSize = 4
  def alignment = 4
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[UInt]:
  def wgslName = "u32"
  def byteSize = 4
  def alignment = 4
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[IVec2]:
  def wgslName = "vec2<i32>"
  def byteSize = 8
  def alignment = 8
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[IVec3]:
  def wgslName = "vec3<i32>"
  def byteSize = 12
  def alignment = 16
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[IVec4]:
  def wgslName = "vec4<i32>"
  def byteSize = 16
  def alignment = 16
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[UVec2]:
  def wgslName = "vec2<u32>"
  def byteSize = 8
  def alignment = 8
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[UVec3]:
  def wgslName = "vec3<u32>"
  def byteSize = 12
  def alignment = 16
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[UVec4]:
  def wgslName = "vec4<u32>"
  def byteSize = 16
  def alignment = 16
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple
