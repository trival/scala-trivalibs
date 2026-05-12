package trivalibs.graphics.buffers

import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.bufferdata.F32
import trivalibs.bufferdata.StructArray
import trivalibs.bufferdata.StructRef
import trivalibs.utils.js.Obj
import trivalibs.utils.numbers.given
import trivalibs.graphics.painter.GPUBuffer
import trivalibs.graphics.painter.GPUBufferUsage
import trivalibs.graphics.painter.GPUDevice

// =============================================================================
// UniformValue[T, F] typeclass
//
// Maps a single Scala value T to its GPU buffer layout F and provides
// read/write operations on StructRef[F]. Instances live in the companion
// object so they are found via implicit search without explicit imports.
// =============================================================================

trait UniformValue[T, F <: Tuple]:
  def write(ref: StructRef[F], value: T): Unit
  def read(ref: StructRef[F]): T

object UniformValue:
  // --- Float ---
  given UniformValue[Float, F32 *: EmptyTuple]:
    inline def write(ref: StructRef[F32 *: EmptyTuple], value: Float): Unit =
      ref.setAt(0)(value)
    inline def read(ref: StructRef[F32 *: EmptyTuple]): Float = ref.getAt(0)

  // --- Double (maps to f32 in WGSL) ---
  given UniformValue[Double, F32 *: EmptyTuple]:
    inline def write(ref: StructRef[F32 *: EmptyTuple], value: Double): Unit =
      ref.setAt(0)(value.toFloat)
    inline def read(ref: StructRef[F32 *: EmptyTuple]): Double = ref.getAt(0)

  // --- Vec2 ---
  given UniformValue[Vec2, Vec2Buffer]:
    inline def write(ref: StructRef[Vec2Buffer], value: Vec2): Unit =
      ref := value
    inline def read(ref: StructRef[Vec2Buffer]): Vec2 = Vec2.from(ref)

  // --- Vec3 — uses Vec4Buffer for std140 padding; w component ignored ---
  given UniformValue[Vec3, Vec4Buffer]:
    inline def write(ref: StructRef[Vec4Buffer], value: Vec3): Unit =
      ref.x = value.x
      ref.y = value.y
      ref.z = value.z
    inline def read(ref: StructRef[Vec4Buffer]): Vec3 =
      Vec3(ref.x, ref.y, ref.z)

  // --- Vec4 ---
  given UniformValue[Vec4, Vec4Buffer]:
    inline def write(ref: StructRef[Vec4Buffer], value: Vec4): Unit =
      ref := value
    inline def read(ref: StructRef[Vec4Buffer]): Vec4 = Vec4.from(ref)

  // --- Mat2 ---
  given UniformValue[Mat2, Mat2Buffer]:
    import Mat2Buffer.given
    inline def write(ref: StructRef[Mat2Buffer], value: Mat2): Unit =
      ref := value
    inline def read(ref: StructRef[Mat2Buffer]): Mat2 = Mat2.from(ref)

  // --- Mat3 — padded buffer; accessors handle padding offsets ---
  given UniformValue[Mat3, Mat3PaddedBuffer]:
    inline def write(ref: StructRef[Mat3PaddedBuffer], value: Mat3): Unit =
      ref := value
    inline def read(ref: StructRef[Mat3PaddedBuffer]): Mat3 = Mat3.from(ref)

  // --- Mat4 ---
  given UniformValue[Mat4, Mat4Buffer]:
    inline def write(ref: StructRef[Mat4Buffer], value: Mat4): Unit =
      ref := value
    inline def read(ref: StructRef[Mat4Buffer]): Mat4 = Mat4.from(ref)

// =============================================================================
// UniformLayout[T] — single-param typeclass that collapses UniformValue[T, F]
// so makeBinding[T] can be called without spelling out F explicitly.
// =============================================================================

trait UniformLayout[T]:
  type Fields <: Tuple
  def uniformValue: UniformValue[T, Fields]

object UniformLayout:
  given [T, F <: Tuple] => (uv: UniformValue[T, F]) => UniformLayout[T]:
    type Fields = F
    inline def uniformValue = uv

// =============================================================================
// BufferBinding[T, F] — holds CPU array + GPU buffer + device
// =============================================================================

final class BufferBinding[T, F <: Tuple](
    private val buffer: StructRef[F],
    private val device: GPUDevice,
    private val uv: UniformValue[T, F],
):

  val gpuBuffer = device.createBuffer(
    Obj.literal(
      size = Math.max(16, buffer.dataView.byteLength),
      usage = GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    ),
  )

  /** Write value to CPU buffer and immediately upload to GPU. */
  inline def set(value: T): Unit =
    uv.write(buffer, value)
    upload()

  inline def :=(value: T): Unit = set(value)

  /** Mutate the CPU buffer via a lambda, then upload to GPU. Use this to
    * leverage the full generic set/assign ops on the buffer: binding.update(_
    * := vec4f) binding.update(ref => { ref.x = 1f; ref.y = 0f })
    */
  inline def update(inline f: StructRef[F] => Unit): Unit =
    f(buffer)
    upload()

  /** Read value from CPU buffer (does NOT read back from GPU). */
  inline def get: T = uv.read(buffer)

  /** Upload current CPU buffer contents to GPU without changing them. */
  private inline def upload(): Unit =
    device.queue.writeBuffer(gpuBuffer, 0.0, buffer.dataView.buffer)

object BufferBinding:
  // Two-param form: BufferBinding[Vec4, Vec4Buffer](device) — explicit F.
  inline def apply[T, F <: Tuple](
      device: GPUDevice,
  )(using uv: UniformValue[T, F]): BufferBinding[T, F] =
    val struct = StructArray.allocate[F](1)(0)
    new BufferBinding[T, F](struct, device, uv)

  inline def apply[T, F <: Tuple](
      device: GPUDevice,
      initialValue: T,
  )(using uv: UniformValue[T, F]): BufferBinding[T, F] =
    val b = BufferBinding[T, F](device)
    b.set(initialValue)
    b

  // One-param form: BufferBinding[Vec4](device) — F from UniformLayout.
  inline def apply[T: UniformLayout as ul](
      device: GPUDevice,
  ): BufferBinding[T, ul.Fields] =
    BufferBinding[T, ul.Fields](device)(using ul.uniformValue)

  inline def apply[T: UniformLayout as ul](
      device: GPUDevice,
      initialValue: T,
  ): BufferBinding[T, ul.Fields] =
    BufferBinding[T, ul.Fields](device, initialValue)(using ul.uniformValue)

extension [T](t: T)
  inline def asBinding[F <: Tuple](device: GPUDevice)(using
      uv: UniformValue[T, F],
  ): BufferBinding[T, F] =
    BufferBinding[T, F](device, t)
