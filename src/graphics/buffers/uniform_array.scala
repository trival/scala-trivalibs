package trivalibs.graphics.buffers

import trivalibs.bufferdata.StructRef
import trivalibs.graphics.math.cpu.Mat2
import trivalibs.graphics.math.cpu.Mat3
import trivalibs.graphics.math.cpu.Mat4
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.Vec4
import trivalibs.utils.js.Arr
import trivalibs.utils.js.jsError

import scala.annotation.implicitNotFound

// =============================================================================
// UniformArrayElem[T] — the element types WGSL admits in a uniform array
// =============================================================================

/** Marker for element types a uniform-address-space array may hold.
  *
  * WGSL requires the element *stride* of a uniform array to be a multiple of 16
  * bytes. That admits `Vec3` (padded to 16), `Vec4`, `Mat2`, `Mat3` (padded to
  * 48) and `Mat4`, and excludes `Float`/`Double` (4) and `Vec2` (8). To carry
  * scalars, pack four per `Vec4` at the call site.
  */
@implicitNotFound(
  "A UniformArray cannot hold ${T}. WGSL requires a uniform array's element stride to be a multiple of 16 bytes: Vec3, Vec4, Mat2, Mat3 and Mat4 qualify, Float and Vec2 do not — pack those into Vec4s.",
)
trait UniformArrayElem[T]

object UniformArrayElem:
  // One shared instance behind every witness — these carry no data, they only
  // gate which element types compile.
  private val marker = new UniformArrayElem[Any] {}
  private inline def of[T]: UniformArrayElem[T] =
    marker.asInstanceOf[UniformArrayElem[T]]

  given UniformArrayElem[Vec3] = of[Vec3]
  given UniformArrayElem[Vec4] = of[Vec4]
  given UniformArrayElem[Mat2] = of[Mat2]
  given UniformArrayElem[Mat3] = of[Mat3]
  given UniformArrayElem[Mat4] = of[Mat4]

// =============================================================================
// UniformArray[T, N] — a fixed-capacity uniform array
// =============================================================================

/** The value of an `array<T, N>` uniform — `N` is the capacity, fixed at
  * compile time, and part of the type so a shade's declaration and its binding
  * cannot disagree.
  *
  * Declare it in a shade's uniform schema and bind it like any other uniform:
  * {{{
  * type Uniforms = (stops: FragmentUniform[UniformArray[Vec4, 8]])
  *
  * val stops = painter.binding[UniformArray[Vec4, 8]]
  * stops := UniformArray[Vec4, 8](Arr(Vec4(…), Vec4(…)))
  * layer.bind("stops" := stops)
  * }}}
  * and index it in a shader body with `ctx.bindings.stops(i)`, by a constant or
  * by an `IntExpr`.
  *
  * Fewer than `N` values is normal — pass a count uniform alongside and mask
  * the tail. Rows past the ones written keep their previous contents (zero on a
  * fresh binding); more than `N` values throws.
  *
  * Element types are restricted by [[UniformArrayElem]].
  *
  * A nominal class rather than an `opaque type` over `Arr[T]`: the DSL maps
  * uniform types to expression types with a match type, and an opaque alias is
  * not provably disjoint from `Vec2` / `Float` / … outside its own file, which
  * stalls every other case of that match.
  */
final class UniformArray[T, N <: Int](val values: Arr[T])

object UniformArray:
  /** Wrap values as an `array<T, N>` uniform value. */
  inline def apply[T: UniformArrayElem, N <: Int](
      values: Arr[T],
  ): UniformArray[T, N] = new UniformArray[T, N](values)

  /** Writes each element through the element type's own [[UniformValue]], one
    * `F` row apart, and reports `N` rows so the binding allocates the whole
    * array. `F` stays the ELEMENT layout: the row count lives here rather than
    * in an N-times-repeated tuple type, which would make the compiler chew
    * through a 64-to-256-element tuple per binding for nothing.
    */
  given [T, N <: Int, F <: Tuple]
    => (
        elem: UniformValue[T, F],
        n: ValueOf[N],
        e: UniformArrayElem[T],
  ) => UniformValue[UniformArray[T, N], F]:

    def write(ref: StructRef[F], value: UniformArray[T, N]): Unit =
      val vs = value.values
      if vs.length > n.value then
        throw jsError(
          s"UniformArray: ${vs.length} values given for an array of ${n.value}",
        )
      val dv = ref.dataView
      val base = ref.offset
      val stride = elem.rowBytes
      var i = 0
      while i < vs.length do
        elem.write(StructRef[F](dv, base + i * stride), vs(i))
        i += 1

    def read(ref: StructRef[F]): UniformArray[T, N] =
      val dv = ref.dataView
      val base = ref.offset
      val stride = elem.rowBytes
      val out = Arr[T]()
      var i = 0
      while i < n.value do
        out.push(elem.read(StructRef[F](dv, base + i * stride)))
        i += 1
      new UniformArray[T, N](out)

    def rowBytes = elem.rowBytes
    override def rows = n.value
