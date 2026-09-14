package trivalibs.graphics.buffers

import trivalibs.bufferdata.StructRef
import trivalibs.graphics.math.cpu.Mat2
import trivalibs.graphics.math.cpu.Mat3
import trivalibs.graphics.math.cpu.Mat4
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.Vec4
import trivalibs.utils.js.Arr
import trivalibs.utils.js.jsError

import scala.annotation.implicitNotFound

// =============================================================================
// UniformArrayElem[T] — the element types WGSL admits in a uniform array
// =============================================================================

/** The element types a uniform-address-space array may hold, and how many of
  * them share one 16-byte row.
  *
  * WGSL requires the element *stride* of a uniform array to be a multiple of 16
  * bytes, so `Float`/`Double` (4 bytes) and `Vec2` (8) cannot be array elements
  * as they stand. They are instead **lane-packed** into `vec4` rows — four
  * scalars or two `Vec2`s per row — which the library does on both sides, so
  * `UniformArray[Double, 8]` is eight floats to its user and
  * `array<vec4<f32>, 2>` to WGSL.
  *
  * [[lanes]] is the whole of that difference: everything whose stride is
  * already a multiple of 16 has one element per row.
  */
@implicitNotFound(
  "A UniformArray cannot hold ${T}. Allowed element types are Float, Double, Vec2, Vec3, Vec4, Mat2, Mat3 and Mat4 — integer and boolean elements have no CPU write path yet.",
)
trait UniformArrayElem[T]:
  /** Array elements per 16-byte uniform row: 4 for scalars, 2 for `Vec2`, 1 for
    * every type that already strides a multiple of 16.
    */
  def lanes: Int

object UniformArrayElem:
  // One shared instance per lane count — these carry no per-type data, they
  // only gate which element types compile and how densely they pack.
  private val one = new UniformArrayElem[Any] { def lanes = 1 }
  private val two = new UniformArrayElem[Any] { def lanes = 2 }
  private val four = new UniformArrayElem[Any] { def lanes = 4 }
  private inline def of[T](marker: UniformArrayElem[Any]): UniformArrayElem[T] =
    marker.asInstanceOf[UniformArrayElem[T]]

  given UniformArrayElem[Float] = of[Float](four)
  given UniformArrayElem[Double] = of[Double](four)
  given UniformArrayElem[Vec2] = of[Vec2](two)
  given UniformArrayElem[Vec3] = of[Vec3](one)
  given UniformArrayElem[Vec4] = of[Vec4](one)
  given UniformArrayElem[Mat2] = of[Mat2](one)
  given UniformArrayElem[Mat3] = of[Mat3](one)
  given UniformArrayElem[Mat4] = of[Mat4](one)

// =============================================================================
// UniformArray[T, N] — a fixed-capacity uniform array
// =============================================================================

/** The value of an `array<T, N>` uniform — `N` is the capacity, fixed at
  * compile time, and part of the type so a shade's declaration and its binding
  * cannot disagree.
  *
  * Declare it in a shade's uniform schema and bind it like any other uniform —
  * the values travel as a plain `Arr`, the capacity comes from the type:
  * {{{
  * type Uniforms = (stops: FragmentUniform[UniformArray[Vec4, 8]])
  *
  * layer.bind("stops" := Arr(Vec4(…), Vec4(…)))   // one-shot: capacity from the schema
  *
  * val stops = painter.binding[UniformArray[Vec4, 8]]   // held and updated
  * stops.set(Arr(Vec4(…), Vec4(…)))
  * layer.bind("stops" := stops)
  * }}}
  * and index it in a shader body with `ctx.bindings.stops(i)`, by a constant or
  * by an `IntExpr`.
  *
  * Binding **fewer than `N` values is supported on purpose**, not a mistake to
  * guard against: the capacity is the maximum a shade can read, and a `count`
  * uniform alongside says how many are live, which is how one shade serves a
  * varying number of elements. Rows past the ones written keep their previous
  * contents — zero on a fresh binding, but *stale* after a longer write, so the
  * count has to be honoured. More than `N` values throws.
  *
  * `N` counts **elements, not GPU rows**. Scalars and `Vec2`s are lane-packed
  * into `vec4` rows (see [[UniformArrayElem]]), so `UniformArray[Double, 8]`
  * declares `array<vec4<f32>, 2>` and is still indexed `0..7` in the shader.
  * `N` need not be a multiple of the lane count — `UniformArray[Double, 7]`
  * simply leaves the last lane dead, and still rejects an eighth value.
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

  /** Rows an array of `count` elements occupies, rounded up to a whole 16-byte
    * uniform row. In units of `F` rows, which is also a count of elements —
    * every element occupies exactly one `F` row.
    */
  inline def rowsFor(count: Int, lanes: Int): Int =
    ((count + lanes - 1) / lanes) * lanes

  /** A [[UniformValue]] over the bare values, sized to `rows` elements rather
    * than to a capacity in the type.
    *
    * For the one caller that has no capacity to read: `panel.bind`, which binds
    * by name with no shade schema in scope and must size the buffer from the
    * values themselves. Deliberately **not** a `given` — everywhere else the
    * capacity is available and should be stated, so that partial fills work.
    */
  private[trivalibs] def valuesOnly[T, F <: Tuple](
      elem: UniformValue[T, F],
      rowCount: Int,
  ): UniformValue[Arr[T], F] =
    new UniformValue[Arr[T], F]:
      def write(ref: StructRef[F], values: Arr[T]): Unit =
        if values.length > rowCount then
          throw jsError(
            s"UniformArray: ${values.length} values given for a buffer sized for $rowCount — a panel binding is sized by its first value",
          )
        val dv = ref.dataView
        val base = ref.offset
        val stride = elem.rowBytes
        var i = 0
        while i < values.length do
          elem.write(StructRef[F](dv, base + i * stride), values(i))
          i += 1

      def read(ref: StructRef[F]): Arr[T] =
        val dv = ref.dataView
        val base = ref.offset
        val stride = elem.rowBytes
        val out = Arr[T]()
        var i = 0
        while i < rowCount do
          out.push(elem.read(StructRef[F](dv, base + i * stride)))
          i += 1
        out

      def rowBytes = elem.rowBytes
      override def rows = rowCount

  /** Writes each element through the element type's own [[UniformValue]], one
    * `F` row apart, and reports enough rows for the binding to allocate the
    * whole array. `F` stays the ELEMENT layout: the row count lives here rather
    * than in an N-times-repeated tuple type, which would make the compiler chew
    * through a 64-to-256-element tuple per binding for nothing.
    *
    * The dense element stride is also what produces the lane packing — four
    * `F32` rows or two `Vec2Buffer` rows fill one `vec4` — so nothing here
    * needs to know about lanes except the rounding in [[rows]].
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

    // Rounded up to a whole 16-byte row, so the allocation matches the size of
    // the WGSL type the shade declares: ⌈N/lanes⌉ rows of 16 bytes. For an
    // unpacked element (lanes = 1) this is plain `N`.
    override def rows =
      val lanes = e.lanes
      ((n.value + lanes - 1) / lanes) * lanes

// =============================================================================
// Arr → UniformArray
// =============================================================================

extension [T](values: Arr[T])
  /** View an `Arr` as the value of an `array<T, N>` uniform, naming only the
    * capacity — the element type comes from the `Arr` itself:
    * {{{
    * val stops = p.binding(colors.asUniform[MaxStops])
    * layer.bind("stops" := colors.asUniform[MaxStops])
    * }}}
    * No copy: the `Arr` becomes the array value's backing store.
    */
  inline def asUniform[N <: Int]: UniformArray[T, N] =
    new UniformArray[T, N](values)
