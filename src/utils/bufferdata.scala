package trivalibs.bufferdata

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.DataView
import scala.scalajs.js.typedarray.Uint8Array

// =============================================================================
// BufferView - js.Object wrapper for DataView + offset, direct property access
// =============================================================================

class BufferView(val dv: DataView, var off: Int) extends js.Object

// =============================================================================
// Primitive Types as Scala 3 Enum with compile-time size information
// =============================================================================

enum PrimitiveType(val byteSize: Int):
  case F32 extends PrimitiveType(4)
  case F64 extends PrimitiveType(8)
  case U8 extends PrimitiveType(1)
  case U16 extends PrimitiveType(2)
  case U32 extends PrimitiveType(4)
  case I8 extends PrimitiveType(1)
  case I16 extends PrimitiveType(2)
  case I32 extends PrimitiveType(4)

export PrimitiveType.*

// =============================================================================
// Type Aliases for Tuple Type Parameters
// =============================================================================

type F32 = PrimitiveType.F32.type
type F64 = PrimitiveType.F64.type
type U8 = PrimitiveType.U8.type
type U16 = PrimitiveType.U16.type
type U32 = PrimitiveType.U32.type
type I8 = PrimitiveType.I8.type
type I16 = PrimitiveType.I16.type
type I32 = PrimitiveType.I32.type

// =============================================================================
// Primitive Views (Opaque Types) - ZERO COST
// =============================================================================

opaque type F32View = BufferView
object F32View:
  inline def apply(view: DataView, offset: Int): F32View =
    BufferView(view, offset)
  extension (v: F32View)
    inline def get: Float = v.dv.getFloat32(v.off, littleEndian = true)
    inline def set(value: Float): Unit =
      v.dv.setFloat32(v.off, value, littleEndian = true)

opaque type F64View = BufferView
object F64View:
  inline def apply(view: DataView, offset: Int): F64View =
    BufferView(view, offset)
  extension (v: F64View)
    inline def get: Double = v.dv.getFloat64(v.off, littleEndian = true)
    inline def set(value: Double): Unit =
      v.dv.setFloat64(v.off, value, littleEndian = true)

opaque type U8View = BufferView
object U8View:
  inline def apply(view: DataView, offset: Int): U8View =
    BufferView(view, offset)
  extension (v: U8View)
    inline def get: Short = v.dv.getUint8(v.off)
    inline def set(value: Short): Unit = v.dv.setUint8(v.off, value)

opaque type U16View = BufferView
object U16View:
  inline def apply(view: DataView, offset: Int): U16View =
    BufferView(view, offset)
  extension (v: U16View)
    inline def get: Int = v.dv.getUint16(v.off, littleEndian = true)
    inline def set(value: Int): Unit =
      v.dv.setUint16(v.off, value, littleEndian = true)

opaque type U32View = BufferView
object U32View:
  inline def apply(view: DataView, offset: Int): U32View =
    BufferView(view, offset)
  extension (v: U32View)
    inline def get: Double = v.dv.getUint32(v.off, littleEndian = true)
    inline def set(value: Double): Unit =
      v.dv.setUint32(v.off, value, littleEndian = true)

opaque type I8View = BufferView
object I8View:
  inline def apply(view: DataView, offset: Int): I8View =
    BufferView(view, offset)
  extension (v: I8View)
    inline def get: Byte = v.dv.getInt8(v.off)
    inline def set(value: Byte): Unit =
      v.dv.setInt8(v.off, value)

opaque type I16View = BufferView
object I16View:
  inline def apply(view: DataView, offset: Int): I16View =
    BufferView(view, offset)
  extension (v: I16View)
    inline def get: Short = v.dv.getInt16(v.off, littleEndian = true)
    inline def set(value: Short): Unit =
      v.dv.setInt16(v.off, value, littleEndian = true)

opaque type I32View = BufferView
object I32View:
  inline def apply(view: DataView, offset: Int): I32View =
    BufferView(view, offset)
  extension (v: I32View)
    inline def get: Int = v.dv.getInt32(v.off, littleEndian = true)
    inline def set(value: Int): Unit =
      v.dv.setInt32(v.off, value, littleEndian = true)

// =============================================================================
// Compile-Time Size and Offset Calculation (Match Types)
// =============================================================================

import scala.compiletime.{constValue, erasedValue, error}
import scala.compiletime.ops.int.+

/** Size of a primitive field type in bytes */
type PrimitiveSize[T] <: Int = T match
  case F32 => 4
  case F64 => 8
  case U8  => 1
  case U16 => 2
  case U32 => 4
  case I8  => 1
  case I16 => 2
  case I32 => 4

/** Total size of a tuple (struct) in bytes - handles both primitives and nested
  * tuples
  */
type TupleSize[Fields <: Tuple] <: Int = Fields match
  case EmptyTuple       => 0
  case (h *: t) *: tail => TupleSize[h *: t] + TupleSize[tail] // Nested tuple
  case head *: tail     => PrimitiveSize[head] + TupleSize[tail] // Primitive

/** Size of any field (primitive or nested tuple) */
type FieldSize[T] <: Int = T match
  case F32    => 4
  case F64    => 8
  case U8     => 1
  case U16    => 2
  case U32    => 4
  case I8     => 1
  case I16    => 2
  case I32    => 4
  case h *: t => TupleSize[h *: t]

/** Offset of field at index N within a tuple */
type FieldOffset[Fields <: Tuple, N <: Int] <: Int = N match
  case 0                               => 0
  case scala.compiletime.ops.int.S[n1] =>
    Fields match
      case (h *: t) *: tail =>
        TupleSize[h *: t] + FieldOffset[tail, n1] // Nested tuple head
      case head *: tail =>
        PrimitiveSize[head] + FieldOffset[tail, n1] // Primitive head

/** Value type mapping for get/set */
type ValueType[T] = T match
  case F32 => Float
  case F64 => Double
  case U8  => Short
  case U16 => Int
  case U32 => Double
  case I8  => Byte
  case I16 => Short
  case I32 => Int

/** Convert schema tuple to value tuple - recursively handles nested tuples */
type ValueTuple[Fields <: Tuple] <: Tuple = Fields match
  case EmptyTuple       => EmptyTuple
  case (h *: t) *: tail =>
    ValueTuple[h *: t] *: ValueTuple[tail] // Nested tuple
  case head *: tail => ValueType[head] *: ValueTuple[tail] // Primitive

/** Return type for field access - StructRef for tuples, FieldRef for primitives
  */
type FieldAccess[T] = T match
  case h *: t => StructRef[T & Tuple]
  case _      => PrimitiveRef[T]

// =============================================================================
// Shared Primitive Setter - ZERO COST: inline helper for setting primitives
// =============================================================================

/** Shared helper: Set a primitive value at an offset (used by both PrimitiveRef
  * and StructRef)
  *
  * Note: The implementation attempts to minimize Scala.js runtime type checks
  * by treating values generically. However, when called from tuple
  * destructuring (productElement returns Any), Scala.js may still insert
  * validation calls like $uF for Float. This is acceptable as the overhead is
  * minimal compared to the ergonomic benefit of bulk tuple setting. For
  * performance-critical code, use field-by-field assignment which generates
  * direct DataView calls.
  */
private inline def getPrimitiveValue[T](
    view: DataView,
    offset: Int,
): ValueType[T] =
  inline erasedValue[T] match
    case _: F32 => view.getFloat32(offset, littleEndian = true)
    case _: F64 => view.getFloat64(offset, littleEndian = true)
    case _: U8  => view.getUint8(offset)
    case _: U16 => view.getUint16(offset, littleEndian = true)
    case _: U32 => view.getUint32(offset, littleEndian = true)
    case _: I8  => view.getInt8(offset)
    case _: I16 => view.getInt16(offset, littleEndian = true)
    case _: I32 => view.getInt32(offset, littleEndian = true)

private inline def setPrimitiveValue[T](
    view: DataView,
    offset: Int,
    value: Any,
): Unit =
  inline erasedValue[T] match
    case _: F32 =>
      view.setFloat32(offset, value.asInstanceOf[Float], littleEndian = true)
    case _: F64 =>
      view.setFloat64(offset, value.asInstanceOf[Double], littleEndian = true)
    case _: U8  => view.setUint8(offset, value.asInstanceOf[Short])
    case _: U16 =>
      view.setUint16(offset, value.asInstanceOf[Int], littleEndian = true)
    case _: U32 =>
      view.setUint32(offset, value.asInstanceOf[Double], littleEndian = true)
    case _: I8  => view.setInt8(offset, value.asInstanceOf[Byte])
    case _: I16 =>
      view.setInt16(offset, value.asInstanceOf[Short], littleEndian = true)
    case _: I32 =>
      view.setInt32(offset, value.asInstanceOf[Int], littleEndian = true)

// =============================================================================
// Typed StructArray - ZERO COST: just (DataView, count), layout is phantom type
// =============================================================================

/** Typed struct array with zero-cost abstractions and implicit Iterable
  * conversion.
  *
  * StructArray provides compile-time type-safe access to arrays of structs
  * stored in contiguous ArrayBuffer memory. All type information and offsets
  * are computed at compile time using match types.
  *
  * ## Iteration Patterns
  *
  * StructArray supports multiple iteration approaches with different
  * performance characteristics. Choose based on your use case:
  *
  * ### Performance-critical hot loops (zero overhead):
  *
  * Use direct indexing with indices range for maximum performance:
  * {{{
  * // Direct indexing with indices range - fully optimized
  * for i <- particles.indices do
  *   particles(i)(0) := i.toFloat
  *
  * // Manual indexing (most control)
  * var i = 0
  * while i < particles.length do
  *   particles(i)(0) := i.toFloat
  *   i += 1
  * }}}
  *
  * ### Generic code / for-comprehensions (5-10% overhead):
  *
  * StructArray implicitly converts to Iterable, enabling all standard
  * collection operations via for-comprehensions:
  * {{{
  * // For-comprehension with element access
  * for p <- particles do
  *   p(0) := 100.0f
  *
  * // For-comprehension with guards (uses withFilter)
  * for p <- particles if p(2).get > 0 do
  *   update(p)
  *
  * // For-yield to collect results (uses map)
  * val positions = for p <- particles yield
  *   (p(0).get, p(1).get)
  *
  * // All standard collection methods work
  * particles.foreach(p => println(p(0).get))
  * particles.filter(p => p(1).get > 0).toList
  * particles.map(p => p(0).get * 2).sum
  *
  * // Passing to functions expecting Iterable[T]
  * trait Hittable { def hit(ray: Ray): Option[Hit] }
  * class HittableList(items: Iterable[Hittable]) { ... }
  *
  * val spheres: StructArray[SphereFields] = ...
  * val hittables = new HittableList(spheres)  // works via implicit conversion!
  * }}}
  *
  * ### Performance characteristics:
  *   - `particles(i)`: Zero-cost, compile-time offset calculation
  *   - `particles.indices`: Zero-cost Range allocation
  *   - `for p <- particles`: Iterator-based, 5-10% overhead (implicit
  *     conversion)
  *
  * ## Recommended Usage:
  *   - Hot loops: Direct indexing with `indices`
  *   - Generic APIs: Use for-comprehensions (implicit Iterable conversion)
  *   - Interop with Scala collections: Pass directly to `Iterable[T]`
  *     parameters
  */
opaque type StructArray[Fields <: Tuple] = BufferView // dv = view, off = count

object StructArray:
  /** Allocate a new struct array with compile-time known stride */
  inline def allocate[Fields <: Tuple](count: Int): StructArray[Fields] =
    val stride = constValue[TupleSize[Fields]]
    val buffer = new ArrayBuffer(stride * count)
    BufferView(new DataView(buffer), count)

  /** Wrap an existing ArrayBuffer */
  inline def wrap[Fields <: Tuple](buffer: ArrayBuffer): StructArray[Fields] =
    val stride = constValue[TupleSize[Fields]]
    val count = if stride == 0 then 0 else buffer.byteLength / stride
    BufferView(new DataView(buffer), count)

  extension [Fields <: Tuple](arr: StructArray[Fields])
    inline def length: Int = arr.off
    inline def stride: Int = constValue[TupleSize[Fields]]
    inline def dataView: DataView = arr.dv
    inline def arrayBuffer: ArrayBuffer =
      arr.dv.buffer.asInstanceOf[ArrayBuffer]

    /** Access element at index - no bounds check, relies on DataView for errors
      */
    inline def apply(index: Int): StructRef[Fields] =
      StructRef[Fields](arr.dv, index * constValue[TupleSize[Fields]])

    /** Returns a Range of valid indices for this array. Enables idiomatic
      * index-based iteration for maximum performance.
      *
      * Performance: Zero-cost - Range is lazy and efficient.
      *
      * Example:
      * {{{
      * for i <- particles.indices do
      *   particles(i)(0) := i.toFloat
      * }}}
      */
    inline def indices: Range = 0 until arr.length

  /** Iterator implementation for StructArray - enables for-comprehensions and
    * Iterable interop. When stride is not provided (or -1), it's computed at
    * runtime from buffer size.
    */
  final class StructArrayIterator[Fields <: Tuple](
      arr: StructArray[Fields],
      stride: Int = -1,
  ) extends Iterator[StructRef[Fields]]:
    private var index = 0
    private val len = arr.off
    private val view = arr.dv
    private val actualStride =
      if stride >= 0 then stride
      else if len == 0 then 0
      else view.buffer.byteLength / len

    inline def hasNext: Boolean = index < len

    inline def next(): StructRef[Fields] =
      val current = index
      index += 1
      StructRef[Fields](view, current * actualStride)

  /** Implicit conversion to Iterable enables all collection operations (map,
    * flatMap, withFilter, etc.) without explicit method definitions. This
    * allows StructArray to work seamlessly with for-comprehensions and be
    * passed to functions expecting Iterable[T].
    *
    * The conversion wraps the array in an Iterable. The iterator computes
    * stride at runtime, adding minimal overhead.
    */
  given [Fields <: Tuple]
      => Conversion[StructArray[Fields], Iterable[StructRef[Fields]]]:
    def apply(arr: StructArray[Fields]): Iterable[StructRef[Fields]] =
      new Iterable[StructRef[Fields]]:
        def iterator: Iterator[StructRef[Fields]] =
          new StructArrayIterator(arr)

// =============================================================================
// struct[] factory - convenience wrapper
// =============================================================================

/** Phantom layout type for API convenience */
final class StructLayout[Fields <: Tuple]:
  inline def sizeInBytes: Int = constValue[TupleSize[Fields]]
  inline def allocate(count: Int): StructArray[Fields] =
    StructArray.allocate[Fields](count)
  inline def apply(): StructRef[Fields] = allocate(1)(0)
  inline def fromBuffer(
      buffer: ArrayBuffer,
      offset: Int = 0,
  ): StructRef[Fields] =
    StructRef[Fields](new DataView(buffer), offset)

/** Create a phantom layout - zero runtime cost */
inline def struct[Fields <: Tuple]: StructLayout[Fields] =
  new StructLayout[Fields]

// =============================================================================
// Typed StructRef - ZERO COST: just (DataView, offset), layout is phantom type
// =============================================================================

opaque type StructRef[Fields <: Tuple] = BufferView

object StructRef:
  inline def apply[Fields <: Tuple](
      view: DataView,
      offset: Int,
  ): StructRef[Fields] =
    BufferView(view, offset)

  /** Recursively set all fields from a value tuple */
  private inline def setFields[Fields <: Tuple](
      view: DataView,
      baseOffset: Int,
      values: Any,
      valueIndex: Int,
  ): Unit =
    inline erasedValue[Fields] match
      case _: EmptyTuple => () // Base case: no more fields

      case _: ((h *: t) *: tail) =>
        // Nested tuple - set the nested struct at current valueIndex
        val nestedOffset = constValue[FieldOffset[Fields, 0]]
        val nestedValues =
          values.asInstanceOf[Product].productElement(valueIndex)
        setFields[h *: t](view, baseOffset + nestedOffset, nestedValues, 0)
        // Process tail fields - offset advances by size of first field
        val tailOffset = baseOffset + constValue[FieldSize[h *: t]]
        setFields[tail](view, tailOffset, values, valueIndex + 1)

      case _: (head *: tail) =>
        // Primitive field - set it at current valueIndex
        val fieldOffset = constValue[FieldOffset[Fields, 0]]
        val value = values.asInstanceOf[Product].productElement(valueIndex)
        setPrimitiveValue[head](view, baseOffset + fieldOffset, value)
        // Process tail fields - offset advances by size of first field
        val tailOffset = baseOffset + constValue[FieldSize[head]]
        setFields[tail](view, tailOffset, values, valueIndex + 1)

  extension [Fields <: Tuple](s: StructRef[Fields])
    inline def dataView: DataView = s.dv
    inline def offset: Int = s.off

    /** Field access by index - compile-time bounds check via Tuple.Elem.
      * Returns StructRef for nested tuples, PrimitiveRef for primitives. Index
      * must be a literal constant; out-of-bounds causes compile error.
      */
    transparent inline def apply(
        n: Int,
    ): FieldAccess[Tuple.Elem[Fields, n.type]] =
      inline erasedValue[Tuple.Elem[Fields, n.type]] match
        case _: (h *: t) =>
          StructRef[Tuple.Elem[Fields, n.type] & Tuple](
            s.dv,
            s.off + constValue[FieldOffset[Fields, n.type]],
          )
        case _ =>
          PrimitiveRef[Tuple.Elem[Fields, n.type]](
            s.dv,
            s.off + constValue[FieldOffset[Fields, n.type]],
          )

    /** Zero-alloc field get - reads primitive field N directly from the
      * DataView without creating an intermediate PrimitiveRef. Use instead of
      * apply(n)() in hot paths.
      */
    inline def getAt(
        n: Int,
    ): ValueType[Tuple.Elem[Fields, n.type]] =
      getPrimitiveValue[Tuple.Elem[Fields, n.type]](
        s.dv,
        s.off + constValue[FieldOffset[Fields, n.type]],
      )

    /** Zero-alloc field set - writes primitive field N directly to the DataView
      * without creating an intermediate PrimitiveRef. Use instead of
      * apply(n)(value) in hot paths.
      */
    inline def setAt(
        n: Int,
    )(value: ValueType[Tuple.Elem[Fields, n.type]]): Unit =
      setPrimitiveValue[Tuple.Elem[Fields, n.type]](
        s.dv,
        s.off + constValue[FieldOffset[Fields, n.type]],
        value,
      )

    /** Zero-alloc nested struct field set - writes a tuple value to a nested
      * tuple field N directly via setFields, without creating intermediate
      * StructRef or PrimitiveRef objects. Use instead of apply(n) := values for
      * nested struct fields.
      */
    inline def setTupleAt(
        n: Int,
    )(values: ValueTuple[Tuple.Elem[Fields, n.type] & Tuple]): Unit =
      setFields[Tuple.Elem[Fields, n.type] & Tuple](
        s.dv,
        s.off + constValue[FieldOffset[Fields, n.type]],
        values,
        0,
      )

    // --- Numbered aliases for setTupleAt ---
    inline def set0(values: ValueTuple[Tuple.Elem[Fields, 0] & Tuple]): Unit =
      setTupleAt(0)(values)
    inline def set1(values: ValueTuple[Tuple.Elem[Fields, 1] & Tuple]): Unit =
      setTupleAt(1)(values)
    inline def set2(values: ValueTuple[Tuple.Elem[Fields, 2] & Tuple]): Unit =
      setTupleAt(2)(values)
    inline def set3(values: ValueTuple[Tuple.Elem[Fields, 3] & Tuple]): Unit =
      setTupleAt(3)(values)
    inline def set4(values: ValueTuple[Tuple.Elem[Fields, 4] & Tuple]): Unit =
      setTupleAt(4)(values)
    inline def set5(values: ValueTuple[Tuple.Elem[Fields, 5] & Tuple]): Unit =
      setTupleAt(5)(values)
    inline def set6(values: ValueTuple[Tuple.Elem[Fields, 6] & Tuple]): Unit =
      setTupleAt(6)(values)
    inline def set7(values: ValueTuple[Tuple.Elem[Fields, 7] & Tuple]): Unit =
      setTupleAt(7)(values)
    inline def set8(values: ValueTuple[Tuple.Elem[Fields, 8] & Tuple]): Unit =
      setTupleAt(8)(values)

    /** Copy data from another struct of the same layout */
    inline def copyFrom(other: StructRef[Fields]): Unit =
      val stride = constValue[TupleSize[Fields]]
      val src = new Uint8Array(other.dv.buffer, other.off, stride)
      val dst = new Uint8Array(s.dv.buffer, s.off, stride)
      dst.set(src)

    /** Extract a slice buffer containing just this struct's bytes */
    inline def sliceBuffer: ArrayBuffer =
      val stride = constValue[TupleSize[Fields]]
      s.dv.buffer.slice(s.off, s.off + stride)

    /** Set all fields at once from a value tuple.
      *
      * Enables bulk setting of struct fields with clean syntax:
      * {{{
      * type Vec2 = (F32, F32)
      * val v = struct[Vec2]()
      * v.set((10.0f, 20.0f))  // Set both x and y at once
      * }}}
      *
      * Works with nested structs:
      * {{{
      * type Particle = (Vec2, U8)
      * val p = struct[Particle]()
      * p.set(((1.0f, 2.0f), 100: Short))  // Set position and life
      * }}}
      *
      * Performance note: Due to Scala.js tuple handling, this generates
      * slightly more JavaScript code than field-by-field assignment:
      *   - Extra intermediate variable assignments for tuple destructuring
      *   - Runtime type validation ($uF for Float, $uD for Double, etc.)
      *
      * The overhead is minimal (no allocations, just variable ops + type
      * checks) and acceptable for the ergonomic benefit. For
      * performance-critical tight loops, consider field-by-field assignment:
      * {{{
      * v(0) := 10.0f  // Direct field access, generates minimal JS
      * v(1) := 20.0f
      * }}}
      */
    inline def set(values: ValueTuple[Fields]): Unit =
      setFields[Fields](s.dv, s.off, values, 0)

    /** Assignment operator alias for set. Provides cleaner syntax with fewer
      * parentheses:
      * {{{
      * v := (10.0f, 20.0f)      // Using := operator
      * v.set((10.0f, 20.0f))    // Equivalent explicit call
      * }}}
      *
      * See [[set]] for details and performance characteristics.
      */
    inline def :=(values: ValueTuple[Fields]): Unit = set(values)

    /** Apply with value alias for set - allows p.pos.x(100.0f) syntax */
    inline def apply(value: ValueTuple[Fields]): Unit = set(value)

// =============================================================================
// Typed PrimitiveRef - ZERO COST reference to a primitive leaf field
// =============================================================================

opaque type PrimitiveRef[T] = BufferView

object PrimitiveRef:
  inline def apply[T](view: DataView, offset: Int): PrimitiveRef[T] =
    BufferView(view, offset)

  extension [T](f: PrimitiveRef[T])
    inline def view: DataView = f.dv
    inline def offset: Int = f.off

    /** Get the value - type is determined by T */
    inline def get: ValueType[T] =
      inline erasedValue[T] match
        case _: F32 => f.dv.getFloat32(f.off, littleEndian = true)
        case _: F64 => f.dv.getFloat64(f.off, littleEndian = true)
        case _: U8  => f.dv.getUint8(f.off)
        case _: U16 => f.dv.getUint16(f.off, littleEndian = true)
        case _: U32 => f.dv.getUint32(f.off, littleEndian = true)
        case _: I8  => f.dv.getInt8(f.off)
        case _: I16 => f.dv.getInt16(f.off, littleEndian = true)
        case _: I32 => f.dv.getInt32(f.off, littleEndian = true)

    /** Set the value - type is determined by T */
    inline def set(value: ValueType[T]): Unit =
      setPrimitiveValue[T](f.dv, f.off, value)

    /** Assignment operator alias for set */
    inline def :=(value: ValueType[T]): Unit = set(value)

    /** Apply alias for get - allows p.pos.x() syntax */
    inline def apply(): ValueType[T] = get

    /** Apply with value alias for set - allows p.pos.x(100.0f) syntax */
    inline def apply(value: ValueType[T]): Unit = set(value)
