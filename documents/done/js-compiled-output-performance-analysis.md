# Compiled JS Performance Analysis: Buffer Access & Render Loop

Analysis of the compiled JS output from `painter_triangle` and
`painter_typed_bindings` drafts, focusing on GC pressure, per-frame allocations,
and buffer update overhead in the render loop. Both fastLink (readable) and
fullLink (optimized) outputs were examined.

---

## Part A: Buffer Data Utilities — Not Zero-Cost

### A1: `(DataView, Int)` Tuple Representation Overhead -- DONE

**Resolution:** Replaced `opaque type StructRef[F] = (DataView, Int)` with
`opaque type StructRef[F] = BufferView` where
`class BufferView(val dv: DataView, var off: Int) extends js.Object`. All opaque
types (`StructRef`, `PrimitiveRef`, `StructArray`, `F32View`–`I32View`) now use
`BufferView` as the underlying representation. All internal access changed from
`._1`/`._2` to `.dv`/`.off`. Result: direct property access in compiled JS
(`ref.dv`, `ref.off`) instead of virtual method dispatch (`ref.g()`, `ref.n()`).

Additionally, added zero-alloc `getAt`/`setAt` methods on `StructRef` that
bypass `PrimitiveRef` entirely, and `setTupleAt`/`set0`–`set8` methods for
zero-alloc nested struct field writes that bypass both `StructRef.apply(n)` and
`PrimitiveRef`. All math type buffer givens (Vec2–4, Mat2–4) updated to use
`getAt`/`setAt`.

**Before (Tuple2 virtual dispatch):**

```js
var offset = ref.n() | 0; // method call for _2 + bitwise OR for int coerce
var _1 = ref.g(); // method call for _1
_1.setFloat32(offset, v, true);
```

**After (direct property access via BufferView):**

```js
ref.dv.setFloat32(ref.off, v, true); // direct property access, zero alloc
```

**Before (PrimitiveRef intermediate allocations for Vec3 write):**

```js
// Each field creates a new BufferView for PrimitiveRef:
var f1 = new BufferView(ref.dv, ref.off); // PrimitiveRef for x
f1.dv.setFloat32(f1.off, v1, true);
var f2 = new BufferView(ref.dv, ref.off + 4); // PrimitiveRef for y
f2.dv.setFloat32(f2.off, v2, true);
```

**After (getAt/setAt bypass PrimitiveRef):**

```js
ref.dv.setFloat32(ref.off, v1, true); // zero alloc
ref.dv.setFloat32((4 + ref.off) | 0, v2, true); // zero alloc
ref.dv.setFloat32((8 + ref.off) | 0, v3, true); // zero alloc
```

**Before (nested struct write via `vertices(0)(0) := (x, y)`):**

```js
var s = new BufferView(vertices.dv, 0); // StructArray.apply(0)
var v = new BufferView(s.dv, s.off); // StructRef.apply(0) nested
var f1 = new BufferView(v.dv, v.off); // PrimitiveRef for F32[0]
f1.dv.setFloat32(f1.off, 0.0, true);
var f2 = new BufferView(v.dv, v.off + 4); // PrimitiveRef for F32[1]
f2.dv.setFloat32(f2.off, 0.3, true);
// 4 BufferView allocations per vertex write
```

**After (`vertices(0).set0(x, y)` via setTupleAt):**

```js
var s = new BufferView(vertices.dv, 0); // StructArray.apply(0) — only alloc
s.dv.setFloat32(s.off, 0.0, true); // direct DataView write
s.dv.setFloat32((4 + s.off) | 0, 0.3, true); // direct DataView write
// 1 BufferView allocation per vertex write
```

**Files changed:** `trivalibs/src/utils/bufferdata.scala`,
`src/gpu/math/vec2.scala`, `src/gpu/math/vec3.scala`, `src/gpu/math/vec4.scala`,
`src/gpu/math/mat2.scala`, `src/gpu/math/mat3.scala`, `src/gpu/math/mat4.scala`

---

### A2: `given Conversion[Double, Float]` Runtime Dispatch -- DONE

**Resolution:** Made all `given Conversion` instances `inline` in both
`trivalibs/src/utils/numbers.scala` and `src/gpu/math/package.scala`. The
`Double->Float` conversion now uses `inline def apply` in an anonymous class,
and the identity/widening conversions use `inline given`. Result: the
`$m_...().f().e()` call chain is completely eliminated -- all conversions now
compile to direct `Math.fround()` (for Double->Float) or nothing (for identity).
The `trivalibs.utils.numbers` import is no longer emitted in files that only
used the conversion.

---

### A3: `Fractional` Type Class Boxing on Mat2Buffer -- DONE

**Resolution:** Replaced `Fractional` with a custom `NumOps` type class
(`trivalibs/src/utils/numbers.scala`) that uses `inline` extension methods for
arithmetic. All vec2-4 and mat2-4 traits now use `Num: NumOps` instead of
`Num: Fractional`. The compiled JS no longer contains any `FractionalOps`
allocations — arithmetic compiles to direct primitive operations.

---

### A4: `UniformValue.write` Virtual Dispatch

**What happens:** `trait UniformValue[T, F]` declares `write` as a trait method.
Even with `inline def write(...)` in implementations, once stored in
`BufferBinding.uv: UniformValue[T, F]`, calls to `uv.write(...)` are virtual
dispatch.

**Assessment:** This is **one virtual dispatch per `set()` call** -- acceptable
overhead. The real cost was inside the `write` body (Problems A1-A3), which are
now all resolved.

---

### A5: Per-Field Overhead Summary (Before vs After)

| Operation                      | Before (fullLink)                 | After                    | Source     |
| ------------------------------ | --------------------------------- | ------------------------ | ---------- |
| Get base offset from StructRef | 2 (`.n()` method + `\| 0`)        | 1 (`.off` property read) | A1 -- DONE |
| Get DataView from StructRef    | 1 (`.g()` method)                 | 1 (`.dv` property read)  | A1 -- DONE |
| Convert Double->Float          | 3 (module + `.f()` + `.e()`)      | 1 (`Math.fround`)        | A2 -- DONE |
| Float multiply via Fractional  | 2 (`new FractionalOps` + `.ab()`) | 1 (`*`)                  | A3 -- DONE |
| PrimitiveRef per field access  | 1 `new BufferView` per field      | 0 (getAt/setAt bypass)   | A1 -- DONE |
| Nested struct field write      | 3-4 `new BufferView` per write    | 1 (setTupleAt/set0–set8) | A1 -- DONE |

**Vec3 UniformValue write:** Was 6 method calls + 3 coercions -> Now 3 direct
`setFloat32` calls with zero intermediate allocations.

**Vertex data setup (per vertex):** Was 4 `new BufferView` allocs -> Now 1
(unavoidable StructArray index).

---

## Part B: Render Loop Allocations

### B1: GPUBindGroup Recreated Every Frame -- HIGHEST IMPACT

**Location:** `src/gpu/painter/painter.scala:201-219` -- `createBindGroup()`
called unconditionally in `draw()`

**Per-frame allocations:**

- 1 `Arr[js.Dynamic]()` for entries
- Nx2 `Obj.literal` for binding entries (binding + resource per slot)
- 1 `Obj.literal` for bind group descriptor
- 1 `device.createBindGroup(...)` -- **expensive GPU driver call**

**Fix:** Cache the `GPUBindGroup` on `Shape`. Invalidate when `bind()` is
called.

```scala
// In Shape:
var cachedBindGroup: GPUBindGroup | Null = null

// In bind/processEntry: cachedBindGroup = null
// In Painter.draw: reuse if non-null, else create and cache
```

**Files:** `src/gpu/painter/shape.scala`, `src/gpu/painter/painter.scala`

---

### B2: Pipeline Cache Key String Every Frame

**Location:** `src/gpu/painter/painter.scala:153-156`

```scala
s"${s.id}|${shape.blendState.isEmpty}|${shape.cullMode}|${f.topology}|${f.frontFace}|${format}"
```

Creates a new String every frame via `StringOps` interpolation. Only used for
`js.Dictionary` cache lookup.

**Fix:** Pre-compute and store on `Shape` at construction. All values are set at
construction time.

**Files:** `src/gpu/painter/shape.scala`, `src/gpu/painter/painter.scala`

---

### B3: Descriptor Objects Every Frame

**Location:** `src/gpu/painter/painter.scala:102-147` -- `draw()`

Per-frame allocations:

- 1-2 `Obj.literal` for `colorAttachment` (+ clearValue)
- 1 `Arr(colorAttachment)` for render pass
- 1 `Obj.literal` for render pass descriptor
- 1 `Arr(encoder.finish())` for submit

**Assessment:** These are small, short-lived JS objects. V8 nursery GC handles
them efficiently. Lower priority than B1/B2.

**Fix options:** Pre-allocate and mutate descriptor objects (update only `view`
per frame). Or accept the overhead.

---

### B4: BindPair + Vec/Mat Objects per bind Call

Every `"rotation" := Mat2.fromRotation(t)` in animate creates:

- 1 `BindPair` object (small, short-lived)
- 1 `Mat2` object (from `Mat2.fromRotation`)

**Assessment:** Acceptable overhead. Users who need maximum performance can use
`BufferBinding.set`/`update` directly.

---

### B5: `Tuple4` for clearColor

Every `painter.draw(shape, clearColor = (0.1, 0.1, 0.1, 1.0))` allocates
`new Tuple4`.

**Fix:** Users can pre-allocate as a `val` outside the loop. Or change parameter
type.

---

### B6: Per-Frame Allocation Summary

| Allocation                     | Count/frame | Avoidable?                | Priority |
| ------------------------------ | ----------- | ------------------------- | -------- |
| **GPUBindGroup + descriptors** | **1 + 2xN** | **Yes -- cache on Shape** | **HIGH** |
| Pipeline key string            | 1           | Yes -- pre-compute        | Medium   |
| Obj.literal descriptors        | 3-4         | Partially -- pre-allocate | Low      |
| js.Array for descriptors       | 2           | Partially                 | Low      |
| GPUCommandEncoder/Buffer       | 2           | No (WebGPU required)      | --       |
| GPUTextureView                 | 1           | No (WebGPU required)      | --       |
| BindPair objects               | per bind    | User-side                 | Low      |
| Tuple4 clearColor              | 1           | User-side                 | Low      |

---

## Recommended Priority (remaining)

1. **B1: BindGroup caching** -- eliminates expensive GPU driver call + N
   descriptor objects per frame
2. **B2: Pipeline key caching** -- eliminates string allocation per frame

All Part A items (A1, A2, A3) are now resolved.
