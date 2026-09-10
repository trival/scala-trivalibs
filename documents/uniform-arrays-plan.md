# Uniform Arrays — `UniformArray[T, N]`

Status: **proposed, awaiting approval**. Nothing implemented yet.

## Where this was planned before

There is **no trivalibs-side plan** for this today. It appears three times in the
sketch repo, twice as a deferral and once as a non-decision:

- **`sketches/rooms/canvases/PLAN.md:209-217`** — the first appearance. Planned
  as `UniformArray[Vec4, N]` to pass N shadow rects in one binding, then
  **dropped** when instanced blending handled arbitrary N. It records the
  research result: _"a `sealed trait UniformArray[T, N]` with a `WGSLType`
  emitting `array<vec4, N>`, DSL indexing, an N-row `StructArray` buffer, and a
  `binding` overload — ~4 small additive changes in `trivalibs/src`"_. That
  estimate still holds, and this document is that research turned into a plan.
- **`documents/grid-ceiling-rooms-plan.md:1827-1851`** (Part 5 §2) — declined
  again. The candidate there was `edgeSetDist` passing ring edges as an array,
  and the reasoning against it is specific to that case and still correct:
  the footprint is fixed at build time, so unrolling constant-folds every term,
  and it runs in a bake. The stated **revival condition** is data that has to
  change **at runtime**.
- **`documents/room-templates-implementation.md:1016`** and
  **`src/utils/room/Fields.scala:14`** — carry the same deferral forward.

The gradient sketch meets the revival condition exactly: the data varies per
draw, the shade is shared, and nothing about it is known when the shader is
built. So this is not a reversal of the earlier decisions — the rooms work
should keep unrolling its build-time constants either way.

---

## Driving use case

A `sketches/textures/` sketch rendering multi-step gradients: several gradients
side by side, differing in **number of steps** and in the **interpolation used
between each pair of steps**, all drawn by **one shade** with per-draw uniform
data.

### Is an array uniform the right representation?

Checked against the three alternatives before adding anything:

**1. Pack into `Mat4` — no new feature at all.** A `mat4x4<f32>` is four vec4s;
two of them give 4 stops plus 4 curve descriptors with zero library work. This
is the "does the collection collapse?" check that
`grid-ceiling-rooms-plan.md` Part 5 §4 asks for. It **doesn't collapse**: the
cap of 4 is too low for the sketch's point (a 6- or 8-step gradient is exactly
what we want to look at), the DSL has no column indexing on `Mat4Expr` so that
would need adding anyway, and `stops[2].w` reading as "column 2's w component of
a matrix" is the kind of encoding that gets miscounted while tuning.

**2. A 1D LUT texture.** Bake each gradient once into a 256×1 panel and sample
it. Genuinely good when a gradient is _consumed_ many times per frame by an
expensive shader — the evaluation cost collapses to one fetch. It is the wrong
tool **here**: it needs one panel plus one bake per gradient (which is what
`sketches/gradients/` already does with one shade per gradient), hardware
filtering softens hard steps, and it makes live tuning a re-bake rather than a
buffer write. Worth revisiting if a later sketch samples one gradient in a hot
loop. It also depends on `Panel.with_static_texture_data`
(`documents/independent-todos.md`) to be CPU-authored rather than shader-baked.

**3. Storage buffers.** Still no — same reasoning as Part 5 §3 of the
grid-ceiling plan. Nothing here is variable-length or large; a fixed capacity
of 8–16 vec4s is the whole requirement.

So: **array uniforms, and the gradient is a good first consumer** — small,
runtime-varying, per-draw data, with a fixed compile-time capacity.

### The gradient's own representation

Recommended shape (this is a sketch-level decision, recorded here because it
drives the capability requirement):

```scala
type MaxStops = 8

// xyz = color, w = position in [0,1]
stops:  UniformArray[Vec4, MaxStops]
// x = curve mode id, y/z = curve params; entry i governs the segment i → i+1
curves: UniformArray[Vec4, MaxStops]
count:  Double // active stop count
```

Evaluated **branch-free and unrolled**, because `MaxStops` is a compile-time
literal — the unrolling is a Scala `for` in the shader builder, so **no WGSL
loop construct is needed** and none is proposed here:

```
col = stops[0].rgb
for i in 1 until MaxStops:
  t   = clamp01((x - stops[i-1].w) / (stops[i].w - stops[i-1].w))
  col = mix(col, stops[i].rgb, ease(curves[i-1], t) * active(i))
```

Each successive `mix` fully overrides once its local `t` reaches 1, so the
chain reproduces the piecewise result exactly, and holds the last color past
the final stop with no clamping special case. `active(i)` is
`step(f32(i), count - 1)`, which zeroes unused stops without a branch.

**A per-panel stop count is not variable-length data.** Panels with 2, 5 and 8
stops all run through one shade with capacity 8: `count` varies per draw,
`active(i)` masks the rest. What a fixed capacity costs is _wasted work_, not
correctness — a 2-stop gradient still evaluates 8 mixes. In a bake that is
free; full-screen it is still nothing. It starts to matter somewhere north of
~64, which is where a loop over `count` earns its place — see _Follow-ups_.

`ease(curve, t)` has two possible forms, and this is **the one open sketch
question**:

- **(a) mode id + select chain** — a small fixed set (linear, smoothstep, pow
  in/out, step, sine) selected by `curves[i].x`. Every mode is evaluated and
  `select`ed, so cost is `MaxStops × modes` cheap ops per fragment. Free in a
  bake, still trivial full-screen. **Recommended** — it is what makes the
  interpolations directly comparable, which is the sketch's whole subject.
- **(b) a parametric family** — e.g. bias/gain, two floats covering linear,
  ease-in, ease-out, ease-in-out continuously. No branching at all, but you
  cannot put `step` or a sine lobe in it.

(a) does not constrain the library feature in any way; both need the same
`array<vec4, N>`.

---

## The library feature

`UniformArray[T, N]` — a **fixed-capacity, uniform-address-space array**, usable
anywhere a uniform value is today: in a shade's uniform schema, as a
`p.binding[…]`, bound on a shape, a panel or a layer.

```scala
// schema
val shade = p.layerShade[(
  stops:  FragmentUniform[UniformArray[Vec4, 8]],
  count:  FragmentUniform[Double],
)]: program => …

// shader body — indexing, constant or dynamic
ctx.bindings.stops(0).rgb
ctx.bindings.stops(i)          // i: IntExpr

// CPU side
val stops = p.binding[UniformArray[Vec4, 8]]
stops := UniformArray[Vec4, 8](Arr(vec4(…), vec4(…), …))
shape.bind("stops" := stops)
```

Emits `@group(0) @binding(k) var<uniform> stops: array<vec4<f32>, 8>;`.

### Why it is this small

The existing machinery is already generic over "a `WGSLType` with a
`UniformValue`". Walking the pipeline, nothing on the path is per-type:

- `derive.generateUniforms` (`shader/derive.scala:154-197`) emits
  `var<uniform> $name: ${WGSLType[T].wgslName}` — an array name drops straight
  in.
- `layouts.bindGroupEntriesImpl` (`shader/layouts.scala:120-150`) emits
  `buffer = {type: "uniform"}` for everything that isn't a sampler — correct for
  arrays as-is.
- `derive.checkUniformFieldType` (`shader/derive.scala:274-330`) accepts
  `V =:= Expected` or `V <:< BufferBinding[Expected, ?]`. If the CPU value type
  **is** `UniformArray[Vec4, 8]`, both rules fire unchanged, so a wrong element
  type _or a wrong capacity_ is a compile error for free.
- `Painter.bindingEntry` (`painter/painter.scala:1875-1886`), the
  `PanelBindingValue` union (`painter/panel.scala:47`), `Shape.processEntry`
  (`painter/shape.scala:233-257`) and `Panel.processPanelEntry`
  (`painter/panel.scala:280-304`) all key off `BufferBinding[?, ?]`. **If the
  array reuses `BufferBinding`, none of them is touched.**

The one thing `BufferBinding` cannot do today is allocate more than one row:
`BufferBinding.apply` does `StructArray.allocate[F](1)(0)`
(`buffers/binding.scala:150`). `StructArray` is already N-row capable, and
`gpuBuffer`'s size and `upload()` both read the whole `DataView` / `ArrayBuffer`,
so they follow automatically. That makes the row count the single hook needed.

### Changes

**1. `UniformValue` gains a row count** — `buffers/binding.scala`

```scala
trait UniformValue[T, F <: Tuple]:
  def write(ref: StructRef[F], value: T): Unit
  def read(ref: StructRef[F]): T
  /** Rows of `F` this value occupies. 1 for every scalar value; N for a
    * UniformArray. */
  def rows: Int = 1
```

All existing instances inherit the default. `BufferBinding.apply` becomes
`StructArray.allocate[F](uv.rows)(0)`. That is the entire change to the binding
machinery — one word plus a defaulted member.

**2. `UniformArray[T, N]` + its element restriction** — new file
`src/graphics/buffers/uniform_array.scala`

```scala
opaque type UniformArray[T, N <: Int] = Arr[T]

object UniformArray:
  inline def apply[T: UniformArrayElem, N <: Int](values: Arr[T]): UniformArray[T, N] = values
  extension [T, N <: Int](a: UniformArray[T, N])
    inline def values: Arr[T] = a
```

`UniformArrayElem[T]` is a marker typeclass with instances **only** for element
types WGSL permits in the uniform address space (see below). A `Float` or `Vec2`
array is then a **compile error at the declaration site** with a message
pointing at vec4 packing, rather than a WGSL validation failure at shade
creation.

The `UniformValue` instance writes each element through the element's own
`UniformValue`, at `ref.offset + i * TupleSize[F]` — `StructRef` already exposes
`dataView` and `offset` (`utils/bufferdata.scala:462-464`), so this needs no
change to `bufferdata`. Elements past `values.length` are left zeroed;
`values.length > N` is a runtime throw (`jsError`, per the no-Scala-exceptions
rule).

**3. `WGSLType[UniformArray[T, N]]`** — `shader/types.scala`, beside the other
instances

```scala
given [T: UniformArrayElem, N <: Int] => (inner: WGSLType[T]) => WGSLType[UniformArray[T, N]]:
  def wgslName = s"array<${inner.wgslName}, ${constValue[N]}>"
  def byteSize = constValue[N] * uniformStride(inner)
  def alignment = inner.alignment
  def vertexFormat = ""          // never a vertex attribute
  type AttribBuffer = EmptyTuple // ditto
  type UniformBuffer = inner.UniformBuffer  // ELEMENT layout; rows carry N
```

Note `UniformBuffer` stays the **element** layout — the row count lives in
`UniformValue.rows`, not in a giant repeated tuple type. That is deliberate:
a `Tuple.Concat`-based `Repeat[F, N]` would work but makes the compiler chew
through 64–256-element tuples per binding for no gain.

**4. DSL indexing** — `math/gpu/expr.scala` + `shader/dsl/types.scala`

```scala
opaque type ArrayExpr[E] <: Expr = Expr
object ArrayExpr:
  extension [E](a: ArrayExpr[E])
    inline def apply(i: Int): E     = Expr.raw(s"${a.wgsl}[$i]").asInstanceOf[E]
    inline def apply(i: IntExpr): E = Expr.raw(s"${a.wgsl}[${i.wgsl}]").asInstanceOf[E]
```

The cast is the same erased trick every `Expr` subtype already relies on (they
are all opaque over the one `Expr` class), and it is the same mechanism
`TypedExprAccessor.selectDynamic` uses for its return type.

Then one case at the **top** of `ToExpr` (`shader/dsl/types.scala:34-50`):

```scala
case UniformArray[t, n] => ArrayExpr[ToExpr[t]]
```

`UniformToExpr` routes through `ToExpr` already, so the visibility wrappers need
nothing.

**5. Export** — add `UniformArray` to `src/prelude/painter.scala:278`.

### Element types

WGSL requires a uniform array's element **stride** to be a multiple of 16. That
admits exactly:

| Element | WGSL          | Stride | CPU layout         |
| ------- | ------------- | ------ | ------------------ |
| `Vec3`  | `vec3<f32>`   | 16     | `Vec4Buffer`       |
| `Vec4`  | `vec4<f32>`   | 16     | `Vec4Buffer`       |
| `Mat2`  | `mat2x2<f32>` | 16     | `Mat2Buffer`       |
| `Mat3`  | `mat3x3<f32>` | 48     | `Mat3PaddedBuffer` |
| `Mat4`  | `mat4x4<f32>` | 64     | `Mat4Buffer`       |

and excludes `Float` / `Double` (stride 4) and `Vec2` (stride 8). The existing
`UniformBuffer` layouts already pad `Vec3` to 16 bytes, so every admitted type
maps 1:1 with no new padding logic. `UniformArrayElem` has instances for exactly
these five.

A scalar array is still reachable by packing four floats per `Vec4` at the call
site. A dedicated `UniformFloatArray` that hides that (`array<vec4, ceil(N/4)>`
with an `a[i/4][i%4]` accessor) is a plausible follow-up and is **not** in this
plan.

### Deliberately not included

- **A WGSL loop construct in the DSL.** The capacity is a compile-time literal,
  so every consumer unrolls in Scala. See _Follow-ups_ for when this changes.
- **Storage buffers.** Unchanged from the earlier decision. See _Follow-ups_.
- **Arrays of structs** (`array<Stop, N>`). Needs struct uniforms first, which
  the library does not have. Two parallel `array<vec4>`s cover the gradient
  case, and are what the sketch will use.
- **Per-element upload.** `set` re-uploads the whole array. At 8–16 vec4s that
  is 128–256 bytes; a `setAt(i, v)` with an offset `writeBuffer` is easy to add
  later if something animates one element per frame.
- **Runtime-resizable capacity.** `N` is part of the type, by design — it is
  what makes the shade's declaration and the binding agree at compile time.

---

## Follow-ups: DSL loops and storage buffers

These two get talked about together, and they are **separable** — with a clear
ordering between them.

### They are not the same axis

|                     | fixed capacity, runtime `count` | genuinely unbounded          |
| ------------------- | ------------------------------- | ---------------------------- |
| **unrolled**        | this plan — works today         | impossible                   |
| **loop over count** | loops alone; no storage buffer  | loops **and** storage buffer |

A uniform array is capped by `maxUniformBufferBindingSize` — 64 KiB by the
WebGPU default, i.e. **4096 vec4s**. Nothing about "this panel has 5 stops and
that one has 8" comes near that. So variable stop counts are answered by this
plan, and neither follow-up is on the gradient sketch's path.

### Loops are the more fundamental of the two, and the smaller

Yes, storage buffers effectively **require** loops: a runtime-sized array
(`array<T>` with `arrayLength()`) is only legal in the storage address space,
and there is nothing to do with an unbounded count except loop over it —
unrolling to a compile-time bound gives back exactly what the storage buffer
was for. So storage-buffers-without-loops buys almost nothing over a uniform
array.

The reverse is not true. Loops pay off on their own, over uniform arrays and
over nothing at all:

- iterating `0 until count` instead of masking a fixed capacity, once the
  capacity is large enough that the waste is real;
- variable-radius blur kernels, raymarch / sphere-trace steps, iterative
  relaxation — none of which involve a buffer.

So the order is **loops first, storage buffers only when a consumer needs
genuinely unbounded or large data** — which is still the revival condition
`grid-ceiling-rooms-plan.md` Part 5 §3 recorded (scattered occluders casting
into a lightmap was the example). A gradient with ≤16 stops is not it.

### What a loop construct costs

Most of the machinery exists. `Stmt.raw`, `Block`, `indentBlock` and the
`when` / `ifElse` / `ifChain` family already build nested WGSL bodies, and
`Block(stmts: Arr[Stmt])` is documented as the build-time-unroll counterpart
(`math/gpu/expr.scala:478`). A `loop(from, to)(i => Block)` emitting
`for (var i = …; i < …; i++) { … }` is a small addition on top. Two design
points, neither hard, both needing a decision rather than a guess:

- **The induction variable needs a name.** Every name in the DSL today comes
  from a named-tuple field (`ctx.locals`, `ctx.in`, `ctx.bindings`); a loop is
  the first construct that has to invent one. Either generate it from a counter
  on `Program`, or take it as an argument.
- **Accumulator declaration escapes the body.** `VarExpr := ` emits `var x = …`
  on _first_ use and `x = …` after (`expr.scala:64-68`). If a `var`'s first use
  is inside the loop body, the declaration lands inside the loop and resets
  every iteration. Accumulators must be initialised before the loop, and that
  needs to be either enforced or clearly documented.

Neither is in scope here. Recorded so the follow-up starts from the real state
of the DSL rather than re-deriving it.

---

## Implementation order

1. **Smoke-test the WGSL first.** Hand-write
   `var<uniform> a: array<vec4<f32>, 4>;` into an existing example's shader and
   confirm Dawn accepts a bare array as a top-level uniform store type.
   Everything below assumes it does; if it doesn't, the emitter has to wrap the
   array in a generated `struct`, which changes step 3 and only step 3.
2. `UniformValue.rows` + the `BufferBinding` allocation change. No behavior
   change for existing bindings; the whole test suite and every example must
   still run.
3. `UniformArray`, `UniformArrayElem`, its `UniformValue`, its `WGSLType`.
4. `ArrayExpr` + the `ToExpr` case.
5. Prelude export.
6. **A test sketch**, `sketches/tests/uniform-array/` — one shade, one quad,
   an `array<vec4, 8>` of colors indexed by a constant and by a computed index,
   with a second draw of the same shade under different data, proving the
   sharing. Registered in `sketches/index.html` in the same change. Per
   `graphics/CLAUDE.md` this is exactly what "a `src/`-or-library util with its
   own rendering behavior" earns.
7. Then the gradient sketch, which gets its own `PLAN.md`.

Steps 2–5 are all in `trivalibs/`; a `trivalibs/test/` unit test covers the CPU
half (row allocation, element offsets, over-length throw) without a GPU.

## Docs to update when this lands

- `documents/independent-todos.md` — nothing to remove; this is its own doc.
- `sketches/rooms/canvases/PLAN.md:209-217` and
  `documents/grid-ceiling-rooms-plan.md:1827-1851` — both say "declined, here
  are the revival conditions". Add one line to each pointing here, and **keep
  the reasoning**: those two call sites should still unroll their build-time
  constants.
- `documents/rust-painter/scala-port-comparison.md` if it tracks uniform kinds.
