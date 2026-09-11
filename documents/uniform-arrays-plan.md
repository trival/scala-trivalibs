# Uniform Arrays — `UniformArray[T, N]`

Status: **milestone 1 implemented in trivalibs and rendering**, with
`examples/uniform_array_gradient/` as the reference implementation and
demonstration. Two design points changed under implementation — see _Result_.

**Milestone 2 — loop primitives in the DSL — is implemented**; see _Result:
milestone 2_ at the end of its section for what changed under implementation.
**Milestone 3 — local arrays — is deferred**, to be decided now that loops
exist.

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

The multi-step gradient meets the revival condition exactly: the data varies per
draw, the shade is shared, and nothing about it is known when the shader is
built. So this is not a reversal of the earlier decisions — the rooms work
should keep unrolling its build-time constants either way.

---

## Driving use case

Multi-step gradients: several gradients side by side, differing in **number of
steps** and in the **interpolation used between each pair of steps**, all drawn
by **one shade** with per-draw uniform data.

### Is an array uniform the right representation?

Checked against the three alternatives before adding anything:

**1. Pack into `Mat4` — no new feature at all.** A `mat4x4<f32>` is four vec4s;
two of them give 4 stops plus 4 curve descriptors with zero library work. This
is the "does the collection collapse?" check that
`grid-ceiling-rooms-plan.md` Part 5 §4 asks for. It **doesn't collapse**: the
cap of 4 is too low for the point (a 6- or 8-step gradient is exactly what we
want to look at), the DSL has no column indexing on `Mat4Expr` so that
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

The consumer-side shape, recorded here because it drives the capability
requirement:

```scala
type MaxStops = 8

// xyz = color, w = position in [0,1]
stops:  UniformArray[Vec4, MaxStops]
// x = curve mode id, y/z = curve params; entry i governs the segment i → i+1
curves: UniformArray[Vec4, MaxStops]
count:  Double // active stop count
```

Evaluated **branch-free and unrolled**, because `MaxStops` is a compile-time
literal — the unrolling is a Scala `for` in the shader builder, so no WGSL loop
construct is needed. **This is the milestone 1 form**; milestone 2 replaces it
with a real loop over `count` and drops the masking — see _The gradient,
rewritten_ there.

```
col = stops[0].rgb
for i in 1 until MaxStops:
  t   = clamp01((x - stops[i-1].w) / (stops[i].w - stops[i-1].w))
  col = mix(col, stops[i].rgb, ease(curves[i-1], t) * active(i))
```

Each successive `mix` fully overrides once its local `t` reaches 1, so the
chain reproduces the piecewise result exactly, and holds the last color past
the final stop with no clamping special case. `active(i)` is
`step(f32(i), count - 1)`, which zeroes unused stops without a branch — a
workaround for iterating the capacity instead of the count, and the first thing
milestone 2 deletes.

**A per-panel stop count is not variable-length data.** Panels with 2, 5 and 8
stops all run through one shade with capacity 8: `count` varies per draw,
`active(i)` masks the rest. What a fixed capacity costs is _wasted work_, not
correctness — a 2-stop gradient still evaluates 8 mixes. In a bake that is
free; full-screen it is still nothing. It starts to matter somewhere north of
~64, which is where a loop over `count` earns its place — see _Milestone 2_.

`ease(curve, t)` has two possible forms, and **neither constrains the library
feature** — both need the same `array<vec4, N>`:

- **(a) mode id + select chain** — a small fixed set (linear, smoothstep, pow
  in/out, step, sine) selected by `curves[i].x`. Every mode is evaluated and
  `select`ed, so cost is `MaxStops × modes` cheap ops per fragment. Free in a
  bake, still trivial full-screen.
- **(b) a parametric family** — e.g. bias/gain, two floats covering linear,
  ease-in, ease-out, ease-in-out continuously. No branching at all, but you
  cannot put `step` or a sine lobe in it.

The example takes the narrowest slice of (b) — one `pow` exponent per segment —
because the interpolation vocabulary is not what it is demonstrating.

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
  /** Byte size of one `F` row — the array's element stride. */
  def rowBytes: Int
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
final class UniformArray[T, N <: Int](val values: Arr[T])

object UniformArray:
  inline def apply[T: UniformArrayElem, N <: Int](values: Arr[T]): UniformArray[T, N] =
    new UniformArray[T, N](values)
```

(Planned as an `opaque type` over `Arr[T]`; nominal for the match-type reason in
_Result_.)

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
  so every consumer unrolls in Scala. See _Milestone 2_ for when this changes.
- **Storage buffers.** Unchanged from the earlier decision. See _Still deferred:
  storage buffers_.
- **Arrays of structs** (`array<Stop, N>`). Needs struct uniforms first, which
  the library does not have. Two parallel `array<vec4>`s cover the gradient
  case, and are what the example uses.
- **Per-element upload.** `set` re-uploads the whole array. At 8–16 vec4s that
  is 128–256 bytes; a `setAt(i, v)` with an offset `writeBuffer` is easy to add
  later if something animates one element per frame.
- **Runtime-resizable capacity.** `N` is part of the type, by design — it is
  what makes the shade's declaration and the binding agree at compile time.

---

## Milestone 2 — loop primitives in the DSL

Status: **planned**. Milestone 1 (uniform arrays + the build-time `unroll`)
shipped; this is the runtime half of the same story.

Loops and storage buffers are always discussed together, and they are
**separable** — with a clear ordering between them. **Decided: loops now,
storage buffers stay deferred.**

### They are not the same axis

|                     | fixed capacity, runtime `count` | genuinely unbounded          |
| ------------------- | ------------------------------- | ---------------------------- |
| **unrolled**        | this plan — works today         | impossible                   |
| **loop over count** | loops alone; no storage buffer  | loops **and** storage buffer |

A uniform array is capped by `maxUniformBufferBindingSize` — 64 KiB by the
WebGPU default, i.e. **4096 vec4s**. Nothing about "this panel has 5 stops and
that one has 8" comes near that. So variable stop counts are answered by this
plan, and neither follow-up is on the gradient's path.

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

---

### What the DSL has today — the API to stay consistent with

All of it lives in `src/graphics/math/gpu/expr.scala`, and all of it is string
building over `opaque type Stmt = String` / `opaque type Block = String`. There
is no AST, so every addition below is purely additive.

| Construct                                               | Shape                                                | Emits                                  |
| ------------------------------------------------------- | ---------------------------------------------------- | -------------------------------------- |
| `select(onFalse, onTrue, cond)`                         | function, WGSL argument order                        | `select(a, b, c)`                      |
| `cond.select(onTrue, onFalse)`                          | extension, reading order                             | same                                   |
| `when(cond, body)`                                      | comma                                                | `if (c) { … }`                         |
| `ifElse(cond, thenBody, elseBody)`                      | comma                                                | `if (c) { … } else { … }`              |
| `ifChain(c, b).elseIf(c, b).elseDo(b)`                  | builder; opaque `IfChain` converts to `Stmt`/`Block` | `if / else if / else`                  |
| `cond.thenDo(body)` / `cond.thenElse(a, b)`             | extension                                            | as above                               |
| `unroll(from, until)(i => body)`                        | **curried**, body takes a Scala `Int`                | no WGSL construct — straight-line code |
| `Block(a, b, …)` / `Block(arr: Arr[Stmt])` / `Stmt.raw` | body assembly                                        | newline-joined statements              |
| `< <= > >= === !==`, `&& \|\| !`                        | symbolic operators → `BoolExpr`                      | the WGSL operator                      |
| `ctx.ret(v)` (inside `WgslFn.dsl` only)                 | context member                                       | `return v;`                            |

Conventions to copy rather than re-decide (they were settled in
`documents/done/shader-control-flow-plan.md`):

- **The body is an argument, never a trailing brace block.** Comma form when the
  body takes nothing (`when`, `ifElse`); curried form when the body takes an
  index (`unroll`). `given Conversion[Stmt, Block]` means a one-statement body
  never needs a `Block(...)` wrapper.
- **Symbolic operator = boolean, named method = numeric step helper.** `a < b`
  is a `BoolExpr`; `a.lt(b)` is `0.0`/`1.0`.
- **Keyword clashes are dodged with a `Do` suffix**, not with a rename:
  `thenDo`, `elseDo` (the latter also dodging `Opt.orElse`).
- **Every WGSL identifier comes from a Scala identifier.** Locals and params are
  named-tuple fields, fn names are the `WgslFn.dsl` argument, bindings are
  uniform-schema fields. **Nothing in the DSL invents an identifier today** —
  the loop counter is the first thing that has to.
- **Indentation is not tracked.** `indentBlock` re-indents a nested body by two
  spaces, so nesting composes without state. `unroll` and `Block` produce bodies
  that already satisfy this.
- Top-level helpers reach sketches through the per-file wildcard exports in
  `src/prelude/painter.scala` (`…`expr$package`.{…, *, given}` at line 110, the
  `dsl` block at 236-239). Adding a helper to an existing file needs no prelude
  change; adding a **new file** needs one export line.

### Scope of this milestone

**In:** a counted loop over a runtime bound, a condition loop, and the jumps
(`break` / `continue` / conditional break). That is the set that makes
"iterate `0 until count`" and "sphere-trace until you hit" expressible.

**Out, deliberately:**

- **`loop { … } continuing { … }`** — WGSL's third form. `while` + `break`
  expresses everything we have a consumer for, and `continuing` exists mainly so
  `continue` still runs the increment, which the `for` form already handles.
- **`switch`** — deferred with the rest of the control-flow plan, no consumer.
- **Reduction helpers** (`sum(from, until)(i => expr)` and friends) — sugar over
  an accumulator `var`; wait for two real uses.
- **Loop-carried values as expressions** (a functional `fold` shape). The DSL is
  statement-oriented everywhere else; a loop should not be the one construct
  that returns a value.
- **Function-local arrays** (`var tmp: array<vec4<f32>, 3>`) — the one real hole
  the loop API leaves. Designed as **milestone 3** below, to be decided once
  loops have been used in anger.
- **Storage buffers** — see the section after this one.

### Proposed API

The body is a **callback**, and that is doing two jobs: it carries the induction
variable where there is one, and — more importantly — it gives the body its own
**Scala** scope, so the locals belonging to it can be declared where they belong.
They are declared the way sketch bodies already declare ad-hoc locals —
`LetVec4("cur")`, `VarFloat("acc")` — and **a WGSL loop body is a real scope**,
so nothing has to be invented to keep those names apart (verified below):

```scala
// counted — the same shape as `unroll`, one level up
// bounds take Int or IntExpr in any position, as plain overloads — that is what
// makes `loop(64)` and `64.loop` work without an `.i` dance
def loop(count: IntExpr)(body: IntExpr => Block): Stmt
def loop(count: Int)(body: IntExpr => Block): Stmt
def loop(from: Int, until: IntExpr)(body: IntExpr => Block): Stmt
def loop(from: IntExpr, until: IntExpr)(body: IntExpr => Block): Stmt
def loop(from: Int, until: Int)(body: IntExpr => Block): Stmt

// condition-driven — by-name body, so the block is a Scala scope too
def loopIf(cond: BoolExpr)(body: => Block): Stmt
extension (cond: BoolExpr) def thenLoop(body: => Block): Stmt

// jumps
val break: Stmt
val continue: Stmt
def breakIf(cond: BoolExpr): Stmt
def continueIf(cond: BoolExpr): Stmt

// every construct also has its receiver-dispatched twin — see the grid below
extension (cond: BoolExpr)
  def thenLoop(body: => Block): Stmt
  def thenBreak: Stmt
  def thenContinue: Stmt
extension (count: IntExpr) def loop(body: IntExpr => Block): Stmt
```

```scala
program.frag: ctx =>
  val stops = ctx.bindings.stops
  val col = VarVec3("col")
  Block(
    col := stops(0).rgb,
    loop(1, ctx.bindings.count.toI32): i =>
      val prev = LetVec4("prev")
      val cur = LetVec4("cur")
      val t = LetFloat("t")
      Block(
        prev := stops(i - 1),
        cur := stops(i),
        t := ((x - prev.w) / (cur.w - prev.w)).clamp01,
        col := col.mix(cur.rgb, t),
      ),
    ctx.out.color := vec4(col, 1.0),
  )
```

#### Every control-flow body becomes a scope

`breakIf` is the deliberate twin of `loopIf` — same `…If(cond)` shape for "do
this one thing when the condition holds" — and `continueIf` comes with it for
free.

**Both a function form and an extension form, for every construct.** Elsewhere
in the library two ways to say one thing is a smell; in control flow it is the
existing, deliberate pattern — `select` / `cond.select`, `when` / `cond.thenDo`,
`ifElse` / `cond.thenElse` all ship both today. While the DSL is still being
explored, the surprising thing is not the redundancy but the **gaps**: having to
remember which constructs have an extension twin and which do not. So every
construct gets both, and the pairs get pruned later on usage evidence rather than
guessed at now.

The rule for the extension half is the library's existing one — **same name,
dispatched by the receiver** (`trivalibs/CLAUDE.md`, the CPU/GPU helper
convention) — with `…then…` names where the receiver is a `BoolExpr`, because
that is what the shipped pairs already do:

| construct          | function                                                          | extension                              |
| ------------------ | ----------------------------------------------------------------- | -------------------------------------- |
| branchless         | `select(onFalse, onTrue, cond)`                                   | `cond.select(onTrue, onFalse)`         |
| if (and if-chains) | `when(cond)(body)`                                                | `cond.thenDo(body)`                    |
| …continued         | `.elseIf(cond)(body)` / `.elseDo(body)` — on the result of either |                                        |
| while              | `loopIf(cond)(body)`                                              | `cond.thenLoop(body)`                  |
| counted loop       | `loop(count)(i => body)`                                          | `count.loop(i => body)`                |
| …with a literal    | `loop(64)(i => body)`                                             | `64.loop(i => body)`                   |
| build-time loop    | `unroll(count)(i => body)`                                        | `count.unroll(i => body)`              |
| over values        | `unroll(xs)((v, i) => body)`                                      | `xs.unroll((v, i) => body)`            |
| break / continue   | `breakIf(cond)` / `continueIf(cond)`                              | `cond.thenBreak` / `cond.thenContinue` |

Two notes on the grid. The **two-bound forms** `loop(from, until)` and
`unroll(from, until)` stay function-only — there is no single natural receiver
for a pair of bounds, and `from.loopUntil(until)` buys nothing. That is a stated
exception, not an oversight. And `xs.unroll((v, i) => …)` is the case where the
extension reads **better** than the function: `sortPairs.unroll: (pair, i) => …`
puts the collection first, where it belongs.

#### `ifChain` folds into `when`, and takes `ifElse` with it

`IfChain` is **not a builder that needs a terminator** — checked: `ifChain` emits
a complete `if (c) { … }` string immediately, `elseIf` appends to it, and
`given Conversion[IfChain, Stmt]` / `[IfChain, Block]` already let an
unterminated chain stand as a statement. `ifChain(c)(b)` and `when(c)(b)`
therefore emit **byte-identical WGSL** and differ only in return type.

So `when` returns `IfChain`, and `ifChain` disappears. The same applies to the
extension half: `cond.thenDo(body)` returns `IfChain`, so `thenChain` is not
needed either.

That cascades onto the **two-body problem** flagged above — and resolves it by
removal rather than by choosing an awkward signature. `ifElse(cond)(thenB)(elseB)`
could only give the else-branch a colon block; the chain gives **both** bodies
one:

```scala
when(x < 0.5):
  val a = LetVec3("a")
  Block(…)
.elseDo:
  val b = LetVec3("b")
  Block(…)
```

So `ifElse` and `cond.thenElse` go too: they are `when(c)(a).elseDo(b)` and
`c.thenDo(a).elseDo(b)` with strictly worse scoping. Four constructs removed
(`ifChain`, `thenChain`, `ifElse`, `thenElse`), nothing lost, and the earlier
open question about two-body signatures is answered.

The grid above already reflects this: one `if` row whose result continues with
`.elseIf` / `.elseDo`, in either the function or the extension spelling.

The by-name body argument is not a loop concern, though — it applies to
**everything in the DSL that takes a body**, and for the same reason. Checked
rather than assumed: `if` bodies are WGSL scopes exactly like loop bodies. The
emitter probe put a `Var`'s first `:=` inside a `when`, and got

```wgsl
  if ((in.position.x > 0.0)) {
    var tmp = delta;
  }
  tmp = (tmp + delta);     // naga: no definition in scope for identifier `tmp`
```

So a local belonging to an `if` body has the same need for a Scala scope to be
declared in, and today's `when(cond, Block(…))` comma form gives it none.

**Proposal: every body is the last parameter of its own parameter list, taken
by-name.** That is what makes the trailing-colon block work, which is what gives
the body a Scala scope:

```scala
def when(cond: BoolExpr)(body: => Block): IfChain
extension (chain: IfChain)
  def elseIf(cond: BoolExpr)(body: => Block): IfChain
  def elseDo(body: => Block): Stmt
extension (cond: BoolExpr) def thenDo(body: => Block): IfChain
```

Verified to parse and compose, including the leading-dot continuation after an
indented block:

```scala
val b = when(x):
  val local = LetFloat("t")
  Block(…)
.elseIf(y):
  Block(…)
.elseDo:
  Block(…)
```

This is **ergonomics only** — the construct evaluates its body immediately, so
emission order is unchanged and no existing shader emits anything different.

The two-body constructs would have been the awkward case — only the last
parameter list can take a colon block — but they are being removed instead; see
the next subsection.

Migration cost, counted: 7 `when`, 3 `ifElse`, 6 `ifChain`, 9 `elseIf`, 4
`elseDo`, 2 `thenDo`, 2 `thenElse` call sites across the library, examples, tests
and sketches. Small enough to do in one pass, and it belongs in this milestone —
adding `loop` / `loopIf` with by-name bodies beside a `when` that takes a comma
argument would ship the inconsistency it is trying to avoid.

#### Do loop bodies need a locals schema? — no, checked with `naga`

An earlier draft of this section gave `loop` a context object with a named-tuple
locals schema, on the assumption that body-local names needed generating. That
assumption was wrong for `loop`, and `naga` 30.0.1 settles it:

- **Shadowing across scopes is legal.** A body declaring `let cur` at function
  scope, again inside a `for`, again inside a nested `for`, and again inside a
  sibling `for`, validates — `Validation successful` — and the function-scope
  `cur` is still readable after the loops.
- **A duplicate in the _same_ scope is a loud error**, not a silent
  miscompile: `error: redefinition of 'cur'`, naming the identifier and both
  lines.

So:

- **`loop` / `loopIf` need nothing.** The body is emitted once into its own
  braces, whatever the trip count, so ad-hoc declarations inside the callback are
  already correct and already scoped. Nothing escapes.
- **`unroll` is the only one with a hygiene problem** — its body is emitted N
  times into the _enclosing_ scope — and the fix needs no machinery either: the
  index is right there, so `LetVec4(s"cur$i")` is the declaration, which is what
  `sketches/textures/lines/` writes by hand today minus its separate `swapId`
  counter. Forgetting the suffix fails loudly at shade build, with the name in
  the message.

That removes the whole reason the loop family would have had to leave
`expr.scala`: with no `ToLocal` / `TypedLocalAccessor` dependency, `loop` lives
beside `when` / `unroll`, `unroll` does not move, and the prelude's
existing wildcard export picks all of it up with no new line.

**A locals schema is not coming later either.** The stage bodies' own schema is
being **removed** — `documents/shader-locals-consolidation.md` has the analysis
and the removal plan: ad-hoc `Let*` / `Var*` / `Const*` declarations are what the
code already uses (229 uses to 15), are strictly more expressive, and avoid a
live re-declaration bug in the schema path. So index-only loop bodies are not a
default to revisit; they are the shape of the API, and they are the same shape
the stage bodies are moving to.

These emit, respectively:

```wgsl
for (var i: i32 = 0; i < count; i++) {
  …
}
while (c) {
  …
}
// loopIf / thenLoop both emit the `while`
break;
continue;
if (c) {
  break;
}
```

The point of matching `unroll`'s shape is that the two read as one pair — the
only difference at the call site is whether the bound is a Scala `Int` or an
`IntExpr`, which is exactly the difference that matters:

```scala
unroll(1, MaxStops)(i => col := col.mix(stops(i).rgb, w(i)))  // Int     → straight-line WGSL
loop  (1, count)   (i => col := col.mix(stops(i).rgb, w(i)))  // IntExpr → a for-loop in WGSL
```

The two bodies are now literally the same text; only the bound's type differs.
That is the strongest argument for keeping both index-only.

### The gradient, rewritten

Milestone 1's fragment body pays for the capacity rather than the count, and
three separate things in it exist only because of that: it runs `MaxStops - 1`
mixes whatever `count` is; it masks the dead ones with
`active(i) = step(f32(i), count - 1)`; and it guards the segment-width division
with `.max(0.00001)` because unused stops sit on top of each other. A loop over
`count` deletes all three:

```scala
program.frag: ctx =>
  val stops = ctx.bindings.stops
  val curves = ctx.bindings.curves
  val x = ctx.in.uv.x
  val col = VarVec3("col")
  Block(
    col := stops(0).rgb,
    loop(1, ctx.bindings.count.toI32): i =>
      val prev = LetVec4("prev")
      val cur = LetVec4("cur")
      val t = LetFloat("t")
      Block(
        prev := stops(i - 1),
        cur := stops(i),
        t := ((x - prev.w) / (cur.w - prev.w)).clamp01,
        col := col.mix(cur.rgb, t.pow(curves(i - 1).x)),
      ),
    ctx.out.color := vec4(col, 1.0),
  )
```

`active(i)` is gone — there is no dead iteration left to mask. The clamped
denominator is gone too: consecutive **live** stops have distinct positions, so
the guard was defending against data the loop never visits. That does turn "stop
positions are strictly increasing" from something the shader tolerated into a
precondition the CPU side owns, which is the honest trade and worth a line in the
example. And a 2-stop band costs one mix instead of seven.

The result is a **literal transcription of the pseudocode** in _The gradient's
own representation_ — which is the real argument for the loop here, ahead of the
saved work. The unrolled version is not wrong; it just says "capacity" in every
place the intent says "count".

**No `.min(MaxStops)` on the bound.** `count` comes from
`randIntInRange(2, MaxStops + 1)` and the arrays are built with exactly that many
entries, so `count <= MaxStops` holds by construction and a clamp would be
defensive noise in a body whose whole job is to read as the gradient definition.
The cap belongs in the docs as the rule for the case the example is not: a
`count` that arrives from somewhere the shade's author does not control. WGSL
does not bounds-check the index, so that case does need it.

The one thing the loop does not remove: the body is uniform control flow only
because `count` is a uniform — which it is, so a `.sample(…)` would still be
legal in there.

### What a ctx'd `unroll` fixes in code we already have

`unroll`'s job is to **replace hand-assembled `Arr[Stmt]` with something that
inlines into a `Block`**. Two shader bodies in the sketch repo still assemble by
hand, and they are the evidence for what is missing — checked rather than
assumed:

- `sketches/textures/lines/Lines.scala:144-187` — `stmts += …` ~30 times across
  a pass loop (`for passOff <- passOffsets`), a `lines.zipWithIndex`, a
  three-pair bubble sort (`Seq((0,1),(1,2),(0,1))`), and a `for j <- 0 until 3`
  blend.
- `sketches/textures/moving-plates/MovingPlates.scala:171-331` — straight-line
  declarations, a nine-call `emitTile` helper that pushes two statements and
  returns a handle, then three `for i <- 0 until 4` loops.

Four distinct blockers, only one of which is about the loop construct at all:

1. **Interleaving loops with straight-line statements** — already solved.
   `unroll` returns a `Stmt`, so `Block(a, b, unroll(…), c)` works today. Both
   files' `for i <- 0 until 4` loops could drop `stmts` right now; they predate
   the helper. Nothing to add, just a refactor.
2. **Iterating values, not an `Int` range** — `passOffsets`, `sortPairs`,
   `lines.zipWithIndex`. `unroll(from, until)` cannot express any of them, and
   this is the most common shape in both files. Needs an overload taking the
   values: `unroll(Seq((0, 1), (1, 2), (0, 1)))((pair, i) => …)`.
   Cheap, and the single biggest win in both files.
3. **Per-iteration locals with hand-written unique names** — `sw${swapId}av` /
   `sw${swapId}bv` / … driven by a manual `swapId` counter (Lines), `r_$name` /
   `t_$name` (MovingPlates), `quv0..quv3` built as an `Arr` of `LetVec2`
   (MovingPlates). The names themselves stay — an unrolled body shares the
   enclosing scope, so two iterations genuinely must not declare the same one —
   but **the separate counter goes away**: inside `unroll` the index is in hand,
   so `LetVec4(s"av$i")` replaces `sw${swapId}av` plus its bookkeeping. `quv0..3`
   becomes one `LetVec2(s"quv$i")` inside the unrolled body.
4. **Helpers that emit statements _and_ return a value** — `emitTile` pushes two
   `let`s and returns the second as a handle. Not a loop at all, so no loop
   construct fixes it. The right answer there is a `WgslFn.dsl` (it is a
   function: `vec2 → vec3`), or a caller-declared local plus a `Block`-returning
   helper. Worth saying in the docs so it stops being written this way.

**Blocker 3 is where `unroll` and `loop` genuinely differ**, and it is worth
being precise about why. A `loop` body is emitted once, inside its own braces —
one scope, so `LetVec4("cur")` is fine however many times it runs. An `unroll`
body is emitted N times into the **enclosing** scope, so an unsuffixed
`LetVec4("cur")` is `let cur` declared N times in one scope. `naga` rejects that
outright (`error: redefinition of 'cur'`, both lines named), so the mistake
surfaces at shade build rather than becoming wrong output — which is what makes
"interpolate the index into the name" an acceptable rule rather than a trap.

The alternative considered and rejected: **wrapping each unrolled iteration in a
bare WGSL `{ … }`** so names need no suffix. It reads well until an accumulator
is involved — a `Var` whose first `:=` lands in iteration 0's braces would be
declared inside them and out of scope in iteration 1, which today works (one
declaration, then assignments, all in one scope). It would break loudly rather
than quietly, but it would break a pattern that is currently correct. Flat output
with suffixed names keeps the accumulator story exactly as it is.

**Would a real WGSL loop be the better refactor for these two? No — and it is
not even reachable.** Every trip count in both bodies is a build-time constant
(3 passes, 3 sort pairs, 3 lines, 4 neighbors, 9 tiles), which is exactly the
case `unroll` exists for: the per-iteration constants (`dirTL`, the pass
offsets, the `segOff`/`lineXOff` pairs) constant-fold into the emitted
expressions, and a driver unrolls a 3-iteration loop anyway. A runtime loop
would trade that folding for nothing.

It is also blocked outright: a real loop over the three lines needs them in a
**function-local array** (`var lines: array<vec4<f32>, 3>`) so `lines[i]` is
indexable — today they are three separate `VarVec4("l0v")`, `…("l1v")`,
`…("l2v")` locals, and the DSL has no local-array declaration at all
(`UniformArray` is a uniform binding, not a `var`). That hole is milestone 3,
below.

So the split is: **`loop` earns its place when the count is runtime or when the
unrolled source gets big** (a 64-tap blur emits 64 copies of its body; three
lines emit three). Neither of these bodies is that.

With 2 and 3 in, both bodies above collapse to a single `Block(...)` with no
`Arr[Stmt]` anywhere — which is the actual goal, not `unroll` for its own sake.
The clean refactor for the existing code is therefore **`unroll` (sequence
iteration + scoped locals) for the loops, `WgslFn.dsl` for the emit-and-return
helpers, and plain `Block` nesting for the rest** — no WGSL loop involved.

**`Int` bounds are allowed** — `loop(64)(i => …)` and `64.loop(i => …)` both
work, alongside the `IntExpr` forms.

An earlier draft of this section refused them, on the grounds that a
compile-time-constant bound is `unroll`'s job and an `Int` overload would make
the two silently interchangeable. That reasoning does not survive contact with
the real choice: **a constant bound does not imply unrolling.** A fixed 64-step
raymarch is a case where you specifically do _not_ want 64 copies of the body in
the shader source, and trading the constant folding for a compact loop is a
deliberate, legitimate decision. The two names already say which one was made —
`unroll(64)` emits straight-line code, `loop(64)` emits a `for` — so nothing is
silent about it. Refusing the overload just forced `64.i.loop` on a real case to
protect against a mistake the name already prevents.

So the bound accepts `Int` or `IntExpr` in every position: `loop(64)`,
`loop(0, 64)`, `loop(1, count)`, `loop(from, until)`. **Still not** a
`given Conversion[Int, IntExpr]` — Scala already has `Conversion[Int, FloatExpr]`
(`float_expr.scala:22`) and a second one would make every bare int argument
ambiguous. These are plain overloads, and `4.i` (`int_expr.scala:44`) remains
available where an `IntExpr` is wanted explicitly.

**`Int` as a receiver is the one place a bare int means `i32`, not `f32`.**
Everywhere else in the DSL a Scala `Int` in expression position becomes `f32(n)`
through that conversion, because shader math is floating-point. In `64.loop`,
`64.unroll` and `4.i` the int is a receiver, not an operand, so no conversion
applies and the count stays an integer count. Worth a line in the guide — it
reads as an inconsistency until someone points out that a loop bound is not
shader math.

### The induction variable's name

Body locals are written by the author; the counter is the one identifier the DSL
has to invent, because the body only ever sees it as the callback's parameter.
Recommended — **derive it from lexical nesting depth**, tracked by a build-time
counter private to `expr.scala`:

```scala
private val LoopVarNames = Arr("i", "j", "k", "l", "m", "n")
private var loopDepth = 0

def loop(from: IntExpr, until: IntExpr)(body: IntExpr => Block): Stmt =
  val name =
    if loopDepth < LoopVarNames.length then LoopVarNames(loopDepth)
    else s"i$loopDepth"
  loopDepth += 1
  val b = body(IntExpr(name))
  loopDepth -= 1
  s"  for (var $name: i32 = ${from.wgsl}; $name < ${until.wgsl}; $name++) {\n${indentBlock(b)}\n  }"
```

Why this and not a monotonic counter: the emitted WGSL stays **deterministic** —
the same body builds byte-identical source every time, which string-level tests
depend on and any future shader-source cache key would too. Sibling loops at the
same depth both get `i`, which is legal (each is its own WGSL scope) and reads
the way hand-written shader code does. Nesting gives `i`, `j`, `k`. Body
building is synchronous and single-threaded, so the counter cannot interleave.

The residual hazard is a user local also named `i` — declared in the enclosing
body or inside the loop — which the counter then shadows. That is the
same hazard `Let`-local names already have against WGSL builtins
(`docs/guide/gotchas.md:19`), so it gets the same treatment: a gotchas entry,
plus a `loopNamed("step")(…)` escape hatch if we decide to add one (**open**,
below).

### Traps to design around

1. **A `var`'s declaration lands wherever its first assignment lands** — not
   where the Scala `val` was created. `VarExpr` carries a per-instance
   declared-flag: the first `:=` built on that instance emits `var x = …`, every
   later one emits `x = …` (`expr.scala:63-68`). The Scala `val` is only a
   handle; it may live anywhere Scala scoping allows. Checked against the real
   emitter and against `naga`, this plays out differently for the two
   constructs:
   - **In `unroll` it is already correct.** All iterations share one scope, so a
     `Var` first assigned inside the body emits `var acc = …` in iteration 0 and
     `acc = …` in the rest — valid WGSL, accumulation intact. (This is the
     pattern the rejected `{ }`-per-iteration variant would have broken.)
   - **In `loop` the declaration lands inside the braces**, making the var
     loop-scoped: re-initialised every iteration, and gone after the loop.
     Reading it afterwards is a shade-build error (`no definition in scope for
identifier: 'acc'`), and an initializer that reads the var itself fails the
     same way. The only quiet case is a var declared _and_ read only inside
     the body — which is a correct per-iteration temporary, not a bug.

   So the rule is about **scope, not repetition**: an accumulator takes its seed
   assignment before the loop — `col := vec3(0.0)` outside, `col := …` inside.
   Nothing enforces it and nothing needs to; wherever the accumulator is actually
   used afterwards, the failure is loud.

   Two corollaries of the same mechanism, each worth a documented line:
   - **"First" means first _built_, not first _placed_.** The declaration is
     baked into the `Stmt` string at `:=` time, so statements pre-built into vals
     or an `Arr[Stmt]` carry it with them: `Block(second, first)` emits
     `a = delta;` before `var a = in.position;`. Build and placement order
     normally coincide — `Block(...)` evaluates its arguments in order — and the
     places they can diverge are the value-iterating `unroll` and any
     hand-assembled statement array.
   - **One Scala `val` per WGSL local.** Two `VarVec2("b")` instances carry two
     flags and emit two declarations — `error: redefinition of 'b'`.

2. **Uniform control flow and `textureSample`.** WGSL only allows
   implicit-derivative sampling in uniform control flow. A bound read from a
   uniform is uniform, so a `loop(0, count.toI32)` body is fine — but a bound or
   a `break` that depends on per-fragment data is not, and a `.sample(...)`
   inside it is a validation error rather than a warning. The rule to document
   and to demonstrate in the example: **inside a loop, read textures with
   `.sampleLevel(...)` or `.load(...)`.** This must be checked against `naga`,
   not assumed — a variable-radius blur is exactly the case that hits it.
3. **Counts arrive as `f32`.** Uniform scalars are `Double`/f32 in this library,
   so the idiom is `loop(0, count.toI32)`. WGSL does not bounds-check an array
   index, so a `count` that could exceed the capacity has to be capped —
   `loop(count.toI32.min(N.i))`. **Cap only when it could**: where the count and
   the array are built together, as in the example, `count <= N` holds by
   construction and the clamp is defensive noise in a body that should read as
   the definition of the thing it computes. Worth a line at the `ArrayExpr`
   indexing docs, phrased that way round.
4. **`break` outside a loop is a WGSL error we will not catch.** Scoping it
   would mean handing the body a receiver to reach it through, which is the
   machinery the index-only design just avoided. Recommended: keep the jumps
   top-level and let `naga` catch the misuse at shade build, as it does for
   everything else the DSL does not track.

### Decisions

Settled ones are struck through with the outcome; two are still open (5 and 6).

1. ~~Index-only bodies, or a locals schema?~~ **Settled: index-only.** A `loop`
   body is a real WGSL scope, so ad-hoc `LetVec4("cur")` inside the callback is
   already correct, and the stage-body schema it would have mirrored is being
   removed altogether
   (`documents/shader-locals-consolidation.md`). The loop family therefore stays
   in `expr.scala`, with no `shader/dsl` dependency.
2. ~~Does `unroll` gain a value-iterating overload?~~ **Settled: yes** —
   `unroll(Seq((0, 1), (1, 2), (0, 1)))((pair, i) => …)`. It is the single
   biggest thing standing between the two hand-assembled shader bodies in
   `sketches/textures/` and a plain `Block`, which is `unroll`'s stated purpose.
3. ~~Name of the condition loop.~~ **Settled: `loopIf(cond)(body)`, plus
   `cond.thenLoop(body)`.** `loop` is the looping primitive and the family is
   named after it — the emitted WGSL already differs (`loop` emits `for`), so the
   Scala names follow the DSL's own vocabulary rather than WGSL's keywords.
   `thenLoop` sits beside `thenDo` on `BoolExpr`, the same way `loopIf` sits
   beside `when`.

   **The body is by-name, not eager** (`body: => Block`, curried): every
   body-taking loop construct then gives its body a Scala scope to declare locals
   in, which is the whole reason the counted form takes a callback. It reads the
   same as the counted form at the call site — `loopIf(x < n):` then an indented
   block — because the trailing-colon syntax works for by-name arguments (it is
   how munit's `test(name)(body: => Any)` is written).

   **Considered and not taken: an overload `loop(cond: BoolExpr)`.** Two reasons.
   `loop(n)` and `loop(c)` would differ only in the type of a single expression
   argument while meaning "repeat n times" and "repeat while c" — a distinction
   the reader has to type-check by eye. And `IntExpr` / `BoolExpr` are both
   opaque over `Expr`, so the overload set needs `@targetName` gymnastics to
   survive erasure, for a name that ends up less clear rather than more.

4. ~~Names for the jumps.~~ **Settled: plain `break` / `continue`.** Neither is
   a Scala keyword; they only collide for someone who also imports
   `scala.util.control.Breaks.*`, which no shader code does.
5. **`loopNamed(name)(from, until)(body)` now, or wait for a collision?** (open)
   Recommended: **wait.** It is a two-line addition whenever someone actually
   shadows something, and every escape hatch shipped unused is API surface to
   document.
6. ~~What the example is.~~ **Settled: update one, add one.**
   `examples/uniform_array_gradient/` is **rewritten onto `loop`** — it is the
   example whose subject this changes, and _The gradient, rewritten_ below is
   exactly that diff: the `active(i)` masking and the clamped denominator go
   away, and a 2-stop band stops paying for 8 mixes. A **new
   `examples/sdf_trace/`** covers what the gradient cannot: `loopIf` + `break`,
   a per-fragment step count, and a loop that is not driven by an array at all.

   **Not a variable-radius blur**, which was the earlier suggestion for the
   second example:
   `shader/lib/blur.scala:17` already provides a separable Gaussian with a
   runtime-controlled diameter. An example that re-rolls a library util teaches
   the opposite of what examples are for — if anything, the blur belongs in an
   example as a **call**, not as a loop to imitate.

   The sphere-trace took its place: nothing in the library does it, and it is
   genuinely unreachable without runtime loops since the step count varies per
   fragment. The uniform-control-flow rule then gets verified where it belongs —
   in the `naga` step of the test plan, on a shader that samples inside a
   uniform-bounded loop — rather than by an example whose only job would be to
   demonstrate a rule.

7. ~~Do local arrays ship in this milestone?~~ **Settled: no — they are
   milestone 3**, and the decision on whether to build them at all waits until
   the loop primitives are implemented and tested. Using loops in anger is what
   will show whether indexed mutable scratch is actually missing. The only thing
   milestone 2 owes them is to keep `ArrayExpr[E]` address-space-agnostic, which
   costs nothing.

   If milestone 3 is not taken up straight away, the _Local arrays_ section is
   self-contained and lifts cleanly out of this document — into
   `documents/independent-todos.md` (it is the shape of the entries there:
   gap, required changes, priority) or into its own plan. It should not stay here
   indefinitely as an appendix to a shipped feature.

### Tests

`test/shader/Loops.test.scala`, in the style of `ControlFlow.test.scala` (exact
emitted-string asserts, no GPU):

- each form's exact WGSL: all three `loop` overloads, `loopIf`, `break`,
  `continue`, `breakIf`, `continueIf`;
- each extension twin emits exactly what its function form emits — one assertion
  per row of the grid, which is what keeps the pairs honest as they change;
- the converted control-flow constructs still emit byte-identical WGSL to what
  the current comma forms emit — the existing `ControlFlow.test.scala`
  assertions, re-pointed at the by-name signatures;
- declaration placement — an ad-hoc `LetVec4("cur")` first assigned inside the
  callback declares **inside** the emitted braces, while a `Var` already assigned
  before the loop assigns (not re-declares) inside the body;
- build order vs placement order — a statement built before another but emitted
  after it carries its `var` declaration along, as the emitted string;
- nesting — counter names `i` / `j` / `k` by depth, sibling loops both `i`,
  indentation of a loop inside an `if` inside a loop;
- determinism — building the same body twice yields identical strings;
- the `unroll` / `loop` pair over the same body, as a documented contrast;
- a loop body indexing an `ArrayExpr` with the induction variable;
- a nested pair declaring the same ad-hoc name in both bodies, and an `unroll`
  whose body declares `s"cur$i"` — the two hygiene cases, as emitted strings.

Plus, as in milestone 1, the example's **full generated shader validated with
`naga` 30.0.1**, including a texture read inside a loop — that is the check that
trap 2 is settled rather than assumed.

### Implementation order

1. `loop` (three overloads) + the depth-derived counter name — `expr.scala`,
   beside `unroll`. No new file, no prelude line, nothing moves.
2. `loopIf`, `break`, `continue`, `breakIf`, `continueIf` — same file — plus the
   extension twins for every row of the grid (`thenLoop`, `thenBreak`,
   `thenContinue`, `IntExpr.loop`, `Int.loop`, `Int.unroll`, `xs.unroll`).
3. Convert the existing control flow to by-name bodies (`when`, `elseIf`,
   `elseDo`, `thenDo`), fold `ifChain` into `when` and delete `ifElse` /
   `thenElse` / `thenChain`, and migrate the ~33 call sites.
4. The value-iterating `unroll` overload, plus a scaladoc line on `unroll` about
   interpolating the index into any name declared inside its body.
5. `test/shader/Loops.test.scala`.
6. Rewrite `examples/uniform_array_gradient/` onto `loop` (the milestone's own
   before/after), and add `examples/sdf_trace/` for `loopIf` + `break`. Validate
   both generated shaders with `naga`, plus one shader that samples a texture
   inside a uniform-bounded loop — the check that trap 2 is settled rather than
   assumed.
7. In the sketch repo, after the library side is published: rewrite
   `sketches/textures/lines/Lines.scala:144-187` and
   `sketches/textures/moving-plates/MovingPlates.scala:171-331` onto `Block` +
   `unroll` + `WgslFn` — the check that the feature actually does its job.
8. Docs (below).

### Docs to update

- **`docs/guide/shader-dsl-guide.md`** — the "Control flow" section lists
  `select` / `when` / `ifElse` / `ifChain` and **does not mention `unroll` at
  all**; milestone 1 left it undocumented. Add both together, led by the
  build-time / runtime distinction, so the pair is learned as a pair.
- **`docs/guide/gotchas.md`** — where a `var` declaration lands (first assignment
  built, not where the Scala `val` is created), and what that means for an
  accumulator across a loop; one `val` per WGSL local; sampling inside a loop
  (`sampleLevel` / `load`); capping a loop bound against a `UniformArray`
  capacity when the count is not construction-guaranteed.
- **`unroll`'s own scaladoc** (`math/gpu/expr.scala:551-556`) is **wrong today**
  and must be corrected in this milestone: it says a `var` first assigned inside
  an unrolled body "would re-declare it every iteration". It does not — the
  declared-flag is per instance, so iteration 0 declares and the rest assign,
  which is why the pattern works. The real caveat for `unroll` is the opposite
  one: names _declared_ per iteration need the index in them.
- **`documents/done/shader-control-flow-plan.md`** — its closing line says loops
  remain deferred. Point it here.
- **`docs/skills/write-sketch/SKILL.md`** — check whether it enumerates the
  control-flow helpers; if it does, `loop` belongs in the list.

---

### Result: milestone 2

Shipped as planned — `loop` (six bound overloads), `loopIf`, `break` /
`continue` / `breakIf` / `continueIf`, the extension twins (`IntExpr.loop`,
`Int.loop`, `Int.unroll`, `Arr.unroll`, `thenDo`, `thenLoop`, `thenBreak`,
`thenContinue`), the by-name-body conversion, `ifChain` folded into `when`, and
`ifElse` / `thenElse` deleted. All of it in `math/gpu/expr.scala`; no new file,
no prelude change. `test/shader/Loops.test.scala` covers it in 23 cases, and
`ControlFlow.test.scala` was migrated rather than rewritten.

Four things worth recording:

**1. `unroll` over values takes an `Arr[T]`, not a `Seq[T]`.** The plan wrote
`Seq`, which would have dragged the Scala collection machinery into every
downstream bundle — the library's own rule. `Arr` is the native equivalent and
`Arr((0, 1), (1, 2))` reads the same at the call site.

**2. `IntExpr` needed Int-literal arithmetic and comparison overloads.** `i - 1`
did not compile: a bare `Int` cannot reach the `IntExpr` overloads, because
`Conversion[Int, FloatExpr]` would make it `f32`. Rather than write `i - 1.i` in
the first body that wanted it, `+ - * /` and `< <= > >= === !==` gained `Int`
forms in the same overload sets (`int_expr.scala`, `expr.scala`) — the
library-side fix the shader-DSL convention asks for.

**3. The colon-block form strands the comma inside an argument list.** It parses
and compiles, but `Block(a, loop(n): i => …, b)` leaves the separating comma on
its own line after the dedent. Inside an argument list the paren form
(`loop(n)(i => …)`) reads better, or hoist the loop to a named `val`. The colon
form is for statement position and last arguments. Documented in the guide.

**4. The uniform-control-flow question is settled, not assumed.** Three shaders
went through `naga` 30.0.1: the rewritten gradient (`for` over `i32(count)`), the
sphere trace (`while` with a compound condition and a `break`), and a
`textureSample` inside a loop bounded by a uniform — `Validation successful` for
all three. So a uniform bound does keep the body in uniform control flow; only a
per-fragment bound or break forces `.sampleLevel` / `.load`.

The examples: `examples/uniform_array_gradient/` is rewritten onto `loop` — its
`active(i)` masking and clamped denominator are gone, exactly as _The gradient,
rewritten_ predicted — and `examples/sdf_trace/` is new, marching per-fragment
with `loopIf` + `break` and reading the closest approach back as a soft shadow.
Both are registered in `examples/index.html`.

Still open from this milestone: `loopNamed` (deliberately not added — no
collision has happened yet), and in the sketch repo the `Arr[Stmt]` rewrites of
`sketches/textures/lines/` and `sketches/textures/moving-plates/`, which the
value-iterating `unroll` now makes possible.

---

## Milestone 3 — local arrays

Status: **deferred, and not committed to.** The decision waits until milestone
2's loop primitives have been implemented, tested and used — writing real loops
is what will show whether indexed mutable scratch is actually missing.

With uniform arrays (milestone 1) and loops (this one), the array story has
exactly one gap left: **there is no way to declare an indexable `var` inside a
shader body.** `UniformArray` is a binding — CPU-written, read-only in the
shader — and a local is a single scalar/vector/matrix. So anything that needs
_indexed mutable scratch_ is unreachable: sorting a small set, gather-then-scan,
a running history of the last N taps, any loop whose accumulator is per-slot
rather than a single value. It is also, concretely, what stops
`sketches/textures/lines/` from being loop-shaped at all: its three lines are
three separate `VarVec4` locals because `lines[i]` does not exist.

### Shape

WGSL wants `var lines: array<vec4<f32>, 3>;` at function scope (zero-initialised
by default). The DSL pieces line up:

```scala
val lines = VarArray[Vec4, 3]("lines")   // ad-hoc, like LetVec4("cur")

Block(
  lines.decl,                            // var lines: array<vec4<f32>, 3>;
  lines.set(0, vec4(…)),                 // element write
  …,
  loop(0, 3): i =>
    col := col.mix(lines(i).xyz, …),
)
```

- **Reads reuse `ArrayExpr[E]`** — the indexing surface milestone 1 already
  shipped, constant and `IntExpr` forms both. Keeping that type
  address-space-agnostic is the one thing milestone 2 must not break.
- **Writes are new.** `a(i)` returns the element expression `E`, which carries
  no `:=`. Either a method (`a.set(i, value): Stmt`) or an assignable accessor
  (`a.at(i) := value`, returning an `AssignTarget`). The second matches how
  `ctx.out.color := …` already reads; the first is one fewer concept.
- **`Var` only, not `Let`/`Const`.** Whether WGSL permits a runtime index into a
  _value_ (non-reference) array is exactly the sort of thing to verify with
  `naga` rather than assume; restricting local arrays to `var` sidesteps it.

### What it costs, honestly

**An explicit declaration statement**, which no other local needs. Every local
today declares itself on first `:=` (`expr.scala:64-68`); an array has no single
first assignment, so `var lines: array<vec4<f32>, 3>;` has to be emitted on its
own. In the ad-hoc style that is one extra statement in the `Block` (`lines.decl`
above) and nothing else — the same shape as declaring any other local, one line
longer — and it is the only local that needs one, which is worth a line in the
docs.

Plus one thing to say in the docs rather than discover: **dynamically indexing a
function-local array is a known performance cliff** — it can push the array out
of registers into scratch memory on some GPUs. Constant indices are free; a
loop-variable index is not always.

### Why it waits

Nothing in milestone 2's surface depends on local arrays existing, and this
repo's rule is that a capability waits for a real consumer — the two
`sketches/textures/` bodies are not it (they should stay unrolled).

The one thing milestone 2 owes this section is to **keep `ArrayExpr[E]` the
shared read surface for any array**, not a uniform-binding-specific type. That
costs nothing and is the only choice that would be expensive to reverse. The
local marker's name (`LocalArray[T, N]` above) and whether it is a second marker
or one array type with an address-space parameter can be settled when it is
built.

If milestone 3 is not picked up soon after, this section lifts out cleanly —
into `documents/independent-todos.md`, whose entries have exactly this shape
(gap, required changes, priority), or into its own plan. It should not linger
here as an appendix to a shipped feature.

---

## Still deferred: storage buffers

Unchanged by milestone 2, for the reasons in _They are not the same axis_ and
_Loops are the more fundamental of the two_ above: a uniform array holds 4096
vec4s, and nothing we are building comes near that. The revival condition stays
the one `grid-ceiling-rooms-plan.md` Part 5 §3 recorded — genuinely unbounded or
large data, scattered occluders casting into a lightmap being the example. Loops
landing first is what makes storage buffers _usable_ when that day comes; it is
not a step toward them.

## Result

### `examples/uniform_array_gradient/`

The example is the feature's demonstration and its test. Six horizontal bands,
**one shade**, each band binding its own arrays:

```scala
type Uniforms = (
    rect:   VertexUniform[Vec4],
    stops:  FragmentUniform[UniformArray[Vec4, MaxStops]],  // xyz = color, w = position
    curves: FragmentUniform[UniformArray[Vec4, MaxStops]],  // x = segment exponent
    count:  FragmentUniform[Double],
)
```

Everything is randomised per page load: **2 to 8 stops per band** (so the
per-draw `count` masking is exercised, not just described), random hues, jittered
stop positions, and a log-uniform exponent in [1/8, 8] per segment — one `pow`
per segment is the whole interpolation vocabulary here, deliberately — the
feature under demonstration is the array, not the easing.

The fragment body is the unrolled `mix` chain from _The gradient's own
representation_, built by a Scala `while` emitting `Arr[Stmt]` into
`Block(stmts)` — a worked instance of the build-time-unroll idiom, and the thing
a DSL loop would later replace.

`test/graphics/UniformArray.test.scala` covers the GPU-free half: the emitted
`var<uniform> stops: array<vec4<f32>, 4>;`, both index forms, `rows`/`rowBytes`,
element offsets in the CPU buffer, untouched rows staying zero, and the
over-capacity throw.

### Two changes from the plan

**1. `UniformArray` is a nominal `final class`, not an `opaque type` over
`Arr[T]`.** The DSL maps uniform types to expression types through the `ToExpr`
match type, and an opaque alias is not provably disjoint from `Vec2` / `Float` /
… outside its defining file. That stalls the _whole_ match — every existing case
started failing to reduce, not just the array one. A nominal class is trivially
disjoint. It costs one small object per uniform-array value, which is a setup or
per-frame cost at most.

**2. `UniformValue` gained `rowBytes` alongside `rows`.** The array's `write`
needs the element stride to place row _i_, and `constValue[TupleSize[F]]` cannot
reduce inside the generic given where `F` is abstract. `rowBytes` is abstract on
the trait and each instance spells it out as `constValue[TupleSize[<its F>]]`,
where `F` _is_ concrete — self-maintaining, no duplicated magic numbers.

### The WGSL smoke test, settled

Step 1 of the plan — does a bare `array<…>` work as a top-level uniform store
type, or does it have to be wrapped in a generated `struct`? — is **answered
yes**. The example's full generated shader passes `naga` (30.0.1, the validator
wgpu and Firefox use) with `Validation successful`, including
`var<uniform> stops: array<vec4<f32>, 8>;`, both index forms, and the unrolled
chain. No struct wrapper is needed.

**Confirmed on Tint (Chromium/Dawn) too** — the example renders, so both WGSL
implementations accept it. The question is closed; the `struct`-wrapper
contingency is not needed and can be forgotten.

## Implementation order (done)

1. `UniformValue.rowBytes` / `rows` + the `BufferBinding` allocation change —
   `StructArray.allocate[F](uv.rows)(0)`. No behavior change for existing
   bindings; the full test suite and every example still build.
2. `UniformArray`, `UniformArrayElem`, its `UniformValue` —
   `src/graphics/buffers/uniform_array.scala`.
3. `WGSLType[UniformArray[T, N]]` — `shader/types.scala`.
4. `ArrayExpr` + its export + the `ToExpr` case.
5. Prelude export.
6. The example and the unit test.
7. **Next:** milestone 2 — loop primitives in the DSL, above.

## Docs to update when this lands

- `documents/independent-todos.md` — nothing to remove; this is its own doc.
- `sketches/rooms/canvases/PLAN.md:209-217` and
  `documents/grid-ceiling-rooms-plan.md:1827-1851` — both say "declined, here
  are the revival conditions". Add one line to each pointing here, and **keep
  the reasoning**: those two call sites should still unroll their build-time
  constants.
- `documents/rust-painter/scala-port-comparison.md` if it tracks uniform kinds.
