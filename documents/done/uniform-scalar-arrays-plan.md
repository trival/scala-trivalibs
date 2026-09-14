# Uniform Arrays of Scalars and `Vec2`

Make `UniformArray[Double, N]` and `UniformArray[Vec2, N]` legal, binding an
`Arr[Double]` / `Arr[Vec2]` on the CPU and indexing it as `stops(i): FloatExpr`
/ `Vec2Expr` in the shader — with the 16-byte lane packing WGSL demands hidden
in the library instead of spelled out at every call site.

## Status: Implemented (2026-09-14)

Shipped as planned, with three deviations worth recording:

- **The `.bind` adaptation could not return an adapted *value*.** The spike
  proved the walk can refine a `transparent inline` result at *its own* call
  site, but that refinement is lost crossing into another `inline` method: the
  enclosing body is typed against its abstract parameters, so the value arrives
  as `Any` and `summonFrom[UniformValue]` fails to reduce. The walk therefore
  builds the binding itself — `derive.bindUniformFieldValue(device, existing,
  value)` — handing off to `storeUniformValue[A]` with `A` named explicitly in
  each branch. Same semantics, and the wrong-element-type guard is unchanged.
- **`ArrayAccess` is an opaque `Int` (the lane count), not a two-method trait.**
  The trait version put a class per element shape in every bundle that indexes
  an array: +3.4 KB on `uniform_array_gradient`. Collapsing it to the house
  opaque-type pattern with two string builders brought that to **+2.6 KB**, and
  the cost is paid only by bundles that use the feature — every other example
  got slightly *smaller*.
- **`asUniform` lives in `uniform_array.scala`**, next to `UniformArray`, rather
  than beside `asBinding` in `binding.scala`. Cohesion won; the naming
  precedent was the point, not the file.

Also landed along the way: `%` on `IntExpr` / `UIntExpr`, and
`checkFieldTypeImpl`'s mismatch branch factored out as
`checkAdaptableFieldType`, which is where the `Arr`-for-array-field case is
accepted and every other mismatch still errors.

**A fourth site landed after the fact: `panel.bind` takes a bare `Arr` too.**
It has no schema, so it sizes the buffer from the values — `UniformArray.valuesOnly`,
a `UniformValue` over the raw `Arr` whose row count is `rowsFor(length, lanes)`
rather than a capacity in the type. That makes the array's length the contract
there: it must match what the consuming shades declare, and a short one fails at
draw-time validation. Which is the trade `panel.bind` already makes for types —
it accepts a `Vec3` for a `Mat4` field and fails at runtime — so the array case
is now consistent with the door it sits in rather than the exception to it.
`asUniform[N]` remains the way to fill fewer than `N` there. Documented in
[gotchas](../../docs/guide/gotchas.md).

Two things that bit during that addition, both worth knowing:

- **`set`'s `Arr` overload wins when `T` is itself an `Arr`**, and then fails its
  `UniformArray` evidence — the panel path writes through `:=`, whose plain
  `(value: T)` alternative still wins on an exact match.
- **A `summonFrom` type pattern rebinds type variables.**
  `case elem: UniformValue[t, f]` introduces a *fresh* `t` that shadows the one
  bound by the enclosing value pattern; it has to be written ``UniformValue[`t`, f]``.

## Why it does not work today

A WGSL rule, not a library decision: in the **uniform** address space an array's
element stride must be a multiple of 16 bytes. Verified with `naga` (wgpu's WGSL
frontend):

```
var<uniform> a: array<f32, 8>;         → error: array stride 4 is not a multiple of the required alignment 16
var<uniform> a: array<vec2<f32>, 8>;   → error: array stride 8 …
```

The **storage** address space has no such rule —
`var<storage, read> a: array<f32, 8>` validates — but storage buffers stay
deferred (see _Out of Scope_).

So the array must be declared as `array<vec4<f32>, R>` whatever its elements
are, and the element→lane mapping has to live somewhere. Today that somewhere is
the call site.

## Current state

`UniformArrayElem` ([src/graphics/buffers/uniform_array.scala:37-41](../../src/graphics/buffers/uniform_array.scala#L37-L41))
has instances for exactly five types — `Vec3`, `Vec4`, `Mat2`, `Mat3`, `Mat4`
— each of which already strides a multiple of 16. `Float` / `Double` / `Vec2`
are a **compile error at the declaration**, with an `@implicitNotFound` message
telling the author to pack into `Vec4`s
([uniform_array.scala:26](../../src/graphics/buffers/uniform_array.scala#L26)).

That packing is what both consumers do by hand, wasting three lanes per value:

```scala
// sketches/textures/stepped-gradient/SteppedGradient.scala:71
out.push(Vec4(2.0.pow(randInRange(-3.0, 3.0)), 0.0, 0.0, 0.0))
// …read back in the shader as curves(i - 1).x
```

Three places assume **one CPU element = one buffer row = one WGSL array
element**:

- `WGSLType[UniformArray[T, N]]`
  ([shader/types.scala:111-124](../../src/graphics/shader/types.scala#L111-L124))
  emits `array<${inner.wgslName}, N>` and
  `byteSize = roundUp16(inner.byteSize) * N`.
- `UniformValue[UniformArray[T, N], F]`
  ([uniform_array.scala:86-117](../../src/graphics/buffers/uniform_array.scala#L86-L117))
  writes element _i_ at `base + i * elem.rowBytes` and reports `rows = N`;
  `BufferBinding` allocates `StructArray.allocate[F](uv.rows)`
  ([buffers/binding.scala:181](../../src/graphics/buffers/binding.scala#L181)).
- `ArrayExpr[E].apply`
  ([math/gpu/expr.scala:143-155](../../src/graphics/math/gpu/expr.scala#L143-L155))
  emits `name[i]`.

Note what is **already right**: the writer's dense stride. For `Double`,
`F = F32 *: EmptyTuple` and `elem.rowBytes = 4`, so writing N doubles at stride
4 _is_ the packed vec4 layout. For `Vec2`, `F = Vec2Buffer` and stride 8 puts
element 0 at `.xy` and element 1 at `.zw` of the same row. The CPU half needs no
new packing code at all — only a corrected row count.

## Design — lanes

One number describes the whole difference: how many array elements share a
16-byte row.

| Element          | lanes | WGSL declared             | element _i_ lives at          |
| ---------------- | ----- | ------------------------- | ----------------------------- |
| `Float`/`Double` | 4     | `array<vec4<f32>, ⌈N/4⌉>` | row `i/4`, component `i%4`    |
| `Vec2`           | 2     | `array<vec4<f32>, ⌈N/2⌉>` | row `i/2`, half `.xy` / `.zw` |
| `Vec3`           | 1     | `array<vec3<f32>, N>`     | row `i`                       |
| `Vec4`           | 1     | `array<vec4<f32>, N>`     | row `i`                       |
| `Mat2/3/4`       | 1     | `array<matNxN<f32>, N>`   | row `i`                       |

**`N` counts elements, not rows** — `UniformArray[Double, 8]` is eight floats,
two vec4 rows, 32 bytes. That is the whole point: the packing is invisible, so
the capacity is stated in the unit the author thinks in.

**`N` need not be a multiple of `lanes`.** `UniformArray[Double, 7]` and
`UniformArray[Vec2, 3]` are perfectly legal declarations; the row count rounds
up and the trailing lanes are dead padding:

| declared                 | rows      | WGSL                  | bytes | dead       |
| ------------------------ | --------- | --------------------- | ----- | ---------- |
| `UniformArray[Double,7]` | ⌈7/4⌉ = 2 | `array<vec4<f32>, 2>` | 32    | 1 lane     |
| `UniformArray[Vec2,3]`   | ⌈3/2⌉ = 2 | `array<vec4<f32>, 2>` | 32    | 1 half-row |

The padding is invisible from both sides: the shader can only index `0..N-1`
worth of live elements, and `N` stays the capacity the write path enforces —
`UniformArray[Double, 7]` still throws on an eighth value, even though the byte
that value would occupy exists. Rounding the capacity up to a lane boundary is
the author's choice to make for a reason (a `count` uniform's range, say), never
something the library does behind the type.

### 1. `UniformArrayElem[T]` carries the lane count

It stops being a bare marker:

```scala
trait UniformArrayElem[T]:
  /** Array elements per 16-byte uniform row. 1 for everything that already
    * strides a multiple of 16; 4 for scalars, 2 for Vec2. */
  def lanes: Int
```

with instances `Float`/`Double` → 4, `Vec2` → 2, `Vec3`/`Vec4`/`Mat2`/`Mat3`/
`Mat4` → 1. The `@implicitNotFound` message changes from "pack those into
Vec4s" to naming what _is_ supported (see _Decisions_ 2 for `Int`).

### 2. `WGSLType` emits the packed declaration

```scala
def rowCount = (n.value + e.lanes - 1) / e.lanes
def wgslName =
  if e.lanes == 1 then s"array<${inner.wgslName}, ${n.value}>"
  else s"array<vec4<f32>, $rowCount>"
def byteSize =
  if e.lanes == 1 then ((inner.byteSize + 15) / 16) * 16 * n.value
  else rowCount * 16
```

The `lanes == 1` branch is today's behaviour verbatim — `Mat3`'s 48-byte and
`Mat4`'s 64-byte strides must not be flattened to 16.

`byteSize` is not consumed by the binding path (`fieldByteSizes` serves vertex
attribute layouts only,
[shader/layouts.scala:40-51](../../src/graphics/shader/layouts.scala#L40-L51)), but
it states the declared size and the tests assert it, so it stays correct.

### 3. `UniformValue.rows` rounds up to a whole row

```scala
override def rows = ((n.value + lanes - 1) / lanes) * lanes
```

`rows` is counted in `F` rows, and `F` stays the **element** layout — so this is
`⌈N/lanes⌉ * lanes` elements' worth of bytes, i.e. exactly `rowCount * 16`, and
the allocation matches the declared WGSL type in every case:

| example                  | F                  | rows | bytes | WGSL                    |
| ------------------------ | ------------------ | ---- | ----- | ----------------------- |
| `UniformArray[Double,6]` | `F32`              | 8    | 32    | `array<vec4<f32>, 2>`   |
| `UniformArray[Vec2,5]`   | `Vec2Buffer`       | 6    | 48    | `array<vec4<f32>, 3>`   |
| `UniformArray[Vec4,4]`   | `Vec4Buffer`       | 4    | 64    | `array<vec4<f32>, 4>`   |
| `UniformArray[Mat3,2]`   | `Mat3PaddedBuffer` | 2    | 96    | `array<mat3x3<f32>, 2>` |

`write` / `read` are untouched: the existing `base + i * elem.rowBytes` loop
lands every element in its lane. Unwritten tail lanes stay zero, which is what a
`count` uniform masks off — unchanged from today.

### 4. The accessor — a given on the **element expression type**

The emitted index expression is the only genuinely new code. All three forms are
naga-validated (`array<vec4<f32>, R>` declared as a uniform):

```wgsl
a[i / 4][i % 4]                                   // scalar, computed index
a[1][2]                                           // scalar, constant index (folded in Scala)
select(a[i / 2].xy, a[i / 2].zw, (i % 2) == 1)    // vec2, computed index
a[1].zw                                           // vec2, constant index
a[i]                                              // lanes == 1, unchanged
```

`ArrayExpr[E]` carries only the element expression type, so the access shape has
to come from somewhere — a small typeclass, resolved on `E`:

```scala
trait ArrayAccess[E]:
  def const(base: String, i: Int): String
  def dyn(base: String, i: String): String

object ArrayAccess extends ArrayAccessLow:   // low-priority parent: the direct `base[i]`
  given ArrayAccess[FloatExpr] = …           // 4 lanes
  given ArrayAccess[Vec2Expr]  = …           // 2 lanes
```

and `apply` gains a using clause, leaving every call site (`stops(0)`,
`stops(i)`) spelled as it is today:

```scala
inline def apply(i: Int)(using acc: ArrayAccess[E]): E
inline def apply(i: IntExpr)(using acc: ArrayAccess[E]): E
```

**Why dispatch on `E` and not on the element type `T`.** `ToExpr`
([shader/dsl/types.scala:28](../../src/graphics/shader/dsl/types.scala#L28)) is a
match type, and splitting `UniformArray[Double, n]` off from
`UniformArray[Vec4, n]` there needs the element types to be _provably disjoint_
— the same wall that made `UniformArray` a nominal class in the first place
(`uniform-arrays-plan.md`, _Result_). Implicit search unfolds aliases correctly
where match types stall; the given-chain pattern in
[buffers/attributes.scala](../../src/graphics/buffers/attributes.scala) is the
precedent. The map is total and unambiguous in this direction too: `FloatExpr`
always means a packed scalar array, `Vec2Expr` always a half-packed one, every
other expression type a direct one.

Givens live in `object ArrayAccess`, which is in the implicit scope of
`ArrayAccess[E]` — no import anywhere.

Two details worth stating:

- **The `Vec2` computed form repeats the index expression three times.** WGSL
  expressions here are pure, so it is correct, and an accessor that returns an
  `Expr` cannot emit a `let` to bind it. Acceptable; if a future call site
  passes a heavy index, it can bind it itself (`val j = LetInt("j")`).
- **No parenthesisation of the incoming index.** Every compound `IntExpr` is
  already self-parenthesised by construction
  ([math/gpu/int_expr.scala:52-65](../../src/graphics/math/gpu/int_expr.scala#L52-L65)),
  which the rest of the DSL relies on as well. `i % 4` is built as a raw string
  here — `IntExpr` has no `%` operator yet (milestone 2 adds it).

## Design — binding an `Arr[T]` directly

Independent of lane packing, and the other half of "the packing is invisible":
`UniformArray` is the **only** uniform type whose value has to be wrapped at the
bind site.

```scala
.bind("rect" := Vec4(…))                          // every other type: the raw value
.bind("stops" := UniformArray[Vec4, MaxStops](arr))  // arrays: a wrapper, with the capacity restated
```

The constructor side already resolves —
`summon[UniformLayout[UniformArray[Vec4, 8]]]` works today (verified), so
`p.binding[UniformArray[Vec4, 8]]` and `p.binding(UniformArray[Vec4, 8](arr))`
both compile. Only the value wrapping is the odd one out.

### Where the capacity comes from

`N` never has to be restated, because each of the four sites already knows it:

| site                   | spelling                                         | `N` from                  |
| ---------------------- | ------------------------------------------------ | ------------------------- |
| one-shot bind          | `.bind("stops" := arr)`                          | the shade's schema        |
| declare a held binding | `val stops = p.binding[UniformArray[Vec4, 8]]`   | stated once, at the `val` |
| …with initial values   | `val stops = p.binding(arr.asUniform[MaxStops])` | the type argument         |
| update it              | `stops.set(arr)`                                 | the binding's own type    |

Row 1 is the adapt walk below. Rows 3 and 4 are two small, independent additions
— both probe-compiled against the real types.

**`asUniform` is the piece that stands on its own.** It depends on nothing in
this plan — not the lane packing, not the `set` overload, not the adapt walk —
and it shortens every `UniformArray` call site that exists **today**:

```scala
// today
.bind("stops" := UniformArray[Vec4, MaxStops](randomStops(count)))
// with asUniform alone
.bind("stops" := randomStops(count).asUniform[MaxStops])
```

The element type stops being restated at the call site, because the `Arr`
already carries it. That holds at `p.binding`, at `.bind`, and at `set` — so
even milestones 6 and 7, which remove the wrapper in their own positions,
benefit from it in the positions they do not reach. It is milestone 5 and can
land first.

```scala
// row 3 — capacity as a TYPE argument. The partial application sits on the Arr,
// where T is already the receiver's, so nothing is restated and no `binding`
// overload is needed: this feeds the EXISTING `binding[T](initial: T)`.
extension [T](values: Arr[T])
  inline def asUniform[N <: Int]: UniformArray[T, N] = new UniformArray[T, N](values)

// row 4 — an overload of `set` ON THE CLASS, not a new operator. E and N are
// recovered from the binding's own T through the evidence.
final class BufferBinding[T, F <: Tuple]:
  inline def set(value: T): Unit = …                                    // unchanged
  inline def set[E, N <: Int](values: Arr[E])(using T =:= UniformArray[E, N]): Unit = …
```

`p.binding(arr.asUniform[MaxStops])` infers
`BufferBinding[UniformArray[Vec4, 8], Vec4Buffer]` — verified by ascription, not
assumed. `asUniform` names itself after the existing `asBinding`
([buffers/binding.scala](../../src/graphics/buffers/binding.scala)).

Row 4 belongs on `set` itself — an overload of the method that already exists,
not a bespoke operator for one element type. `:=` is today a plain alias
(`inline def :=(value: T): Unit = set(value)`,
[buffers/binding.scala:155](../../src/graphics/buffers/binding.scala#L155)), and it
stays exactly that: an `Arr` goes through `stops.set(arr)`. Giving `:=` the same
evidence-guarded overload is a one-line follow-on if the alias should stay total
— but that is a separate call, not something this plan assumes.

The capacity stays a **type** at every site, which is what it is: a compile-time
constant that the schema, the buffer size and the WGSL declaration all agree on.
The same spelling also works as a bind value
(`"stops" := arr.asUniform[MaxStops]`) — today, and for as long as milestone 7
has not landed.

**The consumers' constant stays exactly as it is.** A sketch needs the capacity
as a term as well — `randIntInRange(2, MaxStops + 1)`
([SteppedGradient.scala:74](../../../sketches/textures/stepped-gradient/SteppedGradient.scala#L74))
— so the existing pair is already the right shape, and `asUniform[N]` consumes
the type half directly:

```scala
type MaxStops = 8
val MaxStops: Int = valueOf[MaxStops]   // derived from the type — cannot drift

type Uniforms = (stops: FragmentUniform[UniformArray[Vec4, MaxStops]])
val stops = p.binding(arr.asUniform[MaxStops])
val stopCount = randIntInRange(2, MaxStops + 1)
```

Two declarations, one source of truth: the `val` is `valueOf` of the type, not a
second literal. An `inline val MaxStops = 8` on its own is the one-declaration
alternative, at the cost of `MaxStops.type` in both type positions (verified to
work, including through `WGSLType`). Either is fine; nothing here forces a
change to the consumers.

Two alternatives were tried and rejected:

- **`p.binding(MaxStops, arr)`** — capacity as a _value_ argument, recovered as
  a type via `capacity.type` (the `NamedTuple.apply(n): Elem[V, n.type]` trick).
  It compiles, infers correctly, and needs no type argument at all — but it
  turns a compile-time constant into something that _looks_ like a runtime
  parameter, and it only works for a literal or an `inline val`: a plain
  `val cap: Int = 8` silently yields `UniformArray[Vec4, (cap : Int)]`, which
  then fails to match the schema's `UniformArray[Vec4, 8]` somewhere else
  entirely (`ValueOf` resolves for both, so it does not self-correct). That trap
  is not hypothetical here: the consumers' `val MaxStops: Int = valueOf[MaxStops]`
  is exactly such a non-literal `Int`, so this form would break today's constant
  and force it to an `inline val`. All verified; all reasons to prefer the type
  argument.
- **A capacity-only `p.binding[8](arr)`** — does not work. Scala has no partial
  type application, and the curried-builder workaround collides with the
  existing overload: probing it, `p.binding[8](arr)` resolves to
  `binding[T: UniformLayout](initial: T)` with `T = 8` and then demands an
  `initial: 8`. Moving the partial application onto the `Arr`, as row 3 does, is
  what makes the same shape work.

`Arr[T]` cannot have a `UniformValue` of its own — `N` is not inferable from a
value. But it does not have to be: the capacity is **already in scope at the
bind site**. `checkFieldTypeImpl`
([shader/derive.scala:286-325](../../src/graphics/shader/derive.scala#L286-L325))
walks the schema by field name and binds `type Expected = UnwrapUniform[head]`
— which is `UniformArray[Vec4, 8]`. It is used only for an equality check today.

So: make that walk return an adapted value as well as check it. **This was
spiked end to end and works** — the code below is the spiked shape, not a
sketch:

```scala
// wrap ONLY when V really is an Arr of the field's element type
transparent inline def wrapIfArray[Expected, V](value: V): Any =
  inline erasedValue[Expected] match
    case _: UniformArray[t, n] =>
      summonFrom:
        case _: (V <:< Arr[`t`]) => new UniformArray[t, n](value.asInstanceOf[Arr[t]])
        case _                   => value          // wrong element type, or already wrapped
    case _ => value                                // not an array field at all

transparent inline def adaptImpl[Name <: String, V, Names <: Tuple, Types <: Tuple](
    value: V,
): Any =
  inline (erasedValue[Names], erasedValue[Types]) match
    case (_: EmptyTuple, _) => error("Binding name not found in Uniforms type")
    case (_: (name *: namesRest), _: (head *: typesRest)) =>
      inline constValue[name] match
        case _: Name => wrapIfArray[UnwrapUniform[head], V](value)
        case _       => adaptImpl[Name, V, namesRest, typesRest](value)
```

`processEntry`'s `rawValue` branch
([painter/shape.scala:243-253](../../src/graphics/painter/shape.scala#L243-L253))
then adapts first and checks the adapted type, so its existing
`summonFrom { case uv: UniformValue[A, f] }` resolves with nothing else changed.

What the spike established, against a schema **alias** (`type Uniforms = (…)`,
the case that defeats match types), each proved by ascription at the call site:

| case                                     | result                                |
| ---------------------------------------- | ------------------------------------- |
| `Arr[Vec4]` → `UniformArray[Vec4, 8]` field | wrapped; `UniformValue` then resolves |
| already-wrapped `UniformArray[Vec4, 8]`  | passes through untouched              |
| `Arr[Double]` → `UniformArray[Vec4, 8]` field | falls through **as `Arr[Double]`** — not cast-wrapped |
| `Double` → a `count: Double` field       | untouched; resolves as today          |

Two constraints the implementation must keep:

- **A match type cannot shortcut it.** `UniformFieldType[Name, U]` is the
  obvious spelling, but `U` is almost always a schema _alias_, and named-tuple
  aliases do not reduce in match types. It stays the inline walk — the same
  reason `attributes.scala` uses given chains. (`derive` already has an unused
  `uniformFieldType[Name, U]: Any` walk of this exact shape,
  [derive.scala:234-255](../../src/graphics/shader/derive.scala#L234-L255); the
  adapting walk is its value-carrying sibling.)
- **The `V <:< Arr[t]` guard is what makes it safe, and adapt still runs before
  the check.** Without the guard the wrap is an unchecked cast: `"stops" := Arr(1.0)`
  against a `Vec4` array would compile and write garbage. With it, a mismatched
  `Arr` falls through the identity case and hits the existing
  "binding type mismatch" error — verified, row 3 above.

End state: every uniform field takes its natural CPU value, and the capacity is
stated once, where it is declared.

```scala
.bind("stops" := Arr(Vec4(…), Vec4(…)), "curves" := Arr(1.0, 2.0, 0.5))
```

## Milestones

1. **Lanes.** `UniformArrayElem.lanes` + the five existing instances + `Float`,
   `Double`, `Vec2`. `WGSLType` row/byte arithmetic, `UniformValue.rows`. Tests:
   emitted `wgslName` and `byteSize` for `Double` N=6 and `Vec2` N=5; `rows` for
   both; `Vec3` / `Mat3` / `Mat4` unchanged; a write/read round-trip asserting
   element 4 of a `Double` array lands at byte 16 (row 1, lane 0) and element 1
   of a `Vec2` array at byte 8. N=6 and N=5 are both ragged on purpose — add
   that `UniformArray[Double, 7]` throws on a 8th value, so the padded row does
   not quietly raise the capacity.
2. **Accessor.** `ArrayAccess` + the `apply` using clauses. Tests: constant and
   computed index for each of the three shapes, against the WGSL strings above;
   the existing `"stops[2]"` / `"stops[i]"` tests
   ([test/graphics/UniformArray.test.scala:75-85](../../test/graphics/UniformArray.test.scala#L75-L85))
   keep passing unchanged. **Along the way: `%` for `IntExpr`** — both the
   `IntExpr` and the `Int`-literal form, in the same overload block as
   `+ - * /` ([math/gpu/int_expr.scala:50-68](../../src/graphics/math/gpu/int_expr.scala#L50-L68))
   for the reason its comment gives, plus the matching pair on `UIntExpr`. The
   accessor emits its own WGSL and does not need it; it is simply the operator
   this work makes conspicuous by its absence.
3. **Invert the negative tests.** `UniformArray.test.scala:41-49` currently
   asserts `Float` and `Vec2` do **not** compile — they become positive
   controls, and a still-unsupported element type takes over as the negative
   (`Int`, which has no `UniformValue`).
4. **Docs.** The element-type table in
   [documents/done/uniform-arrays-plan.md:279-297](uniform-arrays-plan.md)
   is superseded — leave the done doc alone and state the current rule in
   [docs/guide/shader-dsl-guide.md](../../docs/guide/shader-dsl-guide.md), with the
   lane table and the "`N` counts elements" sentence. Update the
   `@implicitNotFound` message. A gotcha entry is only worth it if the index
   duplication above ever bites.
5. **`asUniform`** — **depends on nothing, and can land first.** The
   `Arr[T].asUniform[N]` extension, beside `asBinding` in
   `buffers/binding.scala`. It shortens every `UniformArray` call site that
   exists today, before any of 1–4: `p.binding`, `.bind`, and a `set`. Tests:
   `arr.asUniform[8]` types as `UniformArray[Vec4, 8]` and writes the same bytes
   as `UniformArray[Vec4, 8](arr)`; `p.binding(arr.asUniform[8])` types as
   `BufferBinding[UniformArray[Vec4, 8], Vec4Buffer]`.
6. **`Arr` at `set`** — independent of 1–5 and of 7; probe-verified, no spike.
   The evidence-guarded `set` overload on `BufferBinding` itself. Tests: it
   writes the same bytes as the wrapped form; `set(value: T)`, `:=` and
   `p.binding(uniformArrayValue)` still resolve; the overload does not apply to
   a non-array binding; a `typeChecks` negative for an `Arr` of the wrong
   element type.
7. **Raw `Arr` at `.bind`** — **spiked; no longer gated.** Add the adapting
   walk beside `derive`'s existing `uniformFieldType`, with the `V <:< Arr[t]`
   guard, and route `processEntry`'s raw-value branch through it (adapt, then
   check the adapted type). Tests: `"stops" := Arr(…)` against a `UniformArray`
   field binds and writes the same bytes as the wrapped form; an already-wrapped
   `UniformArray` value still binds unchanged; a `BufferBinding` value still
   binds unchanged; `typeChecks` negatives — an `Arr` of the wrong element type,
   and an `Arr` against a non-array field — still fail with the
   binding-mismatch error, not a cast failure. The one thing the spike did not
   cover is expansion cost: `processEntry` is `inline` and expands at every
   `.bind` call site, so compare a full `examples:build` before and after for
   compile time and bundle size, and keep the walk's per-field work to the
   `constValue[name]` comparison it already does.
8. **Consumers.** `examples/uniform_array_gradient` binds `curves` as
   `UniformArray[Double, MaxStops]` (`Arr[Double]`, read as `curves(i - 1)`),
   which deletes the `Vec4(x, 0, 0, 0)` boxing and three quarters of that
   buffer — and, with 5 to 7, the `UniformArray[…](…)` wrapper at both bind
   sites with it. The consumer sketch `sketches/textures/stepped-gradient` is
   the acceptance case, ported after the library lands.

## Decisions

Nothing here is open; each is recorded so it does not come back as a question.

1. **`Vec3` stays lanes = 1 — padded, exactly as a `Vec3` uniform already is.**
   A uniform `Vec3` is written through `Vec4Buffer` and read as `vec3<f32>`
   today ([buffers/binding.scala:67-75](../../src/graphics/buffers/binding.scala#L67-L75));
   a `Vec3` array does the same, one element per 16-byte row, declared
   `array<vec3<f32>, N>` and left to WGSL's own stride padding. Packing 4
   elements into 3 rows was considered and dropped: it would save a quarter of
   the bytes, and cost the property every other element type has — that one
   element is one row and the accessor is a single indexing expression.
2. **Integer arrays are deferred, as a package.** `Int` / `UInt` / `IVec*` /
   `UVec*` have a `WGSLType` but no `UniformValue`, so they cannot be bound at
   all — an `array<i32>` uniform is out of reach for that unrelated reason, not
   because of anything here. If integer uniforms are ever wanted, all forms land
   at once rather than piecemeal; the lane rule then applies to them unchanged
   (4 for the scalars, 2 for `IVec2`/`UVec2`, 1 for the rest). Noted without a
   plan in [independent-todos.md](../independent-todos.md).
3. **A per-binding WGSL accessor `fn`** (`fn stops_get(i: i32) -> f32 { … }`,
   auto-registered like `WgslFn`) would read better in the generated WGSL and
   remove the `Vec2` index duplication, at the cost of tying `ArrayExpr` to the
   fn-registration machinery. Rejected for now; revisit only if generated
   shaders become hard to read.

## Out of Scope

- **Storage buffers**, which would make all of this unnecessary for large
  arrays — and remain the right answer for anything not bounded by ~16 elements.
  Unchanged from the earlier decision.
- **Arrays of structs** (`array<Stop, N>`) — needs struct uniforms first.
- **Per-element upload**, **runtime-resizable capacity** — as in the original
  plan.
