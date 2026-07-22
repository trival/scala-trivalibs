# Vec ↔ Tuple ↔ Expr Interop

Make the CPU `Vec2`/`Vec3`/`Vec4` classes the single authoring representation
for vector-shaped constants, convertible on demand into the two other forms they
already coexist with: plain tuples (`Vec3Tuple` & friends) and GPU shader
expressions (`Vec3Expr` & friends). Retype the color-shaped public APIs — today
all tuples — to `Vec4`.

Companion documents:

- [`src/graphics/math/api_and_naming_conventions.md`](../src/graphics/math/api_and_naming_conventions.md)
  — the three-representation / three-layer split this plan slots into.
- [`documents/done/math-consolidation-design.md`](done/math-consolidation-design.md)
  — established the `math/` → `math/cpu/` + `math/gpu/` layering.

---

## 1. Context

The math library defines three representations per vector type
(`api_and_naming_conventions.md`, lines 9–13): the mutable `Vec3` class, the
immutable `Vec3Tuple`, and the GPU-side `Vec3Expr`. The doc comment at
`src/graphics/math/cpu/vec3.scala:16` claims they "share method names and
convert via givens" — but the givens are operation type classes, not
`Conversion`s. **There is no conversion between the representations at all.**
`toTuple`, `toExpr`, `asTuple` and `toVec` return zero hits across
`trivalibs/src`.

The cost shows up wherever one value needs to appear in more than one form.
`sketches/rooms/canvases/Canvases.scala` writes the same conceptual thing three
different ways:

| Line(s)                | Form                                   | Why                                  |
| ---------------------- | -------------------------------------- | ------------------------------------ |
| 218, 237, 243, 253–254 | `vec3(0.86, 0.86, 0.85)`               | GPU literal, restated per site       |
| 549                    | `val wallColor = (0.9, 0.9, 0.9, 0.0)` | tuple, because `ClearColor` is one   |
| 508–513                | `Arr(Vec3(0.78, 0.30, 0.28), …)`       | CPU class — the form that reads best |

The shader-side literals can't be lifted into shared constants at all, because a
`Vec3Expr` can only be built inside shader-construction code. And
`GridCeiling.scala:582` shows the tuple boundary being paid manually:

```scala
clearColor = (fogColor.x, fogColor.y, fogColor.z, 1.0)
```

### Goals

1. Author a vector constant once, as a CPU `Vec*`; use it in shader code, in
   tuple-shaped parameters, and in color parameters without restating it.
2. All conversions available implicitly, so existing call sites keep compiling.
3. Expression-related functionality confined to `math/gpu/`; tuple conversion
   lives directly on the `cpu` classes.

### Non-goals

- **`Mat*` ↔ tuple.** No use case has come up for expressing a matrix as a
  tuple, and there is a concrete hazard — see §2.3.
- **Collapsing the `Vec3(…)` / `vec3(…)` case distinction.** The capitalisation
  is load-bearing documentation: it tells the reader at a glance whether an
  expression is CPU or GPU domain. Nothing here changes that. The adjacent shape
  changes it _would_ permit are evaluated in §2.6 rather than dismissed.

---

## 2. Design

Three layers, mirroring the existing split.

### 2.1 `math/cpu/` — vector ↔ tuple

In each of `cpu/vec2.scala`, `cpu/vec3.scala`, `cpu/vec4.scala`, add to the
class companion, alongside the existing `given Vec3ImmutableOps[Vec3]`:

```scala
extension (v: Vec3)
  inline def toTuple: Vec3Tuple = (v.x, v.y, v.z)

extension (t: Vec3Tuple)
  inline def toVec3: Vec3 = new Vec3(t._1, t._2, t._3)

given Conversion[Vec3, Vec3Tuple] = _.toTuple
given Conversion[Vec3Tuple, Vec3] = _.toVec3
```

Companion placement puts these in implicit scope for both source and target with
no import, consistent with `cpu/package.scala:12-22` re-exporting `Vec3.given`.
`inline` keeps the emitted JS to a bare tuple or constructor literal — no
wrapper function per conversion.

> **Amended during implementation — split across two locations.** The
> `given Conversion`s stay in each class companion (implicit scope, no import
> needed — this is what keeps the untouched `clearColor = (r, g, b, a)` call
> sites compiling). The _named_ `toTuple` / `toVecN` methods moved to a single
> new file `cpu/tuple_interop.scala`, for two reasons: companion members are not
> reached by `import …math.cpu.*` (the companion is not exported, only its
> `.given` is), and Scala 3 requires all overloads of a top-level name to live
> in one group of top-level definitions — `toTuple` is overloaded per arity.
> Same arrangement as `cpu/swizzles.scala`.

`Vec2Tuple` / `Vec3Tuple` / `Vec4Tuple` already exist (`cpu/vec3.scala:50` and
siblings). Reuse them; introduce no new aliases.

### 2.2 `math/gpu/` — CPU value → GPU expression

New file `src/graphics/math/gpu/cpu_interop.scala`, package
`trivalibs.graphics.math.gpu`:

```scala
extension (v: Vec3)
  def toExpr: Vec3Expr = Vec3Expr(
    s"vec3<f32>(${floatToWgsl(v.x)}, ${floatToWgsl(v.y)}, ${floatToWgsl(v.z)})",
  )
```

> **Amended during implementation — no `given Conversion[Vec*, Vec*Expr]`.** The
> plan originally paired each `toExpr` with an implicit conversion, so that
> plain positions would need no ceremony. That turned out to be unshippable: a
> `Conversion[Vec3, Vec3Expr]` makes every **GPU** extension applicable to a
> **CPU** value, which collides with the identically-named CPU ones. `v3.xy`
> then fails with _"Ambiguous extension methods: both `cpu.xy(v)` and
> `gpu.xy(conversion(v))`"_ in any file importing both namespaces — i.e. the
> standard sketch preamble. It broke 36 assertions in
> `test/math/Swizzle.test.scala`, including one named _"CPU + GPU swizzles
> coexist (sanity)"_ — an invariant the library deliberately tests for.
>
> Dropping the conversions cost nothing measurable: receiver position never
> worked anyway (§4), and plain positions are served by the `vec2`/`vec3`/`vec4`
> constructors (§2.6). Tests, examples and all seven sketches compile unchanged
> without them. **Crossing into the GPU domain is therefore always explicit.**
> The `Vec* ↔ tuple` conversions of §2.1 are unaffected — tuples have no
> competing GPU extension set.

Same for `Vec2` / `Vec4`, and for `Mat2` / `Mat3` / `Mat4` emitting column-major
`mat4x4<f32>(…)` to match what the existing `Mat*Expr` ops emit.

- Reuse `floatToWgsl`, already used by the scalar literal conversions at
  `gpu/float_expr.scala:20-22`, so literal formatting stays consistent.
- **Not** `inline`, and not subject to bundle-size scrutiny: these run at shader
  _build_ time, producing a WGSL string once at startup. Never in the render
  loop.
- Unlike §2.1 these sit at package level rather than in a companion — the
  `Vec*Expr` opaque types are nested inside `object Expr` (`gpu/expr.scala:69`),
  so there is no reachable companion to attach them to. Sketches already write
  `import trivalibs.graphics.math.gpu.{*, given}`, which picks them up.

**Note on `vec3.apply` overloads.** Per `CLAUDE.md:240-254`, Scala won't apply
implicit conversions through an overloaded method set, so adding a `Vec3`
overload naively would break the existing `vec3(0.5)` / `Double → FloatExpr`
path. The precise trigger is a **same-arity** collision — `object vec3`'s
current overloads differ in arity, so they resolve by arity before conversion.
That makes this a constraint rather than a prohibition: §2.6 shows the overload
is viable when paired with an explicit `apply(scalar: Double)`. The standalone
`given Conversion` specified here is unaffected either way.

### 2.3 Why matrices stay tuple-free

`Mat2Tuple` is structurally identical to `Vec4Tuple` — both are
`(Double, Double, Double, Double)`. This is not a new observation:
`cpu/package.scala:24` already declines to export the `Mat2Buffer` / `Mat2Tuple`
givens precisely because they collide with `Vec4`'s.

A `Conversion[Tuple4, Mat2]` would therefore make `Conversion[Tuple4, Vec4]`
ambiguous at every call site. Omitting matrix tuple conversions isn't just scope
trimming — it's the only option that works. `Mat*` gets `toExpr` only.

### 2.4 Color APIs → `Vec4`

Every color-typed API in the codebase is 4-component; there are **no**
3-component color tuples. Alpha is load-bearing (`Canvases.scala:549` uses `0.0`
for a transparent mirror clear; both mirror helpers default to `(0, 0, 0, 0)`).
So this is uniformly `Vec4`.

`src/graphics/painter/panel.scala:11`:

```scala
type ClearColor = Vec4
```

Keep the alias — it documents intent at the declaration sites. (Note it was
already a duplicate of `Vec4Tuple`; now it is a duplicate of `Vec4`.)

Retype to `ClearColor`:

| File:line           | Current                                                                                       |
| ------------------- | --------------------------------------------------------------------------------------------- |
| `panel.scala:70`    | `private[painter] var clearColor: Opt[ClearColor] = null`                                     |
| `panel.scala:232`   | `Panel.set(clearColor: Maybe[Opt[ClearColor]] = …)`                                           |
| `painter.scala:732` | `Painter.panel(clearColor: Maybe[Opt[ClearColor]] = …)`                                       |
| `painter.scala:775` | `Painter.draw(…, clearColor: Opt[(Double × 4)] = null)` — spelled inline, switch to the alias |

Plus, in the sketch-utils repo layer:
`src/utils/mirror/MirrorReflection.scala:252` and
`GaussianMirrorReflection.scala:216` → `clearColor: Vec4 = Vec4(0.0)`, with
their docstrings at `:232` / `:195` updated.

Only two sites destructure, and they are the only consumers:

- `painter.scala:782` — `val (r, g, b, a) = clearColor`
- `painter.scala:824` — `val (r, g, b, a) = panel.clearColor`

Both feed an `Obj.literal(r =, g =, b =, a =)` at `:786` / `:830`. Replace with
`.r/.g/.b/.a`, which `Vec4BaseG` already provides. The `notNull` guards at
`:781` / `:823` stay valid — `Vec4` is a reference type, so `Opt[Vec4]` is still
nullable. (Arguably more natural than nullable tuples.)

**Copy on set.** `Vec4` is a mutable class (`class Vec4(var x: Double, …)`), so
retaining a caller's instance aliases it — a later `wallColor.x = …` would
silently change the clear color. `Panel.set` (`panel.scala:246`) and the two
mirror forwarding sites (`MirrorReflection.scala:309`,
`GaussianMirrorReflection.scala:307`) copy into their own instance. One
allocation at setup time, never in the render loop.

**Call-site churn: none — verified.** The concern was that `Maybe[Opt[Vec4]]`
expands to a _union_ (`Maybe[T] = js.UndefOr[T]`, `Opt[+A] = A | Null`), so a
tuple literal would need a conversion into a union position rather than the
plain subtyping it enjoys today (`Tuple4` **is** `Vec4Tuple`). Implicit search
does not generally decompose union targets.

Compiled against 3.8.4 with the real signature shapes: it works. All three
positions accept a bare tuple literal —

```scala
def plain(clearColor: ClearColor): Unit               // OK
def opt(clearColor: Opt[ClearColor] = null): Unit     // OK
def wrapped(clearColor: Maybe[Opt[ClearColor]]): Unit // OK
```

So all 21 existing `clearColor = (0.05, 0.06, 0.1, 1.0)` sites across
`examples/` and `sketches/` keep compiling untouched.

**One flag required.** Each conversion _use site_ emits a feature warning ("Use
of implicit conversion … should be enabled by adding
`import scala.language.implicitConversions`"). These are invisible in a normal
build — without `-feature` you only get a `there were N feature warnings`
summary line — which is why the codebase carries zero `implicitConversions`
imports today while running 8+ `given Conversion`s. This change would add ~21
more silent warnings. Fix once, in **both** `project.scala` files:

```scala
//> using option -language:implicitConversions
```

Verified to silence every position including the union-wrapped ones.

### 2.5 Evaluated and rejected: `into` (SIP-71)

`into` is reachable — the project is on Scala 3.8.4 and the SIP shipped as a
preview in 3.8.0. It was tested against the actual signature shapes rather than
assessed on paper. **Not adopted**, for three reasons:

1. **It does not address the §4 receiver-position risk at all.** The SIP covers
   parameter types and function result types only; the receiver of a
   method/extension call is out of scope. The one genuinely uncertain part of
   this design is untouched by it.

2. **It fails exactly at the motivating case.** `into class Vec4` removes the
   feature warning when the parameter type is _directly_ `Vec4` — but not for
   `Opt[Vec4]` or `Maybe[Opt[Vec4]]`, because those expand to unions whose other
   members (`Null`, `Unit`) are not valid `into` targets. Every `clearColor`
   signature in the painter is one of the wrapped forms:

   ```scala
   into class Vec4(…)                  // -preview required
   def plain(c: Vec4)                  // warning gone   ✅
   def opt(c: Opt[Vec4])               // warning remains ❌
   def wrapped(c: Maybe[Opt[Vec4]])    // warning remains ❌
   ```

   Scheme A of the SIP — the `into[T]` type constructor, which _could_ reach
   inside the union via `type ClearColor = into[Vec4]` — **is not implemented in
   3.8.4**. Neither `into[Vec4]` nor `scala.into[Vec4]` resolves; only the
   `into` modifier landed.

3. **It costs a project-wide `-preview` flag** for a feature explicitly subject
   to change in the next minor release — a poor trade for a JS project this size
   when the one-line `-language:implicitConversions` above solves the whole
   problem uniformly.

**Re-checked against Scala 3.9.0-RC1** (the next LTS, and trivalibs' intended
minimum). The 3.9 release notes say _"Make `into` type and modifier stable
#26184 (SIP-71, previously a preview feature in 3.8)"_, so the natural question
was whether 3.9 delivers Scheme A and makes `-preview` unnecessary. Tested:

- **`into class Vec4` is stable in 3.9** — compiles with no `-preview` flag. ✅
- **Scheme A does not exist.** `into[Vec4]` is `Not found: type into`, and so
  are `scala.into`, `scala.language.into`, `scala.Predef.into`,
  `scala.annotation.into`. The prefix form `into Vec4` is a parse error. The
  `into[T]` type constructor from the SIP text **was dropped from the shipped
  design**; "into type" in the release note refers to the modifier being allowed
  on classes, traits, and _opaque type aliases_.
- **The union limitation is unchanged in 3.9.** With `into class Vec4`,
  `def plain(c: Vec4)` is warning-free, but `Opt[Vec4]` and `Maybe[Opt[Vec4]]`
  both still warn — identical to 3.8-with-`-preview`.

So there is no future migration path via `into[T]`, and no version of `into`
covers the painter's `clearColor` signatures. `-language:implicitConversions`
remains the correct and complete solution. Enabling `-preview` buys nothing.

**Adopted instead (done):** both `project.scala` files now carry

```scala
//> using option -feature
//> using option -language:implicitConversions
```

`-feature` makes conversion sites visible rather than hidden behind a
`there were N feature warnings` summary; `-language:implicitConversions` enables
them deliberately, project-wide, which matches how this codebase works.
Verified: `trivalibs/src` and `sketches/rooms/canvases` both compile with
**zero** feature warnings under these flags. (One unrelated pre-existing
deprecation remains: `src/preact/component.scala:92`, `memberFields` →
`fieldMembers`.)

### 2.6 Adjacent shape changes — evaluated

Three shapes were considered beyond the plain `Conversion`. All were compiled
**against the real trivalibs types**
(`scala-cli compile <probe> trivalibs/src`), not modelled — a simplified model
gave a false positive on option B and had to be discarded.

#### ⛔ Rejected: mixed-domain operators on the CPU `Vec3`

The tempting fix for §4: declare the cross-domain operator directly, mirroring
the existing left-scalar pattern (`Double * Vec3Expr`, `gpu/expr.scala:563`).

```scala
extension (v: Vec3)
  @targetName("vec3MulFloatExpr") def *(e: FloatExpr): Vec3Expr = v.toExpr * e
  @targetName("vec3MulVec3Expr")  def *(e: Vec3Expr):  Vec3Expr = v.toExpr * e
```

It compiles (with `@targetName`, since the opaque `Expr` types all erase to
`Expr`). **It also silently breaks CPU arithmetic:**

```
val d: Vec3 = CeilTint * 2.0
              ^^^^^^^^^^  Found: gpu.Vec3Expr  Required: cpu.Vec3
val e: Vec3 = CeilTint * Vec3(1.0, 1.0, 1.0)
              ^^^^^^^^^^^^^^^^^^^^^^^^^  Found: gpu.Vec3Expr  Required: cpu.Vec3
```

A _direct_ extension method takes precedence over the one supplied by
`given Vec3ImmutableOps[Vec3]`. So `CeilTint * 2.0` binds the new overload (via
`Double → FloatExpr`) and yields a GPU expression instead of a CPU vector — and
`CeilTint * someVec3` does the same via `Vec3 → Vec3Expr`. Every existing CPU
`Vec3` multiplication in the codebase changes meaning. **Do not do this.** It is
strictly worse than the `.toExpr` it was meant to avoid, because it fails
silently at sites that still typecheck.

#### ✅ Adopted: `vec2/vec3/vec4(cpuVec)` constructor overloads

```scala
object vec3:
  def apply(scalar: FloatExpr): Vec3Expr = …   // existing
  def apply(scalar: Double): Vec3Expr = …      // NEW — required, see below
  def apply(v: Vec3): Vec3Expr = v.toExpr      // NEW
```

This _preserves_ the case distinction rather than eroding it — `vec3(CeilTint)`
reads as "lift this CPU value into the GPU domain", with the lowercase marking
the domain crossing explicitly. It gives a natural spelling at operator sites
that receiver conversion can't serve (§4): `vec3(CeilTint) * noise`.

**The constraint:** `object vec3` is already overloaded, but at _distinct
arities_, so `vec3(0.5)` resolves by arity first and then converts. Adding
`apply(v: Vec3)` introduces a second **1-arity** overload, and Scala will not
apply an implicit conversion through an overloaded set — so `vec3(0.5)` would
break unless `apply(scalar: Double)` is added explicitly alongside. That is
exactly the established in-repo pattern; see the comment at
`gpu/float_expr.scala:56-67`, which documents the same trap for `NumOps`.

Verified working in a standalone model (`vec3(0.5)`, `vec3(1.0, 0.5, 0.25)` and
`vec3(CeilTint)` all compile together). Confirm against the library during
implementation.

**Scope:** apply to all three of `vec2` / `vec3` / `vec4`. Each needs its CPU
overload _plus_ the explicit `apply(scalar: Double)` guard. Check each
constructor's existing overload set for other same-arity collisions before
adding — `vec4` in particular already has `apply(xyz: Vec3Expr, w: FloatExpr)`
and `apply(xy: Vec2Expr, z, w)`, so only the 1-arity slot is at issue there.

**Same-arity only.** Each constructor takes the CPU vector of its _own_ arity —
`vec3(v: Vec3)`, never `vec3(v: Vec4)`. Narrowing is spelled at the call site
with the existing inline swizzles in `cpu/swizzles.scala:22-33`, which compose
with these overloads for free:

```scala
vec3(tint.rgb)    // color — alpha explicitly discarded
vec3(v4.xyz)      // geometric
vec2(v3.xy)
vec2(v4.zw)       // which half is taken stays visible
```

Rationale for adding no narrowing overloads:

- **`.rgb` states the intent a constructor would hide.** "Take the color
  channels, drop alpha" is what `.rgb` _means_; `vec3(tint)` only implies it.
- **It would defeat the purpose.** With `vec3(Vec4)` available, `vec3(tint)` is
  legal whether `tint` is a `Vec3` or a `Vec4` — a reader can no longer infer
  the arity from the call, and the difference now silently changes behaviour.
- **CPU/GPU symmetry.** WGSL forbids `vec3<f32>(v4)`, so the GPU side has no
  such overload and cannot have one. A CPU-only narrowing constructor would make
  `vec3(x)` compile or fail depending on which domain `x` came from — the wrong
  asymmetry to introduce in mixed-domain code.
- Fewer same-arity overloads also means fewer collisions to guard.

#### ✅ Adopted: `toExpr` — and no type rename

The crossing method is **`toExpr`**, and the `*Expr` types keep their names.

A `toGpu` spelling was considered, on the grounds that the hardware barrier is
what a reader needs to see when CPU and GPU code share a function, as they do
throughout `Painter.init`. Rejected, on these grounds:

- **Name symmetry.** `toTuple` yields `Vec3Tuple`; `toExpr` yields `Vec3Expr`.
  Each conversion is named for the representation it produces.
  `toGpu → Vec3Expr` breaks that pairing for no gain.
- **One way to do things.** An alias pair (`toGpu` = `toExpr`) was considered
  and rejected outright — two spellings for one operation is worse than either
  alone; it duplicates the ambiguity instead of resolving it.
- **`Expr` says AST, not value.** These are nodes you can assemble, fold and
  reuse at build time, which is a genuine distinction from a CPU `Vec3` — and it
  is what the method produces.

**Not** claimed as a reason: that `Expr` is backend-neutral. It isn't. The base
class is `class Expr(val wgsl: String)` (`gpu/expr.scala:25`) and every
construction site emits WGSL syntax literally (`s"vec3<f32>(…)"`). Retargeting
would mean rewriting every emitter, not swapping a backend — so "`toExpr`
survives a second backend" is not a live argument today, and this decision does
not lean on it.

The domain crossing stays visible at the call site through `vec3(…)` versus
`Vec3(…)` (the case convention, §Non-goals) and the explicit `.toExpr` call
itself, rather than through a hardware word in the method name.

**Revisit after implementation.** This is a naming call made before seeing the
crossings in real sketch code. Once `Canvases.scala` is converted, re-read the
mixed CPU/GPU lines: if `.toExpr` genuinely fails to signal the barrier there,
renaming a single method (or adding the alias, accepting its cost) is a cheap,
localised change — far cheaper than the type rename it was entangled with.

A broader rename of `*Expr` → `*GPU` was considered and **rejected** on
independent grounds:

- **Ratio.** `*Expr` appears 1722× in `trivalibs/src` and 661× in `examples/`,
  but only **44×** across `sketches/` + `src/`. The confusion is felt at the 44;
  the cost lands on the 2383.
- **The family isn't uniform.** Alongside the typed value types sit `Expr`,
  `LetExpr` (62), `VarExpr` (20), `ConstExpr` (20) and `ToExpr` (87) — genuine
  expression-structure concepts. `LetGPU` is meaningless. A blanket rename
  either mangles those or leaves `Vec3GPU` beside `LetExpr`, which is less
  coherent than the status quo.
- **`GPU*` is already claimed.** `GPUDevice`, `GPUBuffer`, `GPUTextureView`,
  `GPUBindGroupLayout` (74+) name _device resources_ in the painter facade.
  `Vec3GPU` would read as one of those rather than as a shader expression.
- **The barrier bites at crossings, not declarations.** In
  `def shadowMask(uv: Vec2Expr, rect: Vec4Expr): FloatExpr` every parameter is
  GPU and the context already says so. The line that misleads is
  `col := CeilTint * roomNoise(…)` — and the fix there is the crossing marker
  (`.toExpr`, `vec3(…)`), not the type name.

`Expr` also carries real information the domain name would lose: these are AST
nodes, not values, which is what makes build-time shader logic possible.

#### ↩︎ Deferred: `into class Vec3`

Stable in 3.9 (§2.5) and would let _direct_ parameter positions accept tuples
without consumers setting a compiler flag — relevant for a library consumed as a
submodule. It does not help any `clearColor` signature (all union-wrapped), and
`-language:implicitConversions` already covers everything. Revisit only if
trivalibs is published as an artifact and the downstream flag becomes a burden.

---

## 3. Consumer cleanup

### `sketches/rooms/canvases/Canvases.scala`

Lift the inline shader literals to shared CPU constants near the existing
top-level constants (~lines 29–51):

| Constant                                    | Replaces line |
| ------------------------------------------- | ------------- |
| `val FloorTint = Vec3(0.80, 0.78, 0.75)`    | 218           |
| `val CeilTint = Vec3(0.86, 0.86, 0.85)`     | 237           |
| `val HaloColor = Vec3(8.0, 7.6, 6.8)`       | 243           |
| `val WallTintLow = Vec3(0.96, 0.96, 0.95)`  | 253           |
| `val WallTintHigh = Vec3(0.88, 0.88, 0.87)` | 254           |

And line 549 → `val wallColor = Vec4(0.90, 0.90, 0.90, 0.0)`.

### `sketches/rooms/grid-ceiling/GridCeiling.scala:582`

`clearColor = (fogColor.x, fogColor.y, fogColor.z, 1.0)` →
`Vec4(fogColor, 1.0)`. The `Vec4(xyz: Vec3, w: Double)` overload already exists
in `cpu/vec4.scala`.

### Docs

The tuple form stays valid (the conversion keeps it compiling), but
`README.md:73` and `docs/guide/sketch-authoring-guide.md:88` should show the
`Vec4` form as primary.

---

## 4. Settled — receiver-position conversion does _not_ fire

This was the open question. It is now answered empirically against the real
types, and the answer is **no**:

```scala
val CeilTint = Vec3(0.86, 0.86, 0.85)
val a: Vec3Expr = CeilTint * noise   // ERROR
```

```
value * is not a member of trivalibs.graphics.math.cpu.Vec3.
Extension methods were tried, but could not be fully constructed:
    trivalibs.graphics.math.cpu.given_Vec3ImmutableOps_Vec3.*()
    trivalibs.graphics.math.gpu.*()
```

The compiler attempts the extension methods, fails, and does **not** fall back
to converting the receiver. (A simplified stand-in model _did_ compile this,
which is why it was re-tested against `trivalibs/src` — the `*G` trait machinery
resolves differently from plain top-level extensions. Treat model results in
this area as unreliable.)

Plain positions are unaffected — `val c: Vec3Expr = CeilTint` compiles.

**Consequence for the sketch.** Operator sites need an explicit domain crossing.
Both spellings are available; pick per readability:

```scala
col := CeilTint.toExpr * roomNoise(wp, normal)   // via §2.2
col := vec3(CeilTint) * roomNoise(wp, normal)    // via §2.6, if adopted
```

This is not a workaround to be minimised away. §2.6 shows that the one mechanism
which _would_ remove it — mixed-domain operators — silently corrupts CPU
arithmetic, so the explicit crossing is the correct outcome rather than a
concession. It also keeps the CPU/GPU boundary visible exactly where a value
changes domain, consistent with the `Vec3` / `vec3` case convention.

---

## 5. Files touched

| File                                               | Change                                                                 |
| -------------------------------------------------- | ---------------------------------------------------------------------- |
| `src/graphics/math/cpu/vec{2,3,4}.scala`           | `toTuple` / `toVecN` + both `Conversion`s per companion                |
| `src/graphics/math/cpu/tuple_interop.scala`        | **new** — the named `toTuple` / `toVecN` (top-level, one file)         |
| `src/graphics/math/gpu/cpu_interop.scala`          | **new** — `toExpr` for `Vec2-4`, `Mat2-4`; **no** implicit conversions |
| `src/graphics/math/gpu/float_expr.scala`           | `vec2/vec3/vec4` CPU overload + `apply(scalar: Double)` guard (§2.6)   |
| `src/graphics/painter/panel.scala`                 | `ClearColor = Vec4`; copy-on-set                                       |
| `src/graphics/painter/painter.scala`               | param types + 2 destructuring sites                                    |
| `../src/utils/mirror/*Reflection.scala`            | `clearColor: Vec4`                                                     |
| `../sketches/rooms/canvases/Canvases.scala`        | lift color constants; `wallColor` → `Vec4`                             |
| `../sketches/rooms/grid-ceiling/GridCeiling.scala` | `Vec4(fogColor, 1.0)`                                                  |
| `project.scala` **and** `../project.scala`         | ✅ **done** — `-feature` + `-language:implicitConversions` (§2.4/§2.5) |

---

## 6. Verification

1. **`compile-full` must be clean.** This is the real test of the conversion
   design: it type-checks all 21 untouched `clearColor = (…)` call sites through
   the new `Tuple4 → Vec4` conversion, and surfaces any ambiguity between the
   two conversion directions.
2. `bun run sketch rooms/canvases` — the primary sketch; settles §4.
3. `bun run sketch rooms/grid-ceiling`, `rooms/base`, `rooms/columns`,
   `base-triangle` — the other `clearColor` sites, confirming zero-churn
   migration.
4. Dev server on port 3000 is already running; reload `rooms/canvases`. This is
   a pure refactor — **the image must be pixel-identical**. Any visible change
   means a literal was transcribed wrong or a `Vec4` alpha was dropped.
5. Check `sketches/rooms/canvases/main.js` for size regression. The cpu
   conversions are `inline` and should add no wrapper functions.
6. Compile once with `-feature` explicitly and confirm **zero** feature
   warnings, proving `-language:implicitConversions` (§2.4) covers every
   position. Worth doing before the change too, to record the pre-existing
   warning count rather than silently inheriting it.

---

## 7. Implementation outcome

Implemented. Everything below was run and passed.

| Check                                   | Result                                        |
| --------------------------------------- | --------------------------------------------- |
| `trivalibs` test suite                  | **292 tests, 0 failed**                       |
| `trivalibs/src` + `examples` compile    | clean                                         |
| All 7 affected sketches compile         | clean                                         |
| Feature warnings (`-feature` on)        | **0**                                         |
| Untouched `clearColor = (…)` call sites | all 21 compile unchanged                      |
| `main.js` (rooms/canvases)              | 752 376 → 754 036 bytes (**+1 660, +0.22 %**) |

### Pixel-identity

Rather than eyeballing the render, the emitted WGSL was compared directly — the
image is fully determined by it. Every lifted constant produces a byte-identical
string, because `toExpr` and the old `Double → FloatExpr` literal path both
route through `floatToWgsl`:

```
floor    vec3<f32>(0.8, 0.78, 0.75)      IDENTICAL
ceil     vec3<f32>(0.86, 0.86, 0.85)     IDENTICAL
halo     vec3<f32>(8.0, 7.6, 6.8)        IDENTICAL
wallLow  vec3<f32>(0.96, 0.96, 0.95)     IDENTICAL
wallHigh vec3<f32>(0.88, 0.88, 0.87)     IDENTICAL
clearColor tuple → Vec4                  IDENTICAL
Vec4(fogColor, 1.0)                      IDENTICAL
```

### What the plan got wrong

Two design points did not survive contact with the compiler. Both are recorded
inline above (§2.1 and §2.2 amendments); summarised here because both were
asserted confidently at planning time:

1. **The implicit `Conversion[Vec*, Vec*Expr]` was unshippable** — it breaks CPU
   swizzles for any file importing both namespaces. Caught only by the test
   suite; every sketch and example still compiled, because none of them happens
   to call a swizzle on a CPU vector. A latent landmine, not a visible break.
2. **Companion-placed extension methods are unreachable** via
   `import …math.cpu.*`, and top-level overloads must share one file — hence
   `cpu/tuple_interop.scala`.

The methodological lesson worth carrying forward: in this codebase, a simplified
stand-in model of the math types **gives false positives** for anything touching
extension-method resolution. The `*G` trait machinery resolves differently from
plain top-level extensions. Both §4's receiver question and the swizzle
collision compiled fine in a model and failed against `trivalibs/src`. Always
probe against the real sources.
