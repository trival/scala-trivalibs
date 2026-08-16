# CPU `Vec*` ↔ GPU `Vec*Expr` interop overloads

Status: **complete** for everything planned — Stages 0, 1, 3, 4, 4b and 5 done.
Stage 2 cancelled (see finding 7); Stage 6 (tuple mirrors) deferred as a standing
idea. Move this file to `documents/done/` if Stage 6 is dropped for good.

Successor to
[`done/vec-tuple-expr-interop-plan.md`](done/vec-tuple-expr-interop-plan.md),
which produced the `.toExpr` lifts and the `vec2/vec3/vec4(v: VecN)`
constructors this builds on.

## Context

`src/graphics/math/gpu/cpu_interop.scala` lifts CPU `Vec2/3/4` + `Mat2/3/4` into
the shader DSL via `.toExpr`, and `float_expr.scala` exposes
`vec2/vec3/vec4.apply(v: VecN)` on top. A `given Conversion[Vec3, Vec3Expr]` was
deliberately rejected: it makes every GPU extension applicable to a CPU value
and collides with the identically-named CPU extension — `v3.xy` becomes
ambiguous between `cpu.xy` and `gpu.xy(conv(v))` for any file importing both
namespaces, i.e. the standard sketch preamble. That decision stands
(`cpu_interop.scala:66-75`).

The remaining gap is one-sided. Crossing the domain currently needs a wrapper
even when the intent is unambiguous:

```scala
// sketches/rooms/canvases/Canvases.scala:267
halo := band * vec3(HaloColor)

// sketches/templates/rooms/grid-canvases/GridCanvases.scala:1887
vec3(WallTintLow).lerp(vec3(BeamSideTopTint), sideLift)
```

The fix is the one already used for `Double`: **explicit overloads** where at
least one operand is a `Vec*Expr` and the other is a CPU `Vec*`, returning a GPU
expression. This is safe precisely because it introduces no conversion — no
_existing_ GPU extension becomes newly applicable to a CPU value, only the
specific ones written here.

Goal: maximum interop. This is library code written once and reused across many
sketches, so overload count and boilerplate are acceptable costs as long as
compile time and runtime stay sound. The work is staged so every stage is a
valid stopping point.

## What shipped

Roughly 220 overloads, all one-line delegates through `.toExpr`, across:

| Position | Works now |
| --- | --- |
| Arithmetic | `v3e + - * / cpuVec3`, `floatExpr * cpuVec3` |
| Named binary | `dot distance min max pow step reflect refract cross` with a CPU `Vec` |
| Blend | `mix` / `lerp` over the full CPU/GPU grid of both params; `smoothstep` over matching edge kinds |
| Comparison | `< <= > >=` with a CPU `Vec`, a `FloatExpr`, or a `Double` |
| Assignment | `col := cpuVec`, `ctx.out.color := cpuVec`, `col += cpuVec`, and the `Double`/`Int` forms |
| Matrix | `matExpr * cpuVec`, `matExpr * cpuMat` |

Plus four pre-existing bugs closed: `v.step(0.5)` and `v.mix(b, 0.5)` did not
compile (already-overloaded, so the conversion could not fire), and `min`/`max`
had no scalar form at all.

**Still explicit, by design:** the CPU value as *receiver* — `vec3(WallTint) * x`,
not `WallTint * x`. See finding (7); this is now a tested invariant.

## Key findings

### 1. No `@targetName` churn for CPU-`Vec` arguments

The heavy `@targetName` use in these files exists because every `*Expr` is
`opaque type XExpr <: Expr = Expr` (`gpu/expr.scala:73-94`) — they all erase to
the single class `Expr`. CPU `Vec2/Vec3/Vec4` are **ordinary reference classes**
(`cpu/vec3.scala:72`), each erasing to its own distinct class. So

```scala
def *(other: Vec3Expr): Vec3Expr   // erases to (Expr)
def *(scalar: Double): Vec3Expr    // erases to (double)
def *(other: Vec3): Vec3Expr       // erases to (Vec3)  ← new, already distinct
```

coexist without annotation. `vec3.apply(v: Vec3)` (`float_expr.scala:785`) is
the existing proof. Same for `Vec3Tuple = (Double, Double, Double)`, which
erases to `Tuple3`.

### 2. The conversion cascade is the real cost

Scala will not apply an implicit conversion through an overloaded method set.
The codebase records this twice already (`float_expr.scala:58-61`, `:761-764`).

Consequence: **every method that gains its first CPU-`Vec` overload loses the
`Conversion[Double|Int, FloatExpr]` path on its _other_ parameters**, and must
get explicit `Double` siblings in the same change or existing sketch code
breaks:

Stage 0 measured this, and found the situation is **already worse than
assumed**: several of these methods are overloaded _today_ and so have already
lost the conversion. They are live bugs, not risks introduced by this work.

| Method                       | `Double` call site  | Status today                               |
| ---------------------------- | ------------------- | ------------------------------------------ |
| `step(edge: FloatExpr)`      | `v.step(0.5)`       | **already broken** — Vec/scalar pair       |
| `mix(b, t: FloatExpr)`       | `a.mix(b, 0.5)`     | **already broken** — Vec/scalar pair       |
| `refract(n, eta: FloatExpr)` | `v.refract(n, 1.4)` | works; breaks when `n` gains a CPU form    |
| `clamp(lo, hi: FloatExpr)`   | `v.clamp(0.0, 1.0)` | works; not overloaded, no CPU form planned |
| `min`/`max(other)`           | `v.min(0.5)`        | **no scalar form at all**                  |

`+ - * /`, `pow` and `smoothstep` already carry explicit `Double` siblings and
are fine. So Stage 3 both adds CPU-`Vec` overloads _and_ closes these
pre-existing gaps — `step(Double)`, `mix(b, t: Double)`, `min(Double)`,
`max(Double)`.

This is what drove staging — not runtime or bundle cost.

### 3. Runtime and bundle cost ≈ zero

These are non-`inline`, build-time-only string builders that run once at shader
construction. Each is a one-line delegate. Unused ones are removed by DCE under
`jsMode full`.

### 4. Compile time is the one budget to watch

Overload resolution is superlinear in alternatives per name. `*` on `Vec3Expr`
goes from 3 to 5 alternatives (fine). `mix` and `smoothstep` under a full
CPU/GPU cross-product reach ~14 and ~16 — that is where compile time and
error-message quality degrade. Stage 4 is the checkpoint.

**Measured: no detectable cost, at any stage.** Clean full builds, 3-4 runs
each, paired against the same tree with the overloads stashed:

| build            | baseline (warm)      | after Stages 1+3     | after Stage 4               |
| ---------------- | -------------------- | -------------------- | --------------------------- |
| clean `src`      | 7.35 / 6.22 / 5.41 s | 4.28 / 4.24 / 4.81 s | 4.25 / 4.38 / 4.71 / 4.73 s |
| clean `src test` | 4.36 / 5.10 s        | 3.99 / 4.55 / 4.70 s | 3.99 / 4.39 / 4.65 / 4.94 s |

The "after" columns are _faster_ than baseline, which is the real finding:
run-to-run variance (±30%, dominated by JVM/JIT warmup) is far larger than any
effect of the overloads. The honest reading is **no measurable impact — not a
speedup**; do not quote these as a win.

Conclusion after ~170 added overloads, including ~14 alternatives on `mix` and
`lerp`: **the superlinear-resolution concern did not materialise at this
scale.** It remains theoretically real, so keep the method if the surface grows
again (Stage 6 would roughly double it): clean builds, ≥3 runs, warm machine,
and only treat a delta as real if it clears the noise band.

### 5. Tuples need their own overloads

`given Conversion[Vec3Tuple, Vec3]` (`cpu/vec3.scala:100`) will **not** rescue
`vec3(p) * (0.1, 0.2, 0.3)` — same rule as (2), the conversion cannot fire
through an overloaded set. Tuple support means explicitly mirroring each
CPU-`Vec` overload, roughly doubling the count. Its own stage for that reason.

### 6. `shader/lib` is out of scope

`color.scala` / `coords.scala` expose only unary receiver extensions (`hsv2rgb`,
`polarToCart`) — nothing to overload. `WgslFn.apply`
(`shader/dsl/fn.scala:233+`) takes `ToExpr[N]` and is a _single_ method per
arity, so `Conversion[Double, FloatExpr]` still fires there. Giving noise/blur
functions a CPU-`Vec` path would mean a `ToExpr` given for `Vec3` — a separate,
larger design.

### 7. CPU-`Vec`-**receiver** operators: probed and REJECTED

`extension (d: Double)` (`expr.scala:648-660`) makes `0.5 * expr` work, so the
obvious next step was an `extension (v: Vec3)` giving `WallTint * roomNoise(…)`.
It was built and tested in Stage 0. **It does not work, and the failure mode is
worse than a compile error.**

A top-level `*` extension on `Vec3` in the `gpu` package arrives by _wildcard
import_, which is searched before the _implicit scope_ where CPU `Vec3` finds
its own `*` (from `given Vec3ImmutableOps[Vec3]` in its companion). Scala
commits to the imported one and never collects the CPU candidate. In a file
importing both namespaces — the standard sketch preamble — plain CPU arithmetic
breaks:

```scala
val a = Vec3(1, 2, 3)
val b = Vec3(2, 2, 2)

a * b      // error: Found Vec3, Required Expr
a * 2.0    // COMPILES — and silently yields Vec3Expr, not Vec3,
           // because Conversion[Double, FloatExpr] feeds the GPU extension
```

The second line is the disqualifying one: CPU vector arithmetic silently becomes
shader-expression construction, with no diagnostic at the operator. The `Double`
precedent does not transfer because `Double.*` is a **member**, and members
always beat extensions — there is no competing extension to shadow.

This is the same underlying hazard that killed
`given Conversion[Vec3, Vec3Expr]` (`cpu_interop.scala:66-75`): the CPU and GPU
vector APIs are deliberately name-identical, so anything that makes a GPU
operation reachable from a CPU receiver collides with its CPU twin. **Receiver
position stays explicit** — `vec3(WallTint) * x` — and that is now a tested
invariant, not just a convention.

Two secondary constraints, recorded in case this is ever revisited:

- **It could not live in `cpu_interop.scala` anyway.** Scala 3 disallows
  overloaded top-level methods spread across files, so _all_ top-level `+ - * /`
  extensions must share one file — which is why the `Double`/`Int`/`IVec*`
  operator extensions are already crowded into `expr.scala` (header note,
  `:598-604`). Only non-operator methods may go in `cpu_interop.scala`.
- **It needed a witness, not arg-type overloads.** `FloatExpr` and `Vec3Expr`
  both erase to `Expr`; `expr.scala:616-622` records that erasure-identical
  operator overloads "get discarded and member `Int.*` is reported as the
  failure". The probe used a `LeftVec3` witness mirroring `LeftScalar`, and that
  part worked fine — two anonymous `given`s in one object also need explicit
  names, or they collide after erasure.

## Scope decisions

- **CPU `Vec*` classes only** as the argument type — they are the idiomatic CPU
  vector types and what APIs and sketch code actually use. Tuples come later
  (Stage 6) and only if cheap.
- **Full Vec-taking surface**, not a core subset — including `pow`, comparisons,
  `reflect`, `refract`, `cross`, and matrix products.
- **Full cross-product** on two-Vec-parameter ops (`mix`, `lerp`, `smoothstep`),
  subject to the Stage 4 compile-time measurement.

## Stages

Each ends at a green `bun run check` + `bun run test`.

- [x] **Stage 0 — probe.** Done. Results: **(a) PASS** — `def *(other: Vec3)`
      coexists with `*(Vec3Expr)` / `*(FloatExpr)` / `*(Double)`; no erasure
      clash, no `@targetName` needed, exactly as finding (1) predicted. **(b)
      PASS** — `v3e * 2.0` still resolves to the `Double` overload. **(c) PASS,
      and worse than assumed** — `v3e.step(0.5)` and `v3e.mix(b, 0.5)` already
      fail _before_ any change, because those methods are already overloaded.
      See the revised finding (2) table. **(d) FAIL — Stage 2 is cancelled.**
      See finding (7); the CPU-receiver extension shadows the CPU `*` and
      silently retypes `cpuVec * 2.0` to `Vec3Expr`. Reverted. Baseline:
      `bun run check` ≈ 2.8 s incremental, full test suite green.
- [x] **Stage 1 — arithmetic, expr receiver (24).** Done. `+ - * /` on
      `VecNExpr` taking CPU `VecN` (12); the same on `FloatExpr` taking CPU
      `Vec2/3/4` (12), beside the `fAddVec2`-style broadcasts. No `@targetName`
      anywhere, no cascade — `+ - * /` already had their `Double` siblings.
- [x] ~~**Stage 2 — arithmetic, CPU receiver.**~~ **Cancelled** by Stage 0 probe
      (d). Receiver position stays `vec3(WallTint) * x`.
- [x] **Stage 3 — named binary ops, expr receiver (~60).** Done. Per vector
      type: `dot`, `distance`, `min`, `max`, `pow`, `step`, `reflect`,
      `refract(VecN, FloatExpr)`, `refract(VecN, Double)`, plus `cross` on Vec3.
      Closed the pre-existing gaps from finding (2): `step(Double)`,
      `mix(b, t: Double)`, `min(Double)`, `max(Double)`,
      `clamp(Double, Double)`, `refract(VecNExpr, Double)`.

      Two implementation notes worth keeping:

      - The `VecNBaseG` givens were **alias-form** (`given T = new T: …`), which
        hides any member not declared in the trait — so `dot`/`distance` CPU
        overloads could not be added there. Converted to body form
        (`given T:`), matching how the `VecNImmutableOpsG` givens were already
        written. Putting them in a top-level extension instead would have
        re-created the finding-(7) shadowing hazard.
      - `min`/`max` needed a **`@targetName`** (`minScalarG`/`maxScalarG`) on
        their new `FloatExpr` scalar form, since it erases to `(Expr)` like the
        existing `VecNExpr` one. The CPU-`Vec` overloads still need none. These
        are the only `@targetName`s Stages 1+3 added.

      Call-site check after Stage 3: `halo := band * HaloColor` in
      `sketches/rooms/canvases/Canvases.scala:267` now compiles and was migrated.
      `vec3(WallTintLow).lerp(vec3(WallTintHigh), t)` in the same file does
      **not** yet — `lerp` is an `inline` trait member delegating to `mix`, and
      its CPU forms belong to Stage 4. Left as-is.

- [x] **Stage 4 — two-Vec-param ops + comparisons (~84).** Done. Per vector
      type: `mix` and `lerp` over the full `b ∈ {VecNExpr, VecN}` ×
      `t ∈ {VecNExpr, VecN, FloatExpr, Double}` grid; `smoothstep` over matching
      edge kinds; `< <= > >=` with a CPU `VecN`, a `FloatExpr` and a `Double`.

      **`smoothstep` was deliberately not given the full 16-way cross-product.**
      The plan called for it, but mixing a *scalar* edge with a *vector* edge is
      a type error in WGSL — `smoothstep(f32, vec3<f32>, vec3<f32>)` does not
      exist — so those seven combinations could only ever produce invalid shader
      code. Only matching-kind pairs are provided: `(VecN, VecN)`,
      `(VecN, VecNExpr)`, `(VecNExpr, VecN)`, `(Double, FloatExpr)`,
      `(FloatExpr, Double)`, plus the three that already existed. This is fewer
      overloads *and* strictly better typing — the omitted forms are ones a
      caller should not be able to write.

      `@targetName` was needed in exactly two places, both from
      `FloatExpr`/`VecNExpr` sharing the `Expr` erasure: `mix`/`lerp` with a CPU
      `b` (`mixCpuVecG` / `mixCpuScalarG` and the `lerp` twins), and the scalar
      form of each comparison (`ltScalarG` … `gteScalarG`).

      **Compile time: still no measurable effect.** Four clean runs each,
      same method as finding (4): `src` 4.25–4.73 s, `src test` 3.99–4.94 s —
      indistinguishable from the Stage 1+3 numbers and from baseline noise. The
      superlinear-overload-resolution worry did not materialise at ~170 total
      added overloads, so no trimming was needed on those grounds.

      Call sites migrated after this stage:
      `sketches/rooms/canvases/Canvases.scala` (`lerp(WallTintHigh, …)`) and
      `sketches/templates/rooms/grid-canvases/GridCanvases.scala` (three `lerp`
      sites). Both sketches rebuild. Bundle diffs are minified and prove nothing,
      so the guarantee is a permanent equivalence test asserting that dropping a
      `vecN(...)` wrapper emits byte-identical WGSL.

- [x] **Stage 4b — assignment operators (~46).** Not in the original plan; added
      because `:=` is the most common write position in a shader body and
      `ctx.out.color := WallColor` was still forcing a wrapper.

      Three sites, all previously taking a bare `Expr` and **not** overloaded:
      `LetExpr.:=` (`gpu/expr.scala`, inherited by `VarExpr`/`ConstExpr`, whose
      overrides still dispatch correctly because the new forms delegate to the
      `Expr` one), `VarExpr.+= -= *= /=`, and `AssignTarget.:=`
      (`shader/dsl/context.scala`, the `ctx.out.*` path). Each gained `Vec2/3/4`
      — plus `Mat2/3/4` on the `:=` forms — and `Double`/`Int`.

      **The `Double`/`Int` forms were mandatory here, not optional.** `n := 0.5`
      worked because `Conversion[Double, FloatExpr]` conforms to
      `Conversion[Double, Expr]` — `Conversion` is covariant in its result — so
      the conversion reached a parameter typed `Expr`. Overloading `:=` blocks
      that path. This was verified by probe before the change, and is now pinned
      by tests; without it every `x := 0.5` in every shader body would break.

      `context.scala` also needed `import trivalibs.graphics.math.gpu.given`
      added — the existing wildcard `gpu.*` import does not bring givens into
      scope, so the `Double` ascription could not find its conversion.

      Verified by building **all 13 sketches** and `bun run examples:build`,
      since `:=` appears in essentially every shader body.

- [x] **Stage 5 — matrices (6).** Done. `MatNExpr * VecN` and `MatNExpr * MatN`
      for N = 2, 3, 4, in the three `MatNImmutableOpsG` given blocks.

      The anticipated `@targetName` was **not** needed. The existing
      `MatNExpr.*[Vec]` is generic over the vector representation via
      `VecNBaseG[FloatExpr, Vec]` evidence and erases to `(Object)`, the
      `MatNExpr` overload erases to `(Expr)`, and the new CPU forms erase to
      `(VecN)` / `(MatN)` — all four distinct.

      Note the generic `*[Vec]` cannot serve CPU operands even though it looks
      like it should: it demands `VecNBaseG[FloatExpr, Vec]`, and a CPU `Vec4`
      only has `Vec4Base[Vec4]` — i.e. `Vec4BaseG[Double, Vec4]`. Different
      `Num` parameter, so no evidence. Hence the explicit overloads. Tests pin
      that the generic and `MatNExpr` forms still resolve alongside them.
- **Stage 6 — tuple mirrors (~170).** **Deferred, not cancelled** — a standing
  idea to pick up if and when a call site actually wants it. Would mirror Stages
  1-5 with `Vec2Tuple`/`Vec3Tuple`/`Vec4Tuple`; see finding (5) for why the
  existing `Conversion[Vec3Tuple, Vec3]` cannot do it for free. Check the
  `Vec4Tuple` / `Mat2Tuple` erasure overlap (both `Tuple4[Double×4]`) at that
  point — they never meet in one extension block, but `cpu/package.scala:24`
  records a live collision of that kind.

## Files

| File                                      | Role                                                                                                                                                                                     |
| ----------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `src/graphics/math/gpu/float_expr.scala`  | Everything. The `VecNImmutableOpsG` given blocks (`:206-311`, `:332-439`, `:461-566`), `VecNBaseG` blocks, `NumOps[FloatExpr]` (`:50-103`), `Mat4Expr` extension (`:685-694`).           |
| `src/graphics/math/gpu/expr.scala`        | `LetExpr.:=` and `VarExpr.+= -= *= /=` CPU/literal overloads (Stage 4b).                                                                                                                 |
| `src/graphics/shader/dsl/context.scala`   | `AssignTarget.:=` CPU/literal overloads (Stage 4b), plus the `gpu.given` import they need.                                                                                               |
| `src/graphics/math/gpu/cpu_interop.scala` | Comments only. Fix the contradictory pair at `:11-17` (claims the conversions are "also available implicitly" — they are not, per `:66-75`) and record the finding-(7) result beside it. |
| `test/math/CpuVecInterop.test.scala`      | New. Modelled on `test/math/Swizzle.test.scala`; asserts emitted WGSL per family plus the finding-(2) regression block.                                                                  |

Shape of every argument-side overload — a one-line delegate through `.toExpr`,
no `@targetName`, no `override` (these are additions, not trait members),
matching the existing `Double` siblings at `float_expr.scala:217` / `:293`:

```scala
def *(other: Vec3): Vec3Expr = v * other.toExpr
def lerp(b: Vec3, t: FloatExpr): Vec3Expr = v.lerp(b.toExpr, t)
```

## Verification

1. `bun run check` after each stage — primary gate.
2. `bun run test` — `test/math/CpuVecInterop.test.scala`. Assert emitted WGSL
   per overload family, and keep the finding-(2) regression block green:
   `v.step(0.5)`, `v.mix(b, 0.5)`, `v.refract(n, 1.4)`, `v.clamp(0.0, 1.0)`,
   `vec3(x) * 2.0`.
3. In the consuming repo, rewrite the two live call sites and rebuild
   (`bun run sketch rooms/canvases`,
   `bun run sketch templates/rooms/grid-canvases`). `rooms/canvases` must render
   unchanged — this is a pure-syntax change, so any visual difference means an
   overload picked the wrong alternative.
4. Tick the stage off here in the same change, so the doc never drifts from the
   code.
