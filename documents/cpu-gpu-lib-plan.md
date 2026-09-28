# CPU / GPU mirrored library helpers — plan

Status: **planned**. Supersedes and extends the "CPU noise, mirroring
`shader/lib/random/`" entry in `independent-todos.md`.

## Goals

1. **fbm cleanup** — one fbm implementation per noise family, normalized to
   `[-1, 1]`, in the library. The downstream `sketchlib.shaders.Noise`
   (`fbm3`, `tilingFbm3`) goes away.
2. **Consistent seed handling** across all seeded noise: an optional scalar
   `seed`, where any change of seed yields an unrelated noise field.
3. **Extended-noise fbm** — `fbm2d` / `fbm3d` over the extended family
   (psrdnoise: value + gradient, optional tiling), which is renamed from
   `Psrdnoise` and collapsed from six functions to one per dimension.
4. **Full CPU noise** — simplex (2D/3D/4D, seeded, fbm, torus), worley (2D/3D) and
   extended (2D/3D, tiling, rotating, fbm), with the same characteristics as
   the GPU (see "CPU / GPU parity").
5. **One calling convention on both sides** — named, defaulted parameters on
   the CPU **and** on the GPU. Seeded / unseeded, and baked / runtime
   arguments, are chosen inside the wrapper, not by the caller picking a
   suffixed name. The cost model differs per side:
   - **GPU:** Scala-level boilerplate and JS bundle growth are fine, as long as
     the emitted **WGSL** carries none of it.
   - **CPU:** helpers run in hot paths and render loops. The linked JS must be
     a direct call to a scalar kernel: no surviving Scala wrapper, typeclass
     dispatch, allocation or runtime option check.
6. **Extensions everywhere** — color, coords and every noise variant are
   receiver extensions with identical names and parameters on the CPU vectors
   and on the `*Expr` shader types.
7. **Mirrored namespace and folders** — CPU (and the shared extensions) in
   `graphics/lib/…`, GPU in `graphics/shader/lib/…`, same relative paths,
   subpackage, object and member names. Each side
   imports on its own without clashing; the painter prelude exports all
   extensions.
8. **Consistent API surface** — one set of repeating conventions (file /
   object / member / parameter / extension / WGSL naming, return ranges,
   visibility) applied across all graphics utils, so a new helper's name and
   shape can be guessed without looking it up.

## CPU / GPU parity: similar is required, identical is preferred

In practice a sketch computes a given noise or random value on one side or the
other, never in one algorithm that spans both. No use case so far needs a
CPU-computed value to be revalidated on the GPU, or a noise field to continue
seamlessly from CPU-precomputed parts into GPU-computed ones. So:

- **Required:** the same distributions, frequencies, ranges and visual
  characteristics per function and parameter set on both sides. The same
  parameters give the same _kind_ of field.
- **Preferred, not a show stopper:** identical values for the same inputs and
  seed. It's appealing, and cheap where the design below gets it for free
  (integer seed offsets, hash tables). But where exactness costs complexity or
  performance, similar is enough, and the plan says so at the site.
- **Hash (`Hash`) stays GPU only, CPU port deferred** until a hard requirement
  appears: a global seed that must guarantee equal randomness on both targets.
  Until then the CPU uses `utils/random` for randomness, as today.

## Already done: fbm normalization (2026-09-26)

The four `Simplex.fbm*` WgslFns used to return the raw octave sum. With
`gain = 0.8` and 4 octaves that is `±(1 + 0.8 + 0.64 + 0.512) = ±2.952`, so
`.fit1101` produced `[-0.976, 1.976]` and a following `pow` got NaN on the
negative part. They now accumulate `total += amplitude` and return
`sum / total`, i.e. the amplitude-weighted average, in `[-1, 1]` for any
`octaves` / `gain`. `sketchlib.shaders.Noise.fbm3` / `tilingFbm3` already did
this.

**Rule: every fbm is normalized, on both sides.** The CPU fbms and the new
extended fbms return `Σ noiseᵢ·ampᵢ / Σ ampᵢ`. Documented in each Scaladoc.

Call sites that saw a value change (old = new × total). Not yet adjusted —
decided case by case, then the sketch is rebuilt:

| Call site                                                | octaves / gain | total  |
| -------------------------------------------------------- | -------------- | ------ |
| `sketches/experiments/strokes/study1/StrokeStudy1.scala` | 4 / 0.8        | 2.952  |
| `sketches/strokes/base1/BaseStroke1.scala` (color)       | 4 / 0.22       | 1.279  |
| `sketches/strokes/base1/BaseStroke1.scala` (base)        | 4 / 0.8        | 2.952  |
| `sketches/strokes/tile-strokes/TileStrokesSketch.scala`  | 4 / 0.8        | 2.952  |
| `trivalibs/examples/noise_tests/NoiseTests.scala` (×4)   | 5 / 0.5        | 1.9375 |

## Current inventory

### Namespaces and folders today

| Folder                        | Package                               | Side                      | Contents                                                                                                                                         |
| ----------------------------- | ------------------------------------- | ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `graphics/math/`              | `trivalibs.graphics.math`             | shared                    | `Vec2–4BaseG[Num, Vec]` / `…ImmutableOpsG` generic vector traits, `Mat2–4` traits, `LerpBy` / `lerpIn`                                           |
| `graphics/math/cpu/`          | `trivalibs.graphics.math.cpu`         | CPU                       | `Vec2–4`, `Mat2–4`, `Quat`, buffer and tuple variants, swizzles, **`color.scala`**, **`coords.scala`**                                           |
| `graphics/math/gpu/`          | `trivalibs.graphics.math.gpu`         | GPU                       | `FloatExpr`, `Vec2–4Expr`, `IntExpr`, `vec2(…)` constructors, CPU → Expr interop                                                                 |
| `graphics/shader/lib/`        | `trivalibs.graphics.shader.lib.color` | GPU                       | `Color` WgslFns + `Vec3Expr` extensions                                                                                                          |
|                               | `…shader.lib.coords`                  | GPU                       | `Polar` WgslFns + `Vec2Expr` extensions                                                                                                          |
|                               | `…shader.lib.blur`                    | GPU                       | `Blur` (gaussian 5/9/13, box, 2D box/tent pyramid, `*Auto`)                                                                                      |
|                               | `…shader.lib.line`                    | GPU                       | `lineCross`, `cross.lineV`, `cross.lineOffset`                                                                                                   |
| `graphics/shader/lib/random/` | `…shader.lib.random`                  | GPU                       | `Simplex` (noise 2D/3D/4D, seeded, fbm, worley, tiling 2D), `Psrdnoise` (2D/3D tiling + rotating, with gradient), `Hash` (u32/float hashes 1–4D) |
| `utils/`                      | `trivalibs.utils.numbers`             | CPU (+ GPU via `NumBase`) | `NumExt` (`x.sin`, `fit0111`, `smoothstep`, …), `IntExt`                                                                                         |
| `utils/`                      | `trivalibs.utils.random`              | CPU                       | `rand()`, `randInRange`, `randVec2–4`, `arr.pick()` / `shuffle()` (JS `Math.random`)                                                             |
| downstream `src/shaders/`     | `sketchlib.shaders`                   | GPU                       | `Noise.fbm3`, `Noise.tilingFbm3`, `Uv.aspectPreserving`, `Shapes.roundedRect*`                                                                   |

### Helper table: what exists where

✅ exists · ➖ missing · ⛔ not intended

| Helper                                    | GPU                                   | CPU                                    | GPU form today                           | CPU form today              | After this plan                                                                                             |
| ----------------------------------------- | ------------------------------------- | -------------------------------------- | ---------------------------------------- | --------------------------- | ----------------------------------------------------------------------------------------------------------- |
| Vector / matrix algebra                   | ✅                                    | ✅                                     | `a.dot(b)`, `v.normalize` on `Vec3Expr`  | same on `Vec3`              | unchanged; already shared through `Vec*BaseG`                                                               |
| Scalar math                               | ✅                                    | ✅                                     | `x.sin`, `x.fit1101` on `FloatExpr`      | same on `Double` (`NumExt`) | unchanged                                                                                                   |
| Interpolation                             | ✅                                    | ✅                                     | `t.lerpIn(a, b)`                         | same                        | unchanged                                                                                                   |
| rgb ↔ hsv / hsl                           | ✅                                    | ✅                                     | `c.hsv2rgb`, `Color.hsv2rgb(c)`          | `c.hsv2rgb`                 | moves to `…lib.color` on both sides; CPU gets a `Color` object                                              |
| polar ↔ cartesian                         | ✅                                    | ✅                                     | `p.polarToCart`, `Polar.polarToCart(p)`  | `p.polarToCart`             | moves to `…lib.coords`; CPU gets a `Polar` object                                                           |
| Simplex 2D / 3D / 4D                      | ✅                                    | ➖                                     | `Simplex.simplexNoise2d(p)`              | —                           | both, `seed` optional                                                                                       |
| Simplex seeded 2D / 3D                    | ✅ (inconsistent)                     | ➖                                     | `Simplex.simplexNoise3dSeeded(p, vec3)`  | —                           | folded into the above: hashed scalar `seed`                                                                 |
| Simplex fbm 2D / 3D (+ seeded)            | ✅                                    | ➖                                     | `Simplex.fbmSimplex2d(p, 4.i, 2.0, 0.5)` | —                           | both, normalized, defaults, `seed` optional                                                                 |
| Tiling simplex 2D (4D torus)              | ✅                                    | ➖                                     | `Simplex.tilingSimplexNoise2d(p, scale)` | —                           | both, as `Simplex.torusNoise2d`                                                                             |
| Worley 2D                                 | ✅                                    | ➖                                     | `Simplex.worley2d(p, jitter)`            | —                           | own `Worley` object, both; **+ Worley 3D** from upstream `cellular3D.glsl`                                  |
| psrdnoise 2D / 3D (tiling, rot, gradient) | ✅                                    | ➖                                     | `Psrdnoise.tilingNoise3d(p, period)`     | —                           | both, as `Extended.noise2d/3d`; `tilingPeriod` / `rot` / `seed` optional                                    |
| psrdnoise fbm 2D / 3D                     | ➖ (downstream `tilingFbm3`, 3D only) | ➖                                     | —                                        | —                           | new, both: `Extended.fbm2d/3d`                                                                              |
| psrdnoise seeded                          | ➖                                    | ➖                                     | —                                        | —                           | new, both (seed keeps tiling)                                                                               |
| sketchlib `Noise.fbm3`                    | downstream                            | ➖                                     | `Noise.fbm3(p, seed = vec3(9))`          | —                           | **removed** → `p.simplexFbm(…)`                                                                             |
| sketchlib `Noise.tilingFbm3`              | downstream                            | ➖                                     | `Noise.tilingFbm3(p, period)`            | —                           | **removed** → `p.extendedFbmValue(tilingPeriod = …)`                                                        |
| Integer / float hashes                    | ✅                                    | ⛔ (`utils/random` is a different API) | `Hash.hash21(p)`                         | —                           | GPU only, now with GPU-only extensions (`x.hash`, `u.hashU`, `u.hash1`); CPU port deferred (see "CPU / GPU parity"). Only the small seed hash inside noise gets a CPU form |
| Blur kernels                              | ✅                                    | ⛔                                     | `Blur.gaussianBlur9(…)`                  | —                           | GPU only, renamed                                                                                           |
| Line cross coords                         | ✅                                    | ⛔                                     | `ctx.in.cross.lineV`                     | —                           | GPU only, unchanged                                                                                         |

## Target design

### Three layers per domain

| Layer          | GPU (`graphics/shader/lib`)                                                                                | CPU (`graphics/lib`)                                                                                                        |
| -------------- | ---------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| **Definition** | `X.wgsl`: WgslFn values, full explicit WGSL parameter lists, one per code path (seeded / unseeded, …)      | `X.kernel`: plain `def`s over scalar components (`x, y, z: Double`), one per code path, with no vector receivers or options |
| **Object API** | `object X`: ordinary Scala wrapper defs with defaults; pick a `wgsl` fn at shader-build time               | `object X`: `inline def`s with `inline` params and defaults; pick a `kernel` fn at compile time, so they erase completely   |
| **Extensions** | shared, in `graphics/lib` next to the CPU objects: one `transparent inline` def per name for all receivers | same def: its CPU branch expands to the CPU object API, i.e. a direct kernel call                                           |

GPU wrapper (verified against the real DSL):

```scala
object Simplex:
  def fbm2d(
      pos: Vec2Expr,
      octaves: IntArg = 4,          // Int | IntExpr: plain literal or runtime value
      lacunarity: FloatExpr = 2.0,
      gain: FloatExpr = 0.5,
      seed: FloatExpr | Null = null,
  ): FloatExpr =
    if seed == null then wgsl.fbm2d(pos, octaves.toIntExpr, lacunarity, gain)
    else wgsl.fbm2dSeeded(pos, seed, octaves.toIntExpr, lacunarity, gain)
```

```
fbm2d(uv)                          → simplex_fbm_2d(vec2<f32>(0.1, 0.2), 4, 2.0, 0.5)
fbm2d(uv, octaves = 3, seed = 7.0) → simplex_fbm_2d_seeded(…, 7.0, 3, 2.0, 0.5)
fbm2d(uv, gain = 0.8)              → simplex_fbm_2d(…, 4, 2.0, 0.8)
```

The `if` runs in Scala at shader-build time, and the `FnRegistry` records only
the fn actually called. So the unseeded path keeps skipping the extra permute
stages without the caller picking a different name. The seeded/unseeded split
is a GPU implementation detail kept in `X.wgsl`, not an API concept.

CPU wrapper, erased at compile time:

```scala
object Simplex:
  object kernel:
    def fbm2d(x: Double, y: Double, octaves: Int, lacunarity: Double, gain: Double): Double = …
    def fbm2dSeeded(x: Double, y: Double, octaves: Int, lacunarity: Double, gain: Double, seed: Double): Double = …

  inline def fbm2d(
      inline pos: Vec2,
      inline octaves: Int = 4,
      inline lacunarity: Double = 2.0,
      inline gain: Double = 0.5,
      inline seed: Double | Null = null,
  ): Double =
    inline seed match
      case null => kernel.fbm2d(pos.x, pos.y, octaves, lacunarity, gain)
      case s: Double => kernel.fbm2dSeeded(pos.x, pos.y, octaves, lacunarity, gain, s)
```

### Extensions: one `transparent inline` def per name (spike results, 2026-09-28)

**Constraint.** Scala rejects a method name with more than one overloaded
alternative that has default arguments ("two or more overloaded variants of
method fbm have default arguments"). Extensions desugar to methods, and
**both** of these hit it:

- same-named extensions for two receivers in one scope (`Vec2Expr` and
  `Vec3Expr`);
- two objects' extensions exported into one prelude object (CPU and GPU).

Two separate `import`s of the two objects do merge, defaults included, but the
painter prelude is a single object. So each extension name has to be defined
**once**, generic in the receiver.

**Rejected: typeclass dispatch** (`extension [P](p: P)(using n: SimplexOps[P])`
with refined `n.F` defaults). It works, but the CPU call survives in the bundle
as a given-instance method call. Not acceptable for hot paths.

**Chosen: compile-time branching.** One `transparent inline` extension per
name, with the receiver bounded by a union of the supported types and
union-typed `inline` parameters. An `inline erasedValue[P] match` picks the
side; per-argument `inline match` helpers narrow the unions. A `FloatExpr`
passed to a CPU receiver is a compile error with a clear message.

```scala
// graphics/lib/args.scala — package trivalibs.graphics.lib

/** Parameter types that close the gap between CPU and GPU contexts.
  *
  * A shared lib extension (`p.simplexFbm(gain = 0.8)`) is one definition
  * serving CPU receivers (`Vec2`, computing now) and GPU receivers
  * (`Vec2Expr`, building WGSL). Scala forbids default arguments on more than
  * one overloaded alternative, so the two sides can't be separate overloads.
  * Each parameter therefore takes either side's value: `FloatArg` accepts a
  * CPU `Double` or a shader `FloatExpr`, `IntArg` an `Int` or an `IntExpr`,
  * and `VecNArg` a CPU `VecN` or a `VecNExpr`.
  *
  * They are parameter types only, never stored or returned. The narrowing
  * helpers below resolve them to one side's type at compile time: the CPU
  * branch requires the CPU value (a `FloatExpr` there is a compile error), and
  * the GPU branch converts CPU values explicitly (`d.toExpr`, `i.i`,
  * `v.toExpr`). Nothing of them survives into the linked JS or the WGSL.
  */
type FloatArg = Double | FloatExpr
type IntArg   = Int | IntExpr
type Vec2Arg  = Vec2 | Vec2Expr
type Vec3Arg  = Vec3 | Vec3Expr
type Vec4Arg  = Vec4 | Vec4Expr

inline def cpuD(inline x: FloatArg | Null): Double = inline x match
  case d: Double => d
  case _         => compiletime.error("CPU context takes a Double here, not a FloatExpr")
inline def gpuF(inline x: FloatArg | Null): FloatExpr = inline x match
  case d: Double    => d.toExpr
  case e: FloatExpr => e
inline def gpuI(inline x: IntArg): IntExpr = inline x match
  case i: Int     => i.i
  case e: IntExpr => e
inline def gpuV3(inline v: Vec3Arg | Null): Vec3Expr = inline v match
  case c: Vec3     => c.toExpr
  case e: Vec3Expr => e
// … cpuI, cpuV2 / cpuV3 / cpuV4, gpuV2 / gpuV4 likewise
```

```scala
// graphics/lib/noise/extensions.scala — shared layer, package trivalibs.graphics.lib.noise
import trivalibs.graphics.shader.lib.noise as gpu   // CPU objects are this package's own

extension [P <: Vec2 | Vec3 | Vec2Expr | Vec3Expr](inline p: P)
  transparent inline def simplexFbm(
      inline octaves: IntArg = 4,
      inline lacunarity: FloatArg = 2.0,
      inline gain: FloatArg = 0.5,
      inline seed: FloatArg | Null = null,
  ): Double | FloatExpr =
    inline erasedValue[P] match
      case _: Vec2     => Simplex.fbm2d(p.asInstanceOf[Vec2], cpuI(octaves), cpuD(lacunarity), cpuD(gain), cpuSeed(seed))
      case _: Vec3     => Simplex.fbm3d(…)
      case _: Vec2Expr => gpu.Simplex.fbm2d(p.asInstanceOf[Vec2Expr], gpuI(octaves), gpuF(lacunarity), gpuF(gain), gpuSeed(seed))
      case _: Vec3Expr => gpu.Simplex.fbm3d(…)
```

The receiver bound spells out the union too (`Vec2 | Vec3 | Vec2Expr |
Vec3Expr`), since it spans dimensions. `tilingPeriod` on a 3D receiver is
`Vec3Arg | Null`.

The names avoid `Num` on purpose. `Num` is already the type-parameter name in
`Vec2–4BaseG[Num, Vec]` and the root of `NumOps` / `NumBase`, and these unions
have nothing to do with either. Existing neighbors were checked and don't fill
the role: `Lift[C, E]` lifts one way for `:=`, `ToExpr[T]` maps WGSL types to
expression types, and a generic type parameter can't carry a literal default.

The narrowing helpers convert **explicitly** (`d.toExpr`, `i.i`), since the
target type is already known there. They don't rely on the implicit
`Conversion[Double, FloatExpr]`. `Double.toExpr` doesn't exist yet: the
converter behind the implicit, `floatToWgsl`, is `private[gpu]`. It gets added
to `math/gpu` next to the CPU vectors' `.toExpr` (`Vec3.toExpr: Vec3Expr`), as
`extension (v: Double) inline def toExpr: FloatExpr`, with the implicit
conversion and `FloatExpr.liftDouble` rewritten to delegate to it (phase 1).

Verified against the real library, through a prelude-style `export` (probe
kernels, `jsMode full` link, JS names de-minified):

| Call                                             | Result                                                              |
| ------------------------------------------------ | ------------------------------------------------------------------- |
| `Vec2(1, 2).simplexFbm(gain = 0.8)`              | typed `Double`; linked JS `CpuSimplex.fbm2d(a.x, a.y, 4, 2.0, 0.8)` |
| `Vec2(1, 2).simplexFbm(octaves = 3, seed = 3.0)` | linked JS `CpuSimplex.fbm2dSeeded(a.x, a.y, 3, 2.0, 0.5, 3.0)`      |
| `uv.simplexFbm()`                                | typed `FloatExpr`; WGSL `fbm_simplex_2d(uv, 4, 2.0, 0.5)`           |
| `uv.simplexFbm(octaves = 3, seed = 7.0)`         | WGSL `fbm_simplex_2d_seeded(uv, 3, 2.0, 0.5, 7.0)`                  |
| `uv.simplexFbm(gain = u.gain, octaves = u.oct)`  | WGSL `fbm_simplex_2d(uv, u.oct, 2.0, u.gain)` (runtime values)      |

The CPU call site links to exactly one direct kernel call with the defaults as
constants: no wrapper function, no dispatch, no null check.

Parameterless extensions (`c.hsv2rgb`, `p.polarToCart`) use the same shape
(one def, compile-time branch). That also removes the other prelude risk: a
generic CPU receiver (`[V](c: V)(using Vec3Base[V])`) competing with a
`Vec3Expr` receiver in overload resolution.

**CPU receivers are the concrete vector classes** (`Vec2`, `Vec3`, `Vec4`),
not generic over `Vec2Base` / `Vec3Base`. The existing CPU color / coords
extensions take `(using base: Vec3Base[Vec], ops: Vec3ImmutableOps[Vec])` on a
non-inline def, which is exactly the dispatch the CPU rule forbids. They are
rewritten onto kernels. Buffer and tuple forms call `X.kernel` with their
components, or convert.

**Decision: no generic named/defaulted-parameter support in `WgslFn` itself**
(considered as "option A": named-tuple argument, auto-tupled from named
arguments, with literal defaults encoded in the parameter type). It works in
Scala 3.9 but loses IDE parameter help and needs type-level machinery. User
code that wants a convenient call shape for its own WgslFns writes a wrapper
def, the same pattern the library uses.

### Folder structure

Both trees use the same relative paths. CPU: `graphics/lib/`, package
`trivalibs.graphics.lib.*` (CPU implementation + shared extensions). GPU:
`graphics/shader/lib/`, package `trivalibs.graphics.shader.lib.*`.

| Relative path            | Subpackage   | CPU — `graphics/lib/`                                                                                      | GPU — `graphics/shader/lib/`           |
| ------------------------ | ------------ | ---------------------------------------------------------------------------------------------------------- | -------------------------------------- |
| `args.scala`             | package root | `FloatArg` / `IntArg` / `Vec2–4Arg` CPU-GPU parameter unions, `cpuD` / `gpuF` / … narrowing helpers        | —                                      |
| `color.scala`            | `.color`     | `object Color` (+ `Color.kernel`) + shared extensions `c.hsv2rgb` …                                        | `object Color` (+ `Color.wgsl`)        |
| `coords.scala`           | `.coords`    | `object Polar` (+ `Polar.kernel`) + shared extensions `p.polarToCart` …                                    | `object Polar` (+ `Polar.wgsl`)        |
| `noise/simplex.scala`    | `.noise`     | `object Simplex` (+ `Simplex.kernel`)                                                                      | `object Simplex` (+ `Simplex.wgsl`)    |
| `noise/extended.scala`   | `.noise`     | `object Extended` (+ `Extended.kernel`)                                                                    | `object Extended` (+ `Extended.wgsl`)  |
| `noise/worley.scala`     | `.noise`     | `object Worley` (+ `Worley.kernel`)                                                                        | `object Worley` (+ `Worley.wgsl`)      |
| `noise/seed.scala`       | `.noise`     | seed hash (same algorithm as the GPU one)                                                                  | seed hash WgslFn, `private[lib]`       |
| `noise/extensions.scala` | `.noise`     | shared extensions `p.simplexNoise`, `p.simplexFbm`, `p.extendedNoise`, `p.extendedFbm`, `p.worleyNoise`, … | —                                      |
| `random/hash.scala`      | `.random`    | — (deferred)                                                                                               | `object Hash` + GPU-only extensions `.hash`, `.hashU`, `.hash1` |
| `blur.scala`             | `.blur`      | —                                                                                                          | `object Blur`                          |
| `line.scala`             | `.line`      | —                                                                                                          | `object Line` + `lineV` / `lineOffset` |

Rules:

- **`graphics/lib` is the library's home for helpers; `graphics/shader/lib`
  is its shader-side mirror.** The two trees have the same relative paths,
  subpackages, object, member and parameter names. The side is chosen by the
  package prefix (`graphics.lib` vs `graphics.shader.lib`), never by the name:
  `trivalibs.graphics.lib.noise.Simplex` is the CPU object,
  `trivalibs.graphics.shader.lib.noise.Simplex` the GPU one.
- The shared extensions live in `graphics/lib` too, in the same package as the
  CPU objects: at the top level of `color.scala` / `coords.scala`, and in
  `noise/extensions.scala` for the multi-file noise package. They reference
  the GPU objects by their full package. The prelude exports only the
  extension-carrying `…$package` objects, never the CPU objects (see Prelude).
- `random/` keeps only `Hash`. Noise moves to `noise/`, since it is not
  randomness in the `Hash` sense and CPU `random` already means `utils/random`.
- `math/cpu/color.scala` and `math/cpu/coords.scala` move to `graphics/lib/`.
  `math/cpu` goes back to holding only the vector / matrix / quat types. The
  GPU-side top-level extensions in `shader/lib/color.scala` / `coords.scala`
  (on `Vec3Expr` / `Vec2Expr`) are removed in favor of the shared ones.
- GPU-only domains (`Hash`, `Blur`, `Line`) have no shared layer. Their
  extensions (`lineV`, …) stay next to the object.
- **CPU: everything above `X.kernel` is `inline`** (object API, extensions,
  narrowing helpers) and erases at the call site. Kernels are ordinary public
  `def`s. An inline def that reaches a `private` member makes Scala generate
  an accessor method, which is exactly the leftover wrapper to avoid. So kernel
  code and anything inline code touches stay public, or `private[lib]` only
  when no inline def reaches it.

### API conventions

Apply to every module under `graphics/lib/` and `graphics/shader/lib/`.
Existing modules are audited against them (table below).

| #   | Convention                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | Example                                                                                       |
| --- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------- |
| C1  | **One domain per file, one object per domain**, capitalized singular noun, and the file named after the domain.                                                                                                                                                                                                                                                                                                                                                                                                                     | `noise/simplex.scala` → `object Simplex`                                                      |
| C2  | **The object API is wrapper defs** with named, defaulted parameters, identical in shape on CPU and GPU. GPU: ordinary defs over WgslFn values in the nested `X.wgsl`. CPU: `inline def`s with `inline` params over scalar kernels in the nested `X.kernel`. `wgsl` / `kernel` are public (raw WGSL composition, `.withDeps`, hand-tuned CPU loops) but not the everyday API.                                                                                                                                                        | `Simplex.fbm2d(pos, gain = 0.8)`; `Simplex.wgsl.fbm2dSeeded`; `Simplex.kernel.fbm2d(x, y, …)` |
| C3  | **Object members: `<what><dim>d`**, with the dimension always spelled out (wrapper defs can't overload on the receiver type once they have defaults). Prefixes only for a genuinely different mechanism (`torus` in `Simplex.torusNoise2d`), never for an option. No family name repeated inside its object.                                                                                                                                                                                                                        | `Simplex.noise3d`, `Extended.fbm2d`, `Blur.gaussian9`                                         |
| C4  | **Optional behavior is a parameter, not a name.** `seed`, `tilingPeriod`, `rot` default to "off" (`null` / zero). Picking the matching code path is the wrapper's job. `wgsl` / `kernel` members may keep a `Seeded` suffix as the internal name of a separate code path.                                                                                                                                                                                                                                                           | `Extended.noise3d(pos, tilingPeriod = vec3(8, 0, 8), seed = 3.0)`                             |
| C5  | **Extension names: `<family><Operation>`**, no dimension (the receiver type carries it), no side marker, same parameters and defaults as the object API minus `pos`.                                                                                                                                                                                                                                                                                                                                                                | `p.simplexFbm(gain = 0.8, seed = 7.0)`                                                        |
| C6  | **Extensions are defined once, in `graphics/lib/<domain>`**, as `transparent inline` defs with the receiver bounded by a union of the supported vector types, branching on it with `inline erasedValue[P] match`. No typeclass dispatch.                                                                                                                                                                                                                                                                                            | `extension [P <: Vec2 \| Vec3 \| Vec2Expr \| Vec3Expr](inline p: P)`                          |
| C7  | **Parameter names and order:** `pos` first, then required arguments, then optional ones in a fixed order: tuning (`octaves`, `lacunarity`, `gain`, `jitter`, `scale`), then modes (`tilingPeriod`, `rot`), then `seed` last. Option names say what they switch on (`tilingPeriod`, not `period`). Colors are `c`, directions `dir`, textures `tex`.                                                                                                                                                                                 | `fbm2d(pos, octaves, lacunarity, gain, seed)`                                                 |
| C8  | **Argument types:** GPU object API: numbers `FloatExpr` (literals convert), counts `Int \| IntExpr`, "off" = `null` in `T \| Null`. CPU object API: `Double`, `Int`, `Double \| Null`. Shared extensions: `FloatArg = Double \| FloatExpr`, `IntArg = Int \| IntExpr`, `Vec2–4Arg = VecN \| VecNExpr` (the CPU/GPU gap-closing unions in `args.scala`), narrowed per side at compile time with explicit conversions (`d.toExpr`, `i.i`), never the implicit `Conversion`; a `FloatExpr` given to a CPU receiver is a compile error. | `octaves: IntArg = 4`, `seed: FloatArg \| Null = null`                                        |
| C9  | **Units spelled out and uniform:** hue in `[0, 1]`; rotation `rot` in turns `[0, 1]`; angles from `cartToPolar` in radians (documented).                                                                                                                                                                                                                                                                                                                                                                                            | `rot = 0.25` = 90°                                                                            |
| C10 | **Documented output range for every function**, and every noise scalar output nominally in `[-1, 1]`, fbm strictly. The user remaps with `.fit1101`.                                                                                                                                                                                                                                                                                                                                                                                | Scaladoc line `Returns: [-1, 1]`                                                              |
| C11 | **WGSL function name = snake_case(object + wgsl member)**, which makes it globally unique and predictable when composing raw WGSL.                                                                                                                                                                                                                                                                                                                                                                                                  | `Simplex.wgsl.fbm2dSeeded` → `simplex_fbm_2d_seeded`                                          |
| C12 | **Internal helpers are `private[lib]`**, never public members. GPU: helper WgslFns. CPU: only helpers no inline def reaches (see the accessor rule above); kernel-internal scalar math is inlined into the kernel or `private` inside it.                                                                                                                                                                                                                                                                                           | `permute3`, `mod289v4f`, `taylorInvSqrt4`, `u32ToF32`                                         |
| C13 | **CPU and GPU mirrors are identical in names, parameters, defaults and ranges.** A helper without a mirror states why in its Scaladoc (GPU-only: texture sampling, line varyings).                                                                                                                                                                                                                                                                                                                                                  | `Color.hsv2rgb` on both sides                                                                 |
| C14 | **Cost model per side.** GPU: Scala boilerplate and JS size are free, and the emitted WGSL must contain exactly the needed fn calls and nothing else. CPU: the linked JS for any call through the object API or an extension must be one direct kernel call with scalar arguments. Verify both when adding a helper.                                                                                                                                                                                                                | JS `Simplex.kernel.fbm2d(a.x, a.y, 4, 2.0, 0.8)`                                              |

#### Audit of existing modules

| Module                         | Deviations                                                                                                                                                                                                                                                         | Fix                                                                                                                                                                                                                                                                       |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Simplex`                      | WgslFns are the API; members repeat the family (`simplexNoise2d`, `fbmSimplex2d`); seeded variants are separate names; worley lives inside; `tilingSimplexNoise2d` named after an option rather than its torus mechanism; helpers public; params `v` / `pos` mixed | WgslFns → `Simplex.wgsl`; wrapper defs per C2–C8; `torusNoise2d`; `Worley` gets its own object; helpers `private[lib]`                                                                                                                                                    |
| `Psrdnoise`                    | algorithm name as API (hard to remember); six names for one function (`tilingRotNoise2d`, `tilingNoise2d`, `rotNoise2d`, ×2 dims); `normRot` (C7 / C9); `mod289v3f` etc. public; no fbm / seed                                                                     | rename to `Extended` (file `noise/extended.scala`); `Extended.noise2d/3d(pos, tilingPeriod = null, rot = null, seed = null)` + `noiseValue*`, `fbm*`, `fbmValue*`; `Extended.wgsl` = the full upstream 8-variant set per dim (+ seeded), picked by the wrapper; privatize |
| `Hash`                         | WgslFns are the API; `u32ToF32` public; otherwise follows its own consistent `hash<in><out>` scheme                                                                                                                                                                | wrapper defs (no defaults, so plain forwarders); privatize the helper. `hash21` / `hash2i` naming is a well-known shader idiom: keep it, documented exception to C3                                                                                                       |
| `Color`                        | WgslFns are the API; CPU side has no object (C1 / C13); GPU WGSL names lack a prefix (`hsv2rgb`)                                                                                                                                                                   | wrapper defs + `Color.wgsl`; CPU `Color` object; WGSL names → `color_hsv2rgb`                                                                                                                                                                                             |
| `Polar`                        | same as `Color`; GPU parameter `p` in one fn and `v` in the other                                                                                                                                                                                                  | same as `Color`; parameter → `pos`                                                                                                                                                                                                                                        |
| `Blur`                         | WgslFns are the API; members repeat the family (`gaussianBlur9`, `boxBlur2dAuto`); `*Auto` twins exist only because the `res` argument can't be omitted                                                                                                            | wrapper defs; `Blur.gaussian5/9/13`, `Blur.gaussian`, `Blur.box`; `box2d` / `tent2d` with `res: Vec2Expr \| Null = null` absorbing the `*Auto` variants (C4)                                                                                                              |
| `line`                         | free functions, no object (C1)                                                                                                                                                                                                                                     | `object Line` holding `cross(uvY, width)`; `lineV` / `lineOffset` extensions stay                                                                                                                                                                                         |
| downstream `sketchlib.shaders` | `Noise` removed by this plan; `Uv` / `Shapes` are out of scope (sketch-side, not library)                                                                                                                                                                          | —                                                                                                                                                                                                                                                                         |

### Naming

#### Noise families: `simplex` and `extended`

Two noise families, named by what they give the caller rather than by
algorithm name, since `psrd` is hard to remember when writing the API by hand:

- **simplex noise:** value only (Ashima simplex, 2D / 3D / 4D). The cheaper
  choice.
- **extended noise:** value **plus analytic derivative** (gradient), with
  optional tiling and gradient rotation (Gustavson / McEwan psrdnoise, 2D /
  3D). Each Scaladoc names the psrdnoise origin, for anyone searching for it.

Neither family carries "tiling" in any name. Tiling is the optional
`tilingPeriod` parameter of the extended family (default `null` = no wrapping;
a zero component of a given period leaves that axis unwrapped).

**Extended stays 2D and 3D, deliberately.** Upstream has no 4D (see below), so
there is no todo for one.

#### Upstream psrdnoise: what exists (researched 2026-09-28)

[stegu/psrdnoise](https://github.com/stegu/psrdnoise), last pushed 2023-03,
MIT, by Stefan Gustavson and Ian McEwan. Published in
[JCGT 11(1)](http://jcgt.org/published/0011/01/02/).

| File(s)                                    | What                                                                                                    |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------- |
| `src/psrdnoise2.glsl`, `psrdnoise3.glsl`   | the canonical, commented reference: `float psrdnoise(vec x, vec period, float alpha, out vec gradient)` |
| `src/psrdnoise2.wgsl`, `psrdnoise3.wgsl`   | official WGSL port, **8 functions per dimension named after their argument lists** (below)              |
| `src/psrddnoise2.glsl`, `psrddnoise3.glsl` | + second derivatives (GLSL only)                                                                        |
| `src/mpsrdnoise2.glsl`                     | 2D variant safe for 16-bit `mediump` floats                                                             |
| `src/*-min.glsl`, `src/hlsl/`              | compacted copies; HLSL ports by a contributor                                                           |
| —                                          | **no 4D anywhere** (README: "Tiling simplex flow noise in 2-D and 3-D")                                 |

The WGSL split, verbatim from the file header ("WGSL lacks overloading of user
defined functions, and considering the unfinished state of the platform I
don't trust the dead code removal, so the functions are named after their
argument lists"):

| upstream (3D; 2D likewise) | period | rotation | returns                 |
| -------------------------- | ------ | -------- | ----------------------- |
| `psrnoise3(x, p, alpha)`   | ✓      | ✓        | `f32`                   |
| `psnoise3(x, p)`           | ✓      |          | `f32`                   |
| `srnoise3(x, alpha)`       |        | ✓        | `f32`                   |
| `snoise3(x)`               |        |          | `f32`                   |
| `psrdnoise3(x, p, alpha)`  | ✓      | ✓        | `NG3` (value, gradient) |
| `psdnoise3(x, p)`          | ✓      |          | `NG3`                   |
| `srdnoise3(x, alpha)`      |        | ✓        | `NG3`                   |
| `sdnoise3(x)`              |        |          | `NG3`                   |

Other facts to carry over: periods are integers **up to 289** per axis (larger
periods corrupt the field), and upstream `alpha` is an angle in radians, where
our `rot` is in turns (C9, converted in the wrapper).

**Deferred:** the second-derivative (`psrddnoise2/3.glsl`, GLSL only) and
half-precision (`mpsrdnoise2.glsl`) variants aren't ported. The source file
header says so. It's already in the current `psrdnoise.scala` and moves into
`noise/extended.scala` on both sides.

Our current port has only the full `psr·d` core, plus two forwarders that pass
zeros. So every call today pays for period wrapping and gradient rotation.

#### Extended: one API over the upstream variant set

- **`X.wgsl` mirrors upstream 1:1**: all 8 variants per dimension, ported from
  the upstream WGSL files, plus a seeded twin of each (16 per dimension). The
  GPU side can carry that boilerplate; each shader emits only the fns it calls.
- **The wrapper picks the variant** from which options are present:
  `tilingPeriod` (`null` → `s…` instead of `ps…`), `rot` (`null` → no `r`),
  `seed` (`null` → unseeded), and value-only vs gradient by API member (below).
  On the GPU that is a shader-build-time `if`; on the CPU, `inline match` at
  compile time. So `tilingPeriod` and `rot` default to `null`, like `seed`, not
  to zero, and turning an option off costs nothing at runtime.
- **Value-only is a first-class member**, not a CPU-only kernel detail,
  because upstream provides it on the GPU too: `Extended.noiseValue2d/3d` and
  `fbmValue2d/3d`, with extensions `p.extendedNoiseValue(…)` /
  `p.extendedFbmValue(…)`. Same parameters, returns `F`.

Object API (both sides unless marked):

| Object            | Members (wrapper defs)                                                                                                                                                                                                          |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Simplex`         | `noise2d/3d/4d(pos, seed)` → value; `fbm2d/3d/4d(pos, octaves, lacunarity, gain, seed)` → value; `torusNoise2d(pos, scale, seed)` → value                                                                                       |
| `Extended`        | `noise2d/3d(pos, tilingPeriod, rot, seed)` → `Vec3` / `Vec4` (value, gradient); `noiseValue2d/3d(…)` → value; `fbm2d/3d(pos, octaves, lacunarity, gain, tilingPeriod, rot, seed)` → `Vec3` / `Vec4`; `fbmValue2d/3d(…)` → value |
| `Worley`          | `noise2d(pos, jitter, seed)`, `noise3d(pos, jitter, seed)` → `Vec2` (F1, F2)                                                                                                                                                                         |
| `Color`           | `rgb2hsv`, `rgb2hsl`, `hsv2rgb`, `hsv2rgbSmooth`, `hsv2rgbSmoother`, `hsl2rgb` (all `(c)`)                                                                                                                                      |
| `Polar`           | `polarToCart(pos)`, `cartToPolar(pos)`                                                                                                                                                                                          |
| `Hash` (GPU only) | `hash1`, `hash1i`, `hash21`, `hash21i`, `hash2`, `hash2i`, `hash3`, `hash3i`, `hash4`, `hash4i`, `hash1f` … `hash4f`                                                                                                            |
| `Blur` (GPU only) | `gaussian`, `gaussian5`, `gaussian9`, `gaussian13`, `box`, `box2d(…, res = null)`, `tent2d(…, res = null)`                                                                                                                      |
| `Line` (GPU only) | `cross(uvY, width)`; extensions `cross.lineV`, `cross.lineOffset`                                                                                                                                                               |

`X.wgsl` members (GPU only) keep one fn per code path:

- `Simplex.wgsl`: `noise2d`, `noise2dSeeded`, `noise3d`, `noise3dSeeded`,
  `noise4d`, `noise4dSeeded`, `fbm2d`, `fbm2dSeeded`, `fbm3d`, `fbm3dSeeded`,
  `fbm4d`, `fbm4dSeeded`, `torusNoise2d`, `torusNoise2dSeeded`.
- `Worley.wgsl`: `noise2d`, `noise2dSeeded`, `noise3d`, `noise3dSeeded`.

**Worley 3D follows upstream `stegu/webgl-noise` `src/cellular3D.glsl`**
(Gustavson, MIT): a 3×3×3 search window, "good F2 everywhere", returning
`vec2(F1, F2)`, the 3D sibling of the `cellular2D.glsl` our 2D port comes
from. Its `#define jitter 1.0` becomes the `jitter` parameter, as the 2D port
already does, and the seed stages go where it permutes (per the seed cost
budget). The upstream fast variants `cellular2x2.glsl` / `cellular2x2x2.glsl`
(2×2 / 2×2×2 windows) are **not** ported. Their headers say "F2 is often
wrong and has sharp discontinuities", which doesn't fit a lib function that
returns F2. Cost note: 27 cells per sample makes 3D worley the most
expensive noise here, which the benchmark shows.
- `Extended.wgsl`: the upstream set per dimension (`psr`, `ps`, `sr`, `s`,
  `psrd`, `psd`, `srd`, `sd` for 2D and 3D), each also `…Seeded`, plus
  the fbm loops over them (value and gradient, tiling or not, seeded or not).
  Member names follow upstream with our suffixes (`Extended.wgsl.psrdnoise3`,
  `Extended.wgsl.psrdnoise3Seeded`). This layer is where the upstream
  vocabulary lives, so the port stays diffable against the upstream files.

WGSL names per C11 (`simplex_noise_2d`, `extended_psrdnoise_3`,
`extended_psrdnoise_3_seeded`, `extended_fbm_3d`). The `NG2` / `NG3` return
structs map to our `vec3` / `vec4` (value in `.x`, gradient in the rest) at the
wgsl boundary, as the current port already does.

`Simplex.torusNoise2d` (formerly `tilingSimplexNoise2d`) is a different
mechanism: 4D simplex sampled on a torus, `pos` in `[0, 1]` tiling the unit
square, `scale` for frequency. So it stays its own function, named after the
mechanism.

Extensions (identical on CPU and GPU receivers; `P` = `Vec2` / `Vec3` /
`Vec4`, or the matching `*Expr`; `F` = `Double` / `FloatExpr`):

| Extension                                                                                                     | Receivers | Returns                                        |
| ------------------------------------------------------------------------------------------------------------- | --------- | ---------------------------------------------- |
| `p.simplexNoise(seed = null)`                                                                                 | 2, 3, 4   | `F`                                            |
| `p.simplexFbm(octaves = 4, lacunarity = 2.0, gain = 0.5, seed = null)`                                        | 2, 3, 4   | `F` in `[-1, 1]`                               |
| `p.simplexTorusNoise(scale, seed = null)`                                                                     | 2         | `F`                                            |
| `p.extendedNoise(tilingPeriod = null, rot = null, seed = null)`                                               | 2, 3      | `Vec3` / `Vec4` (value, gradient)              |
| `p.extendedNoiseValue(tilingPeriod = null, rot = null, seed = null)`                                          | 2, 3      | `F`                                            |
| `p.extendedFbm(octaves = 4, lacunarity = 2.0, gain = 0.5, tilingPeriod = null, rot = null, seed = null)`      | 2, 3      | `Vec3` / `Vec4` (value in `[-1, 1]`, gradient) |
| `p.extendedFbmValue(octaves = 4, lacunarity = 2.0, gain = 0.5, tilingPeriod = null, rot = null, seed = null)` | 2, 3      | `F` in `[-1, 1]`                               |
| `p.worleyNoise(jitter = 1.0, seed = null)`                                                                    | 2, 3      | `Vec2` (F1, F2)                                |
| `c.rgb2hsv` … `c.hsl2rgb`                                                                                     | 3         | same type as `c`                               |
| `p.polarToCart` / `p.cartToPolar`                                                                             | 2         | same type as `p`                               |

#### Hash extensions (GPU only)

Hashes are everyday shader utilities, so the common ones get extensions,
exported by the prelude. They live next to `object Hash` in
`shader/lib/random/hash.scala` and forward `inline` to it. Everything else
stays on the `Hash` object.

| Extension | Receivers                 | Maps to                            | Returns                  |
| --------- | ------------------------- | ---------------------------------- | ------------------------ |
| `x.hash`  | `FloatExpr`, `Vec2–4Expr` | `hash1f` … `hash4f`                | same dimension, `[0, 1)` |
| `u.hash`  | `UIntExpr`, `UVec2–4Expr` | `hash1`, `hash2`, `hash3`, `hash4` | same dimension, `[0, 1)` |
| `u.hashU` | `UIntExpr`, `UVec2–4Expr` | `hash1i` … `hash4i`                | same dimension, `u32`    |
| `u.hash1` | `UVec2Expr`               | `hash21`                           | scalar `[0, 1)`          |

- **Receivers are shader types only.** On a CPU value, `v.hash` doesn't
  resolve ("not a member of Vec3") and completion never offers it, the same
  way `cross.lineV` works. That keeps the extension from suggesting CPU
  support. CPU randomness stays `utils/random`.
- **Forward-compatible:** if the deferred CPU `Hash` port happens, the same
  names move into the shared `transparent inline` form (C6) by adding the CPU
  branch, with no call-site change.
- **Plain extensions, not the shared form:** they have no default parameters
  and one receiver type per alternative, so ordinary overloaded extensions
  work (the defaults restriction doesn't apply). The `hash` overloads on float
  and uint receivers resolve by receiver type.

#### Family symmetry

The two families share one shape. Extended's parameter lists are simplex's plus
the extended-only options, inserted in the same place, and extended's return is
simplex's plus the gradient:

|                    | simplex                                        | extended                                                               |
| ------------------ | ---------------------------------------------- | ---------------------------------------------------------------------- |
| noise params       | `(pos, seed)`                                  | `(pos, tilingPeriod, rot, seed)`                                       |
| fbm params         | `(pos, octaves, lacunarity, gain, seed)`       | `(pos, octaves, lacunarity, gain, tilingPeriod, rot, seed)`            |
| fbm defaults       | `octaves = 4, lacunarity = 2.0, gain = 0.5`    | same                                                                   |
| noise / fbm return | value                                          | value + gradient (`Vec3` for 2D, `Vec4` for 3D), for noise **and** fbm |
| dimensions         | 2, 3, 4 (noise and fbm)                        | 2, 3 (psrdnoise has no 4D)                                             |
| seed               | optional on everything                         | optional on everything                                                 |
| family-only        | `torusNoise2d` (unit-square tiling through 4D) | `tilingPeriod`, `rot`                                                  |
| value-only         | is the default                                 | `noiseValue*` / `fbmValue*` members, `…Value` extensions (both sides)  |

Remaining asymmetries are inherent to the algorithms: 4D exists only for
simplex, and tiling and rotation only for extended (simplex tiles only through
the torus trick).

`tilingPeriod` is a vector of the receiver's dimension (`Vec3` on a CPU
`Vec3`, `Vec3Expr` on a `Vec3Expr`, narrowed like the numbers), with integer
components up to 289, and a zero component leaves that axis unwrapped, as in
`vec3(8, 0, 8)`. Its default `null` selects the non-tiling upstream variant. Whether a scalar shorthand is worth adding is
left to implementation. It can't be an overload next to the defaulted def, but
it could be a union type.

## Seed handling

### Today: three different behaviors

| Variant                | Seed type | Mechanism                                                     | Effect of a small seed change                                                                      |
| ---------------------- | --------- | ------------------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| `simplexNoise2dSeeded` | `Float`   | extra `permute(p + seed)` after the lattice hash              | continuous but rapidly changing field; integer steps give new fields                               |
| `simplexNoise3dSeeded` | `Vec3`    | `floor(seed + 0.5)` added to the lattice index before hashing | **none** until it crosses a half-integer, then the _same_ field moved by an integer lattice vector |
| `sketchlib Noise.fbm3` | `Vec3`    | `pos * freq + seed`                                           | the field slides continuously                                                                      |

The 3D variant is a quantized position offset in disguise, so it adds nothing
over `noise3d(p + offset)`.

### Target: optional hashed scalar seed, everywhere

Wanted behavior: **any change of seed yields an unrelated field** (the 2D
intent, made reliable). Design:

1. `seed` is an optional scalar (`Double` / `FloatExpr`, default `null` = the
   unseeded code path) on every noise wrapper and extension: simplex 2D / 3D /
   4D, their fbms, extended noise 2D / 3D and its fbms.
2. The seed is hashed from its **f32 bit pattern** with an integer hash
   (`Hash.hash1i` over `bitcast<u32>(seed)`), so `0.5` and `0.5000001` are as
   unrelated as `1` and `2`.
3. The hash gives two **integer** offsets `s1, s2 ∈ [0, 289)`. They feed two
   extra permute stages after the lattice hash:
   `p = permute(permute(p + s1) + s2)`. That makes 289² ≈ 83k distinct
   fields, and every permute input stays an integer below 2²⁴.
4. **Why integers:** permute is `((34x + 1)·x) mod 289`. On integer input it is
   exact in f32 and f64 alike, so the CPU gets the GPU's gradient choice for
   free, which is the preferred same-seed-same-field behavior (see "CPU / GPU
   parity"). It also keeps every seeded field a proper permutation of the
   lattice hash. A fractional offset (what 2D does today) gives chaotic,
   continuous-in-seed variation instead, and loses that parity. 83k fields
   are plenty.
5. **Extended noise:** the stages go after its own `mod tilingPeriod` wrap and
   before gradient selection, so a seeded field still tiles exactly.
6. **fbm seeded:** octave `i` uses `s1 + i` (wrapped to 289), so octaves
   don't share a hash pattern.

"Slide the field" is still spelled `p + offset` and needs no API.

### Cost budget (GPU and CPU)

Seeding must stay cheap next to the noise itself. Nothing notably expensive
gets added to the upstream kernels:

- **Per call, once:** one scalar integer hash of the seed (`bitcast` plus a few
  `u32` multiply / xor / shift ops) and the split into `s1, s2`. It's computed
  once per noise call, not per corner. In fbm it's computed **once before the
  octave loop**, and octaves only add `+ i`.
- **Per corner group:** exactly two extra permute steps, done as the same
  vectorized `permute` the upstream already uses (`permute_3_` / `permute_4_`
  over all corners at once, not per corner). No extra `floor` / `fract`,
  no transcendental functions, no branches, no loops.
- **Unseeded paths are untouched:** the wrapper routes `seed = null` to the
  unmodified upstream fn. Its WGSL is byte-identical to the unseeded port.
- **CPU:** the same budget becomes two extra table lookups per corner and the
  one scalar seed hash per call.
- **Check:** compare the seeded WGSL body with the upstream one in review
  (the diff must show only the seed stages), and compare seeded vs unseeded
  ops/sec in the CPU benchmark.

### Tests

- Determinism: same seed gives the same value.
- Decorrelation: nearby seeds (`s`, `s + 1e-6`) have near-zero correlation over
  a grid of samples.
- Tiling survives seeding:
  `p.extendedNoise(tilingPeriod = t, seed = s) == (p + t).extendedNoise(tilingPeriod = t, seed = s)`.
- WGSL emission: an unseeded wrapper call emits only the unseeded fn.

## Extended fbm

Port of `sketchlib.shaders.Noise.tilingFbm3`, as a WGSL loop (like the simplex
fbms) and as a CPU loop:

- **`lacunarity` is a parameter, as in simplex fbm** (default 2.0). Octave `i`
  samples `pos · lacⁱ` with tiling period `tilingPeriod · lacⁱ`. That stays a
  whole number, so the fbm still tiles, only for a **whole-number lacunarity**
  while `tilingPeriod` is set. Without tiling any lacunarity works. This is
  documented and can't be checked for runtime values; `tilingFbm3` hard-coded
  2 instead.
- `tilingPeriod` components must be whole numbers (the lattice wraps at integer
  coordinates). A zero component leaves that axis unwrapped, and the all-zero
  default makes it a plain non-tiling fbm with gradient-noise octaves.
- Optional `rot` (rotation in turns) is passed to every octave, which gives
  flow-noise animation.
- **Returns value + gradient**, like extended noise. The value is the
  normalized `Σ ampᵢ·nᵢ / Σ ampᵢ`, and the gradient is its exact derivative,
  `Σ ampᵢ·lacⁱ·∇nᵢ / Σ ampᵢ`, since each octave's gradient scales with its
  frequency. `fbmValue2d/3d` loops over the value-only upstream variants
  instead.
- The caller rules from `tilingFbm3`'s Scaladoc move with it: whole period,
  whole-number lacunarity when tiling, only non-tiling axes may shear into
  tiling ones.

## CPU implementation

- Kernels over `Double` scalars, following library bundle discipline: `while`
  loops, scalar locals, no Scala tuples or collections, no intermediate vector
  allocations inside the kernels. Simplex / worley per "CPU simplex" below;
  extended per the upstream-psrdnoise rule further down.
- **Kernels take scalar components and return scalars:**
  `kernel.fbm2d(x, y, octaves, lacunarity, gain): Double`. The inline object
  API and the extensions unpack `pos.x, pos.y` at the call site, so a call
  allocates nothing.
- **Vector results** (color conversions, polar, worley F1/F2, extended value +
  gradient) come in two kernel forms:
  - `…Into(…, out: Vec3): Unit` writes into a caller-owned vector, the
    hot-loop form;
  - the allocating form returns a fresh `Vec3` / `Vec4`, matching the GPU
    return and the immutable vector ops.

  The object API and the extensions use the allocating form. Extended's
  value-only members (`noiseValue*`, `fbmValue*`) map to scalar kernels and
  allocate nothing.

- **Extended follows upstream as closely as possible.** Port from the
  canonical commented GLSL (`src/psrdnoise2.glsl`, `src/psrdnoise3.glsl`),
  statement by statement, keeping upstream variable names (`uv`, `i0`, `f0`,
  `o1`, `v0`–`v3`, `x0`–`x3`, `iu`, `iv`, `hash`, `psi`, `gx`, `gy`, `w`, `w2`,
  `w4`, `n`, `dw`, `dn0`, …) and step order, so a side-by-side diff with the
  upstream file reads line for line. Use the WGSL files for the variant split:
  the same 8 kernels per dimension (`Extended.kernel.psrdnoise3`, `psnoise3`, …;
  value-only kernels return `Double`, gradient kernels write into an `out`
  vector or return one). Deviations are limited to three things, each commented
  at the site: GLSL `out` parameters becoming our return / `out` vector, `alpha`
  radians fed from `rot` turns in the wrapper, and the seed permute stages.
  The existing GPU port gets diffed against upstream `psrdnoise*.wgsl`
  (version 2022-02-28) during phase 1, and upstream fixes are taken over.
- Kernels are plain module methods: the Scala.js optimizer may inline small
  ones, and large ones (noise) stay a single direct call.
- The seed hash uses the same algorithm as the GPU: f32 bits via a shared
  `Float32Array` / `Uint32Array` scratch, u32 arithmetic via `Math.imul` and
  `>>> 0`. It costs one scalar hash per call and gives same-seed-same-field
  for free. The array types here are **set by the algorithm**, not a
  performance choice like the gradient tables: the GPU hashes
  `bitcast<u32>(seed)`, the f32 bit pattern, and a `Float32Array` aliased by a
  `Uint32Array` over one 4-byte buffer is how JS reads those bits. A
  `Float64Array` would hash the f64 pattern, giving a different field per
  seed. If the benchmark ever shows the seed hash mattering, cache the last
  seed and its hash (seeds are usually constant across many calls) rather than
  changing storage.
- Parity (see "CPU / GPU parity"): **required** is the same characteristics
  (range, distribution, frequency, look) per parameter set. **Preferred** is
  the same hash and gradient choice as the GPU, which the table design below
  gets almost for free, so final values agree within f32 precision (the CPU
  kernel math runs in f64). Where a preferred-parity detail costs kernel
  performance or real complexity, it gives way.
- The existing `rgb2hsv` / `rgb2hsl` CPU code uses Scala tuple destructuring
  (`val (px, py, pz, pw) = …`), which allocates. Rewrite it with scalar locals
  during the move.

### CPU simplex: references and idioms (researched 2026-09-28)

Two references, one for **what** is computed and one for **how**:

| Reference                                                                                                                                                                                   | Role                                                                                                                                                                                                                                         |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [stegu/webgl-noise](https://github.com/stegu/webgl-noise) `src/noise2D.glsl`, `noise3D.glsl`, `noise4D.glsl`, `cellular2D.glsl`, `cellular3D.glsl` (Ashima / Gustavson, MIT, maintained)                                | the upstream of our WGSL simplex / worley. Defines the field: skew constants, `mod289` permute hash, gradient derivation (2D: `fract(p / 41)` ramp; 3D: 7×7 points on an octahedron + `taylorInvSqrt`; 4D: `grad4`), kernel radius and scale |
| [jwagner/simplex-noise.js](https://github.com/jwagner/simplex-noise.js) `simplex-noise.ts` (MIT, ~1.8k★, 2024) — itself Gustavson's speed-improved Java reference + Eastman's optimizations | the idiomatic, benchmarked JS shape of a simplex kernel: the performance patterns to copy                                                                                                                                                    |

They disagree on the hash and the gradient set. simplex-noise.js uses a random
256-entry permutation and 12 edge gradients in 3D, where Ashima uses the
`mod289` hash and 49 octahedron gradients. That's a different field with
a slightly different look and range. The _required_ part of parity (same
characteristics) already favors Ashima's math, and its _preferred_ part
(identical values) comes along at no kernel cost. So: **Ashima's math,
simplex-noise.js's execution.**

- **Hash as tables, identical to the GPU.** On integer input, Ashima's
  `permute(x) = mod289((34x + 1)·x)` is a pure function of `x ∈ [0, 289)`, so
  a precomputed `Int32Array` gives exactly the same values as the polynomial.
  Chained lookups add a lattice coordinate first (`perm[perm[i] + j]`), so the
  table is extended past 289 (to cover `max(perm) + max(i) + 1`), in the same
  spirit as simplex-noise.js's doubled 512 table, and no `%` is needed in the
  kernel. The seed stages (`s1`, `s2`) are two more lookups.
- **Gradients as tables per hash value.** Ashima derives a gradient
  arithmetically from the hash on every call (3D: `j = p − 49·floor(p/49)`,
  octahedron mapping, then `taylorInvSqrt` normalization). On the CPU that is
  a function of the hash value alone, so precompute `gradX/Y/Z` tables (289
  entries) once, as simplex-noise.js does with `permGrad2x` / `permGrad2y`.
  Storage type per the next point.
- **f32 behavior of the tables, mostly for free.**
  - **Gradient values** are rounded to f32 when stored: either stored into a
    `Float32Array`, or `Math.fround`-ed into a `Float64Array` (the same value).
    Computed in f64 and then rounded once, they match the GPU's f32 gradients
    to within an ulp, which is enough. `taylorInvSqrt`'s approximation is used
    instead of an exact normalize, since that's a formula difference, not
    rounding.
  - **The permute hash is integers**, exact in any storage type. The one GPU
    quirk storage can't reproduce is inside Ashima's
    `mod289(x) = x − floor(x · (1/289)) · 289`: `1/289` isn't exact in f32, so
    for some multiples of 289 the product lands just under a whole number and
    the result is 289 instead of 0. The generator reproduces it with a
    `Math.fround` on that multiply and on the product. It's generation-time
    code only, nothing per call. If that turns out fiddly, exact integer `mod`
    is acceptable (similar is enough).
  - **Storage type is decided by the benchmark.** simplex-noise.js measured
    `Float64Array` faster than `Float32Array` for gradient tables ("double
    seems to be faster than single or int's"), likely because every
    `Float32Array` read widens to double. So the default is f32-rounded values
    in a `Float64Array`, with `Float32Array` benchmarked against it. The
    permute table is an `Int32Array` (or whichever integer array benchmarks
    best).
- **simplex-noise.js kernel idioms:** `Math.floor(x) | 0` for lattice indices;
  skew / unskew with named constants (`F2`, `G2`, `F3`, `G3`, `F4`, `G4`);
  corner ordering by comparisons (2D / 3D) and rank ordering (4D, Gustavson
  2012); an **early out per corner** (`if (t >= 0)`) where the WGSL computes
  `max(0, t)` for all corners. The result is the same, and the CPU skips work.
  Contributions go into scalar locals `n0…n3`, then scale by Ashima's factor
  (`130` / `105` / `49`) to keep its output range.
- **Where they conflict, follow Ashima** (for the field) and comment the site.
  Everything else follows simplex-noise.js's structure, so its benchmarks and
  commentary stay applicable.
- **fbm, seeds, torus noise** are thin loops or wrappers over those kernels.
  Worley 2D and 3D follow the same pattern: `cellular2D.glsl` /
  `cellular3D.glsl` math, table hash, JS kernel idioms. The 3D kernel loops
  over the 27 cells with scalar F1 / F2 tracking instead of upstream's
  vectorized swizzle sorting, which is the natural CPU form of the same
  computation.

The extended family is the exception: it follows upstream psrdnoise statement
by statement (see above), since its upstream reference is already scalar-ish
GLSL and diffability matters more there. Whether its `mod289` / permute steps
also get the table treatment is decided when benchmarking (below).

#### Benchmark

A small `bun` benchmark script (not a test) measures ops/sec for
`simplexNoise` 2D / 3D / 4D, `simplexFbm`, `extendedNoiseValue` and
`extendedNoise` against simplex-noise.js as the yardstick (dev-only
dependency), and picks the table storage (`Float64Array` vs `Float32Array`
for gradients, integer array type for the permute table). Target: simplex kernels in the same range as simplex-noise.js.
Extended is expected to be slower (gradient + rotation) and is measured
per variant.

### Tests (`test/math/`)

- Range: noise in about `[-1, 1]`; fbm strictly within `[-1, 1]` for various
  octaves / gains.
- **Reference values:** a handful of `(input → output)` pairs per function,
  taken from the GLSL originals (stegu/webgl-noise, stegu/psrdnoise
  `psrdnoise2/3.glsl`) run in f64, for every extended variant (with and without
  period, rotation, gradient). They pin the port, and the WGSL gets checked
  against the same numbers through the new example.
- Tiling: `extendedNoise` and `extendedFbm` repeat under `p + tilingPeriod`.
- Seed tests as above.

## Prelude

`trivalibs.prelude.painter` exports the shared extension layer, which covers
both sides through its compile-time branches:

```scala
export trivalibs.graphics.lib.color.`color$package`.{*, given}
export trivalibs.graphics.lib.coords.`coords$package`.{*, given}
export trivalibs.graphics.lib.noise.`extensions$package`.{*, given}
export trivalibs.graphics.lib.`args$package`.{FloatArg, IntArg, Vec2Arg, Vec3Arg, Vec4Arg}
```

The **objects** (`Color`, `Simplex`, …) are not exported: each name exists in
both side packages, and they are the explicit-import path. The `…$package`
exports above carry only top-level definitions, so the CPU objects in the same
packages stay out. The `Hash`, `Blur` and `Line` objects stay explicit
imports as today. The GPU-only hash extensions are exported too:

```scala
export trivalibs.graphics.shader.lib.random.`hash$package`.{*, given}
```

Blur has no extensions, and Line's `lineV` / `lineOffset` stay an explicit
import.

## Usage after the plan

```scala
import trivalibs.prelude.core.{*, given}
import trivalibs.prelude.painter.{*, given}

// CPU — setup code
val bg: Vec3 = Vec3(0.6, 0.5, 0.4).hsv2rgb
val height: Double = Vec2(x, z).simplexFbm(octaves = 5, gain = 0.6)
val tile: Vec4 = Vec3(x, 0.0, z).extendedFbm(tilingPeriod = Vec3(8, 0, 8))   // value + gradient
val tileValue: Double = Vec3(x, 0.0, z).extendedFbmValue(tilingPeriod = Vec3(8, 0, 8)) // scalar kernel, no allocation
val slope: Vec3 = Vec2(x, z).extendedNoise()          // value + gradient

// GPU — the same calls in a shader body
color := vec3(hue, 0.5, 0.4).hsv2rgb
n := ctx.in.uv.simplexFbm(octaves = 5, gain = 0.6).fit1101
grain := (wp * scale).extendedFbmValue(octaves = 3, tilingPeriod = vec3(8, 0, 8), seed = 7.0)
flow := ctx.in.uv.extendedNoise(rot = t * 0.1).yz     // animated gradient field
n := ctx.in.uv.simplexFbm(octaves = ctx.bindings.octaves, gain = ctx.bindings.gain)

// object form — same parameters, explicit side
import trivalibs.graphics.shader.lib.noise.Simplex
n := Simplex.fbm2d(uv, gain = 0.8, seed = 3.0)

// raw WGSL composition — the definition layer
myFn.withDeps(Simplex.wgsl.fbm2d)
```

**`X.wgsl` is public, by decision.** It's the only way to hand lib fns to
`withDeps` for raw WGSL composition. A raw WGSL string never passes through a
wrapper, so no wrapper can register the dep for it.

**`withDeps` only accepts the `wgsl` layer, and the compiler enforces it.**
`withDeps(ds: WgslFnData*)` takes WgslFn values. A wrapper def passed by
mistake fails to compile (verified with a probe against the real library):

| Call                                   | Result                                                                                                 |
| -------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| `withDeps(Simplex.wgsl.fbm2d)`         | compiles                                                                                               |
| `withDeps(Simplex.fbm2d)`              | error: Found `(Vec2Expr, IntExpr, FloatExpr, FloatExpr) => FloatExpr` (eta-expanded), Required `WgslFnData` |
| `withDeps(Simplex.fbm2d(uv))`          | error: Found `FloatExpr`, Required `WgslFnData`                                                        |

The messages are clear enough to point at the fix. They get locked in by an
munit test using `scala.compiletime.testing.typeCheckErrors`, so a later
refactor (e.g. a `Conversion` into `WgslFnData`) can't silently loosen it.

The remaining risk is one the compiler can't see: **picking the wrong `wgsl`
variant.** Raw WGSL calls a fn by name. If the body says
`simplex_fbm_2d_seeded(…)` but the dep is `Simplex.wgsl.fbm2d`, it compiles in
Scala and fails at WGSL compile time. That's covered by docs:

- C11 makes the name mapping mechanical (`Simplex.wgsl.fbm2dSeeded` ↔
  `simplex_fbm_2d_seeded`), and each `wgsl` member's Scaladoc states its WGSL
  name and signature.
- The `X.wgsl` object Scaladoc says: this is the layer for `.withDeps` and raw
  WGSL composition. For shader DSL code use `X.*` / the extensions, which
  register their deps automatically. For raw WGSL, depend on exactly the
  variant you call by name.
- `withDeps`' own Scaladoc (`shader/dsl/fn.scala`) gets a line: pass `WgslFn`
  values (lib: the `X.wgsl` members), not the wrapper defs.

## Phases

0. **Pattern check in the real library** (the core pattern is already verified
   by the probe above): the `transparent inline` extension shape on
   `Color` / `Polar` (parameterless, vector-returning), on a 2D + 3D receiver
   union, and with a `tilingPeriod` vector argument. Check the linked JS of each CPU
   call site (C14), and IDE hover / completion on the union-typed parameters.
1. **Restructure and conventions:** add `Double.toExpr` to `math/gpu`,
   create `graphics/lib/`, move GPU noise to
   `shader/lib/noise/`, split `Worley` out, move WgslFns into `X.wgsl` behind
   wrapper defs, rename `Psrdnoise` → `Extended` (diff the port against
   upstream `psrdnoise*.wgsl`, then port the missing upstream variants so
   `Extended.wgsl` holds the full 8-per-dimension set behind the unified
   wrapper), and
   apply the audit (C1–C14) to `Simplex`, `Extended`,
   `Hash`, `Color`, `Polar`, `Blur` and `Line`: members, parameters, WGSL
   names, private helpers. Move CPU color / coords to `graphics/lib/`, onto
   `Color.kernel` / `Polar.kernel` + inline object API + shared extensions,
   dropping the `Vec3Base` typeclass receivers. Update the prelude, tests,
   examples and downstream imports / call sites. No behavior change.
2. **Seeds:** seed hash WgslFn, rework the seeded simplex fns, add 4D seeded
   and the seeded fbms, route them through the optional `seed`. WGSL emission
   tests. Every simplex WGSL change starts from the upstream reference
   (`stegu/webgl-noise` `noise2D/3D/4D.glsl`): diff the current port against
   it first and take over upstream fixes. Seeded fns are the upstream fn plus
   only the seed stages, placed where upstream permutes. Stay within the cost
   budget in "Seed handling".
3. **Extended fbm + seeded extended noise** (GPU), folded into the
   `Extended.noise*/fbm*` wrappers. **Worley 3D** (GPU) ported from upstream
   `cellular3D.glsl`, with a seeded twin.
4. **Hash extensions** (GPU only, next to `object Hash`, prelude export) and
   **shared extensions** for all noise (GPU branches; CPU branches stubbed
   with `compiletime.error("CPU … not yet implemented")` until phase 5).
5. **CPU noise:** kernels (simplex and worley: Ashima math on f32-emulated
   tables in simplex-noise.js style; extended: upstream-psrdnoise port; fbms,
   seeds, `…Into` forms), inline object API, CPU extension branches, munit
   tests with reference values, the benchmark against simplex-noise.js, and a
   linked-JS check of representative call sites (C14).
6. **Example:** `examples/noise_cpu_gpu`, which shows CPU-evaluated noise
   (e.g. a height line or dots) over the GPU-shaded field of the same function
   and makes the characteristics comparable, including identical values
   where the preferred parity holds. Update `noise_tests` to the new API.
7. **Downstream migration** (graphics repo):
   - Replace `Noise.fbm3` in `rooms/base`, `rooms/canvases`,
     `templates/rooms/{hex-partitions, l-room, grid-canvases}` and
     `tests/texture-bake` with `p.simplexFbm(…)`. `seed = vec3(k)` becomes
     `(p + k).simplexFbm(…)` (slide) or `p.simplexFbm(seed = k)` (new field).
   - Replace `Noise.tilingFbm3` in `templates/open-space` and `gradients` with
     `p.extendedFbmValue(tilingPeriod = …)`. Update the template prose that
     mentions `Noise.scala`.
   - Rename `Psrdnoise.*` call sites (`templates/open-space`, `gradients`, any
     direct users) to `Extended` / `extendedNoise`.
   - Delete `src/shaders/Noise.scala`. `Uv` and `Shapes` stay.
   - Settle the normalization call sites listed above.
   - Rebuild all touched sketches.
8. **Docs:** update `independent-todos.md` (entry → done); trivalibs
   `CLAUDE.md`: replace the receiver-extension rule with the three-layer
   pattern (`X.wgsl` / `X.kernel`, wrapper object API, `transparent inline`
   extensions in `graphics/lib`), the convention table C1–C14 as the
   standing rule for new lib helpers, and the defaults-overload restriction
   plus the CPU cost model as the reasons; the graphics `CLAUDE.md`
   `sketchlib.shaders` examples (`Noise.fbm3`); `docs/guide` where it covers
   these. Move this plan to `documents/done/`.

## Open questions


None at the moment. All questions raised during planning are resolved above.
