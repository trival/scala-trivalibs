# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in the **trivalibs** library repository.

trivalibs is a standalone Scala.js library, consumed by downstream projects as a
git submodule (or, eventually, a published artifact). This repo can be opened
directly as its own workspace — `project.scala` at the root is the single source
of truth for lib + tests + examples.

## Build & Dev Commands

```bash
bun run check            # Type-check the library in isolation (scala-cli compile src)
bun run test             # Run all tests
bun run examples:build   # Build all examples → examples/out/
bun run examples:watch   # Incremental examples build with file watching
bun run examples:dev     # Bun static dev server for examples
bun run publish:local    # Publish a Scala artifact to ~/.ivy2/local
```

All scripts run from the repo root and pass `src` / `examples` / `test` plus
`project.scala` explicitly as scala-cli inputs (never a bare `.` — that pulls in
unrelated sources and can hang). Never use sbt.

## Standalone workspace

`project.scala` at the root carries every `using` directive for the whole repo
(lib + tests + examples) as a single scala-cli project. Opening `trivalibs/`
directly in VS Code / Metals gives a fully type-checked view of everything.

Downstream consumers exclude this file
(`//> using exclude trivalibs/project.scala`) so their own `project.scala` is
the only config Metals sees when the consumer repo is the workspace root.

## Tests

Tests live in `test/`, picked up automatically by the `.test.scala` suffix. No
per-folder setup file — directives come from the root `project.scala`.

**Adding a new test file**: create `test/<area>/<Name>.test.scala`, extend
`munit.FunSuite`.

```scala
// test/shader/MyFeature.test.scala
package trivalibs.graphics.shader.dsl

import munit.FunSuite

class MyFeatureTest extends FunSuite:
  test("description"):
    assertEquals(...)
```

## Project Overview

A Scala.js library for web and graphics programming, evolving toward a type-safe
painting framework. The long-term vision is a unified CPU/GPU programming model
where Scala types and math work identically on both sides — replacing WGSL
strings with a Scala shader DSL.

The library spans two integrated halves: the **graphics** stack (math, shader
DSL, buffers, geometry, scene, painter) for GPU rendering, and **preact** —
type-safe Preact bindings with signals and an HTML DSL — for the interactive DOM
layer that wraps and drives the graphics canvases (UI controls, overlays, layer
composition on top of the rendered output).

Imperative style throughout: it targets JavaScript APIs directly and avoids
functional patterns and the Scala standard library to minimize overhead. Scala 3
is used for its metaprogramming and contextual abstraction — great tailored DSLs
with minimal low-level overhead.

## Architecture

### Source Layout

- `src/graphics/math/` — Vec2–4, Mat2–4 with three representations each (mutable
  class, immutable tuple, buffer type). Operations via type class traits
  (`Vec3Base[Num, Vec]`, `Vec3Mutable`, `Vec3ImmutableOps`).
- `src/graphics/shader/` — `ShaderDef` with 7 type parameters. Named tuple type
  params → compile-time WGSL structs + WebGPU layouts. Key files: `types.scala`
  (WGSLType type class), `derive.scala` (WGSL generation), `layouts.scala`
  (vertex/bind group layouts), `builtins.scala`. `shader/lib/` holds reusable
  WGSL function libraries (noise, color, blur, hashing, coords).
- `src/graphics/buffers/` — `BufferBinding[T, F]` (CPU↔GPU uniform sync),
  `AttribLayout` + `allocateAttribs` (typed vertex data).
- `src/graphics/geometry/` — mesh / plane / grid / polygon / shape geometry.
- `src/graphics/scene/` — transform / camera / scene graph.
- `src/graphics/painter/` — the Painter abstraction. `webgpu.scala` holds the
  manual WebGPU JS facade traits — painter-internal, not a generic binding.
- `src/utils/` — generic JS helpers: `bufferdata.scala`
  (`StructArray`/`StructRef`, zero-cost typed binary buffers), `numbers.scala`
  (numeric extensions), `js.scala` (interop helpers), `animate.scala`
  (`requestAnimationFrame` lifecycle), `misc.scala`, `random.scala`.
- `src/preact/` — type-safe Preact bindings with signals and HTML DSL; the
  interactive DOM layer that integrates with and drives the graphics layers.
- `examples/` — one example per feature, each with `index.html` + `main.js` +
  `*.scala`. Compiled JS lands in `examples/out/`. Examples double as
  idiomatic-usage docs and ship with the library.

### Critical Type-Level Patterns

**Named tuples as schema**: `type Attribs = (position: Vec2, color: Vec4)` —
tuple position = layout index, field name = WGSL variable name, type = WGSLType
mapping.

**Named tuple aliases don't reduce in match types**: The compiler can't prove a
type alias is a `NamedTuple`. Use `given` chains (implicit search) instead of
match types — implicit search unfolds aliases correctly. See
`src/graphics/buffers/attributes.scala` for the pattern.

**`transparent inline` + `summonFrom`**: When a function must return a concrete
`StructArray[F]` derived from a type class, declare return type `Any` with
`transparent inline`. The compiler substitutes the concrete type at each call
site. Used in `allocateAttribs`.

### Design Documents

- `documents/*.md` — Living blueprints for feature designs and implementations,
  for not yet completed features.
- `documents/done/*.md` — Completed designs, for reference and historical
  record, including base designs and decisions that shaped the current codebase.
- `documents/scala-reference/*.md` — Scala language features and conventions,
  for reference when writing new code or reviewing PRs. Advanced recent features
  not yet present in general language model knowledge.
- `documents/rust-painter/*.md` — Reference of the Rust Painter implementation,
  that the Scala port follows. Our scala implementation has mostly reached
  feature parity.

## JS Bundle Size Rules

**This is the library — every byte here is inlined into every downstream
bundle.** Optimization is critical: zero Scala stdlib at runtime, only the type
system and compile-time features. Every runtime construct must compile to
minimal JS:

- **No `enum`**: Use `opaque type Foo = String` with `val` constants in the
  companion. Add `extension (x: Foo) inline def toJs: js.Any` if the opaque type
  must be passed to `js.Dynamic.literal`. Note: `inline val` is not allowed on
  opaque types — use plain `val`.
- **No `scala.collection.*`**: Use `Arr` (`js.Array`), `Dict` (`js.Dictionary`),
  or manual loops. `Dict[V]` works as a string-keyed cache (plain JS object).
  For integer-indexed sparse data, use `Arr[T | Null]`.
- **No `Option`**: Two substitutes in `utils/js.scala`, pick by semantics:
  - `Opt[T]` (`T | Null`) — nullable value. Empty is `null`. Check with
    `.isNull` / `.notNull`, unwrap with `.getOr(default)` / `.get`.
  - `Maybe[T]` (`js.UndefOr[T]`) — "not provided" semantics. Empty is
    `Maybe.Not`. Unwrap with `.orElse(default)` / `.safe`, chain with
    `.orMaybe(...)`. Build conditionally with `maybe(condition, value)`.
- **No `case class` for keys**: Build string keys manually with `s"..."` for
  cache lookups in `Dict`.
- **`.map/.filter/.sortBy` must delegate to JS**: Use `inline` extension methods
  on `Arr` that compile to raw `js.Array` methods — never Scala collection
  traits. Add new helpers as needed.
- **JS-native classes for structured data**: `class Foo(...) extends js.Object`
  preserves field names in JS output with zero overhead. But try pure scala
  classes without js.Object first, to let the scala compiler apply its full set
  of optimisations. If we observe compile size regressions or need runtime field
  names, we can resolve to js.Object extension.
- **`js.Dynamic` / `Obj.literal`** fine internally; user-facing API typed.
- **Trivalibs helpers everywhere**: `Arr`, `Dict`, `Obj.literal`, `Opt`,
  `Maybe`, `maybe()`.

### Library code vs. example code

Optimisation aggressiveness depends on _where_ the code lives:

- **Library code (`src/`)**: optimise for native JS APIs aggressively. Library
  code is inlined into every downstream bundle, so any Scala-stdlib leak
  multiplies across every consumer. Prefer raw JS calls, `while` loops, and
  native helpers even when more verbose. Add `inline` extension methods so the
  ergonomic call site still compiles to native JS.
- **Example code (`examples/`)**: Scala convenience shorthands are allowed.
  `for`-comprehensions, string interpolation etc. are fine here — readability
  wins, and the cost is local to one example bundle.

Note: `Arr(...)` literals are safe in library code too — `Arr.apply` has
concrete-arity `inline` overloads (0..12) that compile to native `js.Array(...)`
with no varargs pipeline. The library-vs-example split is about Scala-stdlib
constructs that have _no_ native-compiling helper, not about trivalibs helpers.

### Known stdlib-leak traps (verified bundle-size wins)

These all _look_ harmless but pull large Scala-stdlib subtrees. In library code,
avoid them; in `inline` library helpers, route through native JS:

- **`f"...%.1f..."` interpolator** → links the whole `java.util.Formatter`
  ecosystem (~1.9k lines: Formatter, Locale, BigInteger, BigDecimal, every
  `IllegalFormat*Exception`). Use `js.JSNumberOps`'s `.toFixed(n)` instead.
- **Scala varargs (`xs: T*`)** → every call site goes through
  `ScalaRunTime.wrapRefArray` → `ArraySeq` → `toJSVarArgsImpl`, dragging in
  `scala.-Array$` (~4k lines) + WrappedArray + WrappedVarArgs + Tuple2-4. For
  hot-path / frequently-called library constructors, provide concrete-arity
  `inline` overloads (see `Arr.apply`, `Painter.paint`). One-shot
  shader-build-time varargs (e.g. `Block(stmts*)`) may keep varargs.
- **`.mkString(sep)`** → `IterableOnceOps.mkString` + StringBuilder + collection
  traversal. On an `Arr`/`js.Array` use the native `.join(sep)`.
- **`0 until n` / `for i <- 0 until n`** → allocates `Range$Exclusive`; its
  `.foreach` pulls in the `IndexedSeq` iterator machinery → the entire
  `Vector1-6` + `VectorBuilder` + `NewVectorIterator` family (~76k chars in one
  observed chunk). Use a plain `var i = 0; while i < n do … i += 1` loop.
- **`String.contains(char)` / `.nonEmpty`** → `scala.collection.StringOps`. Use
  native `.indexOf(c) >= 0` and `.length > 0`.
- **`js.Dictionary` `apply` / `update` / `dict(k) = v`** → routes through
  `scala.scalajs.js.WrappedDictionary` (a Scala collection wrapper). Use the
  `Dict` extensions `.at(key)` / `.set(key, value)` / `.has(key)` from
  `trivalibs.utils.js` — they compile to raw JS property access. Prefer the
  `Dict` alias over `js.Dictionary` everywhere; the extensions are in scope
  wherever `Dict` is imported.

### Diagnosing bundle size

Temporarily set `jsModuleSplitStyleStr smallestmodules` in `project.scala` to
split the bundle into one module per class — module filenames then reveal
exactly which classes (and which stdlib subtrees) are pulled in. Compare a
`jsMode full` build (DCE applied, the deployment target) against `jsMode fast`
(strict runtime checks, no DCE) to attribute weight precisely. Restore
`fewestmodules` + `full` when done.

Note: `jsMode fast` enables strict runtime type checks (`$uF`, `$isFloat`, etc.)
that `full` mode erases. Code that casts a `Double` to `Float` (or any narrowing
`asInstanceOf`) compiles fine under `full` but throws `ClassCastException` under
`fast`. `full` is the deployment target; treat `fast` as a diagnostics-only
mode.

## Conventions

- **Examples**: Each implementation step gets a new example in `examples/`.
  Previous examples are never deleted and must keep compiling.
- When writing new shader code in examples, prefer the scala shader DSL over raw
  WGSL strings.

## Documentation

- **`docs/` vs `documents/`**: `docs/` holds **consumable** documentation for
  _using_ the library (the public reference — `docs/guide/` manuals, `docs/api/`
  generated/reference, `docs/skills/`). `documents/` holds **internal**
  feature-planning / contribution docs for _extending_ the library.
- **Doc-comments**: write Scaladoc (`/** … */`) on public APIs — one-line
  purpose, params, and any non-obvious behaviour / gotcha. Comments are stripped
  before JS bundling — document freely. Group-level comments (one per op family)
  are fine where per-method comments would be noise. Use `[[Symbol]]` wiki-links
  only for symbols Scaladoc can resolve in scope.
- **Generate the API site**: `bun run docs`. Output (`docs/api/html/`) is
  gitignored and published by CI; never commit it.

## Scala Conventions

- make use of named tuples @documents/scala-reference/named-tuples.md
- use new given syntax: @documents/scala-reference/given-syntax.md
- never put multiple statements on the same line, even if they are short. We
  don't want semicolons anywhere in Scala.
- the same one-statement-per-line rule applies to WGSL bodies in shader strings
  (`WgslFn.raw` bodies, `ShaderDef` bodies, etc.). Each statement gets its own
  line — no collapsing pairs of statements onto a single line.
- when working with typeclasses, use [T: Typeclass] notation instead of
  [T](using Typeclass[T]) where possible
- When doing floating point math, prefer trivalibs NumExt extensions instead of
  math library methods if possible. I.e. `x.sin` instead of `math.sin(x)`,
  `x.sqrt` instead of `math.sqrt(x)`, etc.
