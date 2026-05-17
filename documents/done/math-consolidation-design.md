# Math Library Consolidation Design

## Problem

The math library (CPU) and shader DSL (GPU) implement overlapping math operations
in separate, disconnected locations:

- **CPU math**: `src/gpu/math/` — traits (`Vec3Base`, `Vec3ImmutableOps`, etc.)
  with given instances for `Vec3`, `Vec3Tuple`, `StructRef[Vec3Buffer]`, etc.
- **GPU math**: `src/gpu/shader/dsl/expr.scala` — hand-written given instances
  for `Vec3Expr`, `LocalVec3`, etc. that produce WGSL strings.
- **GPU arithmetic overrides**: `src/gpu/shader/dsl/local_ops.scala` — extension
  methods that shadow trait defaults with native WGSL operators.

This separation means:

1. Adding a new operation (e.g. `cross` for Vec3) requires editing files in two
   different directories and remembering to keep them in sync.
2. `local_ops.scala` exists solely to override component-wise defaults with
   native WGSL ops — a pattern that should be built into the trait architecture.
3. The GPU types don't implement the same trait surface as CPU types. For example,
   `Vec3Expr` has `+`/`-`/`*`/`/` via `local_ops.scala` extensions but no
   `cross`, `normalized`, `dot`, `length`, etc.
4. The top-level `gpu` namespace is misleading — the math library, buffer tools,
   and painter are not GPU-specific concepts.

## Goals

1. **Single source of truth**: when adding a math operation, do it in one place
   and both CPU and GPU types get it (or get a compilation error reminding you).
2. **Override-friendly traits**: any trait method can be overridden per type.
   GPU types override most methods to emit WGSL strings. CPU types use defaults.
3. **Eliminate `local_ops.scala`**: its contents become proper given instances of
   the unified traits.
4. **Co-located implementations**: CPU and GPU given instances for each
   type/operation live in the same file or adjacent files in the math module.
5. **Rename `gpu` → `graphics`**: more accurate top-level namespace.

## Current Namespace Inventory

```
gpu.math                  — Vec2–4, Mat2–4 traits + CPU implementations
gpu.buffers               — BufferBinding, AttribLayout, allocateAttribs
gpu.painter               — Painter, Form, Shape, Shade, enums
gpu.shader                — ShaderDef, WGSLType, derive, layouts, builtins
gpu.shader.dsl            — Expr types, context, fn, program, local_ops
gpu.utils.animation       — animate utility
webgpu                    — JS facade traits
```

## Proposed Namespace Hierarchy

```
graphics/
├── math/                          — trait definitions (pure, no CPU or GPU deps)
│   ├── traits/                    — Vec*Base, Vec*ImmutableOps, Mat*Base, etc.
│   ├── cpu/                       — CPU concrete types + given instances
│   │   ├── vec2.scala             — Vec2, Vec2f, Vec2Tuple, Vec2fTuple, Vec2Buffer, ...
│   │   ├── vec3.scala
│   │   ├── vec4.scala
│   │   ├── mat2.scala
│   │   ├── mat3.scala
│   │   └── mat4.scala
│   └── gpu/                       — GPU Expr types + given instances
│       ├── expr.scala             — opaque types: FloatExpr, Vec2Expr, ...
│       ├── vec2.scala             — given Vec2Base[FloatExpr, Vec2Expr], etc.
│       ├── vec3.scala
│       ├── vec4.scala
│       ├── mat2.scala
│       ├── mat3.scala
│       └── mat4.scala
├── shader/                        — ShaderDef, WGSLType, derive, layouts, builtins
│   └── dsl/                       — context, fn, program (no more local_ops/expr math)
├── buffers/                       — BufferBinding, AttribLayout, mesh tools (future)
├── painter/                       — Painter, Form, Shape, Shade
└── utils/                         — animate, etc.

webgpu/                            — JS facade traits (unchanged)
```

### Package names

| Current                | Proposed                   |
| ---------------------- | -------------------------- |
| `gpu.math`             | `graphics.math`            |
| `gpu.buffers`          | `graphics.buffers`         |
| `gpu.painter`          | `graphics.painter`         |
| `gpu.shader`           | `graphics.shader`          |
| `gpu.shader.dsl`       | `graphics.shader.dsl`      |
| `gpu.utils.animation`  | `graphics.utils`           |
| `webgpu`               | `webgpu` (unchanged)       |

The key structural change is splitting `graphics.math` into three layers:
**traits**, **cpu**, and **gpu**.

## Trait Architecture

### Current trait hierarchy (Vec3 example)

```
Vec3Base[Num, Vec]           — x, y, z, dot, length_squared, length
Vec3Mutable[Num, Vec]        — x_=, y_=, z_= (extends Vec3Base)
Vec3ImmutableOps[Num, Vec]   — create, +, -, *, /, cross, normalized
Vec3MutableOps[Num, Vec]     — set, add, sub, mul, div, normalize (with out param)
```

### Problem with GPU types

The GPU `Vec3Expr` only needs `Vec3Base` + `Vec3ImmutableOps`. It does not need
`Vec3Mutable` or `Vec3MutableOps` (WGSL expressions are immutable — mutation is
handled by `Stmt.assign`).

But the current `Vec3ImmutableOps` has **default implementations** that expand
component-wise via `Vec3Base`:

```scala
// Current: Vec3ImmutableOps
inline def +(other: Vec): Vec =
  create(v.x + other.x, v.y + other.y, v.z + other.z)
```

For GPU types, this would produce:
```
vec3<f32>(a.x + b.x, a.y + b.y, a.z + b.z)
```

But WGSL supports native vector arithmetic, so we want:
```
(a + b)
```

Currently `local_ops.scala` works around this by defining extension methods that
shadow the trait methods. This is fragile and doesn't compose.

### Proposed: open methods with defaults

Make the operations regular `def`s (not `inline`) with default implementations
in the trait body, so given instances can override them:

```scala
trait Vec3Ops[Num, Vec](using base: Vec3Base[Num, Vec], num: NumOps[Num]):
  def create(x: Num, y: Num, z: Num): Vec

  // Default: component-wise via base accessors
  extension (v: Vec)
    def +(other: Vec): Vec =
      create(v.x + other.x, v.y + other.y, v.z + other.z)
    def -(other: Vec): Vec =
      create(v.x - other.x, v.y - other.y, v.z - other.z)
    def *(other: Vec): Vec =
      create(v.x * other.x, v.y * other.y, v.z * other.z)
    def /(other: Vec): Vec =
      create(v.x / other.x, v.y / other.y, v.z / other.z)
    def dot(other: Vec): Num =
      v.x * other.x + v.y * other.y + v.z * other.z
    def cross(other: Vec): Vec =
      create(
        v.y * other.z - v.z * other.y,
        v.z * other.x - v.x * other.z,
        v.x * other.y - v.y * other.x,
      )
    def normalized: Vec = v / v.length
    def length: Num = v.length_squared.sqrt
    def length_squared: Num = v.dot(v)
```

Then the GPU given instance overrides the methods it needs:

```scala
// in graphics/math/gpu/vec3.scala
given Vec3Ops[FloatExpr, Vec3Expr]:
  def create(x: FloatExpr, y: FloatExpr, z: FloatExpr): Vec3Expr =
    Vec3Expr(s"vec3<f32>(${x.wgsl}, ${y.wgsl}, ${z.wgsl})")

  extension (v: Vec3Expr)
    override def +(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} + ${other.wgsl})")
    override def -(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} - ${other.wgsl})")
    override def *(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} * ${other.wgsl})")
    override def /(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"(${v.wgsl} / ${other.wgsl})")
    override def dot(other: Vec3Expr): FloatExpr =
      FloatExpr(s"dot(${v.wgsl}, ${other.wgsl})")
    override def cross(other: Vec3Expr): Vec3Expr =
      Vec3Expr(s"cross(${v.wgsl}, ${other.wgsl})")
    override def normalized: Vec3Expr =
      Vec3Expr(s"normalize(${v.wgsl})")
    override def length: FloatExpr =
      FloatExpr(s"length(${v.wgsl})")
    // length_squared — no WGSL builtin, so default (dot(v,v)) is fine
```

### Revised trait split

The key insight is to **merge Base + ImmutableOps** into fewer traits, since the
GPU types need both field access and operations but the split between them creates
friction:

```
Vec3Base[Num, Vec]             — ABSTRACT: x, y, z (field accessors only)
                                  aliases: r/g/b, u/v (convenience, in Base)
Vec3Ops[Num, Vec]              — ABSTRACT: create
                                  DEFAULT: +, -, *, /, dot, cross, normalized,
                                  length, length_squared, scalar variants (+s, *s, etc.)
                                  Requires Vec3Base, NumOps, NumExt
Vec3Mutable[Num, Vec]          — ABSTRACT: x_=, y_=, z_= (extends Vec3Base)
Vec3MutableOps[Num, Vec]       — DEFAULT: set, add, sub, mul, div, normalize, +=, -=
                                  Requires Vec3Mutable
```

**CPU types**: provide givens for `Vec3Base` + `Vec3Ops` (tuples, classes) and
additionally `Vec3Mutable` + `Vec3MutableOps` (classes, StructRef).

**GPU types**: provide givens for `Vec3Base` + `Vec3Ops` only. Override all
operations in `Vec3Ops` to emit WGSL.

### What about `inline`?

Current CPU trait methods are `inline` for performance. But `inline` methods
cannot be overridden in given instances — they are compile-time expanded.

**Solution**: Two-tier approach.

1. The **trait definitions** use regular (non-inline) `def`s so they can be
   overridden.
2. CPU given instances that want `inline` performance can use `inline` on the
   given instance methods directly. Scala 3 allows `inline` on extension method
   implementations in a given instance even when the trait declares them as
   regular `def`. The `inline` in the given body is an implementation choice, not
   a trait constraint.

```scala
// Trait: regular def (overridable)
trait Vec3Ops[Num, Vec]:
  extension (v: Vec)
    def +(other: Vec): Vec

// CPU given: inline implementation
given Vec3Ops[Double, Vec3]:
  extension (v: Vec3)
    inline def +(other: Vec3): Vec3 =
      new Vec3(v.x + other.x, v.y + other.y, v.z + other.z)
```

Note: this needs testing — Scala 3's handling of `inline` on given instance
methods overriding non-inline trait methods may have edge cases. If this doesn't
work, the fallback is to keep CPU methods non-inline (the JIT will inline hot
paths anyway in a JS runtime via V8/SpiderMonkey).

### Matrix-vector multiplication

Currently matrix-vector multiply is in `Mat4ImmutableOps`:
```scala
def *[Vec](v: Vec)(using Vec4Base[Num, Vec], Vec4ImmutableOps[Num, Vec]): Vec
```

And in `local_ops.scala` as a separate extension:
```scala
extension (m: Mat4Expr)
  def *(v: Vec4Expr): Vec4Expr = Vec4Expr(s"(${m.wgsl} * ${v.wgsl})")
```

In the new design, this stays in `Mat4Ops` with a default component-wise
implementation. The GPU given overrides it to emit `(m * v)`.

## Detailed file plan

### `graphics/math/traits/` — Pure trait definitions

These files contain ONLY trait definitions. No implementations, no givens, no
concrete types.

| File | Contents |
|------|----------|
| `vec2.scala` | `Vec2Base`, `Vec2Ops`, `Vec2Mutable`, `Vec2MutableOps` |
| `vec3.scala` | `Vec3Base`, `Vec3Ops`, `Vec3Mutable`, `Vec3MutableOps` |
| `vec4.scala` | `Vec4Base`, `Vec4Ops`, `Vec4Mutable`, `Vec4MutableOps` |
| `mat2.scala` | `Mat2Base`, `Mat2Ops`, `Mat2Mutable`, `Mat2MutableOps` |
| `mat3.scala` | `Mat3Base`, `Mat3Ops`, `Mat3Mutable`, `Mat3MutableOps` |
| `mat4.scala` | `Mat4Base`, `Mat4Ops`, `Mat4Mutable`, `Mat4MutableOps` |
| `package.scala` | Exports of all traits |

Package: `graphics.math`

Traits import `NumOps` and `NumExt` from trivalibs (unchanged).

### `graphics/math/cpu/` — CPU concrete types + givens

Each file contains the concrete types AND their given instances, exactly as today
but using the new trait names.

| File | Contents |
|------|----------|
| `vec2.scala` | `Vec2`, `Vec2f`, `Vec2Tuple`, `Vec2fTuple`, `Vec2Buffer`, `Vec2dBuffer` + all givens |
| `vec3.scala` | `Vec3`, `Vec3f`, `Vec3Tuple`, `Vec3fTuple`, `Vec3Buffer`, `Vec3dBuffer` + all givens |
| `vec4.scala` | Similar |
| `mat2.scala` | `Mat2`, `Mat2Tuple`, `Mat2Buffer` + all givens |
| `mat3.scala` | `Mat3`, `Mat3Tuple`, `Mat3Buffer`, `Mat3PaddedBuffer` + all givens |
| `mat4.scala` | `Mat4`, `Mat4Tuple`, `Mat4Buffer` + all givens |
| `package.scala` | `export` all givens (like current `gpu.math/package.scala`) |

Package: `graphics.math.cpu`

The types themselves (`Vec3`, `Mat4`, `Vec3Tuple`, etc.) are defined here and
exported so user code can do `import graphics.math.cpu.*` to get everything.

### `graphics/math/gpu/` — GPU Expr types + givens

| File | Contents |
|------|----------|
| `expr.scala` | Opaque types: `FloatExpr`, `Vec2Expr`, ..., `BoolExpr`, `LocalExpr` hierarchy, numeric conversions |
| `vec2.scala` | `given Vec2Base[FloatExpr, Vec2Expr]`, `given Vec2Ops[FloatExpr, Vec2Expr]` |
| `vec3.scala` | `given Vec3Base[FloatExpr, Vec3Expr]`, `given Vec3Ops[FloatExpr, Vec3Expr]` |
| `vec4.scala` | Similar |
| `mat2.scala` | `given Mat2Base[FloatExpr, Mat2Expr]`, `given Mat2Ops[FloatExpr, Mat2Expr]` |
| `mat3.scala` | Similar |
| `mat4.scala` | Similar |
| `constructors.scala` | `object vec2 { def apply(...) }`, `object vec3`, `object vec4` |
| `stmt.scala` | `Stmt`, `Block` opaque types (moved from expr.scala) |
| `package.scala` | Exports |

Package: `graphics.math.gpu`

**No more `local_ops.scala`**. All GPU arithmetic is in the given instances in
these files, overriding the trait defaults.

### Migration of `expr.scala` contents

Current `expr.scala` has several responsibilities that get split:

| Current location in expr.scala | New location |
|-------------------------------|--------------|
| `Expr` base class, opaque types | `graphics/math/gpu/expr.scala` |
| `LocalExpr` hierarchy | `graphics/math/gpu/expr.scala` |
| `Local*` → `*Expr` conversions | `graphics/math/gpu/expr.scala` |
| Numeric literal conversions | `graphics/math/gpu/expr.scala` |
| `NumOps[FloatExpr]` given | `graphics/math/gpu/expr.scala` |
| `NumExt[FloatExpr]` given | `graphics/math/gpu/expr.scala` |
| `Vec2Base[FloatExpr, Vec2Expr]` given | `graphics/math/gpu/vec2.scala` |
| `Vec2ImmutableOps[FloatExpr, Vec2Expr]` given | `graphics/math/gpu/vec2.scala` (now `Vec2Ops`) |
| `vec2` constructor object | `graphics/math/gpu/constructors.scala` |
| `Stmt`, `Block` | `graphics/math/gpu/stmt.scala` or `graphics/shader/dsl/stmt.scala` |

### Migration of `local_ops.scala` contents

Every extension in `local_ops.scala` becomes an `override` in the corresponding
`given Vec*Ops` / `given Mat*Ops` instance in `graphics/math/gpu/`:

| `local_ops.scala` extension | New location |
|-----------------------------|--------------|
| `Vec2Expr.+`, `-.`, `*`, `/` (vec and scalar) | `given Vec2Ops` in `gpu/vec2.scala` |
| `Vec3Expr.+`, `-`, `*`, `/` (vec and scalar) | `given Vec3Ops` in `gpu/vec3.scala` |
| `Vec4Expr.+`, `-`, `*`, `/` (vec and scalar) | `given Vec4Ops` in `gpu/vec4.scala` |
| `Mat2Expr * Vec2Expr` | `given Mat2Ops` in `gpu/mat2.scala` |
| `Mat3Expr * Vec3Expr` | `given Mat3Ops` in `gpu/mat3.scala` |
| `Mat4Expr * Vec4Expr` | `given Mat4Ops` in `gpu/mat4.scala` |

## Synchronization guarantee

With this design, adding a new method to e.g. `Vec3Ops` (say `reflect`) means:

1. Add `def reflect(normal: Vec)` with a default implementation to the trait.
2. The CPU types get it for free (the default expands component-wise).
3. The GPU given in `gpu/vec3.scala` also gets the default — it will emit
   component-wise WGSL. If there's a native WGSL builtin (`reflect()`), you
   override it there.
4. If you want to **force** GPU types to provide an explicit implementation (no
   default), make the method abstract. The compiler will error until you add it
   to every given instance.

This is the single-source-of-truth guarantee: the trait is the contract, and both
CPU and GPU must satisfy it.

## Operations to synchronize

Operations currently in CPU traits but missing from GPU:

| Operation | CPU trait | GPU status |
|-----------|----------|------------|
| `dot` | `Vec*Base` | Missing |
| `length` | `Vec*Base` | Missing |
| `length_squared` | `Vec*Base` | Missing |
| `cross` | `Vec3ImmutableOps` | Missing |
| `normalized` | `Vec*ImmutableOps` | Missing |
| `determinant` | `Mat*SharedOps` | Missing |
| `transposed` | `Mat*ImmutableOps` | Missing |
| `inversed` | `Mat*ImmutableOps` | Missing |
| `rotatedX/Y/Z` | `Mat4ImmutableOps` | Missing |
| `fromRotationX/Y/Z` | `Mat4ImmutableOps` | Missing |
| `identity` | `Mat*ImmutableOps` | Missing |
| scalar `+`/`-`/`*`/`/` for vecs | `Vec*ImmutableOps` | In `local_ops.scala` |
| mat-vec `*` | `Mat*ImmutableOps` | In `local_ops.scala` |

Operations currently in GPU but missing from CPU:

| Operation | GPU location | CPU status |
|-----------|-------------|------------|
| (none significant) | — | — |

The GPU is a strict subset of CPU operations today. The consolidation adds all
missing GPU operations.

## Stmt and Block placement

`Stmt` and `Block` are shader DSL concepts (WGSL statement generation), not math.
They should stay in `graphics.shader.dsl`, not move to `graphics.math.gpu`.

Current `expr.scala` defines them at the bottom — they should be extracted to
`graphics/shader/dsl/stmt.scala`.

## Import ergonomics

User code imports:

```scala
// CPU math
import graphics.math.*       // traits
import graphics.math.cpu.*   // concrete types + givens

// GPU shader DSL
import graphics.math.*       // traits
import graphics.math.gpu.*   // Expr types + givens

// Both (rare, but possible in shared algorithm code)
import graphics.math.*
```

The trait package (`graphics.math`) exports all trait definitions. The `cpu` and
`gpu` sub-packages export their concrete types and given instances.

## Migration strategy

### Phase 1: Namespace rename `gpu` → `graphics`

Mechanical rename of all package declarations and imports. No structural changes.
All existing tests must pass.

### Phase 2: Restructure math into traits/cpu/gpu

1. Extract trait definitions from current math files into `graphics/math/traits/`.
   Remove `inline` from trait method signatures (keep them in implementations).
2. Rename `ImmutableOps` → `Ops` (since it now includes what was in `Base`'s
   derived methods like `dot`, `length`).
3. Move CPU concrete types + givens to `graphics/math/cpu/`.
4. Move GPU Expr types + givens from `shader/dsl/expr.scala` to
   `graphics/math/gpu/`.
5. Move `local_ops.scala` overrides into the GPU given instances.
6. Delete `local_ops.scala`.
7. Extract `Stmt`/`Block` to `graphics/shader/dsl/stmt.scala`.

### Phase 3: Synchronize GPU operations

Add missing GPU implementations for all trait methods that have meaningful WGSL
equivalents: `dot`, `length`, `cross`, `normalize`, `determinant`, `transpose`,
`inverse`.

### Phase 4: Add new operations to both

With the architecture in place, add missing operations (swizzles, `mix`/`lerp`,
`reflect`, `refract`, `smoothstep`, `step`, `min`/`max`, `fract`, `mod`, etc.)
to the traits with defaults, and override in GPU givens where WGSL builtins
exist.

## Open questions

1. **`inline` compatibility**: Can Scala 3 given instances override non-inline
   trait methods with `inline` implementations? Needs a quick test. If not, CPU
   performance may depend on JIT inlining (which is fine for JS targets).

2. **`NumOps`/`NumExt` for `FloatExpr`**: Currently `FloatExpr` has its own
   `NumOps` and `NumExt` givens in `expr.scala`. These should stay in
   `graphics/math/gpu/expr.scala` since they are the GPU's "numeric type"
   implementation — analogous to how `Double`'s givens live in trivalibs.

3. **Trait package**: Should traits live in `graphics.math` directly (and cpu/gpu
   be sub-packages), or should traits get their own sub-package
   `graphics.math.traits`? The former is simpler for imports (`import
   graphics.math.*` gets traits); the latter is cleaner separation. Recommendation:
   **traits in `graphics.math` directly** — they are the primary API surface.

4. **`from` / factory methods on companion objects**: Currently `Vec3.create()`
   and `Vec3Tuple.create()` exist as methods on companion objects that extend
   `Vec3ImmutableOps`. In the new design, the companion object pattern stays the
   same — `object Vec3 extends Vec3Ops[Double, Vec3]` — but now `Vec3Ops`
   includes the full operation set, which is cleaner.
