# Math API & Naming Conventions

## Design rationale

The ops traits are designed to be implemented by multiple concrete data types
with different performance characteristics. The same algorithm (e.g. matrix
multiplication) can be used across all of them without duplication:

| Concrete type             | Mutability              | Allocation cost                         | Typical use                                          |
| ------------------------- | ----------------------- | --------------------------------------- | ---------------------------------------------------- |
| `Vec2Tuple` / `Mat2Tuple` | immutable (Scala tuple) | cheap (stack/value)                     | functional pipelines, pattern matching               |
| `Vec2` / `Mat2` (class)   | mutable                 | moderate (heap object)                  | general-purpose, intermediate results                |
| `StructRef[Vec2Buffer]`   | mutable                 | **expensive to create**, cheap to write | GPU buffer slots — pre-allocated, reused every frame |

The key insight: **`StructRef` slots must be pre-allocated once and then mutated
in place**. Creating a new `StructRef` is as expensive as allocating a new GPU
buffer slot. In a render loop running at 60fps, any allocation inside the loop
will cause GC pressure in the JS runtime. The mutable ops API
(`transposeTo(out)`, `normalizeTo(out)`) exists specifically to support this
zero-allocation pattern.

Tuples, by contrast, are pure values — they compose and transform freely in
functional style, and their `ImmutableOps` let you chain operations without
worrying about aliasing or mutation.

## CPU / GPU split

The math types live in three layers, mirrored by the directory layout:

- `math/` (root) — the **generic** traits `Vec*BaseG[Num, Vec]` /
  `Vec*ImmutableOpsG[Num, Vec]`. These are the purely abstract shared contract:
  the same operation set, parameterized over the numeric type `Num` and the
  container type `Vec`. They have no implementations of their own.
- `math/cpu/` — the **CPU** layer. `Vec*Base[Vec]` extends
  `Vec*BaseG[Double, Vec]`, fixing the numeric type to `Double`, and adds
  concrete Double implementations plus CPU-only traits (`Vec*Mutable`,
  `Vec*ImmutableOps`, `Vec*MutableOps`). Concrete types: the mutable class
  (`Vec2`), the tuple (`Vec2Tuple`), and the buffer types (`Vec2Buffer`,
  `Vec2dBuffer`).
- `math/gpu/` — the **GPU** layer. Opaque expression types (`Vec2Expr`,
  `FloatExpr`, …) wrap WGSL source strings; they implement
  `Vec*BaseG[FloatExpr, Vec2Expr]` and `Vec*ImmutableOpsG[FloatExpr, Vec2Expr]`,
  so the same operation names build WGSL instead of computing values. This
  package also re-exports the CPU class names (`Vec2`, `Mat4`, …) for use in
  shader contract definitions.

The key point: the generic `*G` traits are what make a single algorithm work
across both CPU values and GPU expressions. The CPU side specializes `Num` to
`Double`; the GPU side specializes `Num` to `FloatExpr`.

## Type hierarchy

On the CPU side, each vector/matrix type is supported by a set of traits:

- `Vec*BaseG` / `Mat*BaseG` — generic read-only contract (the abstract shared
  layer)
- `Vec*Base` / `Mat*Base` — extends `*BaseG[Double, …]`; read-only field
  accessors (`x`, `y`, `m00`, …) plus scalar helpers that return no new instance
  and cause no allocation (`dot`, `length_squared`, `length` for vectors;
  `determinant` for matrices)
- `Vec*Mutable` / `Mat*Mutable` — extends Base, adds field setters (`x_=`,
  `m00_=`, …)
- `Vec*ImmutableOpsG` / `Mat*ImmutableOpsG` — generic contract for operations
  that return a new instance via `create`
- `Vec*ImmutableOps` / `Mat*ImmutableOps` — the `Double` CPU implementation of
  the immutable ops
- `Vec*MutableOps` / `Mat*MutableOps` — operations that write into an existing
  target (CPU-only)

## Immutable vs mutable operation naming

The naming convention aligns with WGSL built-in function names and makes
allocation vs. mutation visible at the call site:

| Form                  | Trait          | Allocates?        | Works on tuples? | Returns         | Example                                    |
| --------------------- | -------------- | ----------------- | ---------------- | --------------- | ------------------------------------------ |
| Present tense (WGSL)  | `ImmutableOps` | yes, via `create` | yes              | new `Mat`/`Vec` | `m.transpose`, `v.normalize`               |
| Verb + `Self`         | `MutableOps`   | no                | no               | `self`          | `m.transposeSelf`, `v.normalizeSelf`       |
| Verb + `To(out, ...)` | `MutableOps`   | no                | no               | `out`           | `m.transposeTo(out)`, `v.normalizeTo(out)` |
| Operator `+=` / `-=`  | `MutableOps`   | no                | no               | `Unit`          | `m += other`                               |

### ImmutableOps — WGSL name, returns new value

```scala
m.transpose       // -> Mat  (new allocation)
m.inverse         // -> Mat  (new allocation)
v.normalize       // -> Vec  (new allocation)
m.rotate(angle)   // -> Mat  (new allocation)
```

- Always allocates a new instance through the abstract `create` method
- Available for immutable types (tuples) and mutable classes; **not** provided
  for `StructRef` buffer types since those are pre-allocated and only mutated
- Safe to chain: `m.transpose.inverse`

### MutableOps — `Self` (in-place) and `To` (write to target)

```scala
m.transposeSelf           // mutates m, returns m
m.transposeTo(out)        // writes into out, returns out
m.inverseSelf             // mutates m, returns m
m.inverseTo(out)          // writes into out, returns out
v.normalizeSelf           // mutates v, returns v
v.normalizeTo(out)        // writes into out, returns out
m.rotateSelf(angle)       // mutates m, returns m
m.rotateTo(out, angle)    // writes into out, returns out
```

- Zero allocation — reads source, writes into target, returns target
- `*Self` methods are `inline` and delegate to the corresponding `*To` method
  with `self` as the output target
- `*To` methods take `out` as the first parameter, followed by any operation
  parameters (e.g. angle)
- Returning the target allows chaining and inline passing without an extra
  `val`:
  ```scala
  upload(m.inverseTo(scratch))          // compute into scratch, pass directly
  val result = a.transposeTo(b)         // transpose a into b
  ```
- All implementations read all fields into local `val`s before writing any
  output, so `self` as target is safe
- Only available for mutable types (classes, `StructRef`s); tuples do not have
  `MutableOps`
- Prefer these in hot paths (render loops) where `StructRef` allocations are
  expensive

## Scalar type convention

- **`Double`** is the default numeric type for Scala-side math (native JS
  number, no overhead in ScalaJS)
- **`Float`** is only used in GPU buffers (`Vec2Buffer`, `Mat4Buffer`, etc.) for
  WebGPU upload

## Type naming

JS has only one native number type (`Double`), and WebGPU buffers are `f32` — so
CPU-side math is uniformly `Double`, and `Float` appears only inside GPU buffer
storage types.

| Category               | Naming                     | Example                                             |
| ---------------------- | -------------------------- | --------------------------------------------------- |
| Mutable class (Double) | `Vec2`, `Vec3`, `Vec4`     | `class Vec2(var x: Double, var y: Double)`          |
| Tuple (Double)         | `Vec2Tuple`, `Vec3Tuple`   | `type Vec2Tuple = (Double, Double)`                 |
| F32 GPU buffer         | `Vec2Buffer`, `Mat4Buffer` | `type Vec2Buffer = (F32, F32)`                      |
| F64 CPU buffer         | `Vec2dBuffer`              | `type Vec2dBuffer = (F64, F64)`                     |
| Matrix class (Double)  | `Mat2`, `Mat3`, `Mat4`     | `class Mat2(var m00: Double, …)`                    |
| Matrix tuple           | `Mat2Tuple`, `Mat4Tuple`   | `type Mat2Tuple = (Double, Double, Double, Double)` |
| GPU expression         | `Vec2Expr`, `Mat4Expr`     | `opaque type Vec2Expr <: Expr = Expr`               |

The GPU expression types (`Vec2Expr`, `Vec3Expr`, …, `FloatExpr`, `Mat*Expr`)
live in `math/gpu/`. They are opaque wrappers around WGSL source strings, with
`Let*` / `Var* `/ `Const*` subtypes for local declarations. Their operations
implement the generic `*G` traits with `Num = FloatExpr`, so writing `a + b` on
two `Vec2Expr`s emits `(a + b)` as WGSL.

## Buffer types

`*Buffer` types use F32 by default — matching WebGPU's `f32` requirement. F64
buffer variants (`Vec2dBuffer` etc.) exist for CPU-side double-precision storage
but are not used for GPU upload.

## WGSL mapping

`Vec2` maps to `vec2<f32>` in WGSL, using `Vec2Buffer` (F32) for the actual GPU
data. WGSL does not support `f64`. There is a single `Vec2` type — `Double` on
the CPU, `f32` on the GPU — sharing one operation contract via the generic `*G`
traits; the numeric representation differs per context but the API does not.

## Trait type parameter order

The generic `*G` traits are parameterized `[Num, Mat]` or `[Num, Vec]` — numeric
type first, container type second. The CPU layer instantiates `Num = Double`;
the GPU layer instantiates `Num = FloatExpr`.
