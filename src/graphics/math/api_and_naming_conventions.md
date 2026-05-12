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

## Type hierarchy

Each vector/matrix type is supported by a set of traits:

- `Vec*Base` / `Mat*Base` — read-only field accessors (`x`, `y`, `m00`, …) plus
  scalar helpers that return no new instance and cause no allocation (`dot`,
  `length_squared`, `length` for vectors; `determinant` for matrices)
- `Vec*Mutable` / `Mat*Mutable` — extends Base, adds field setters (`x_=`,
  `m00_=`, …)
- `Vec*ImmutableOps` / `Mat*ImmutableOps` — operations that return a new
  instance via `create`
- `Vec*MutableOps` / `Mat*MutableOps` — operations that write into an existing
  target

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

| Category                | Naming                     | Example                                             |
| ----------------------- | -------------------------- | --------------------------------------------------- |
| Default (Double) vector | `Vec2`, `Vec3`, `Vec4`     | `class Vec2(var x: Double, var y: Double)`          |
| Float vector            | `Vec2f`, `Vec3f`, `Vec4f`  | `class Vec2f(var x: Float, var y: Float)`           |
| Default tuple (Double)  | `Vec2Tuple`, `Vec3Tuple`   | `type Vec2Tuple = (Double, Double)`                 |
| Float tuple             | `Vec2fTuple`, `Vec3fTuple` | `type Vec2fTuple = (Float, Float)`                  |
| F32 GPU buffer          | `Vec2Buffer`, `Mat4Buffer` | `type Vec2Buffer = (F32, F32)`                      |
| F64 CPU buffer          | `Vec2dBuffer`              | `type Vec2dBuffer = (F64, F64)`                     |
| Matrix (Double)         | `Mat2`, `Mat3`, `Mat4`     | `class Mat2(var m00: Double, …)`                    |
| Matrix tuple            | `Mat2Tuple`, `Mat4Tuple`   | `type Mat2Tuple = (Double, Double, Double, Double)` |

Matrices have no Float variants yet — use `Mat*Buffer` (F32) exclusively for GPU
upload.

## Buffer types

`*Buffer` types use F32 by default — matching WebGPU's `f32` requirement. F64
buffer variants (`Vec2dBuffer` etc.) exist for CPU-side double-precision storage
but are not used for GPU upload.

## WGSL mapping

Both `Vec2` (Double) and `Vec2f` (Float) map to `vec2<f32>` in WGSL, using
`Vec2Buffer` (F32) for the actual GPU data. WGSL does not support `f64`.

## Trait type parameter order

Always `[Num, Mat]` or `[Num, Vec]` — numeric type first, container type second.
Use `Fractional` (not `Numeric`) as the constraint since all math types are
floating-point.
