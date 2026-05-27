# Math Library — Design & Implementation Reference

Living reference for the CPU/GPU math library in `src/graphics/math/`. Tracks
design principles, the complete operation inventory (done and planned), and
implementation status.

**Driving goal**: The math library must support the `graphics.scene` layer —
`Transform` (TRS composition via `Quat`), `PerspectiveCamera` (perspective
projection, view matrix), and `SceneObject` (model/view/projection/normal
matrices). See `documents/transform-camera-scene-design.md` for the full spec.
The planned additions in §8.2 are prioritised to unblock that layer.

---

## Table of Contents

1. [Design Principles](#1-design-principles)
2. [Type Inventory](#2-type-inventory)
3. [Scalar Operations](#3-scalar-operations)
4. [Vector Operations](#4-vector-operations)
5. [Matrix Operations](#5-matrix-operations)
6. [Quaternion](#6-quaternion)
7. [Constructors and Swizzles (GPU DSL)](#7-constructors-and-swizzles-gpu-dsl)
8. [Implementation Status](#8-implementation-status)

---

## 1. Design Principles

### 1.1 CPU/GPU Naming Parity

Every math operation must use the **same name and syntax** on CPU (`Vec3`,
`Mat4`, `NumExt`) and GPU (DSL expression types `Vec3Expr`, `Mat4Expr`,
`FloatExpr`). The goals and rules:

- **Prefer isomorphic operations** — if an operation can be efficiently
  expressed on both CPU and GPU with the same name and semantics, add it to
  both. User code should not need to know whether it's running on CPU or GPU.
- **CPU helpers are welcome** when they improve ergonomics and can be
  implemented in terms of isomorphic primitives (e.g. `clamp01`, `fit0111`,
  `smoothstep`, matrix constructors, `Quat`). CPU-only utilities that have no
  GPU analog (serialization, string formatting, debug printing) go in separate
  CPU-only packages.
- **Extension method syntax over free functions.** Match the existing `x.sin`,
  `x.clamp(lo, hi)`, `v.dot(w)` pattern. Prefer `x.log` over `math.log(x)` — all
  math in user code should read the same on CPU and GPU.
- **Free functions only for operations with no natural receiver** (e.g.
  `select(f, t, cond)` — ternary-like).

### 1.2 Immutable vs Mutable Naming

| Form                  | Trait          | Allocates? | Works on tuples? | Returns      | Example                      |
| --------------------- | -------------- | ---------- | ---------------- | ------------ | ---------------------------- |
| Present tense (WGSL)  | `ImmutableOps` | yes        | yes              | new instance | `v.normalize`, `m.transpose` |
| Operator `+` / `-`    | `ImmutableOps` | yes        | yes              | new instance | `v + w`, `m * n`             |
| Verb + `Self`         | `MutableOps`   | no         | no               | `self`       | `v.normalizeSelf`            |
| Verb + `To(out, ...)` | `MutableOps`   | no         | no               | `out`        | `v.normalizeTo(out)`         |
| Operator `+=` / `-=`  | `MutableOps`   | no         | no               | `Unit`       | `m += other`                 |

MutableOps have **no GPU equivalent** — WGSL is a functional language with no
in-place mutation. MutableOps remain CPU-only.

### 1.3 Numeric Types

- **`Double`** is the default for all Scala-side math (native JS `number`).
- **`Float`** appears only in F32 buffer tuple types (`Vec2Buffer`,
  `Mat4Buffer`) for WebGPU upload. Buffer type class instances use `Double` at
  the trait level with `.toFloat` conversion in setters.
- Float class types (`Vec2f`, `Vec3f`, `Vec4f`, `Vec*fTuple`) have been removed.
  All CPU math is Double-only. WGSL does not support `f64`, so all types map to
  `f32` on the GPU side.

### 1.4 Trait Structure

Traits are split into **generic** (`*G` suffix) and **CPU-specific** (no suffix)
variants:

```
Generic (CPU + GPU shared contract) — PURELY ABSTRACT, no implementations:
  Vec*BaseG[Num, Vec]          — abstract: component accessors, dot, length_squared, length
                                  inline aliases: u/v, r/g/b/a (delegate to abstract accessors)
  Vec*ImmutableOpsG[Num, Vec]  — abstract: create, all WGSL-isomorphic ops (arithmetic,
                                  normalize, unary_-, math fns, comparisons, cross for Vec3)
  Mat*BaseG[Num, Mat]          — abstract: mNN accessors, determinant
  Mat*ImmutableOpsG[Num, Mat]  — abstract: create, matMul, vecMul

CPU-specific (Double) — concrete implementations:
  Vec*Base[Vec]                — extends BaseG[Double, Vec], concrete: dot, length_squared, length
  Vec*Mutable[Vec]             — extends Base, adds setters (Double)
  Vec*ImmutableOps[Vec]        — concrete impls of all ImmutableOpsG ops + CPU-only: from
  Vec*MutableOps[Vec]          — in-place ops (set, :=, +=, normalizeTo, etc.)
  Mat*Base[Mat]                — extends BaseG[Double, Mat], concrete: determinant, col/row getters
  Mat*Mutable[Mat]             — extends Base, adds setters + col/row setters (Double)
  Mat*ImmutableOps[Mat]        — concrete matMul/vecMul + CPU-only: from, identity, inverse,
                                  transpose, rotate*, constructors
  Mat*MutableOps[Mat]          — in-place ops (set, :=, setIdentity, transposeTo, inverseTo,
                                  rotateTo)
```

G traits are used by the GPU DSL (`expr.scala`) which implements them for
expression types (`FloatExpr`, `Vec*Expr`, `Mat*Expr`). CPU traits fix
`Num = Double` and provide concrete implementations.

Quat traits (`QuatImmutableOps[Q]`, `QuatMutableOps[Q]`) are CPU-only with no
generic variant — quaternions have no GPU correspondence.

### 1.5 Column-Major Convention

All matrices use **column-major storage** with `mColRow` field naming: `m10` =
column 1, row 0. This matches WGSL and WebGPU conventions.

---

## 2. Type Inventory

### CPU Types

| Category   | Double (default)    | GPU buffer (F32) | F64 buffer    |
| ---------- | ------------------- | ---------------- | ------------- |
| 2D vector  | `Vec2`, `Vec2Tuple` | `Vec2Buffer`     | `Vec2dBuffer` |
| 3D vector  | `Vec3`, `Vec3Tuple` | `Vec3Buffer`     | `Vec3dBuffer` |
| 4D vector  | `Vec4`, `Vec4Tuple` | `Vec4Buffer`     | `Vec4dBuffer` |
| 2×2 matrix | `Mat2`, `Mat2Tuple` | `Mat2Buffer`     | —             |
| 3×3 matrix | `Mat3`, `Mat3Tuple` | `Mat3Buffer`     | —             |
| 4×4 matrix | `Mat4`, `Mat4Tuple` | `Mat4Buffer`     | —             |
| Quaternion | `Quat`              | via `Vec4Buffer` | —             |

All buffer types use Double at the trait level. F32 buffer setters convert via
`.toFloat`; F32 buffer getters widen via `: Double` type ascription.

### GPU DSL Expression Types (`src/graphics/math/gpu/expr.scala`)

| Type        | Wraps    | Represents                    |
| ----------- | -------- | ----------------------------- |
| `FloatExpr` | `String` | WGSL `f32` expression         |
| `Vec2Expr`  | `String` | WGSL `vec2<f32>` expression   |
| `Vec3Expr`  | `String` | WGSL `vec3<f32>` expression   |
| `Vec4Expr`  | `String` | WGSL `vec4<f32>` expression   |
| `Mat2Expr`  | `String` | WGSL `mat2x2<f32>` expression |
| `Mat3Expr`  | `String` | WGSL `mat3x3<f32>` expression |
| `Mat4Expr`  | `String` | WGSL `mat4x4<f32>` expression |
| `BoolExpr`  | `String` | WGSL `bool` expression        |
| `Texture2D` | `String` | WGSL `texture_2d<f32>` handle |
| `Sampler`   | `String` | WGSL `sampler` handle         |

---

## 3. Scalar Operations

### 3.1 CPU/GPU Scalar Operation Mapping

`NumExt` (CPU, in `trivalibs`) ↔ `FloatExpr` extensions (GPU DSL).

| CPU (`NumExt`)         | GPU (`FloatExpr`)      | WGSL Output             | Status  |
| ---------------------- | ---------------------- | ----------------------- | ------- |
| `x.sin`                | `x.sin`                | `sin(x)`                | ✅ Done |
| `x.cos`                | `x.cos`                | `cos(x)`                | ✅ Done |
| `x.tan`                | `x.tan`                | `tan(x)`                | ✅ Done |
| `x.asin`               | `x.asin`               | `asin(x)`               | ✅ Done |
| `x.acos`               | `x.acos`               | `acos(x)`               | ✅ Done |
| `x.atan`               | `x.atan`               | `atan(x)`               | ✅ Done |
| `x.atan2(y)`           | `x.atan2(y)`           | `atan2(x, y)`           | ✅ Done |
| `x.sqrt`               | `x.sqrt`               | `sqrt(x)`               | ✅ Done |
| `x.pow(e)`             | `x.pow(e)`             | `pow(x, e)`             | ✅ Done |
| `x.abs`                | `x.abs`                | `abs(x)`                | ✅ Done |
| `x.floor`              | `x.floor`              | `floor(x)`              | ✅ Done |
| `x.ceil`               | `x.ceil`               | `ceil(x)`               | ✅ Done |
| `x.clamp(lo, hi)`      | `x.clamp(lo, hi)`      | `clamp(x, lo, hi)`      | ✅ Done |
| `x.clamp01`            | `x.clamp01`            | `saturate(x)`           | ✅ Done |
| `x.fit0111`            | `x.fit0111`            | `(x * 2.0 - 1.0)`       | ✅ Done |
| `x.fit1101`            | `x.fit1101`            | `(x * 0.5 + 0.5)`       | ✅ Done |
| `x.min(y)`             | `x.min(y)`             | `min(x, y)`             | ✅ Done |
| `x.max(y)`             | `x.max(y)`             | `max(x, y)`             | ✅ Done |
| `x.mix(b, t)`          | `x.mix(b, t)`          | `mix(x, b, t)`          | ✅ Done |
| `x.step(edge)`         | `x.step(edge)`         | `step(edge, x)`         | ✅ Done |
| `x.smoothstep(lo, hi)` | `x.smoothstep(lo, hi)` | `smoothstep(lo, hi, x)` | ✅ Done |
| `x.fma(b, c)`          | `x.fma(b, c)`          | `fma(x, b, c)`          | ✅ Done |
| `x.sign`               | `x.sign`               | `sign(x)`               | ✅ Done |
| `x.round`              | `x.round`              | `round(x)`              | ✅ Done |
| `x.trunc`              | `x.trunc`              | `trunc(x)`              | ✅ Done |
| `x.fract`              | `x.fract`              | `fract(x)`              | ✅ Done |
| `x.exp`                | `x.exp`                | `exp(x)`                | ✅ Done |
| `x.exp2`               | `x.exp2`               | `exp2(x)`               | ✅ Done |
| `x.log`                | `x.log`                | `log(x)`                | ✅ Done |
| `x.log2`               | `x.log2`               | `log2(x)`               | ✅ Done |
| `x.inverseSqrt`        | `x.inverseSqrt`        | `inverseSqrt(x)`        | ✅ Done |
| `x.degrees`            | `x.degrees`            | `degrees(x)`            | ✅ Done |
| `x.radians`            | `x.radians`            | `radians(x)`            | ✅ Done |
| `x.sinh`               | `x.sinh`               | `sinh(x)`               | ✅ Done |
| `x.cosh`               | `x.cosh`               | `cosh(x)`               | ✅ Done |
| `x.tanh`               | `x.tanh`               | `tanh(x)`               | ✅ Done |

> **Note on argument order**: `step` and `smoothstep` have reversed argument
> order vs WGSL. The extension syntax reads naturally: `x.smoothstep(lo, hi)` =
> "smooth-step x between lo and hi". The emitted WGSL correctly swaps them.

> **NumExt parity**: All scalar ops above exist on both `NumExt` (CPU, in
> [trivalibs/src/utils/numbers.scala](../trivalibs/src/utils/numbers.scala)) and
> `FloatExpr` (GPU DSL). Prefer `x.log` over `math.log(x)` in all user code.

---

## 4. Vector Operations

### 4.1 Vec2 / Vec3 / Vec4 — Shared Operations

CPU traits: `Vec*Base`, `Vec*ImmutableOps`. GPU: `Vec*Expr` extensions in
`expr.scala` + `local_ops.scala`.

| CPU                    | GPU                    | WGSL Output           | Status              |
| ---------------------- | ---------------------- | --------------------- | ------------------- |
| `v + w`                | `v + w`                | `(v + w)`             | ✅ Done             |
| `v - w`                | `v - w`                | `(v - w)`             | ✅ Done             |
| `v * w`                | `v * w`                | `(v * w)`             | ✅ Done (component) |
| `v * s`                | `v * s`                | `(v * s)`             | ✅ Done             |
| `v / w`                | `v / w`                | `(v / w)`             | ✅ Done (component) |
| `v / s`                | `v / s`                | `(v / s)`             | ✅ Done             |
| `-v`                   | `-v`                   | `(-v)`                | ✅ Done             |
| `v.dot(w)`             | `v.dot(w)`             | `dot(v, w)`           | ✅ Done             |
| `v.length`             | `v.length`             | `length(v)`           | ✅ Done             |
| `v.length_squared`     | `v.length_squared`     | `dot(v, v)`           | ✅ Done             |
| `v.normalize`          | `v.normalize`          | `normalize(v)`        | ✅ Done             |
| `v.abs`                | `v.abs`                | `abs(v)`              | ✅ Done             |
| `v.floor`              | `v.floor`              | `floor(v)`            | ✅ Done             |
| `v.ceil`               | `v.ceil`               | `ceil(v)`             | ✅ Done             |
| `v.fract`              | `v.fract`              | `fract(v)`            | ✅ Done             |
| `v.trunc`              | `v.trunc`              | `trunc(v)`            | ✅ Done             |
| `v.exp2`               | `v.exp2`               | `exp2(v)`             | ✅ Done             |
| `v.inverseSqrt`        | `v.inverseSqrt`        | `inverseSqrt(v)`      | ✅ Done             |
| `v.pow(e)`             | `v.pow(e)`             | `pow(v, e)`           | ✅ Done (vec/scalar)|
| `v.sin`                | `v.sin`                | `sin(v)`              | ✅ Done             |
| `v.cos`                | `v.cos`                | `cos(v)`              | ✅ Done             |
| `v.clamp(lo, hi)`      | `v.clamp(lo, hi)`      | `clamp(v, lo, hi)`    | ✅ Done             |
| `v.clamp01`            | `v.clamp01`            | `saturate(v)`         | ✅ Done             |
| `v.min(w)`             | `v.min(w)`             | `min(v, w)`           | ✅ Done             |
| `v.max(w)`             | `v.max(w)`             | `max(v, w)`           | ✅ Done             |
| `v.mix(w, t)`          | `v.mix(w, t)`          | `mix(v, w, t)`        | ✅ Done             |
| `v.step(edge)`         | `v.step(edge)`         | `step(edge, v)`       | ✅ Done             |
| `v.smoothstep(lo, hi)` | `v.smoothstep(lo, hi)` | `smoothstep(lo,hi,v)` | ✅ Done             |
| `v.fma(w, c)`          | `v.fma(w, c)`          | `fma(v, w, c)`        | ✅ Done             |
| `v.distance(w)`        | `v.distance(w)`        | `distance(v, w)`      | ✅ Done             |
| `v.reflect(n)`         | `v.reflect(n)`         | `reflect(v, n)`       | ✅ Done             |
| `v.refract(n, eta)`    | `v.refract(n, eta)`    | `refract(v, n, eta)`  | ✅ Done             |

### 4.2 Vec3-Only Operations

| CPU          | GPU          | WGSL Output   | Status  |
| ------------ | ------------ | ------------- | ------- |
| `v.cross(w)` | `v.cross(w)` | `cross(v, w)` | ✅ Done |

### 4.3 Component Access & Aliases

| CPU         | GPU         | WGSL Output | Types       |
| ----------- | ----------- | ----------- | ----------- |
| `.x` / `.r` | `.x` / `.r` | `.x`        | All vectors |
| `.y` / `.g` | `.y` / `.g` | `.y`        | All vectors |
| `.z` / `.b` | `.z` / `.b` | `.z`        | Vec3, Vec4  |
| `.w` / `.a` | `.w` / `.a` | `.w`        | Vec4 only   |

### 4.4 Swizzles

CPU swizzles live in
[src/graphics/math/cpu/swizzles.scala](../trivalibs/src/graphics/math/cpu/swizzles.scala)
as concrete-receiver `inline extension`s on the Vec class and Vec Tuple
families. Buffer types are intentionally out of scope — buffer slots are read
component-by-component before swizzling. GPU swizzles live as `extension`s on
`Vec*Expr` in
[gpu/float_expr.scala](../trivalibs/src/graphics/math/gpu/float_expr.scala).
Naming matches WGSL — field names in component order. Same-arity swizzles (`yx`,
`zyx`, `wzyx`) preserve the source family.

Concrete receivers per storage type were chosen over a single generic
`[V: {Vec*Base, Vec*ImmutableOps}]` block because Scala's overload resolution
can't disambiguate two generic extensions that share a method name even when
their context bounds differ — the using-clauses are elaborated after the
overload pick. Concrete receivers also let `cpu.*` and `gpu.*` wildcard imports
coexist in the same file: each `.xy` / `.yz` etc. resolves uniquely by receiver
type.

The implemented set is the minimum from §1 needs: consecutive sub-vector splits

- the full-arity reverse, plus `rgba` aliases on Vec3 / Vec4 mirroring the
  existing single-component `r/g/b/a` aliases on `Vec*BaseG`. (`Vec2` has only
  `u/v` aliases, no `r/g` — so no `rg` swizzle alias.) More permutations can be
  added as usage demands.

| Source         | Splits (→ smaller sibling)                                                                                              | Full reverse                | Status  |
| -------------- | ----------------------------------------------------------------------------------------------------------------------- | --------------------------- | ------- |
| `Vec2`         | —                                                                                                                       | `yx → Vec2`                 | ✅ Done |
| `Vec2Tuple`    | —                                                                                                                       | `yx → Vec2Tuple`            | ✅ Done |
| `Vec3`         | `xy`/`rg`, `yz`/`gb` → `Vec2`                                                                                           | `zyx`/`bgr` → `Vec3`        | ✅ Done |
| `Vec3Tuple`    | `xy`/`rg`, `yz`/`gb` → `Vec2Tuple`                                                                                      | `zyx`/`bgr` → `Vec3Tuple`   | ✅ Done |
| `Vec4`         | `xy`/`rg`, `yz`/`gb`, `zw`/`ba` → `Vec2`; `xyz`/`rgb`, `yzw`/`gba` → `Vec3`                                             | `wzyx`/`abgr` → `Vec4`      | ✅ Done |
| `Vec4Tuple`    | same set as `Vec4`, returning `Vec*Tuple`                                                                               | `wzyx`/`abgr` → `Vec4Tuple` | ✅ Done |
| GPU `Vec*Expr` | same set as the CPU class side, returning `Vec*Expr` (rgba aliases are Scala-side only — they emit `.xy`/`.xyz`/… WGSL) |                             | ✅ Done |

---

## 5. Matrix Operations

### 5.1 Shared Matrix Operations (All Mat\*)

CPU: `Mat*Base` (scalar-returning) and `Mat*ImmutableOps` (allocating). GPU:
`Mat*Expr` extensions.

| CPU             | GPU             | WGSL Output      | Trait (CPU)    | Status                      |
| --------------- | --------------- | ---------------- | -------------- | --------------------------- |
| `m * n`         | `m * n`         | `(m * n)`        | `ImmutableOps` | ✅ Done                     |
| `m * v`         | `m * v`         | `(m * v)`        | `ImmutableOps` | ✅ Done                     |
| `m.transpose`   | `m.transpose`   | `transpose(m)`   | `ImmutableOps` | ✅ Done                     |
| `m.determinant` | `m.determinant` | `determinant(m)` | `Base`         | ✅ Done                     |
| `m.inverse`     | —               | —                | `ImmutableOps` | CPU only (no WGSL built-in) |

> **Removed**: Matrix `+`, `-`, and scalar `*` have been removed from CPU
> traits. These operations are rarely meaningful in graphics (matrix addition is
> not a standard transform operation). The GPU DSL retains them since WGSL
> supports them natively.

### 5.2 Mat4 Constructors (in `Mat4ImmutableOps`)

These are static factory methods on the companion object:

| Constructor                                       | Description                                                | Status     |
| ------------------------------------------------- | ---------------------------------------------------------- | ---------- |
| `Mat4.identity`                                   | Identity matrix                                            | ✅ Done    |
| `Mat4.fromRotationX(a)`                           | Rotation around X axis                                     | ✅ Done    |
| `Mat4.fromRotationY(a)`                           | Rotation around Y axis                                     | ✅ Done    |
| `Mat4.fromRotationZ(a)`                           | Rotation around Z axis                                     | ✅ Done    |
| `Mat4.fromTranslation(v)`                         | Translation matrix from `Vec3`                             | ✅ Done    |
| `Mat4.fromScale(v)`                               | Non-uniform scale from `Vec3`                              | ✅ Done    |
| `Mat4.fromAxisAngle(axis, a)`                     | Rotation around arbitrary axis                             | 📋 Planned |
| `Mat4.fromQuat(q)`                                | Build rotation matrix from quaternion                      | 📋 Planned |
| `Mat4.fromTranslationRotationScale(t, r, s)`      | Full TRS matrix in one step (used by `Transform.toMatrix`) | ✅ Done    |
| `Mat4.perspective(fov, aspect, near, far)`        | Perspective projection (WebGPU [0,1] depth, right-handed)  | ✅ Done    |
| `Mat4.lookAt(eye, center, up)`                    | View matrix (right-handed)                                 | ✅ Done    |
| `Mat4.ortho(left, right, bottom, top, near, far)` | Orthographic projection                                    | 📋 Planned |
| `Mat4.from[Mat4_](other)`                         | Convert from another Mat4 type                             | ✅ Done    |

### 5.3 Mat4 Apply Transforms (in `Mat4ImmutableOps` — pre/post multiply)

These apply a transform to an existing matrix (post-multiply):

| Method                       | Equivalent to                | Status     |
| ---------------------------- | ---------------------------- | ---------- |
| `m.rotateX(a)`               | `m * fromRotationX(a)`       | ✅ Done    |
| `m.rotateY(a)`               | `m * fromRotationY(a)`       | ✅ Done    |
| `m.rotateZ(a)`               | `m * fromRotationZ(a)`       | ✅ Done    |
| `m.translate(v)`             | `m * fromTranslation(v)`     | 📋 Planned |
| `m.scale(v)`                 | `m * fromScale(v)`           | 📋 Planned |
| `m.rotateAxisAngle(axis, a)` | `m * fromAxisAngle(axis, a)` | 📋 Planned |

### 5.4 Mat4 Mutable Variants

All immutable ops have corresponding mutable `*Self` / `*To(out)` variants via
`Mat4MutableOps`. Mutable variants for new constructors should be added where
useful in hot paths (e.g., `m.rotateSelf(a)`, `m.translateSelf(v)`).

### 5.5 Notes on WebGPU Conventions

- **Reversed-Z projection** (`near → 1.0, far → 0.0`) is preferred for WebGPU —
  better depth precision. Optional: offer both regular and reversed-Z variants.
- **Right-handed coordinate system** — same as WGSL and wgpu-matrix.
- **Column-major storage** — already the project convention.

---

## 6. Quaternion

Quaternions are a compact representation of 3D rotations (4 floats vs 16 for
Mat4). They are cheaper to interpolate (slerp), compose, and invert. The Rust
painter uses `glam::Quat` extensively.

### 6.1 Storage Design

Quaternion layout is `(x, y, z, w)` — identical to `Vec4`. Three storage forms:

| Type                       | Base accessors | Immutable ops        | Mutable ops        |
| -------------------------- | -------------- | -------------------- | ------------------ |
| `class Quat`               | `Vec4Mutable`  | `QuatImmutableOps` ✓ | `QuatMutableOps` ✓ |
| `Vec4Tuple` / `Vec4fTuple` | `Vec4Base`     | `QuatImmutableOps` ✓ | —                  |
| `StructRef[Vec4Buffer]`    | `Vec4Base`     | —                    | `QuatMutableOps` ✓ |

`Vec4Tuple` and `Vec4Buffer` can hold a quaternion with no conversion — they
already have the right layout. `Vec4` (mutable class) is **not** given quat ops
— use `Quat` instead when quat semantics are needed on a mutable value.

The base field accessors (`.x`, `.y`, `.z`, `.w`) come from `Vec4Base[Q]` and
`Vec4Mutable[Q]` — no separate `QuatBase` trait is needed.

> **No WGSL type**: quaternions are CPU-only. Convert to `Mat4` before
> uploading. No `QuatExpr` DSL type is planned.

### 6.2 `QuatImmutableOps[Q]` — Allocating Operations

All quat-specific operations use distinct `quat*`-prefixed names so that Vec4
and quat operations can coexist on the same type (`Vec4Tuple`) without
ambiguity. No operator overloads in the trait. Quat traits are Double-only (no
`Num` type parameter).

```scala
trait QuatImmutableOps[Q]:
  extension (q: Q)(using Vec4Base[Q])
    def quatMul(p: Q): Q                                        // Hamilton product → new Q
    def quatRotate[V](v: V)(using Vec3Base[V], Vec3ImmutableOps[V]): V  // rotate Vec3 → new V
    def quatConjugate: Q                                        // (−x, −y, −z, w) → new Q
    def quatInverse: Q                                          // conjugate / |q|² → new Q
    def slerp(p: Q, t: Double): Q                               // spherical linear interp
    def toMat4: Mat4                                            // 4×4 rotation matrix
```

`normalize`, `length`, `dot` are **not** in `QuatImmutableOps` — they are
already provided by `Vec4Base` / `Vec4ImmutableOps`. `slerp` gets no `quat`
prefix because it has no Vec4 analogue (Vec4 uses `mix` for linear lerp).

### 6.3 `QuatMutableOps[Q]` — In-Place Operations

In-place operations for mutable types (`Quat`). No WGSL equivalent — CPU only,
no GPU DSL entry.

```scala
trait QuatMutableOps[Q]:
  extension (q: Q)(using Vec4Mutable[Q])
    def quatMulSelf(p: Q): Unit   // self = p * self  (pre-multiply — p applied first)
    def conjugateSelf: Unit                                        // (−x, −y, −z, w) in-place
    def inverseSelf: Unit                                          // conjugate/|q|² in-place
    def normalizeSelf: Unit                                        // normalize in-place
    def setFromAxisAngle[V](axis: V, angle: Double)(using Vec3Base[V]): Unit
    def setFromRotationX(angle: Double): Unit
    def setFromRotationY(angle: Double): Unit
    def setFromRotationZ(angle: Double): Unit
    def setFromLookRotation[V](fwd: V, up: V)(using Vec3Base[V]): Unit
```

> **`*=` order**: `q *= p` means `q = p * q` (pre-multiply — `p` applied first,
> then `q`). This is reversed from the standard scalar `*=` convention but
> matches the "apply additional rotation on top" semantics:
> `transform.rotation *= deltaQ` reads as "apply deltaQ on top".

`normalizeSelf` is already in `Vec4MutableOps` and applies to quaternions
unchanged — not duplicated here.

### 6.4 `class Quat` — Dedicated Mutable Type

`Quat` is a mutable class alongside `Vec4`. It is the reference implementation:
it implements all four trait tiers — `Vec4Mutable` (base accessors + setters),
`QuatImmutableOps`, and `QuatMutableOps` — and additionally defines operator
aliases on the class that have unambiguous quat semantics:

```scala
class Quat(
  var x: Double = 0.0,
  var y: Double = 0.0,
  var z: Double = 0.0,
  var w: Double = 1.0,   // identity
)

object Quat:
  given Vec4Mutable[Quat]      = ...
  given QuatImmutableOps[Quat] = ...
  given QuatMutableOps[Quat]   = ...
```

Operator aliases on `Quat` — unambiguous since `Quat` is the dedicated type:

```scala
extension (q: Quat)
  def *(p: Quat): Quat    = q.quatMul(p)      // Hamilton product
  def *(v: Vec3): Vec3    = q.quatRotate(v)   // rotate Vec3
  def unary_- : Quat      = q.quatConjugate   // -q = conjugate
```

`Vec4`-style operators (`+`, `-`, scalar `*`) are **not** added to `Quat` since
they have no meaningful quaternion interpretation.

### 6.5 Vec4Tuple and Vec4Buffer — Both Op Sets

`Vec4Tuple` gets `QuatImmutableOps`; `StructRef[Vec4Buffer]` gets
`QuatMutableOps`. Vec4 operators take priority on each type; quat operations are
accessed via the `quat*` prefixed named methods:

```scala
// Quat — the primary type; has both immutable and mutable quat ops + operators
val q: Quat = Quat.fromRotationY(0.5)
val rotated  = q.quatRotate(Vec3(1, 0, 0))   // QuatImmutableOps → new Vec3
val composed = q.quatMul(q2)                  // QuatImmutableOps → new Quat
val composed2 = q * q2                        // operator alias for quatMul
val v        = q * Vec3(1, 0, 0)             // operator alias for quatRotate
q.quatMulSelf(q2)                             // QuatMutableOps, in-place
q.setFromRotationX(0.3)                       // QuatMutableOps, reset

// Vec4Tuple — both Vec4ImmutableOps and QuatImmutableOps, no mutable ops
val qt: Vec4Tuple = (0.0, 0.0, 0.0, 1.0)    // quaternion stored as tuple
val rotated2 = qt.quatRotate(Vec3(1, 0, 0)) // QuatImmutableOps
val scaled   = qt * 2.0                      // Vec4ImmutableOps (component-wise, not quat!)
```

### 6.6 Constructors — `object Quat`

Constructors return `Quat` (mutable), which can be copied into any storage:

| Constructor                         | Description                                |
| ----------------------------------- | ------------------------------------------ |
| `Quat.identity`                     | `(0, 0, 0, 1)` — no rotation               |
| `Quat.fromRotationX(a: Double)`     | Rotation around X axis                     |
| `Quat.fromRotationY(a: Double)`     | Rotation around Y axis                     |
| `Quat.fromRotationZ(a: Double)`     | Rotation around Z axis                     |
| `Quat.fromAxisAngle(axis: Vec3, a)` | Rotation around arbitrary normalized axis  |
| `Quat.fromLookRotation(fwd, up)`    | Orient `-Z` toward `fwd`, `+Y` toward `up` |
| `Quat.fromMat4(m: Mat4)`            | Extract rotation from upper 3×3            |

### 6.7 File Layout

```
src/graphics/math/cpu/quat.scala
  — trait QuatImmutableOps[Q]
  — trait QuatMutableOps[Q]
  — class Quat(x, y, z, w)
  — object Quat: constructors, given Vec4Mutable, given QuatImmutableOps,
      given QuatMutableOps, operator extensions (* Hamilton, * Vec3, unary -)
  — given QuatImmutableOps[Vec4Tuple]
  — given QuatMutableOps[StructRef[Vec4Buffer]]
```

---

## 7. Constructors

### 7.1 Vector Constructors

Isomorphic constructors on CPU and GPU — same overload set, differing only in
case (CPU uses the type name, GPU uses a lowercase free function to avoid
collision with the CPU types):

| CPU                        | GPU DSL            |
| -------------------------- | ------------------ |
| `Vec2(x, y)`               | `vec2(x, y)`       |
| `Vec3(x, y, z)`            | `vec3(x, y, z)`    |
| `Vec3(xy: Vec2, z)`        | `vec3(xy, z)`      |
| `Vec3(x, yz: Vec2)`        | `vec3(x, yz)`      |
| `Vec4(x, y, z, w)`         | `vec4(x, y, z, w)` |
| `Vec4(xyz: Vec3, w)`       | `vec4(xyz, w)`     |
| `Vec4(xy: Vec2, z, w)`     | `vec4(xy, z, w)`   |
| `Vec4(xy: Vec2, zw: Vec2)` | `vec4(xy, zw)`     |
| `Vec4(x, yzw: Vec3)`       | `vec4(x, yzw)`     |

CPU: `apply` overloads on the companion object, creating concrete instances.
GPU: lowercase free functions emitting inline WGSL expressions. Status: ✅ Done
on both CPU and GPU.

### 7.2 Matrix Constructors (GPU DSL — Low Priority)

WGSL supports constructing matrices from column vectors. Matching GPU DSL
constructors:

```scala
mat4(col0: Vec4Expr, col1: Vec4Expr, col2: Vec4Expr, col3: Vec4Expr): Mat4Expr
mat3(col0: Vec3Expr, col1: Vec3Expr, col2: Vec3Expr): Mat3Expr
mat2(col0: Vec2Expr, col1: Vec2Expr): Mat2Expr
```

In practice, transform matrices (MVP, normal, etc.) are computed on the CPU and
passed to shaders as `Mat4` uniforms — no quaternion or TRS computation happens
on the GPU. These constructors are therefore low priority; they may be useful
for shaders that assemble a matrix from separate per-column uniforms, but that
pattern is uncommon. Tracked as P6 in §8.2.

---

## 8. Implementation Status

### 8.1 Completed

- **Trait separation** — all Vec/Mat traits split into generic (`*G`, purely
  abstract) and CPU-specific (Double, concrete) variants. GPU DSL implements G
  traits. ✅
- **Float type removal** — `Vec2f`, `Vec3f`, `Vec4f`, `Vec*fTuple` removed. All
  CPU math is Double-only. F32 buffer types use `.toFloat` in setters. ✅
- **Vec2, Vec3, Vec4** — all CPU types (mutable class, tuple, F32/F64 buffer),
  full `Base`, `ImmutableOps`, `MutableOps` trait instances ✅
- **Mat2, Mat3, Mat4** — all CPU types (mutable class, tuple, F32 buffer), full
  `Base` (incl. `determinant`), `ImmutableOps`, `MutableOps` trait instances ✅
- **Mat4 constructors** — `fromRotationX/Y/Z`, `identity`, `fromTranslation`,
  `fromScale`, `fromTranslationRotationScale`, `perspective`, `lookAt` ✅
- **Mat4 apply-transform methods** — `rotateX/Y/Z` (immutable + mutable) ✅
- **Quaternion** — `QuatImmutableOps[Q]`, `QuatMutableOps[Q]`, `class Quat`,
  `object Quat` constructors, operator aliases, `given` for `Vec4Tuple` ✅
- **GPU DSL expression types** — `FloatExpr`, `Vec*Expr`, `Mat*Expr`,
  `BoolExpr`, `Texture2D`, `Sampler` ✅
- **GPU DSL scalar operations** — full `FloatExpr` extension set ✅
- **GPU DSL vector operations** — full `Vec*Expr` extension set ✅
- **GPU DSL matrix operations** — `Mat*Expr` `*`, `transpose`, `determinant` ✅
- **GPU DSL constructors** — `vec2`, `vec3`, `vec4` ✅
- **Swizzles** — consecutive splits + full reverse, on `Vec*` class /
  `Vec*Tuple` CPU and on `Vec*Expr` GPU. See §4.4. ✅
- **`distance` / `reflect` / `refract`** — isomorphic CPU + GPU. Added to
  `Vec*BaseG` (`distance`) and `Vec*ImmutableOpsG` (`reflect`, `refract`) with
  docstrings. See §4.1. ✅
- **NumExt parity** — full scalar set (`trunc`, `exp2`, `inverseSqrt`,
  `degrees`, `radians`, `sinh`, `cosh`, `tanh`) now isomorphic across
  `NumExt[Double]` / `NumExt[Float]` and `NumExt[FloatExpr]`. See §3.1. ✅

### 8.2 Unplanned (deferred until usage requires)

The following gaps remain. Not scheduled — implement only when a real use site
needs them, to keep the library lean.

- **Mat4 apply-transforms** — `m.translate(v: Vec3)`, `m.scale(v: Vec3)` /
  `m.scale(s: Double)` + mutable `*Self` / `*To` variants. `Transform` composes
  T·R·S directly, so these aren't currently needed on `Mat4`.
- **Extra Mat4 / Quat constructors** — `Mat4.fromAxisAngle`, `Mat4.fromQuat`,
  `Mat4.ortho`, `Quat.fromMat4`, `Quat.toAxisAngle`.
- **GPU DSL matrix constructors** — `mat2` / `mat3` / `mat4` from column
  vectors. Rarely needed — transforms are CPU-computed and uploaded.
- **Swizzle expansion** — additional permutations beyond consecutive splits +
  reverse (`xz`, `xw`, `yw`, etc.), and swizzle support for `*Buffer` types.

---

## 9. File Map

| Path                                              | Contents                                                                                          |
| ------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| `src/graphics/math/vec2.scala`                    | `Vec2BaseG`, `Vec2ImmutableOpsG`, `Vec2Base`, `Vec2Mutable`, `Vec2ImmutableOps`, `Vec2MutableOps` |
| `src/graphics/math/vec3.scala`                    | `Vec3BaseG`, … (same pattern, + `cross`)                                                          |
| `src/graphics/math/vec4.scala`                    | `Vec4BaseG`, …                                                                                    |
| `src/graphics/math/mat2.scala`                    | `Mat2BaseG`, `Mat2ImmutableOpsG`, `Mat2Base`, `Mat2Mutable`, `Mat2ImmutableOps`, `Mat2MutableOps` |
| `src/graphics/math/mat3.scala`                    | `Mat3BaseG`, … (same pattern)                                                                     |
| `src/graphics/math/mat4.scala`                    | `Mat4BaseG`, `Mat4ImmutableOpsG`, `Mat4Base`, `Mat4Mutable`, `Mat4ImmutableOps`, `Mat4MutableOps` |
| `src/graphics/math/cpu/vec*.scala`                | Concrete CPU types (`Vec2`, `Vec2Tuple`, `Vec2Buffer`, `Vec2dBuffer`, …)                          |
| `src/graphics/math/cpu/mat*.scala`                | Concrete CPU types (`Mat4`, `Mat4Tuple`, `Mat4Buffer`, …)                                         |
| `src/graphics/math/cpu/quat.scala`                | `QuatImmutableOps`, `QuatMutableOps`, `class Quat`                                                |
| `src/graphics/math/gpu/expr.scala`                | GPU DSL expression types implementing `*G` traits                                                 |
| `src/graphics/math/api_and_naming_conventions.md` | Design rationale and naming rules                                                                 |
