# trivalibs_nostd → Scala Shader DSL — Porting & Implementation Plan

Living blueprint for bringing the Rust `trivalibs_nostd` shader helper crate
into the Scala WebGPU stack, and for adding the integer-type support that those
helpers require.

Companion documents:

- [rust-painter/repomix-trivalibs-core.xml](rust-painter/repomix-trivalibs-core.xml)
  — full Rust source bundle (the `trivalibs_nostd/` section is the port target).
- [done/wgsl-scala-dsl-functions.md](done/wgsl-scala-dsl-functions.md) —
  `WgslFn` design (the mechanism all ported functions plug into).
- [done/wgsl-scala-dsl-implementation.md](done/wgsl-scala-dsl-implementation.md)
  — shader DSL expression AST, opaque-type layout, local-variable conventions.

External shader references checked into the noise lib for cross-checking
implementations (`src/graphics/shader/lib/random/`):

- [noisy_bevy.wgsl](../src/graphics/shader/lib/random/noisy_bevy.wgsl) —
  source the 2D/3D simplex + Worley + seeded variants were ported from.
- [psrdnoise2.wgsl](../src/graphics/shader/lib/random/psrdnoise2.wgsl),
  [psrdnoise3.wgsl](../src/graphics/shader/lib/random/psrdnoise3.wgsl) — Stefan
  Gustavson's psrdnoise, source for the rotating/tiling/derivative variants.
- [noise4D_stegu.glsl](../src/graphics/shader/lib/random/noise4D_stegu.glsl) —
  canonical Ashima/Stegu GLSL `snoise(vec4)`. Reference for diagnosing 4D bugs.
- [snoise_lygia.wgsl](../src/graphics/shader/lib/random/snoise_lygia.wgsl),
  [snoise_lygia_helpers.wgsl](../src/graphics/shader/lib/random/snoise_lygia_helpers.wgsl)
  — LYGIA WGSL port of `snoise4` plus its `grad4` / `permute` / `mod289`
  helpers (snoise file `#include`s them).

> **Note on the `done/` design docs.** These were the blueprint while the DSL
> was being built and remain useful for understanding intent and rationale. When
> implementing this feature, however, **always treat the code itself as the
> first source of truth** — the actual implementation may have evolved past the
> original design (renames, simplifications, additional helpers, dropped
> branches). Read the relevant `src/graphics/shader/**` and
> `src/graphics/math/gpu/**` files before extending them, and only fall back to
> the design docs for context the code does not make obvious.

- [math-library-design.md](math-library-design.md) — scalar/vector math parity
  (ambient context; not a direct dependency).

---

## Implementation Status

| Phase | Description                                            | Status                              |
| ----- | ------------------------------------------------------ | ----------------------------------- |
| 1     | Integer DSL integration                                | ✅ Complete                         |
| 2     | Port `hash.rs`                                         | ✅ Complete                         |
| 3     | Port `color.rs` + `coords.rs`                          | ✅ Complete                         |
| 4a    | Simplex noise — Gustavson classic 2D/3D + fBm + Worley | ✅ Complete                         |
| 4a    | Simplex noise — 4D + tiling via torus                  | ✅ Complete                         |
| 4b    | PSRD-noise 2D + 3D (tiling, rotating, derivatives)     | ✅ Complete                         |
| 5     | Gaussian blur library consolidation                    | ✅ Complete                         |
| 6     | `noise_tests` example                                  | ✅ Complete                         |

**Files shipped:**

| Path                                             | Phase |
| ------------------------------------------------ | ----- |
| `trivalibs/src/utils/numbers/uint.scala`         | 1     |
| `src/graphics/math/gpu/int_types.scala`          | 1     |
| `src/graphics/math/gpu/expr.scala` (extended)    | 1     |
| `src/graphics/shader/types.scala` (extended)     | 1     |
| `src/graphics/shader/dsl/types.scala` (extended) | 1     |
| `src/graphics/shader/dsl/fn.scala` (extended)    | 1     |
| `test/shader/IntDsl.test.scala`                  | 1     |
| `src/graphics/shader/lib/random/hash.scala`      | 2     |
| `test/shader/lib/HashFns.test.scala`             | 2     |
| `src/graphics/shader/lib/random/simplex.scala`   | 4a    |
| `test/shader/lib/SimplexFns.test.scala`          | 4a    |
| `src/graphics/shader/lib/random/psrdnoise.scala` | 4b    |
| `test/shader/lib/PsrdnoiseFns.test.scala`        | 4b    |
| `examples/noise_tests/`                          | 6     |
| `src/graphics/shader/lib/color/color.scala`      | 3     |
| `src/graphics/shader/lib/coords/polar.scala`     | 3     |
| `src/graphics/shader/lib/blur/blur.scala`        | 5     |
| `test/shader/lib/ColorFns.test.scala`            | 3     |
| `test/shader/lib/CoordsFns.test.scala`           | 3     |
| `test/shader/lib/BlurFns.test.scala`             | 5     |

**Deviations from plan:**

- Phase 3: Rust upstream's `color.rs::rgb2hsl` returns `max(R,G,B)` — that's V,
  not L, so it's actually `rgb2hsv`. The Scala port exposes it under the correct
  name (`rgb2hsv`) and provides a proper `rgb2hsl` alongside.
- Phase 4a: `simplexNoise4d` and `tilingSimplexNoise2d` were initially skipped
  but later added (ported from the same Rust source). Three porting bugs were
  found and fixed against the canonical Stegu GLSL + LYGIA WGSL references:
  (1) `step` arg order swapped on `is_x` / `is_yz` rank-sort, (2) `permute_1_`
  used `+ 10.0` instead of `+ 1.0`, (3) `grad_4_` sign correction inverted
  (`step(0, p)` instead of `lessThan(p, 0)`, fixed by `1.0 - step(0, p)`).
  Both shaders now render correctly. References are checked into
  `src/graphics/shader/lib/random/` (see Companion documents above).
- Phase 4a: seeded variants (`simplexNoise2dSeeded`, `simplexNoise3dSeeded`,
  `fbmSimplex2dSeeded`, `fbmSimplex3dSeeded`) were added beyond the plan scope,
  matching the Rust `noisy_bevy` port. Worley 2D (`worley2d`) was also added.
- Phase 4b naming: functions follow the Rust `trivalibs_nostd` naming
  (`tilingRotNoise2d`, `tilingNoise2d`, `rotNoise2d`) rather than the plan's
  `psrdNoise2d` alias — more descriptive and consistent with the upstream
  prefix.
- Phase 6: 18 panels (vs. plan's 7) — all hash and noise variants are exercised
  individually rather than grouped: hash, 2D/3D/4D simplex, 2D/3D seeded,
  2D/3D fBm + seeded, tiling simplex 2D, Worley, plus six psrdnoise
  sub-functions. The 18 shaders are now generated through a nested
  `aspectShade(fns*)((uv, t, r, out) => Block)` helper that bakes in the
  shared boilerplate (program.fn registrations including `aspectUv`, plus
  ctx access). Hash and tiling-simplex shaders stay inline (different
  shape).

---

## 1. Context

The shader DSL (`src/graphics/shader/` + `src/graphics/math/gpu/expr.scala`) can
express float shader math as typed Scala expressions that erase to WGSL strings
at runtime. `WgslFn` wraps that machinery into reusable helper functions
authored in either the Scala DSL or a raw WGSL body. With the painter almost
done and `WgslFn` landed, the next build-out is helper-library parity with the
Rust `trivalibs_nostd` crate:

> `random/hash.rs`, `random/simplex.rs`, `bits.rs`, `blur.rs`, `color.rs`,
> `coords.rs`, `num_ext.rs`, `vec_ext.rs`

**The blocker for `hash.rs`.** The hash family (`hashi`, `hashi_triple32`,
`hash21i`, `hash2di`, `hash3di`, `hash4di`, and their float-normalised variants)
operates on `u32` / `UVec2..4` using bitwise XOR, right shifts, and wrapping
multiplies. The Scala DSL today has **no integer types**. `Int` literals
auto-cast to `f32` via a `Conversion[Int, FloatExpr]`; there are no `IntExpr` /
`UIntExpr`, no integer vectors, no bitwise operators, and no bitcast helpers.
Porting `hash.rs` is impossible without first introducing these primitives.

**What this plan covers.**

- **Phase 1 — Integer DSL integration** (implementation task): scalar `IntExpr`
  / `UIntExpr`, signed and unsigned integer vectors, Float ↔ Int / UInt
  conversions, and bitcast helpers. This is the foundation that unblocks
  everything downstream.
- **Phase 2 — Port `hash.rs`** via `WgslFn`. First real consumer of the integer
  DSL.
- **Phase 3 — Port `color.rs` + `coords.rs`**. No integer dependency; sequenced
  after Phase 2 because hash functions are the more-pressing feature.
- **Phase 4 — Simplex noise**, two variants: §5.1 Gustavson classic (2D/3D/4D
  scalar + FBM + torus-tiling) and §5.2 PSRD-noise (rotating, per-axis tiling,
  analytical derivatives). Reuse vetted WGSL source for both; `octaves: Int`
  parameters in the FBM wrappers are the cross-phase dependency on Phase 1.
- **Phase 5 — Gaussian blur**. Consolidate the existing
  `examples/blur/Blur.scala` into the library; independent of Phase 1.
- **Phase 6 — Port the Rust `noise_tests` example**. Definition-of-done
  capstone: a single interactive example cycling through every hash and noise
  variant we ported. Verifies all of Phases 2 + 4 visually in the browser;
  depends on nothing else.

### 1.1 Confirmed decisions

| Question                    | Answer                                                                                                                                                                                                                                               |
| --------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Integer DSL scope           | Scalars + integer vectors (`IVec2..4`, `UVec2..4`). Full foundation for `hash.rs` in one pass.                                                                                                                                                       |
| Unsigned representation     | `opaque type UInt = Int` in trivalibs. Bit-identical to `Int`, distinct in the type system.                                                                                                                                                          |
| Integer literal conversion  | **Deferred.** Keep existing `Conversion[Int, FloatExpr]`. Add explicit `.i` / `.u` extensions for integer literals. Revisit after writing real shader code (Phase 2 hash + Phase 4 simplex) — the compiler will catch any later switch's call-sites. |
| Library layout              | `src/graphics/shader/lib/{random,blur,color,coords}/`. Parallels Rust module tree.                                                                                                                                                                   |
| Simplex noise / blur source | Vetted MIT/CC0 WGSL where proven; port from Rust for hash/color/coords. Attribute licences inline.                                                                                                                                                   |

### 1.2 Explicit non-goals

- **CPU-side integer Vec/Mat math.** Firm non-goal: no `IVec*`, `UVec*`,
  `IMat*`, `UMat*` types or operations in the CPU math library. JS numbers are
  IEEE-754 doubles — integer Vec/Mat types and operations have no meaningful CPU
  representation in this runtime, and we have no use case for them. The integer
  phantom traits introduced in §2.2 exist only to carry `WGSLType` instances for
  the shader DSL; they intentionally have no CPU value representation, no
  constructors, and no methods.
- **CPU ↔ GPU integer buffer bindings.** Out of scope for this plan but a
  plausible additive change later. `AttribBuffer` and `UniformBuffer` type
  members for `Int` / `UInt` / `UVec*` stay as `EmptyTuple` for now (§2.4). If
  added later, the user-facing API would accept the existing double-based `Vec*`
  / `Mat*` types as input — analogous to how JS WebGPU handles integer buffer
  fields, where JS doubles are written into an `Int32Array` / `Uint32Array` view
  at the binary layer. The `StructArray` / `StructRef` machinery in trivalibs
  already understands `Int` / `UInt` as storage types, so the missing piece is
  the binding surface, not the layout / serialisation.
- **Replacing `Conversion[Int, FloatExpr]`.** Existing float-heavy code depends
  on the `42 → FloatExpr("f32(42)")` convenience. Adding a parallel
  `Int → IntExpr` conversion would cause ambiguity at unconstrained `Expr`
  call-sites. Decision deferred until real shader code (Phases 2 + 4) shows
  whether bare-int int-context literals appear often enough to justify the flip;
  until then integer literals use the `.i` / `.u` extensions (`42.i`,
  `0x21f0aaad.u`) — idiomatic call-site shape — and fall back to the
  `IntExpr(...)` / `UIntExpr(...)` constructors only for symbol-named
  expressions.
- **Matrix integer ops.** WGSL does not have `imat*` / `umat*`, and Rust
  `trivalibs_nostd` has no matrix helpers. Skipped.
- **Rust `trivalibs_core/src/data/{grid,neighbour_list,vertex_index}`.** These
  are CPU data-structure helpers, not shader helpers — outside `trivalibs_nostd`
  scope.

---

## 2. Phase 1 — Integer DSL Integration

> **Status: ✅ Complete.** All files listed in §2.8 are shipped and tests pass.

All Phase 1 work is additive and local to the shader DSL. No existing
`FloatExpr` / `Vec*Expr` / `Mat*Expr` call-sites change; the existing
`Conversion[Int, FloatExpr]` is preserved (see §2.3.1).

### 2.1 `UInt` opaque type in trivalibs

**New file:** `trivalibs/src/utils/numbers/uint.scala`

```scala
package trivalibs.utils.numbers

opaque type UInt = Int

object UInt:
  inline def apply(v: Int): UInt = v

extension (u: UInt) inline def toInt: Int = u
extension (v: Int)  inline def u: UInt = UInt(v)
```

Zero runtime cost — at the bit level identical to `Int`. Hex hash constants like
`0x21f0aaad.u` work directly. Scala's `Int` is signed 32-bit, but at the bit
pattern level this matches WGSL `u32`, so arithmetic is well-defined in both
worlds (WGSL wraps u32 arithmetic by default, matching our semantic intent).

### 2.2 Integer phantom types

**New file:** `src/graphics/math/gpu/int_types.scala`

```scala
package graphics.math.gpu

sealed trait IVec2
sealed trait IVec3
sealed trait IVec4
sealed trait UVec2
sealed trait UVec3
sealed trait UVec4
```

Phantom markers only — mirror the role of `Texture2D` / `Sampler` in
`graphics.math.gpu.Expr`. Their only function is carrying `WGSLType` instances
and serving as `WgslFn` parameter / return types. No CPU-side value
representation, no constructors, no methods.

**Rationale.** Mirroring the pattern of `Vec2` / `Vec3` / `Vec4` (full CPU
types) would require porting glam-like integer vector classes — wasted work when
we do not need CPU evaluation of integer vectors.

### 2.3 Expression types and operations

**File:** `src/graphics/math/gpu/expr.scala`

Inside `object Expr`, alongside the existing `FloatExpr` / `Vec*Expr` /
`Mat*Expr`, add:

```scala
opaque type IntExpr  <: Expr = Expr
object IntExpr:
  def apply(s: String): IntExpr = new Expr(s)
  def apply(v: Int): IntExpr    = new Expr(v.toString)

opaque type UIntExpr <: Expr = Expr
object UIntExpr:
  def apply(s: String): UIntExpr = new Expr(s)
  def apply(v: Int): UIntExpr    = new Expr(s"${v}u")

opaque type IVec2Expr <: Expr = Expr
object IVec2Expr { def apply(s: String): IVec2Expr = new Expr(s) }

// … same for IVec3Expr, IVec4Expr, UVec2Expr, UVec3Expr, UVec4Expr
```

Plus `Let*`, `Var*`, `Const*` variants for every new type following the pattern
already in place at `expr.scala:82-130`:

```scala
opaque type LetInt   <: IntExpr   & LetExpr   = LetExpr
opaque type VarInt   <: IntExpr   & VarExpr   = VarExpr
opaque type ConstInt <: IntExpr   & ConstExpr = ConstExpr
// … same for UInt, IVec2..4, UVec2..4
```

All new opaque types are exported from the `Expr` companion alongside the
existing exports.

#### 2.3.1 Literal conversions (decision deferred)

Keep the existing `Conversion[Int, FloatExpr]` (auto-lifting
`42 → FloatExpr("f32(42)")`) **unchanged**. Adding a parallel
`Conversion[Int, IntExpr]` would cause ambiguity at unconstrained `Expr`
call-sites, so we don't add one — for now. Integer literals in DSL code use
explicit constructors or the `.i` / `.u` extensions:

```scala
// Keep existing (line 188 of expr.scala, unchanged):
given Conversion[Int, FloatExpr] = v => FloatExpr(s"f32($v)")

// New — only the unsigned chain via the trivalibs UInt opaque type:
given Conversion[UInt, UIntExpr] = v => UIntExpr(s"${UInt.toInt(v)}u")
```

Explicit `.i` and `.u` extensions on `Int` for readability and to give a concise
integer-literal path:

```scala
// In the shader DSL (graphics.math.gpu.expr or sibling extensions file):
extension (v: Int) inline def i: IntExpr = IntExpr(v)
// .u: UInt continues to live in trivalibs (§2.1); chains to UIntExpr via the
// Conversion[UInt, UIntExpr] above, so 0x21f0aaad.u typed as UIntExpr works.
```

Usage:

- `IntExpr("i") + 1.i` — explicit integer literal in int-context.
- `hash1(0x21f0aaad.u)` — unsigned literal at the call-site.
- `vec3(0, 1, 0)` — bare `Int` continues to lift to `FloatExpr` via the kept
  conversion; no migration needed.

**Why deferred and not now.** The migration cost today is one test line, but we
don't yet have first-hand experience writing real shader code with the integer
DSL. The hash port (Phase 2) keeps integer math inside raw WGSL strings, and
simplex (Phase 4) only uses `octaves: Int` as a parameter type, not as a
literal. After those phases ship, if `.i` everywhere in DSL int code feels heavy
enough to outweigh the float-tweaking convenience, flip the conversion in a
follow-up. Both `IntExpr` and `FloatExpr` are distinct opaque types, so a future
flip surfaces at compile time on every affected call-site — a mechanical
migration, not a research project.

#### 2.3.2 Arithmetic and bitwise operators

`NumOps[IntExpr]` and `NumOps[UIntExpr]`:

```scala
given NumOps[IntExpr]:
  extension (a: IntExpr)
    def +(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} + ${b.wgsl})")
    def -(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} - ${b.wgsl})")
    def *(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} * ${b.wgsl})")
    def /(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} / ${b.wgsl})")
    def unary_- : IntExpr      = IntExpr(s"(-${a.wgsl})")
  def zero: IntExpr = IntExpr("0")
  def one:  IntExpr = IntExpr("1")

given NumOps[UIntExpr]:
  // same; zero = UIntExpr("0u"), one = UIntExpr("1u"), no unary_-
```

`IntExt[IntExpr]` / `IntExt[UIntExpr]` — derived from a dedicated `IntExt` trait
(CPU side, defined in trivalibs alongside `NumExt`). Tracked in
[scala-webgpu-review-todo.md → Math / NumExt → `IntExt`](scala-webgpu-review-todo.md).
Surface: `min`, `max`, `clamp`, and the `gte` / `gt` / `lte` / `lt` / `step`
predicates returning the integer 1 or 0. Int-only `abs` and `sign` live as
standalone extensions on `IntExpr` only — neither is meaningful on unsigned
values (`abs(u32)` is a no-op in WGSL; `sign(u32)` is undefined). Omit the trig
/ transcendental / fractional methods — they belong to `NumExt` and are
float-only in WGSL.

**Bitwise operators.** WGSL and Scala share notation (`|`, `&`, `^`, `<<`, `>>`,
`~`) and operator precedence aligns tier-for-tier (`|` < `^` < `&` < shifts <
`+`/`-` < `*`/`/`/`%`), so operator syntax in Scala maps cleanly to WGSL output
without surprising regroupings. Expose operators as the primary surface, with
named-method aliases for readability in dense bit-twiddling code (e.g. hash
bodies):

All operators and named aliases get **substantive docstrings**. Bitwise math is
unfamiliar to many readers; hover docs should teach the operation, not just name
it. Each docstring covers: what the operation does at the bit level, a typical
use case, and any signed/unsigned nuance — not just "`^` means xor". The full
set:

```scala
extension (a: IntExpr)
  /** Bitwise OR — output bit is 1 if either operand has that bit set
   *  (`1010 | 0110 = 1110`). Used to combine flag sets and merge disjoint
   *  bit fields (`READ | WRITE`). Alias: [[or]]. */
  def |(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} | ${b.wgsl})")
  /** Bitwise OR with a compile-time `Int` constant — emitted directly into
   *  the WGSL output, no conversion. Useful for setting known flag bits. */
  def |(b: Int):     IntExpr = IntExpr(s"(${a.wgsl} | $b)")

  /** Bitwise AND — output bit is 1 only when both operands have that bit
   *  set (`1010 & 0110 = 0010`). The standard tool for masking: `x & 0xff`
   *  keeps the low 8 bits and zeros the rest. Alias: [[and]]. */
  def &(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} & ${b.wgsl})")
  /** Bitwise AND with a compile-time `Int` constant — useful for masks
   *  written as hex literals (`x & 0xffff_0000`). */
  def &(b: Int):     IntExpr = IntExpr(s"(${a.wgsl} & $b)")

  /** Bitwise exclusive-OR — output bit is 1 when exactly one operand has
   *  that bit set, 0 when both or neither do (`1010 ^ 0110 = 1100`). Core
   *  hash-mixing primitive: `x ^ (x >> 16)` folds entropy from the high
   *  bits down into the low bits. Also toggles a flag without reading it
   *  first (`flags ^ HOVER`). Alias: [[xor]]. */
  def ^(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} ^ ${b.wgsl})")
  /** Bitwise XOR with a compile-time `Int` constant — common shape in
   *  hash functions (`x ^ 0x21f0aaad`). */
  def ^(b: Int):     IntExpr = IntExpr(s"(${a.wgsl} ^ $b)")

  /** Shift bits left by `b` positions — equivalent to multiplying by 2^b.
   *  New low-order bits are filled with zero; bits shifted off the high
   *  end are discarded (wraps modulo 2^32). Alias: [[shl]]. */
  def <<(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} << ${b.wgsl})")
  /** Shift left by a compile-time `Int` count. */
  def <<(b: Int):     IntExpr = IntExpr(s"(${a.wgsl} << $b)")

  /** Shift bits right by `b` positions. On `IntExpr` (signed) this is an
   *  *arithmetic* shift — the sign bit is repeated into the new high bits,
   *  so negatives stay negative (`-8 >> 1 == -4`). Equivalent to integer
   *  division by 2^b rounding toward negative infinity. For zero-fill
   *  (logical) right shift, use `UIntExpr` instead. Alias: [[shr]]. */
  def >>(b: IntExpr): IntExpr = IntExpr(s"(${a.wgsl} >> ${b.wgsl})")
  /** Arithmetic shift right by a compile-time `Int` count. */
  def >>(b: Int):     IntExpr = IntExpr(s"(${a.wgsl} >> $b)")

  /** Bitwise NOT — flips every bit (0 ↔ 1). For a signed `i32`,
   *  `~x == -x - 1` (two's-complement identity). Often paired with `&` to
   *  clear specific bits: `flags & ~HOVER` removes the `HOVER` flag.
   *  Alias: [[not]]. */
  def unary_~ : IntExpr      = IntExpr(s"(~${a.wgsl})")

  /** Bitwise OR (alias of [[|]]). Useful in dense bit-twiddling code where
   *  operator stacks become hard to scan. Combines bits from `a` and `b`. */
  inline def or(b: IntExpr):  IntExpr = a | b
  /** Bitwise AND (alias of [[&]]). Keeps only bits set in both `a` and `b`
   *  — the standard masking shape. */
  inline def and(b: IntExpr): IntExpr = a & b
  /** Bitwise XOR (alias of [[^]]). Flips bits of `a` wherever `b` has a 1.
   *  The hashing/mixing workhorse. */
  inline def xor(b: IntExpr): IntExpr = a ^ b
  /** Shift left (alias of [[<<]]). Multiplies by 2^b, zero-fills low bits. */
  inline def shl(b: IntExpr): IntExpr = a << b
  /** Arithmetic shift right (alias of [[>>]]). Divides by 2^b, preserves
   *  the sign bit on `IntExpr`. */
  inline def shr(b: IntExpr): IntExpr = a >> b
  /** Bitwise NOT (alias of `unary_~`). Flips every bit of `a`. */
  inline def not:             IntExpr = ~a

// Same shape for UIntExpr, with two adjustments worth calling out in the
// docstrings on the UInt side:
//   - `Int` overloads emit the `u` suffix: `def |(b: Int) = UIntExpr(s"(${a.wgsl} | ${b}u)")`
//   - `>>` doc says "logical (zero-fill) shift right — high bits become 0,
//     unlike `IntExpr.>>` which preserves the sign bit"
// Same set applies component-wise for IVec*Expr and UVec*Expr — docstrings
// can be shorter there ("component-wise [[IntExpr.|]]") and link back to
// the scalar definitions.
```

**Why the `Int` overloads.** They let compile-time `Int` constants flow into
runtime WGSL expressions ergonomically: `seed >> 16`, `mask & 0xff`,
`acc ^ 0x21f0aaad`. This is independent of the deferred
`Conversion[Int, IntExpr]` decision (§2.3.1) — the per-operator overloads are
scoped to `IntExpr`/`UIntExpr` operands, so they don't introduce ambiguity at
generic `Expr` call-sites.

**Asymmetry note.** Overloads live on `IntExpr` only, never on `Int` itself (we
don't pollute the global `Int` operator surface). So `seed & 0xff` compiles, but
`0xff & seed` doesn't — write the expression-typed operand on the left.
Constant-only expressions (`(0xff & 0x0f).i`) evaluate at compile time via
Scala's normal `Int` arithmetic, which is the most efficient encoding anyway.

**`>>` semantics.** WGSL chooses arithmetic vs. logical shift by operand type:
`i32 >> n` is arithmetic, `u32 >> n` is logical. Our `IntExpr`/`UIntExpr` split
makes this implicit — `IntExpr.>>` lines up with Scala's signed `>>`,
`UIntExpr.>>` is logical (Scala's `>>>` semantics, but spelled `>>` to match
WGSL). We deliberately don't expose `>>>` on either type.

#### 2.3.3 Conversion and bitcast extensions

Explicit type casts (WGSL `i32(…)`, `u32(…)`, `f32(…)`):

```scala
extension (a: FloatExpr) def toI32: IntExpr   = IntExpr(s"i32(${a.wgsl})")
extension (a: FloatExpr) def toU32: UIntExpr  = UIntExpr(s"u32(${a.wgsl})")
extension (a: IntExpr)   def toF32: FloatExpr = FloatExpr(s"f32(${a.wgsl})")
extension (a: IntExpr)   def toU32: UIntExpr  = UIntExpr(s"u32(${a.wgsl})")
extension (a: UIntExpr)  def toF32: FloatExpr = FloatExpr(s"f32(${a.wgsl})")
extension (a: UIntExpr)  def toI32: IntExpr   = IntExpr(s"i32(${a.wgsl})")
```

Bitcast extensions — the `bits.rs` equivalent, essential for hashes that feed
float positions into integer hash algorithms:

```scala
extension (a: FloatExpr) def bitsToU32: UIntExpr  = UIntExpr(s"bitcast<u32>(${a.wgsl})")
extension (a: UIntExpr)  def bitsToF32: FloatExpr = FloatExpr(s"bitcast<f32>(${a.wgsl})")

extension (v: Vec2Expr)  def bitsToU32: UVec2Expr = UVec2Expr(s"bitcast<vec2<u32>>(${v.wgsl})")
extension (v: UVec2Expr) def bitsToF32: Vec2Expr  = Vec2Expr(s"bitcast<vec2<f32>>(${v.wgsl})")
// same for Vec3 ↔ UVec3, Vec4 ↔ UVec4
```

#### 2.3.4 Integer vector algebra

Reuse the generic `Vec2BaseG[N, V]` / `Vec2ImmutableOpsG[N, V]` trait pair from
`graphics.math.cpu.*` (the same shape used at `expr.scala:261-350`). Provide
four instances per arity:

- `Vec2BaseG[IntExpr,  IVec2Expr]` / `Vec2ImmutableOpsG[IntExpr,  IVec2Expr]`
- `Vec2BaseG[UIntExpr, UVec2Expr]` / `Vec2ImmutableOpsG[UIntExpr, UVec2Expr]`
- same for arity 3 and 4

Operations:

- `.x`, `.y`, `.z`, `.w` — return `IntExpr` / `UIntExpr`.
- `+ - * /` — vec/vec and vec/scalar, following the existing pattern.
- `.dot(other)` — returns scalar (`IntExpr` / `UIntExpr`).
- `abs`, `min`, `max`, `clamp` — WGSL supports these on ivec/uvec.
- Bitwise `| & ^ << >> ~` (and named aliases `or / and / xor / shl / shr / not`)
  — component-wise (WGSL supports these on ivec/uvec).

Constructors:

```scala
object uvec2:
  def apply(x: UIntExpr, y: UIntExpr): UVec2Expr =
    UVec2Expr(s"vec2<u32>(${x.wgsl}, ${y.wgsl})")

object uvec3: /* similar */
object uvec4: /* similar */
object ivec2: /* similar */
object ivec3: /* similar */
object ivec4: /* similar */
```

### 2.4 `WGSLType` instances

**File:** `src/graphics/shader/types.scala`

Add after the existing `Mat4` instance:

```scala
given WGSLType[Int]:
  def wgslName = "i32"
  def byteSize = 4
  def alignment = 4
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[UInt]:
  def wgslName = "u32"
  def byteSize = 4
  def alignment = 4
  def vertexFormat = ""
  type AttribBuffer = EmptyTuple
  type UniformBuffer = EmptyTuple

given WGSLType[IVec2]: /* wgslName = "vec2<i32>", byteSize = 8,  alignment = 8  */
given WGSLType[IVec3]: /* wgslName = "vec3<i32>", byteSize = 12, alignment = 16 */
given WGSLType[IVec4]: /* wgslName = "vec4<i32>", byteSize = 16, alignment = 16 */
given WGSLType[UVec2]: /* wgslName = "vec2<u32>", … */
given WGSLType[UVec3]: /* wgslName = "vec3<u32>", … */
given WGSLType[UVec4]: /* wgslName = "vec4<u32>", … */
```

`AttribBuffer` / `UniformBuffer` are `EmptyTuple` — integers deliberately stay
shader-only. The empty `vertexFormat` keeps these types out of the vertex-layout
code path. These fields become meaningful later if a CPU-side integer buffer use
case appears.

### 2.5 DSL type mappings

**File:** `src/graphics/shader/dsl/types.scala`

Extend `ToExpr` (currently at lines 24-34):

```scala
type ToExpr[T] = T match
  case Float   => FloatExpr
  case Double  => FloatExpr
  case Boolean => BoolExpr
  case Int     => IntExpr         // new
  case UInt    => UIntExpr        // new
  case Vec2    => Vec2Expr
  case Vec3    => Vec3Expr
  case Vec4    => Vec4Expr
  case IVec2   => IVec2Expr       // new
  case IVec3   => IVec3Expr       // new
  case IVec4   => IVec4Expr       // new
  case UVec2   => UVec2Expr       // new
  case UVec3   => UVec3Expr       // new
  case UVec4   => UVec4Expr       // new
  case Mat2    => Mat2Expr
  case Mat3    => Mat3Expr
  case Mat4    => Mat4Expr
  case _       => T
```

Extend `ToLocal` (currently at lines 52-67) with `Var[Int]`, `Var[UInt]`,
`Var[IVec2..4]`, `Var[UVec2..4]` and the corresponding `Const[…]` / bare-type
cases.

Extend `populateKinds` if necessary — the current `Var[?]` / `Const[?]` pattern
(lines 88-92) already matches wildcard arguments, so it should work without
changes.

### 2.6 `callExpr` return-type dispatch

**File:** `src/graphics/shader/dsl/fn.scala`

Extend the `callExpr[R]` match (currently lines 109-119):

```scala
inline def callExpr[R](s: String): ToExpr[R] =
  inline erasedValue[R] match
    case _: Float   => FloatExpr(s).asInstanceOf[ToExpr[R]]
    case _: Double  => FloatExpr(s).asInstanceOf[ToExpr[R]]
    case _: Int     => IntExpr(s).asInstanceOf[ToExpr[R]]      // new
    case _: UInt    => UIntExpr(s).asInstanceOf[ToExpr[R]]     // new
    case _: Vec2    => Vec2Expr(s).asInstanceOf[ToExpr[R]]
    case _: Vec3    => Vec3Expr(s).asInstanceOf[ToExpr[R]]
    case _: Vec4    => Vec4Expr(s).asInstanceOf[ToExpr[R]]
    case _: IVec2   => IVec2Expr(s).asInstanceOf[ToExpr[R]]    // new
    case _: IVec3   => IVec3Expr(s).asInstanceOf[ToExpr[R]]    // new
    case _: IVec4   => IVec4Expr(s).asInstanceOf[ToExpr[R]]    // new
    case _: UVec2   => UVec2Expr(s).asInstanceOf[ToExpr[R]]    // new
    case _: UVec3   => UVec3Expr(s).asInstanceOf[ToExpr[R]]    // new
    case _: UVec4   => UVec4Expr(s).asInstanceOf[ToExpr[R]]    // new
    case _: Mat2    => Mat2Expr(s).asInstanceOf[ToExpr[R]]
    case _: Mat3    => Mat3Expr(s).asInstanceOf[ToExpr[R]]
    case _: Mat4    => Mat4Expr(s).asInstanceOf[ToExpr[R]]
    case _: Boolean => BoolExpr(s).asInstanceOf[ToExpr[R]]
```

### 2.7 Tests

**New file:** `test/shader/IntDsl.test.scala`, extending `munit.FunSuite`.

Test groups:

Tests should reflect the **idiomatic call-site shape**: integer literals go
through the `.i` / `.u` extensions (the public surface), and `IntExpr(...)` /
`UIntExpr(...)` constructors taking strings appear only when the test
legitimately constructs a symbol expression (variable name, parameter
reference). One or two direct-constructor tests are kept where they pin down the
constructor's own behaviour.

**Literals**

- `42.i.toString == "42"` — `.i` extension produces `IntExpr` from `Int`.
- `(-7).i.toString == "-7"` — negative literal preserved verbatim.
- `42.u.toString == "42u"` — `.u` chains `Int → UInt → UIntExpr` and emits the
  `u` suffix.
- `0x21f0aaad.u.toString == "569534125u"` — hex literal flows through Scala's
  `Int.toString` (decimal output, equal bit pattern).
- Direct-constructor anchor (one test, kept to pin the string path used by
  symbol expressions): `IntExpr("count").toString == "count"`.

**Scalar arithmetic**

- `(IntExpr("a") + IntExpr("b")).toString == "(a + b)"` — symbol + symbol.
- `(UIntExpr("a") * UIntExpr("b")).toString == "(a * b)"`.
- `IntExpr("a").abs.toString == "abs(a)"`.

**Bitwise** (operator syntax primary; named-alias parity covered separately)

- `(UIntExpr("a") ^ UIntExpr("b")).toString == "(a ^ b)"`
- `(UIntExpr("a") & UIntExpr("b")).toString == "(a & b)"`
- `(UIntExpr("a") | UIntExpr("b")).toString == "(a | b)"`
- `(~UIntExpr("a")).toString == "(~a)"`
- Int-overload (compile-time constant flowing in):
  `(UIntExpr("a") >> 16).toString == "(a >> 16u)"`
- Int-overload on `IntExpr`: `(IntExpr("a") & 0xff).toString == "(a & 255)"`
- `.u`-literal flowing in via the regular `UIntExpr` operator:
  `(UIntExpr("a") ^ 0x21f0aaad.u).toString == "(a ^ 569534125u)"` — verifies the
  `.u` literal path composes with the bitwise surface in hash-style code.
- Named-alias parity: `UIntExpr("a").xor(UIntExpr("b")).toString == "(a ^ b)"`,
  `UIntExpr("a").shr(16.u).toString == "(a >> 16u)"`,
  `UIntExpr("a").not.toString == "(~a)"` — confirms aliases inline to the same
  WGSL output as operators.
- Precedence sanity:
  `(IntExpr("a") + IntExpr("b") | IntExpr("c")).toString == "((a + b) | c)"` —
  verifies Scala parser groups `+` tighter than `|`, matching WGSL.

**Conversions**

- `FloatExpr("x").toU32.toString == "u32(x)"`
- `UIntExpr("x").toF32.toString == "f32(x)"`

**Bitcasts**

- `FloatExpr("x").bitsToU32.toString == "bitcast<u32>(x)"`
- `UIntExpr("x").bitsToF32.toString == "bitcast<f32>(x)"`
- `Vec2Expr("v").bitsToU32.toString == "bitcast<vec2<u32>>(v)"`
- `UVec2Expr("u").bitsToF32.toString == "bitcast<vec2<f32>>(u)"`

**Vector algebra**

- `uvec2(UIntExpr("a"), UIntExpr("b")).toString == "vec2<u32>(a, b)"` —
  symbol-typed components.
- Literal-typed components via `.u`:
  `uvec2(0.u, 1.u).toString == "vec2<u32>(0u, 1u)"`.
- `(uvec2(…) * uvec2(…)).toString` — expected `"(vec2<u32>(…) * vec2<u32>(…))"`.
- `UVec2Expr("u").x.toString == "u.x"`.
- Component-wise bitwise:
  `(UVec2Expr("u") ^ UVec2Expr("v")).toString == "(u ^ v)"`.

**`WgslFn` integration**

- `WgslFn[(seed: UInt), UInt]` — verify `data.src` contains `"seed: u32"` and
  `"-> u32"`.
- `WgslFn[(v: UVec2), UVec2]` — verify `"v: vec2<u32>"` and `"-> vec2<u32>"`.
- `WgslFn[(p: UVec3), Vec3]` — mixed types at param and return.
- Apply-site: `val f: WgslFn[(x: UInt), UInt] = WgslFn.raw("f")("return x;")`;
  assert `f(5.u).toString == "f(5u)"` and type is `UIntExpr`.

**Implicit-conversion guards** (deferred-decision boundary)

- Existing: `(42: FloatExpr).toString == "f32(42)"` — confirms the
  `Int → FloatExpr` conversion is preserved (deferred decision, see §2.3.1).
- New: `42.i.toString == "42"` (explicit `.i` extension produces `IntExpr`).
- New: `(42.u: UIntExpr).toString == "42u"` (chained `Int → UInt → UIntExpr` via
  the new `Conversion[UInt, UIntExpr]`).
- Negative test: `summon[Conversion[Int, IntExpr]]` does **not** compile —
  guards against accidentally adding the implicit conversion before the decision
  is taken.

### 2.8 File-change summary (Phase 1)

| Path                                     | Change                                                                        |
| ---------------------------------------- | ----------------------------------------------------------------------------- |
| `trivalibs/src/utils/numbers/uint.scala` | **NEW** — `opaque type UInt = Int` + `.u` extension.                          |
| `src/graphics/math/gpu/int_types.scala`  | **NEW** — phantom markers `IVec2..4`, `UVec2..4`.                             |
| `src/graphics/math/gpu/expr.scala`       | **EXTEND** — opaque exprs, ops, bitwise, conversions, bitcasts, constructors. |
| `src/graphics/shader/types.scala`        | **EXTEND** — `WGSLType[Int/UInt/IVec*/UVec*]`.                                |
| `src/graphics/shader/dsl/types.scala`    | **EXTEND** — `ToExpr`, `ToLocal` match types.                                 |
| `src/graphics/shader/dsl/fn.scala`       | **EXTEND** — `callExpr[R]` match.                                             |
| `test/shader/IntDsl.test.scala`          | **NEW** — tests listed in §2.7.                                               |

### 2.9 Reused patterns and utilities

- Opaque-type-inside-`Expr`-object layout — `expr.scala:45-130`.
- `Let* <: Expr & LetExpr`, `Var* <: Expr & VarExpr`,
  `Const* <: Expr & ConstExpr` — lines 82-130.
- Generic `Vec{2,3,4}BaseG[N, V]` / `Vec{2,3,4}ImmutableOpsG[N, V]` traits from
  `graphics.math.cpu.*` — used for float vectors at lines 261-541; re-used for
  integer vectors with different `N` type parameter.
- `NumOps[T]` from `trivalibs.utils.numbers` — same pattern as the existing
  `FloatExpr` instance at lines 195-255. (`NumExt` is float-only; integer-side
  ops come from the dedicated `IntExt` trait — see §2.3.2.)
- `TypedLocalAccessor` / `buildLocalKinds` — `dsl/types.scala:72-93`. The
  `Var[?]` / `Const[?]` wildcard in `populateKinds` already handles any type
  argument, so new `Var[Int]` / `Var[UVec2]` etc. need no special handling
  beyond adding cases to `ToLocal`.

### 2.10 Phase 1 verification

- `bun run build` compiles — confirms new opaque types, match-type extensions,
  and `WGSLType` givens are consistent.
- `scala test .` passes — all existing tests plus the new `IntDsl.test.scala`.
- Manual spot-check: add a one-line `WgslFn[(s: UInt), UInt]` inside any example
  and confirm the generated WGSL compiles when the example renders in the
  browser (`bun run dev`).
- No visual regression on existing examples: `simple_triangle`,
  `buffer_triangle`, `blur`, `painter_dsl`.

---

## 3. Phase 2 — Port `hash.rs`

> **Status: ✅ Complete.** All functions in §3.1, the float-input wrappers in
> §3.2, and tests in `test/shader/lib/HashFns.test.scala` are shipped.

**Location:** `src/graphics/shader/lib/random/hash.scala`.

Port each Rust function as a `WgslFn`. Use `WgslFn.raw` throughout — the bodies
are short, dominated by bitwise operations, and read most clearly as literal
WGSL strings. The Scala DSL would add line noise without adding safety, since
every operation is a direct intrinsic.

### 3.0 Preserve all comments and attributions verbatim

The Rust `hash.rs` carries non-trivial provenance and legal information that
**must be reproduced in the Scala port**:

- **Source-attribution comments**, e.g.
  `// Imported and ported from https://www.shadertoy.com/view/WttXWX`,
  `// from Chris Wellons https://nullprogram.com/blog/2018/07/31/`,
  `// https://github.com/skeeto/hash-prospector`,
  `// see https://www.shadertoy.com/view/XlGcRh`,
  `// https://www.pcg-random.org/`,
  `// http://www.jcgt.org/published/0009/03/02/`. These document where the
  algorithm came from and let future readers verify quality or pick between
  variants.
- **Quality metadata**, e.g. `// bias: 0.10760229515479501`,
  `// bias: 0.020888578919738908 = minimal theoretic limit`,
  `// probably hashi is good enough for most cases`. Useful for readers deciding
  which hash to reach for; preserve verbatim.
- **License blocks** (legally required to preserve). `hash.rs` includes a full
  MIT license header from Inigo Quilez covering the `hash21i` / `hash21` family
  — this entire block must appear in the Scala source above the corresponding
  functions, unaltered.

**How to place them in Scala source:**

- Function-level comments (above each `pub fn`) → as a Scaladoc block
  (`/** ... */`) or block comment immediately above the corresponding
  `val xxx: WgslFn[...] = ...` declaration. Preserve every URL and number.
- Multi-function license blocks → as a block comment header above the first
  function the licence covers, with a short note clarifying which functions it
  applies to. Do not split or paraphrase the licence text.
- File-level comments (top of `hash.rs`) → top of `hash.scala`.
- Any in-body comments → inside the WGSL string body (WGSL `//` comments are
  valid).

When in doubt, copy the comment as-is. The cost of an extra line is trivial; the
cost of dropping a license or attribution is real.

### 3.1 Function table

| Rust                     | Scala `WgslFn`        | Signature                   |
| ------------------------ | --------------------- | --------------------------- |
| `hashi(x: u32)`          | `val hashU32`         | `WgslFn[(x: UInt), UInt]`   |
| `hashi_triple32(x: u32)` | `val hashU32Triple32` | `WgslFn[(x: UInt), UInt]`   |
| `u32_to_f32(x: u32)`     | `val u32ToF32`        | `WgslFn[(x: UInt), Float]`  |
| `hash(x: u32)`           | `val hash1`           | `WgslFn[(x: UInt), Float]`  |
| `hash21i(p: UVec2)`      | `val hash21i`         | `WgslFn[(p: UVec2), UInt]`  |
| `hash21(p: UVec2)`       | `val hash21`          | `WgslFn[(p: UVec2), Float]` |
| `hash2di(v: UVec2)`      | `val hash2di`         | `WgslFn[(v: UVec2), UVec2]` |
| `hash2d(v: UVec2)`       | `val hash2d`          | `WgslFn[(v: UVec2), Vec2]`  |
| `hash3di(v: UVec3)`      | `val hash3di`         | `WgslFn[(v: UVec3), UVec3]` |
| `hash3d(v: UVec3)`       | `val hash3d`          | `WgslFn[(v: UVec3), Vec3]`  |
| `hash4di(v: UVec4)`      | `val hash4di`         | `WgslFn[(v: UVec4), UVec4]` |
| `hash4d(v: UVec4)`       | `val hash4d`          | `WgslFn[(v: UVec4), Vec4]`  |

### 3.2 Float-input ergonomic wrappers

Callers often have `Vec2` / `Vec3` positions and want a normalised `f32` or
`Vec*` hash without writing `bitcast<vec2<u32>>(pos)` manually. Provide thin
wrappers:

```scala
val hash2dFromVec: WgslFn[(v: Vec2), Vec2] =
  WgslFn.raw("hash2d_from_vec")("return hash2d(bitcast<vec2<u32>>(v));")
val hash3dFromVec: WgslFn[(v: Vec3), Vec3] = /* similar */
val hash4dFromVec: WgslFn[(v: Vec4), Vec4] = /* similar */
val hash1FromFloat: WgslFn[(x: Float), Float] =
  WgslFn.raw("hash1_from_float")("return hash(bitcast<u32>(x));")
```

These `WgslFn`s call other `WgslFn`s by name — the caller must register both the
underlying and the wrapper via `program.fn(...)`. Registration is idempotent
(see `program.scala:...`), so double-registration of `hash2d` when both `hash2d`
and `hash2dFromVec` are used is free.

### 3.3 Body style

Port the Rust body verbatim to WGSL, **and carry the surrounding comments
across** as specified in §3.0. Example (Rust `hashi` with its provenance and
bias-metadata comments):

```rust
// Imported and ported from https://www.shadertoy.com/view/WttXWX

// // --- from Chris Wellons https://nullprogram.com/blog/2018/07/31/
// https://github.com/skeeto/hash-prospector

// bias: 0.10760229515479501
// has excellent results if tested here: https://www.shadertoy.com/view/XlGcRh
pub fn hashi(x: u32) -> u32 {
    let mut x = x;
    x ^= x >> 16;
    x = x.wrapping_mul(0x21f0aaad);
    x ^= x >> 15;
    x = x.wrapping_mul(0xd35a2d97);
    x ^ (x >> 15)
}
```

becomes:

```scala
// Imported and ported from https://www.shadertoy.com/view/WttXWX

// from Chris Wellons https://nullprogram.com/blog/2018/07/31/
// https://github.com/skeeto/hash-prospector

/** bias: 0.10760229515479501.
 *  Has excellent results if tested here: https://www.shadertoy.com/view/XlGcRh */
val hashU32: WgslFn[(x: UInt), UInt] = WgslFn.raw("hash_u32"):
  """  var v = x;
  v ^= v >> 16u;
  v = v * 0x21f0aaadu;
  v ^= v >> 15u;
  v = v * 0xd35a2d97u;
  return v ^ (v >> 15u);"""
```

The shape is: file-level / family-level comments stay as `//` line comments; the
per-function comment (typically the bias-and-source note) becomes the Scaladoc
block on the `val`, which makes it discoverable on hover. WGSL `u32` arithmetic
wraps by default — no explicit `wrapping_mul` needed.

For the `hash21` family, the full Inigo Quilez MIT license block stays as a
`/* ... */` block comment header above the first function it covers (typically
`hash21i`), with a short note like
`// MIT-licensed material below applies to hash21i / hash21`.

### 3.4 Tests

**New file:** `test/shader/lib/HashFns.test.scala`.

- Each `WgslFn.src` contains the expected operations (`0x21f0aaad`, `>> 16u`,
  `bitcast<vec2<u32>>`, etc.).
- Composed calls: `hash1.src` references `hash_u32` and `u32_to_f32`.
- Idempotent registration: `program.fn(hashU32); program.fn(hashU32)` emits the
  body exactly once.
- Apply-site typing: `hash2d(uvec2Expr).toString == "hash2d(uvec2Expr)"` and
  resulting type is `Vec2Expr`.

No shader-execution tests — we verify emission only, trusting WGSL to behave
identically to the Rust semantics of the bit-level ops.

---

## 4. Phase 3 — `color.rs` + `coords.rs`

> **Status: ⬜ Not started.**

### 4.1 `color.rs` → `src/graphics/shader/lib/color/color.scala`

| Rust                | Scala `WgslFn`         | Signature                 |
| ------------------- | ---------------------- | ------------------------- |
| `rgb2hsl`           | `val rgb2hsl`          | `WgslFn[(c: Vec3), Vec3]` |
| `hsv2rgb`           | `val hsv2rgb`          | `WgslFn[(c: Vec3), Vec3]` |
| `hsv2rgb_smooth`    | `val hsv2rgbSmooth`    | `WgslFn[(c: Vec3), Vec3]` |
| `hsv2rgb_smoother`  | `val hsv2rgbSmoother`  | `WgslFn[(c: Vec3), Vec3]` |
| `hsv2rgb_smoothest` | `val hsv2rgbSmoothest` | `WgslFn[(c: Vec3), Vec3]` |

Rust uses `NumExt` helpers (`clamp01`, `fit1101`, `smoothen`) and `VecExt`
helpers (`sin`, `cos`, `frct`). All already exist on `FloatExpr` / `Vec3Expr` in
`expr.scala`. Bodies can be written in the Scala DSL or as raw WGSL — prefer raw
WGSL for byte-for-byte parity with the Rust versions and to avoid translating
between DSL operator names.

### 4.2 `coords.rs` → `src/graphics/shader/lib/coords/polar.scala`

Rust `PolarCoord` is a CPU struct. On the shader side, pack `(radius, angle)`
into a `Vec2`:

```scala
val polarToCart: WgslFn[(radiusAngle: Vec2), Vec2] = WgslFn.raw("polar_to_cart"):
  """  return vec2<f32>(radius_angle.x * cos(radius_angle.y),
                        radius_angle.x * sin(radius_angle.y));"""

val cartToPolar: WgslFn[(v: Vec2), Vec2] = WgslFn.raw("cart_to_polar"):
  """  return vec2<f32>(length(v), atan2(v.y, v.x));"""
```

### 4.3 `bits.rs`

The scalar and vector bitcast helpers from §2.3.3 cover `bits.rs` entirely; no
separate file is needed. The Rust `FloatBits` trait's role is filled by the
`.bitsToU32` / `.bitsToF32` extensions on the expression types.

### 4.4 `num_ext.rs` / `vec_ext.rs`

These are already substantially implemented on `FloatExpr` and `Vec*Expr` in
`expr.scala`. The `math-library-design.md` document already tracks parity gaps
(sign, round, trunc, fract, exp, log, inverseSqrt, degrees, radians, sinh, cosh,
tanh, distance, reflect, refract). Closing those gaps is scheduled in that
document; this plan does not re-schedule them.

---

## 5. Phase 4 — Simplex noise

> **Status: ✅ Complete (with deviations — see status table above).**
> `simplexNoise2d/3d`, seeded variants, `fbmSimplex2d/3d` (seeded and unseeded),
> `worley2d`, and all psrdnoise functions are shipped and tested.
> `simplexNoise4d` and `tilingSimplexNoise2d` were deliberately skipped.

**Location:** `src/graphics/shader/lib/random/simplex.scala`.

Two distinct simplex variants land in this phase, mirroring the Rust
`trivalibs_nostd::random::simplex` module. Both are needed — they cover
different use cases and neither subsumes the other:

1. **Gustavson classic** — the widely-adopted webgl-noise family. 2D / 3D / 4D
   scalar noise plus FBM wrappers, plus a `tiling_simplex_noise_2d` that maps
   `pos` to a torus via `simplex_noise_4d` for seamless wrapping.
2. **PSRD-noise rotating-tiling variant** — Stefan Gustavson and Ian McEwan's
   [psrdnoise](https://github.com/stegu/psrdnoise/), which adds periodic tiling
   (configurable period per axis), rotating gradients, and analytical
   derivatives in a single function. Returns the noise value **and** the
   gradient vector. The repo upstream maintains a WGSL port directly — use it
   as-is, do not re-derive from GLSL.

Per §3.0, every URL, attribution comment, and licence header from the Rust
source (and from the WGSL source for psrdnoise) must be preserved verbatim in
`simplex.scala`. Both algorithm families come with their own licences; keep them
attached to the functions they cover.

### 5.1 Gustavson classic (Phase 4a)

WGSL source: pick a verified MIT-licensed translation from the webgl-noise
family. Licence header with attribution at the top of the file (or above the
affected function block, if multiple licences coexist).

```scala
val simplexNoise2d: WgslFn[(pos: Vec2), Float]
val simplexNoise3d: WgslFn[(pos: Vec3), Float]
val simplexNoise4d: WgslFn[(pos: Vec4), Float]

val fbmSimplex2d: WgslFn[
  (pos: Vec2, octaves: Int, freqFactor: Float, amplitudeFactor: Float),
  Float,
]
val fbmSimplex3d: WgslFn[
  (pos: Vec3, octaves: Int, freqFactor: Float, amplitudeFactor: Float),
  Float,
]

/** Seamlessly tiling 2D simplex via 4D noise on a torus. `pos` should be in
 *  [0, 1] for clean wrapping. Wraps `simplexNoise4d`. */
val tilingSimplexNoise2d: WgslFn[(pos: Vec2, scale: Float), Float]
```

`octaves: Int` is the first cross-phase consumer beyond the hash port — requires
Phase 1 to be in place.

The FBM bodies need an integer loop counter and `< octaves` comparison; WGSL
supports both. Sample body for `fbm_simplex_2d`:

```wgsl
  var total: f32 = 0.0;
  var frequency: f32 = 1.0;
  var amplitude: f32 = 1.0;
  var max_amp: f32 = 0.0;
  for (var i: i32 = 0; i < octaves; i = i + 1) {
    total = total + simplex_noise_2d(pos * frequency) * amplitude;
    max_amp = max_amp + amplitude;
    frequency = frequency * freq_factor;
    amplitude = amplitude * amplitude_factor;
  }
  return total / max_amp;
```

### 5.2 PSRD-noise — rotating, tiling, with derivatives (Phase 4b)

Source: [github.com/stegu/psrdnoise](https://github.com/stegu/psrdnoise/) by
Stefan Gustavson and Ian McEwan. The repo ships GLSL, HLSL, MSL, and WGSL ports
directly — pull the WGSL version verbatim rather than re-deriving from another
language. Carry the upstream licence header (MIT) and any inline
source-attribution comments verbatim per §3.0; the
`// Implementation based on psrdnoise by Stefan Gustavson and Ian McEwan. See <https://github.com/stegu/psrdnoise/>`
comment from the Rust port at
[rust-painter/repomix-trivalibs-core.xml:7049-7052](rust-painter/repomix-trivalibs-core.xml#L7049-L7052)
becomes the Scaladoc for the Scala-side `val`.

PSRD-noise is **not** a replacement for the Gustavson classic in §5.1 — it
covers a different feature set:

| Feature                | Gustavson classic    | PSRD-noise                               |
| ---------------------- | -------------------- | ---------------------------------------- |
| 2D / 3D scalar noise   | yes                  | 2D primary; 3D variant in upstream       |
| FBM convenience        | yes                  | not provided here (callers compose)      |
| Periodic tiling        | torus-mapped 2D only | per-axis configurable period             |
| Rotating gradients     | no                   | yes — animated noise without re-sampling |
| Analytical derivatives | no                   | yes — gradient returned alongside value  |

Returns both the noise value **and** the analytical gradient — the upstream WGSL
packs them. The DSL signature surfaces the joint return as a `Vec3` (`.x` =
noise, `.yz` = gradient) since the existing DSL has no struct-return primitive.
If/when struct returns land in `WgslFn`, the signature can flip to a named
struct without breaking call-sites that already destructure the `Vec3`.

```scala
/** 2D simplex noise with rotating gradients, periodic tiling, and
 *  analytical derivatives. Implementation based on psrdnoise by Stefan
 *  Gustavson and Ian McEwan — see https://github.com/stegu/psrdnoise/.
 *
 *  @param pos     point (x, y) to evaluate
 *  @param period  desired periods along x and y; set a component to 0 (or
 *                 negative) to skip wrapping along that axis. Setting both
 *                 to 0 makes the function ~15% faster (tiling logic
 *                 short-circuited)
 *  @param normRot normalised rotation for swirling gradients; 1.0 == TAU
 *                 (one full turn). Drives animation without re-sampling.
 *  @return        Vec3 packing (noise, grad.x, grad.y); noise is in
 *                 approx [-1, 1]
 */
val psrdNoise2d: WgslFn[
  (pos: Vec2, period: Vec2, normRot: Float),
  Vec3,
]
```

If the upstream WGSL also exposes the 3D variant (`psrdnoise3.wgsl` in the
repo), port it under the same shape:

```scala
val psrdNoise3d: WgslFn[
  (pos: Vec3, period: Vec3, alpha: Float),
  Vec4,  // noise + 3D gradient
]
```

Skip 3D for the initial cut if it's not immediately needed — the 2D variant is
the high-value case (tiling textures, animated swirls) and the 3D variant can be
added later as a purely additive change.

**Tests** (`test/shader/lib/SimplexFns.test.scala` covers both variants):

- Each `WgslFn.src` contains the expected attribution / licence comment block
  (§3.0 compliance).
- `psrdNoise2d.src` mentions `period` handling and the rotation step (grep for
  `period`, `norm_rot` / `normRot`, `cos`, `sin` in the body).
- Apply-site typing: `psrdNoise2d(posExpr, periodExpr, rotExpr).toString`
  composes correctly and the result is `Vec3Expr`.
- Performance-shape note: registering only `psrdNoise2d` (no fbm wrappers) emits
  exactly one function — confirms we did not accidentally inline a
  Gustavson-classic helper.

---

## 6. Phase 5 — Gaussian blur

> **Status: ⬜ Not started.** `examples/blur/Blur.scala` remains the working
> reference; consolidation into the library has not been done.

**Location:** `src/graphics/shader/lib/blur/blur.scala`.

Consolidate the existing `examples/blur/Blur.scala` (which already defines
`gaussian_blur_9`) into the library. Gaussian weights are public-domain; no
attribution needed beyond citing the standard separable-blur formulation.

- `val gaussianBlur5:  WgslFn[(tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, dir: Vec2), Vec4]`
- `val gaussianBlur9:  WgslFn[… same params], Vec4]`
- `val gaussianBlur13: WgslFn[… same params], Vec4]`
- `val gaussianBlur:   WgslFn[(tex: Texture2D, s: Sampler, diameter: Float, uv: Vec2, res: Vec2, dir: Vec2), Vec4]`

All parameters are float/Vec types — Phase 5 does **not** depend on Phase 1 and
can be scheduled independently. The existing `examples/blur/` code is a strong
reference point; re-using it wholesale is the cheapest path.

---

## 7. Phase 6 — Port the Rust `noise_tests` verification example

> **Status: ✅ Complete.** `examples/noise_tests/` is shipped and compiles. 16
> panels cycle on click, covering all hash and noise variants. Dev servers and
> the index page are updated. See the status table for panel-count deviation.

**Location:** `examples/noise_tests/` (new example dir, parallels the existing
`examples/blur`, `examples/painter_dsl`, etc.).

**Goal:** Definition-of-done capstone for the integer-DSL + helper-library work.
The Rust `trivalibs_painter` repo ships a `noise_tests` example that binds every
hash and noise function we are porting into seven fragment shaders the user
cycles through with mouse clicks. Porting it serves as the visual integration
test — if every layer renders correctly in the browser, all the underlying ports
(Phases 2 and 4) are working end-to-end. Source:
[rust-painter/repomix-painter.xml `noise_tests/main.rs`](rust-painter/repomix-painter.xml#L3160)
and [`noise_tests/shader/src/lib.rs`](rust-painter/repomix-painter.xml#L2928).

**Layers exercised** (one fragment shader each, cycled via pointer click):

| Rust shader fn           | Exercises                                                          |
| ------------------------ | ------------------------------------------------------------------ |
| `hash_shader`            | `hash`, `hashi`, `hash21`, `hash2d`, `hash3d` (all of §3.1)        |
| `simplex_2d_shader`      | `simplex_noise_2d` + time-driven scale animation                   |
| `simplex_3d_shader`      | `simplex_noise_3d` (z = time, animated)                            |
| `simplex_4d_shader`      | `simplex_noise_4d` (extra dim for richer animation)                |
| `tiling_simplex_shader`  | `tiling_simplex_noise_2d` (the torus-mapped 4D tiling helper §5.1) |
| `tiling_noise_2d_shader` | `tiling_rot_noise_2d` (psrdnoise 2D §5.2)                          |
| `tiling_noise_3d_shader` | `tiling_rot_noise_3d` (psrdnoise 3D, if §5.2's 3D variant lands)   |

Each shader takes the same uniforms (`size: UVec2`, `time: f32`),
aspect-corrects the UV, runs the helper, and tone-maps via `fit1101` +
`pow(GAMMA)` for display.

**Scala mapping:**

- Use the painter (`painter_dsl` is the closest existing reference) to set up
  the canvas, time uniform, and pointer-click cycle through layers.
- Each fragment shader becomes a `ShaderDef` (or `WgslFn` composed into a
  `ShaderDef`) calling the corresponding library helper.
- Pointer-click navigation: forward on primary, backward on secondary — mirror
  the Rust `event` handler shape at
  [repomix-painter.xml:3252-3261](rust-painter/repomix-painter.xml#L3252-L3261).
- Reuse `trivalibs` `aspect_preserving_uv` shape inline (or add to the shader
  lib if it ends up being used elsewhere).
- Display FPS counter (already wired in other examples).

**Comment / attribution preservation:** Per §3.0, carry over any attribution
comments or licence headers that travelled with the helper implementations
themselves. The shader bodies in the example can be DSL code (since the helpers
are already library functions); the example itself does not have its own
non-trivial licence concerns.

**Verification:** This phase is itself the verification — `bun run dev`, open
the example, click through all seven layers, confirm each one animates as
expected. No automated test suite for visual output; visual parity with the Rust
example is the success criterion.

**Sequencing:** Land after Phase 4 (both 4a and 4b) so all noise variants the
example depends on are available. Hash-only sub-layer (`hash_shader`) could land
earlier as a stub example after Phase 2 if useful for verifying the hash port in
isolation, but the full seven-layer example needs all of Phase 4 first.

---

## 8. Sequencing and risk

**Suggested order of work** once Phase 1 ships:

1. Phase 2 (`hash.rs`) — unblocks the most-requested helper. Small surface.
2. Phase 4 (simplex noise) — high-value visual feature. Depends on Phase 1 only
   for `octaves: Int`.
3. Phase 6 (`noise_tests` example port) — capstone verification of Phases 2 + 4.
   Land as soon as both predecessors ship; the visual cycle through all hash and
   noise variants is the definition-of-done check.
4. Phase 3 (`color.rs` + `coords.rs`) — small, independent files.
5. Phase 5 (blur) — lowest priority (existing `examples/blur` already works for
   callers that need it today).

**Risks and mitigations.**

- _Match-type expansion pitfalls._ `ToExpr` / `ToLocal` have a history of subtle
  failures with type aliases (per `CLAUDE.md` "Critical Type-Level Patterns").
  Test explicitly with `type Seed = UInt` and `WgslFn[(s: Seed), Seed]` to catch
  this early.
- _Operator surface._ Bitwise ops are exposed as Scala operators (`|`, `&`, `^`,
  `<<`, `>>`, `~`) on `IntExpr` / `UIntExpr`, with named-method aliases (`or`,
  `and`, `xor`, `shl`, `shr`, `not`) for dense bit-twiddling code. `Int`-side
  overloads (e.g. `seed >> 16`) let compile-time constants flow into runtime
  expressions; they live on the expression types only, never on global `Int`, so
  `0xff & seed` (Int on the left) does not compile — expression operand goes
  left.
- _Literal ambiguity / deferred decision._ `Conversion[Int, FloatExpr]` is
  preserved (see §2.3.1); we deliberately do **not** add a parallel
  `Conversion[Int, IntExpr]` because it would be ambiguous at unconstrained
  `Expr` call-sites. Integer literals in DSL int code use the `.i` / `.u`
  extensions (`42.i`, `0x21f0aaad.u`); the `IntExpr(...)` / `UIntExpr(...)`
  constructors are reserved for symbol-named expressions. Re-evaluate after
  Phases 2 + 4 — if `.i` shows up everywhere, flip the conversion in a
  follow-up. Both `IntExpr` and `FloatExpr` are distinct opaque types, so the
  compiler will flag every affected call-site if/when we flip.
- _Bundle size._ All new types are `opaque` and all new extensions are `inline`.
  Expected runtime cost: zero beyond the existing `Expr` wrapper. Verify via the
  existing JS-size checks if adding these grows the bundle noticeably.

---

## 9. Out of scope (reiterated)

- CPU-side integer Vec/Mat math (`IVec*`, `UVec*`, `IMat*`, `UMat*`) — JS
  numbers are doubles, no meaningful CPU representation, no use case. See §1.2.
- CPU ↔ GPU integer buffer bindings (vertex attributes, uniform buffers with
  integer fields) — out of scope for this plan, additive future work. The
  `StructArray` / `StructRef` binary layer already supports `Int` / `UInt`; the
  missing piece is the binding API, which would accept the existing double-based
  `Vec*` / `Mat*` types on input. See §1.2.
- Adding `Conversion[Int, IntExpr]` — deliberately deferred (§1.2, §2.3.1).
  Existing `Conversion[Int, FloatExpr]` is preserved; integer literals use `.i`
  / `.u` extensions or explicit constructors.
- Integer matrices (`IMat*`, `UMat*`) — not supported by WGSL, not needed.
- Rust `trivalibs_core/src/data/{grid,neighbour_list,vertex_index}` — CPU data
  structures, outside `trivalibs_nostd` scope.
- Parity closure for `num_ext` / `vec_ext` — tracked separately in
  [math-library-design.md](math-library-design.md).
