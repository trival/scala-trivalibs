# Typed shader assignment

Make `:=` on shader locals and outputs type-directed, so a mismatched
assignment is a Scala compile error instead of a WGSL one.

## Status: Implemented (2026-09-19)

Shipped in `da8d9e0`. Everything the plan set out to do is in, with three
deviations, all recorded below:

- **The compound operators were dropped from scope.** The plan originally gave
  `+= -= *= /=` "the same treatment"; they keep their untyped `Expr` signatures.
  See [Out of scope](#out-of-scope-the-compound-operators) for why, and for the
  design sketch if it is ever picked up.
- **`AssignTarget[E]` carries no `<: Expr` bound** — `ToExpr[T]` cannot be
  reduced for an abstract field type, so the bound does not hold generically.
- **No identity `Lift` instance was needed.** The plan assumed one; the
  fast-path overload accepts subtypes by ordinary conformance, so only the 12
  CPU-value instances exist.

Landed in:

- [`math/gpu/expr.scala:51`](../../src/graphics/math/gpu/expr.scala#L51) — `Lift`
- [`expr.scala:61`](../../src/graphics/math/gpu/expr.scala#L61),
  [`:74`](../../src/graphics/math/gpu/expr.scala#L74),
  [`:121`](../../src/graphics/math/gpu/expr.scala#L121) — `LetExpr[T]` /
  `VarExpr[T]` / `ConstExpr[T]`, plus the 48 opaque local types and the 12
  `Lift` instances in the element companions
- [`shader/dsl/context.scala:70`](../../src/graphics/shader/dsl/context.scala#L70)
  — `AssignTarget[E]`
- [`shader/dsl/types.scala:62`](../../src/graphics/shader/dsl/types.scala#L62) —
  `ToAssign[T] = AssignTarget[ToExpr[T]]`
- [`docs/guide/shader-dsl-guide.md`](../../docs/guide/shader-dsl-guide.md) — the
  assignment, compound-op and numeric-literal sections

## The problem

`:=` was defined on the untyped classes `LetExpr` / `VarExpr` / `ConstExpr`.
Its signature was `:=(value: Expr)` plus a fixed set of CPU-literal overloads.
Nothing connected the receiver's element type to the right-hand side, so all of
this compiled and emitted invalid WGSL:

```scala
val seg = VarInt("seg")
seg := 0                  // var seg = f32(0);   — Int means f32 everywhere
letFloat := someVec3Expr  // let t = v;
ctx.out.color := 0.5      // out.color = 0.5;    — AssignTarget was untyped too
```

The first line is the one that cost real debugging time. `:=(value: Int)`
routed through `Conversion[Int, FloatExpr]`, declaring an f32 local behind a
statically-`IntExpr` name. Every *later* use type-checked — `stops(seg)` wants
an `IntExpr` and `seg` is one — so the Scala types stayed green while the
emitted WGSL was inconsistent. The browser reported the first conflict:

```
:33:21 error: cannot assign 'i32' to 'f32'
    gradientSegment = i;
```

which blames the loop body, not the declaration that fixed the type.

Everything downstream of the declaration was already precisely typed:
`ArrayExpr.apply(i: IntExpr)`, `WgslFn` calls returning `ToExpr[R]`,
`ctx.in.*` via `Fields`. One untyped assignment poisoned an otherwise sound
chain — which is why the fix was narrow.

## Design

The local classes are parameterized by their element type, and CPU values cross
through a lift type class instead of per-type overloads.

```scala
@implicitNotFound("Cannot assign a ${C} to a shader local of type ${E}.")
trait Lift[C, E]:
  def lift(c: C): E

class LetExpr[T <: Expr](val name: String) extends Expr(name):
  def :=(value: T): Stmt = Stmt.let(name, value)                       // fast path
  def :=[C](value: C)(using l: Lift[C, T]): Stmt = this := l.lift(value)
```

Each `Let*` / `Var*` / `Const*` opaque type names its element type:

```scala
opaque type VarInt <: IntExpr & VarExpr[IntExpr] = VarExpr[IntExpr]
```

`Lift` instances live in the *element type's* companion (`object IntExpr`,
`object FloatExpr`, …). Inside `object Expr` all opaque types are transparent,
so instances sharing one owner would collide; in separate companions they don't,
and at a use site — where the types are distinct again — implicit search reaches
exactly the right one through the target type's implicit scope.

### Why the two-alternative overload set

The first alternative takes `T` directly: no implicit, no allocation, and it
accepts subtypes (`LetFloat` into a `FloatExpr` slot) by ordinary conformance.
That covers nearly every call site and erases to what `:=` compiled to before.
It is also why no identity `Lift` instance is needed — the generic alternative
is only reached when the argument is *not* already a `T`.

The second carries CPU values (`Vec3`, `Double`, `Int`) for which no
`Conversion` exists or should exist — `cpu_interop.scala` deliberately refuses
`Conversion[Vec3, Vec3Expr]` because it makes every GPU extension applicable to
CPU values. `Lift` gives assignment the crossing without opening that door.

An overload set blocks implicit conversions, which is what the old
`:=(Double)` / `:=(Int)` overloads existed to work around. `Lift` subsumes them:
`Lift[Double, FloatExpr]`, `Lift[Int, FloatExpr]`, `Lift[Int, IntExpr]`, and the
vector / matrix / bool / uint cases.

### Literals are target-directed

The same literal resolves by the slot it lands in:

```scala
varInt   := 0    // var seg = 0;
varFloat := 0    // var f = f32(0);
```

`Conversion[Int, FloatExpr]` remains the default everywhere else — function
arguments, mixed arithmetic. The change is scoped to assignment, where a target
type exists to direct it. `.i` is not needed at declarations.

### `AssignTarget[E]` is unbounded

`ToAssign[T] = AssignTarget[ToExpr[T]]` reuses the existing `ToExpr` match type
to close the `ctx.out.color := 0.5` hole. `E` carries **no `<: Expr` bound**:
`ToExpr[T]` ends in `case _ => T`, which the compiler cannot reduce for an
abstract field type, so the bound does not hold generically. `:=` casts to
`Expr` for emission instead. The DSL has no non-`Expr` values, so the cast is
total in practice.

## Migration cost

Call sites did not change. ~570 local assignments across sketches, examples and
tests recompiled as written. Two kinds of site needed an annotation, both of
them places that named the previously-unparameterized types directly:

- `AssignTarget` as a **parameter type** — `src/utils/bake/Bake.scala` in the
  consuming repo, and the `noise_tests` example.
- **Direct construction** of `AssignTarget(…)` / `new LetExpr(…)` in
  `ShaderDsl.test.scala` and `CpuVecInterop.test.scala`.

Separately, assigning a bare `Expr.raw(…)` into a typed local is an error. A
few test sites did this; they moved to the typed constructor (`Vec3Expr(…)`),
which emits identical WGSL.

One test encoded the old behavior and was rewritten rather than repaired:
`CpuVecInterop` asserted that a single output slot accepted a `Vec4`, a `Double`
and a `Vec3`. That was the untyped hole under test; it now uses three
correctly-typed slots.

## Out of scope: the compound operators

`+= -= *= /=` keep their untyped `Expr` signatures, deliberately:

- A compound op cannot **declare** a local, so it can never fix a wrong element
  type behind a correctly-typed name — the failure mode that made the original
  bug expensive. A mismatch fails in WGSL at the line as written.
- They are legitimately mixed-type. WGSL broadcasts a scalar across `+ - * /`
  for vectors (`vec3 *= f32`), and across `* /` but *not* `+ -` for matrices.

Typing them therefore needs per-operator operand rules — an `Operand[Op, C, T]`
shaped type class rather than `Lift`, where `Op` is a phantom tag distinguishing
the additive from the multiplicative case, and the matrix scalar-broadcast
instance exists only for the latter. That is a larger design than this change,
and a standing idea rather than committed work.

## Non-goals

- **No runtime shader-error reporting.** Deliberate: the target is a live-coding
  environment where shader source changes constantly. A failed compile must not
  throw or tear down the scene — a black frame is an acceptable outcome, and the
  browser already logs the WGSL error next to the `log(wgsl)` dump. Editor-time
  errors are the channel to improve.
- Raw WGSL (`WgslFn.raw` bodies, `Expr.raw`) stays unchecked by construction.

## Verification

- `bun run check` — clean.
- `bun run test` — 37 suites green, including five new `IntDslTest` cases
  asserting the emitted WGSL for target-directed literals:

  ```
  VarInt("seg")  := 0    →  var seg = 0;
  VarFloat("f")  := 0    →  var f = f32(0);
  VarUInt("n")   := 3    →  var n = 3u;
  LetFloat("t")  := 0.5  →  let t = 0.5;
  ```

- `bun run examples:build` — every example compiles.
- Downstream `bun run sketches` — all 21 sketches build.

Compile-fail cases are the point of the change, so they are checked by hand
rather than in the suite (munit cannot assert a non-compile):

```
Cannot assign a Double to a shader local of type IntExpr.
Cannot assign a FloatExpr to a shader local of type IntExpr.
Cannot assign a Double to a shader local of type Vec3Expr.
```
