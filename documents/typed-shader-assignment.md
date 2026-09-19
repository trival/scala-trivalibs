# Typed shader assignment

Make `:=` on shader locals and outputs type-directed, so a mismatched
assignment is a Scala compile error instead of a WGSL one.

## The problem

`:=` is defined on the untyped classes `LetExpr` / `VarExpr` / `ConstExpr`
(`math/gpu/expr.scala`). Its signature is `:=(value: Expr)` plus a fixed set of
CPU-literal overloads. Nothing connects the receiver's element type to the
right-hand side, so all of this compiles and emits invalid WGSL:

```scala
val seg = VarInt("seg")
seg := 0                  // var seg = f32(0);   — Int means f32 everywhere
letFloat := someVec3Expr  // let t = v;
ctx.out.color := 0.5      // out.color = 0.5;    — AssignTarget is untyped too
```

The first line is the one that cost real debugging time. `:=(value: Int)`
routes through `Conversion[Int, FloatExpr]`, declaring an f32 local behind a
statically-`IntExpr` name. Every *later* use type-checks — `stops(seg)` wants an
`IntExpr` and `seg` is one — so the Scala types stay green while the emitted
WGSL is inconsistent. The browser reports the first conflict:

```
:33:21 error: cannot assign 'i32' to 'f32'
    gradientSegment = i;
```

which blames the loop body, not the declaration that fixed the type.

Everything downstream of the declaration was already precisely typed:
`ArrayExpr.apply(i: IntExpr)`, `WgslFn` calls returning `ToExpr[R]`,
`ctx.in.*` via `Fields`. One untyped assignment poisons an otherwise sound
chain — which is why the fix is narrow.

## Design

Parameterize the local classes by their element type, and route CPU values
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

`Lift` givens live in the *element type's* companion (`object IntExpr`,
`object FloatExpr`, …). Inside `object Expr` all opaque types are transparent,
so the givens would collide if they shared an owner; in separate companions
they don't, and at a use site — where the types are distinct again — implicit
search reaches exactly the right one through the target type's implicit scope.

### Why the two-alternative overload set

The first alternative takes `T` directly: no implicit, no allocation, and it
accepts subtypes (`LetFloat` into a `FloatExpr` slot) by ordinary conformance.
That covers nearly every call site and erases to what `:=` compiles to today.

The second carries CPU values (`Vec3`, `Double`, `Int`) for which no
`Conversion` exists or should exist — `cpu_interop.scala` deliberately refuses
`Conversion[Vec3, Vec3Expr]` because it makes every GPU extension applicable to
CPU values. `Lift` gives assignment the crossing without opening that door.

Note that an overload set blocks implicit conversions, which is what the
current `:=(Double)` / `:=(Int)` overloads exist to work around. `Lift`
subsumes them: `Lift[Double, FloatExpr]`, `Lift[Int, FloatExpr]`,
`Lift[Int, IntExpr]`.

### Literals become target-directed

The same literal resolves by the slot it lands in:

```scala
varInt   := 0    // var seg = 0;
varFloat := 0    // var f = f32(0);
```

`Conversion[Int, FloatExpr]` stays the default everywhere else — function
arguments, mixed arithmetic. This change is scoped to assignment, where a
target type exists to direct it. `.i` stops being needed at declarations.

## Scope — as implemented

- `LetExpr` / `VarExpr` / `ConstExpr` gained a type parameter. They appear as a
  parameter or return type nowhere outside `expr.scala`, so this was contained.
- 48 `opaque type Let* / Var* / Const*` definitions name their element type.
- `AssignTarget` became `AssignTarget[E]`, with
  `ToAssign[T] = AssignTarget[ToExpr[T]]` (`shader/dsl/types.scala`) — this is
  what closes the `ctx.out.color := 0.5` hole, reusing the existing `ToExpr`
  match type.

  `E` carries **no `<: Expr` bound**. `ToExpr[T]` ends in `case _ => T`, which
  the compiler cannot reduce for an abstract field type, so the bound does not
  hold generically; `:=` casts to `Expr` for emission instead. The DSL has no
  non-`Expr` values, so the cast is total in practice.

- Call sites did not change. ~570 local assignments across sketches, examples
  and tests recompiled as written. Four kinds of site needed an annotation, all
  of them places that named the previously-unparameterized types directly:
  `AssignTarget` as a *parameter type* (`Bake.scala`, the `noise_tests`
  example), and direct `new LetExpr(…)` / `AssignTarget(…)` construction in
  tests.
- Assigning a bare `Expr.raw(…)` into a typed local is now an error. The few
  test sites doing this switched to the typed constructor (`Vec3Expr(…)`),
  which emits identical WGSL.

## Deferred: the compound operators

`+= -= *= /=` keep their untyped `Expr` signatures for now, deliberately:

- A compound op cannot **declare** a local, so it can never fix a wrong element
  type behind a correctly-typed name — the failure mode that made the original
  bug expensive. A mismatch fails in WGSL at the line as written.
- They are legitimately mixed-type. WGSL broadcasts a scalar across `+ - * /`
  for vectors (`vec3 *= f32`), and across `* /` but *not* `+ -` for matrices.
  Typing them needs per-operator operand rules — an `Operand[Op, C, T]` shaped
  type class rather than `Lift` — which is a larger design than this change.

## Non-goals

- **No runtime shader-error reporting.** Deliberate: the target is a live-coding
  environment where shader source changes constantly. A failed compile must not
  throw or tear down the scene — a black frame is an acceptable outcome, and the
  browser already logs the WGSL error next to the `log(wgsl)` dump. Editor-time
  errors are the channel we want to improve.
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
