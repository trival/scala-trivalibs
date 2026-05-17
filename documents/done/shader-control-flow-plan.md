# Shader DSL Control Flow Plan

## Status: Implemented (2026-05-06)

All planned items shipped, with two minor naming/shape deviations:

- **`when` / `ifElse` are comma-form, not curried** —
  `when(cond, body)` / `ifElse(cond, thenBody, elseBody)`
  ([expr.scala:881-885](../src/graphics/math/gpu/expr.scala#L881-L885)).
  The `Conversion[Stmt, Block]` already lets a single statement pass without
  `Block(...)` wrapping, so currying for brace syntax wasn't needed.
- **`BoolExpr.then` → `BoolExpr.thenDo`** — `then` is a Scala reserved soft
  keyword in some positions; `thenDo` avoids the clash
  ([expr.scala:896](../src/graphics/math/gpu/expr.scala#L896)).

Shipped:

- BoolExpr-returning `<`, `<=`, `>`, `>=`, `===`, `!==` on `FloatExpr`,
  `IntExpr`, `UIntExpr`
  ([expr.scala:366-376, 993-1017](../src/graphics/math/gpu/expr.scala#L366-L376)).
- `&&`, `||`, `unary_!` on `BoolExpr`
  ([expr.scala:903-909](../src/graphics/math/gpu/expr.scala#L903-L909)).
- Both `select` forms — function (WGSL order) and `BoolExpr.select` extension
  (flipped) ([expr.scala:875-892](../src/graphics/math/gpu/expr.scala#L875-L892)).
- `Stmt.ifBlock` / `Stmt.ifElseBlock` with re-indented bodies
  ([expr.scala:851-854](../src/graphics/math/gpu/expr.scala#L851-L854)).
- Top-level `when` / `ifElse`, plus `BoolExpr.thenDo` / `BoolExpr.thenElse`
  ([expr.scala:881-900](../src/graphics/math/gpu/expr.scala#L881-L900)).
- Tests covering each operator and statement form, including nesting/indent
  ([test/shader/ControlFlow.test.scala](../test/shader/ControlFlow.test.scala)).
- `hashDisplay` rewritten as DSL using nested `ifElse` + `ifChain`
  ([examples/noise_tests/NoiseTests.scala:23-68](../examples/noise_tests/NoiseTests.scala#L23-L68)).
- **`elseIf` chain builder** — `ifChain(c, b).elseIf(c, b)...elseDo(b)`
  ([expr.scala](../src/graphics/math/gpu/expr.scala)). `elseDo` (not `orElse`)
  to avoid clashing with `trivalibs.utils.js.Opt.orElse`. `IfChain` is an
  opaque String alias with implicit conversions to `Stmt` and `Block`, so a
  chain without a final else can be used directly as a statement.

Deferred items from "Out of Scope" remain deferred (loops, numeric `&&`/`||`/`!`,
numeric `eq`/`ne`, vector bool ops, `switch`).

## Context

The `hash_display` function in
[examples/noise_tests/NoiseTests.scala:20-69](../examples/noise_tests/NoiseTests.scala#L20-L69)
is currently written as a raw WGSL string (`WgslFn.raw`). To express it via the
Scala DSL we need control flow — nested `if`/`else if`/`else` chains gated on
`==` and `<`/`>` comparisons of UInts and Floats — none of which the DSL
currently supports.

## Current State of the DSL

Verified in
[src/graphics/math/gpu/expr.scala](../src/graphics/math/gpu/expr.scala):

- **`BoolExpr` exists as an opaque type**
  ([line 67](../src/graphics/math/gpu/expr.scala#L67)) but has **zero
  operators** — no comparisons produce it, no `&&`/`||`/`!`. Only used as the
  result type for params/locals declared as `Boolean`.
- **No control-flow statements**: no `if`/`else`, `when`, `for`, `while`,
  `loop`, `break`, `continue`. Only `ret`
  ([dsl/fn.scala:387](../src/graphics/shader/dsl/fn.scala#L387)) emits `return`.
- **`Stmt` and `Block` are opaque `String` types**
  ([expr.scala:802-825](../src/graphics/math/gpu/expr.scala#L802-L825)), so
  adding control-flow constructors is purely additive — no AST refactor needed.
- **Existing `gt`/`lt`/`lte`/`gte` on numerics**
  ([lines 343-354, 873-880, 892-899](../src/graphics/math/gpu/expr.scala#L343-L354))
  return _numeric_ `0`/`1` via `select(0,1,a<b)` or `step(...)`. Useful as
  branchless step helpers, **not** comparison operators. They stay as-is.
- **No `select(...)` helper at the DSL level** even though raw WGSL examples
  (e.g. [simplex.scala:44](../src/graphics/shader/lib/random/simplex.scala#L44),
  [psrdnoise.scala:52](../src/graphics/shader/lib/random/psrdnoise.scala#L52))
  use it heavily.

## Approach: Full BoolExpr

`if`/`else` conditions in WGSL must be `bool`. We thread proper `BoolExpr`
through everywhere — comparisons return `BoolExpr`, branching consumes
`BoolExpr`. No numeric truthiness layer (would only matter if we built numeric
control flow, which we're not).

Numeric `&&` / `||` / `!` overloads are not added — they only make sense in a
numeric-control-flow world.

Numeric `eq` / `ne` (returning 0/1 like the existing `gt`/`lt`) are deferred —
potentially useful for branchless shader algorithms but no concrete need today.

## Proposed Additions

### 1. Comparison and equality operators returning `BoolExpr`

On `FloatExpr`, `IntExpr`, `UIntExpr` (Vec componentwise variants deferred until
needed):

```scala
extension (a: FloatExpr)
  def <   (b: FloatExpr): BoolExpr
  def <=  (b: FloatExpr): BoolExpr
  def >   (b: FloatExpr): BoolExpr
  def >=  (b: FloatExpr): BoolExpr
  def === (b: FloatExpr): BoolExpr
  def !== (b: FloatExpr): BoolExpr
```

`===` / `!==` because Scala's `==` is universal `Any` equality and cannot be
cleanly overridden on opaque types (cats convention, unambiguous).

Convention then becomes: **symbolic operator = boolean, named method = numeric
step helper**. If we later add numeric `eq` / `ne` step helpers (currently
deferred), they live in the named-method namespace and don't collide.

### 2. Boolean combinators on `BoolExpr`

```scala
extension (a: BoolExpr)
  def && (b: BoolExpr): BoolExpr  // (a && b)
  def || (b: BoolExpr): BoolExpr  // (a || b)
  def unary_! : BoolExpr          // !(a)
```

### 3. `select` — two forms

Function form, matching WGSL signature exactly (`falseValue`, `trueValue`,
`condition`):

```scala
def select[T <: Expr](onFalse: T, onTrue: T, cond: BoolExpr): T
```

Extension form on `BoolExpr` for the codebase's preferred style:

```scala
extension (cond: BoolExpr)
  def select[T <: Expr](onTrue: T, onFalse: T): T
```

Note the deliberate argument flip in the extension —
`cond.select(onTrue, onFalse)` reads naturally ("if cond pick onTrue else
onFalse"), while the function form preserves the WGSL spec order so it can sit
next to other WGSL-mirrored helpers without surprise.

Replaces raw `select(...)` in existing noise libs over time.

### 4. Control-flow `Stmt` constructors

```scala
object Stmt:
  def ifBlock(cond: BoolExpr, thenBody: Block): Stmt
  def ifElseBlock(cond: BoolExpr, thenBody: Block, elseBody: Block): Stmt
```

Generated WGSL: `if (cond) {\n<indented body>\n}` — nested blocks need each line
re-indented by 2 spaces. This is the only mildly fiddly part of the
implementation.

### 5. Top-level DSL helpers + BoolExpr extensions

Top-level (curried for nice brace syntax):

```scala
def when  (cond: BoolExpr)(thenBody: Block): Stmt
def ifElse(cond: BoolExpr)(thenBody: Block)(elseBody: Block): Stmt
```

Extension form on `BoolExpr` (for the codebase's preferred style):

```scala
extension (cond: BoolExpr)
  def `then`    (thenBody: Block): Stmt              // = when(cond)(thenBody)
  def thenElse  (thenBody: Block, elseBody: Block): Stmt
```

Use whichever reads better at the call site. Note that
`given Conversion[Stmt, Block]` already exists
([expr.scala:820](../src/graphics/math/gpu/expr.scala#L820)), so a single
statement (including a nested `ifElse` / `thenElse`, since they return `Stmt`)
is automatically a `Block` — no wrapping needed when nesting directly.
`Block(...)` is only needed when joining multiple statements.

```scala
(qi.y === 0u).thenElse(
  (qi.x === 0u).thenElse(
    ret(...),
    ret(...),
  ),
  Block(
    someAssign,
    ret(...),
  ),
)
```

### 6. Else-if ergonomics

For deep `else if` chains like `hash_display`, two viable forms:

- Nest `ifElse` / `thenElse` in the else-block (works out of the box, indents
  heavily).
- A chained `elseIf` builder — defer until we have more examples.

## Files to Modify

- [src/graphics/math/gpu/expr.scala](../src/graphics/math/gpu/expr.scala) —
  BoolExpr-returning comparison/equality on `FloatExpr`/`IntExpr`/`UIntExpr`;
  `&&`/`||`/`unary_!` on `BoolExpr`; both `select` forms;
  `Stmt.ifBlock`/`Stmt.ifElseBlock`; `then`/`thenElse` extensions on `BoolExpr`.
- New file (or addition to existing dsl package) — top-level `when` / `ifElse`
  helpers.
- New tests in [test/shader/](../test/shader/) — verify generated WGSL strings
  for each operator and statement form, including nesting/indentation.
- Optional proof-of-concept: rewrite `hashDisplay` in
  [examples/noise_tests/NoiseTests.scala](../examples/noise_tests/NoiseTests.scala)
  using the new DSL.

## Out of Scope (this iteration)

- `for`, `while`, `loop`, `break`, `continue` — `hash_display` doesn't need
  them.
- Numeric `&&`/`||`/`!` overloads — would only matter under numeric control
  flow.
- Numeric `eq`/`ne` step helpers — defer until a branchless algorithm needs
  them.
- Componentwise vector bool ops (`vec2<bool>`, `all`, `any`) — defer until
  needed.
- Renaming the existing numeric `gt`/`lt`/`lte`/`gte` step helpers —
  load-bearing in shader libs.
- `switch` statement — `else if` chains cover this case.

## Verification

1. `bun run build` compiles cleanly.
2. New unit tests in `test/shader/` check exact generated WGSL strings for each
   operator and each statement form (including nested if/else indentation).
3. Rewrite `hashDisplay` in the DSL and run the noise_tests example in the
   browser (`bun run dev`) — visually identical to the current raw-WGSL output.
