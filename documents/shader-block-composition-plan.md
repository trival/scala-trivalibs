# Shader DSL: Nested Blocks

## Status: Implemented (2026-09-13)

Shipped as planned, with one discovery and one deviation:

- **`Stmt`'s opacity was leaking.** Before the change, `stmt: String` compiled
  from outside the package; giving `Stmt` the `<: Block` bound closes that, so
  the emitted WGSL is now reachable only via `Block.unwrap`. The shader test
  suites compared it as text in 44 places — they get a test-scope
  `Conversion[Block, String]`
  ([test/shader/BlockText.test.scala](../test/shader/BlockText.test.scala))
  rather than 44 rewrites.
- **Milestone 5 (example) dropped.** The two consumer-repo sketches were ported
  instead and cover the same ground. Converting
  `examples/uniform_array_gradient` was tried and reverted: splitting its body
  into a `Block`-returning helper plus a named `gradient` group scattered the
  `col` accumulator — declaration, first assignment, per-step mixing and final
  use ended up in four places, where the flat body reads as one linear pass.
  Nesting has to earn itself; a body this short is the wrong demo for it.

Landed: the subtype chain, `Block.apply` over `Block*` / `Arr[Block]` with
empty-part filtering, the three conversions removed, `scope`, `unroll`
returning `Block` plus a no-index `Arr` overload, 15 new tests in
[test/shader/Blocks.test.scala](../test/shader/Blocks.test.scala), and the
guide + gotchas updates.

Two small changes that make a statement group a first-class value, so the
`Arr[Stmt]` accumulator pattern is never the shape of a shader body:

1. **`Block` accepts `Block`s** — nest arbitrarily deep, flattened into one
   WGSL statement list (`Stmt <: Block` subtyping).
2. **`scope`** — the same, but emitting a real WGSL `{ … }` scope.

`unroll` stays as it is (returning `Block` instead of `Stmt`). Replacing it
with `for … yield` was investigated and rejected — see the section below.

## Context

Two ported texture sketches in the consumer repo build their fragment bodies by
pushing into a mutable statement array:

```scala
val stmts = Arr[Stmt]()
stmts += (uv := Uv.aspectPreserving(ctx.in.uv, ctx.bindings.res))
stmts += (uvScaled := uv * NumTiles)
…
for i <- 0 until 4 do
  stmts += (uvs(i) := (uvTile - dirs(i)) * (1.0 - tiles(i).y * 0.14))
…
Block(stmts)
```

Most of that was avoidable already: `unroll(…)` returns `Stmt`, `Block` takes
`Stmt*`, so an unrolled group can sit directly in the argument list (verified —
see "The callback form already composes" below). What is left is the part that
genuinely does not work:

- **Whether a group composes depends on its _inferred type_.** `unroll(…)`
  infers `Stmt` and slots into `Block(…)` fine. Factor the same statements into
  a helper and it infers `Block` — and stops compiling
  (`Found: Block, Required: Stmt`). Same code, same emitted WGSL, different
  type, arbitrary outcome. That trap is what pushes a body onto the accumulator
  array, where everything is `Stmt` and the question never arises.
- **Emission order becomes a side effect, not syntax.** Once the array is
  there, a helper like `emitTile` both pushes statements and returns a handle;
  where its statements land depends on when it is _called_, not on where it
  appears. Reordering two `val`s reorders the shader.
- **Names have to carry the index.** All unrolled iterations share the
  enclosing WGSL scope, so locals need `s"cur$i"` / a `swapId` counter.

The fix is not a bigger builder API, and not a new iteration construct. It is
making a statement group one type that composes with itself.

## Current State

Verified in
[src/graphics/math/gpu/expr.scala](../src/graphics/math/gpu/expr.scala):

- `Stmt` and `Block` are **two unrelated opaque `String` types**
  ([expr.scala:537-543](../src/graphics/math/gpu/expr.scala#L537-L543)), bridged
  one way by `given Conversion[Stmt, Block]`
  ([expr.scala:569](../src/graphics/math/gpu/expr.scala#L569)).
- `Block.apply(stmts: Stmt*)` joins with `"\n"`; `Block.apply(stmts: Arr[Stmt])`
  is the js-native twin
  ([expr.scala:571-581](../src/graphics/math/gpu/expr.scala#L571-L581)). Both
  accept a `Stmt` — so `unroll(…)`, `when(…)` and `:=` all compose as arguments
  — but neither accepts a `Block`, so a group that is _named_ `Block` cannot.
- Nesting/indentation is **already solved**: every `Stmt` string carries its own
  two-space indent, and `indentBlock`
  ([expr.scala:589-601](../src/graphics/math/gpu/expr.scala#L589-L601))
  re-indents a whole body line by line. Concatenating two `Block`s is therefore
  already well-formed WGSL — only the _types_ forbid it.
- `IfChain` is a third opaque `String` with two more conversions to `Stmt` and
  `Block` ([expr.scala:781-796](../src/graphics/math/gpu/expr.scala#L781-L796)).
- `unroll` exists in five spellings — `unroll(count)`, `unroll(from, until)`,
  `unroll(xs)`, `Int.unroll`, `Arr.unroll`
  ([expr.scala:752-830](../src/graphics/math/gpu/expr.scala#L752-L830)).
  **No call sites outside tests** yet
  ([test/shader/Loops.test.scala](../test/shader/Loops.test.scala),
  [test/shader/ControlFlow.test.scala](../test/shader/ControlFlow.test.scala)),
  so their signatures are still free to change.
- `loop` / `loopIf` / `break` / `continue` emit real WGSL loops and are
  **unaffected** by everything below.

## Feature 1 — `Block` accepts `Block`s

### Design

Make the statement types a subtype chain instead of a conversion mesh:

```scala
opaque type Block = String
opaque type Stmt <: Block = String
opaque type IfChain <: Stmt = String
```

All three are `String` inside `expr.scala`, so the bounds check trivially.
Outside, every `Stmt` **is** a `Block`, and `Block.apply` widens to:

```scala
object Block:
  def apply(parts: Block*): Block          // was: Stmt*
  def apply(parts: Arr[Block]): Block      // was: Arr[Stmt]
  val empty: Block = ""
```

Flattening is free — a `Block` is the newline-joined text of its parts, and a
part that is itself a joined group concatenates as-is. No AST, no traversal.

### Empty parts

`apply` must **skip empty parts** rather than `mkString`-ing them, or every
`Block.empty` leaves a blank line in the WGSL. With that in place,
`Block.empty` becomes the useful "emit nothing" element that build-time
conditionals need:

```scala
unroll(taps): (tap, _) =>
  if tap.weight > 0.0 then col += sample(tap) else Block.empty
```

### What it deletes

- `given Conversion[Stmt, Block]`
- `given Conversion[IfChain, Stmt]`
- `given Conversion[IfChain, Block]`

Three implicit conversions replaced by two subtype bounds. `Stmt` stays as a
distinct name because it is genuinely useful documentation — `:=` returns
exactly one statement, and `Stmt.raw` / `Stmt.assign` keep their meaning.

### What it fixes, concretely

```scala
def group: Block = Block(x := 2.0, x := 3.0)
Block(x := 0.0, group)   // today: Found Block, Required Stmt
```

Every group-producing construct — `:=`, `when`, `loop`, `loopIf`, `unroll`,
`scope`, and any helper a sketch factors out — ends up assignable to the same
type, and a body is a tree of them instead of a list built by side effect.
`unroll` changes its return type from `Stmt` to `Block` in the process, since
a group of statements is what it is.

## Feature 2 — `scope`

```scala
def scope(body: => Block): Stmt
```

Lowercase, because the DSL's casing rule is Scala's own: **an uppercase name is
a type** (`Block`, `Stmt`, `Vec3Expr`, `LetVec2`, `WgslFn`, all written in
signatures), **a lowercase name is a term-level operation** (`when`, `loop`,
`select`, `break`, `ret`, plus `vec2/3/4` and `ivec*`/`uvec*`, which are
lowercase to mirror WGSL's own spelling). `scope` names no type, so it belongs
with `when` / `loopIf` / `loop`. `ScopedBlock` would be the only uppercase name
in the DSL that is not a type.

Emits a WGSL compound statement:

```wgsl
  {
    let av = …;
    l0v = av;
  }
```

### Validity — verified, not assumed

The whole feature rests on a bare brace block being legal in ordinary statement
position. It is. The WGSL grammar lists `compound_statement : '{' statement* '}'`
as an alternative of the `statement` rule, so it is not restricted to
`if` / `loop` / `for` bodies. Checked with `naga` (wgpu's WGSL frontend), which
reports `Validation successful` for a shader that, inside one function body:

- opens a bare `{ … }` at the top level of the body;
- declares `let av` / `let ah` in **two sibling blocks** — no redeclaration
  error, which is exactly what removes the index-suffixed names;
- shadows an outer `var col` from an inner block;
- assigns outer `var`s from inside a block.

And the negative control fails as it must — reading a block-local identifier
after the block is `error: no definition in scope for identifier: 'av'`. So the
scope is real, not merely tolerated. (Tint, Chrome's compiler, was not
available to test here; the grammar rule is spec-level, so this is not an
implementation liberty.)

The body is **by-name and in its own parameter list**, matching `when` /
`loopIf` / `loop` — so it is also a Scala scope, and the locals belonging to
that iteration are declared inside it:

```scala
unroll(sortPairs): (pair, _) =>
  val (a, b) = pair
  scope:
    val av = LetVec4("av")
    val ah = LetFloat("ah")
    Block(
      av := (lineH(a) > lineH(b)).select(lineV(b), lineV(a)),
      ah := (lineH(a) > lineH(b)).select(lineH(b), lineH(a)),
      lineV(a) := av,
      lineH(a) := ah,
    )
```

This is what removes index-suffixed local names from unrolled code: each
iteration gets its own WGSL scope, so plain `LetVec4("av")` is correct in all
of them. The accumulator rule is unchanged and now uniform across `loop`,
`scope` and `when` — a `var` read after the block must be first assigned
_outside_ it. Assigning an outer `var` from inside a scope is fine.

## Rejected — `for … yield` emits a `Block`

The idea: an iteration source whose `map` returns `Block`, so a for-comprehension
_is_ the unroll and `unroll` could be deleted. It is feasible, and it is not
worth it. Recorded here because the question will come back.

### It is feasible

Scala 3 desugars comprehensions onto the _source's_ methods:

| written                        | desugars to                                                   |
| ------------------------------ | ------------------------------------------------------------- |
| `for i <- s yield e`           | `s.map(i => e)`                                               |
| `for i <- s1; j <- s2 yield e` | `s1.flatMap(i => s2.map(j => e))`                             |
| `for i <- s if p yield e`      | `s.withFilter(i => p).map(i => e)`                            |
| `for (a, b) <- s yield e`      | `s.map { case (a, b) => e }` (tuple patterns are irrefutable) |

A source with `map(f: T => Block): Block` therefore makes `for … yield` produce
a `Block` — no macro. `flatMap` has the same signature, since the inner
generator's `map` has already produced a `Block`.

### But the yield's type cannot drive it

The return type is the _source_ type's business. `Range.map` is
`IterableOps.map[B](f: A => B): IndexedSeq[B]`; yielding a `Stmt` only infers
`B = Stmt`. There is no return-type-directed overload to hook, and an extension
method never wins over an existing member, so `for i <- 0 until 4` cannot be
redirected. Hijacking `Arr` is worse: `.map` on it comes from `js.ArrayOps`, and
a competing `extension def map(f: T => Block): Block` would collide with
ordinary `.map` usage in the same files.

So the comprehension needs a wrapper in the generator position —
`for i <- unroll(4) yield …` — which is the same wrapper, offering exactly the
same iteration shapes as today's `unroll(4)(i => …)`, in more syntax. It buys
familiarity and flat multi-generator syntax, and costs a four-method source API
(`map` / `flatMap` / `withFilter` / `indexed`), worse error messages when the
yield is not a `Block`, and a second idiom for something the DSL already has.

### The callback form already composes

Verified against the current DSL (`scala-cli compile --test`; note that
`scala-cli compile` alone silently skips `*.test.scala`, so a check without
`--test` proves nothing):

```scala
Block(
  x := 0.0,
  unroll(3): i =>
    Stmt.raw(s"  y$i;"),
  x := 1.0,
)
// x = 0.0; y0; y1; y2; x = 1.0;
```

Fewer-braces bodies in an argument list, a trailing comma on the body's last
line, and `unroll` nested in `unroll` all compile and emit in source order. The
comprehension's one real advantage — sitting as an argument of `Block` so that
its source position is its WGSL position — is a property the callback form
already has.

### The `Seq[Block]` conversion, also rejected

A conversion (or `Block` overload) taking `Seq[Block]` would make plain
`for i <- 0 until 4 yield …` work with no new source type, firing wherever the
expected type is `Block`. It drags `Range` + the `IndexedSeq`/`Vector` iterator
machinery into library code that every shader bundle links against (see the
stdlib-leak list in [CLAUDE.md](../CLAUDE.md)), and it is invisible at the call
site. Purely additive, so it stays available if plain ranges are ever wanted.

### What survives

`unroll` stays, in all five spellings, with one change: it **returns `Block`**
instead of `Stmt`, like every other group-producing construct under the new
hierarchy. The `unroll` / `loop` pairing the guide teaches is untouched —

```scala
unroll(1, MaxStops)(i => col := col.mix(stops(i).rgb, w(i)))  // Int     → build time
loop  (1, count.toI32)(i => col := col.mix(stops(i).rgb, w(i)))  // IntExpr → runtime
```

## Consequences for `Arr[Stmt]` accumulation

With both in place, no shader body needs a mutable statement array. The
pattern that replaces the side-effecting `emitTile`-style helper is: **build
the handles as Scala data first, emit from them second.**

```scala
val tiles = Neighbors.map: n =>
  (r = LetVec2(s"r_${n.name}"), t = LetVec3(s"t_${n.name}"), dir = n.dir)

Block(
  idx := uvScaled.floor + 11.0,
  unroll(tiles): (t, _) =>
    Block(
      t.r := Hash.hash2(((idx + t.dir) * 17.123411).bitsToU32),
      t.t := tileVec(t.r),
    ),
  …
)
```

The `Arr[Stmt]` / `Block(Arr)` overload stays — it is still the right tool when
statements genuinely have to be accumulated across unrelated code paths — but
it stops being the default shape of a shader body.

## Milestones

1. **Subtype chain.** `IfChain <: Stmt <: Block`; `Block.apply` over `Block*`
   and `Arr[Block]`; empty-part filtering; drop the three conversions. Update
   the `prelude` export lists ([src/prelude/painter.scala](../src/prelude/painter.scala)).
   Tests: nesting depth ≥ 3 flattens to one statement list, `Block.empty`
   contributes no line, indentation of a nested group inside `when`/`loop`.
2. **`scope`.** Emission + indentation tests; a test that two sibling
   scopes may declare the same local name.
3. **`unroll` returns `Block`.** All five spellings; a one-argument
   `unroll(xs)(v => …)` overload alongside the `(v, i) => …` one, since the
   index is often unused. Existing tests in
   [test/shader/Loops.test.scala](../test/shader/Loops.test.scala) and
   [test/shader/ControlFlow.test.scala](../test/shader/ControlFlow.test.scala)
   should keep passing unchanged.
4. **Docs.** Add `scope` to the control-flow table in
   [docs/guide/shader-dsl-guide.md](../docs/guide/shader-dsl-guide.md) and show
   a nested-`Block` body there (the `loop` vs `unroll` section stands); revisit the "unrolled body shares the enclosing scope" note in
   [docs/guide/gotchas.md](../docs/guide/gotchas.md) now that `scope`
   answers it. Also write the casing rule down — uppercase names are types,
   lowercase ones are operations, with `vec*`/`ivec*`/`uvec*` lowercase to
   mirror WGSL — since it is currently followed everywhere and stated nowhere.
5. **Example.** Give
   [examples/uniform_array_gradient](../examples/uniform_array_gradient/UniformArrayGradient.scala)
   a nested-group body, so the composition shape is demonstrated in the file
   the guide already points at. The two consumer-repo texture sketches are the
   acceptance case for the whole plan, ported after the library lands.

## Open Questions

1. ~~**Generator name.**~~ Moot — no generator; `unroll` keeps its current
   spellings.
2. ~~**`ScopedBlock` vs `scope`.**~~ Settled: `scope`. The DSL's casing rule is
   uppercase = type, lowercase = operation, and it holds without exception
   today (the one deliberate override being `vec*`/`ivec*`/`uvec*`, spelled as
   WGSL spells them). `Block` stays uppercase for the same reason — it is a
   type, written in signatures throughout — and `Let*`/`Var*`/`Const*` are
   opaque types too, not stylistic leftovers.
3. **Keep `Stmt` at all?** With subtyping, `Stmt` is documentation rather than
   a constraint. Collapsing to a single `Block` type would be one fewer name;
   keeping it preserves the "`:=` returns one statement" signal.
4. **Auto-named locals** (out of scope here, but adjacent): `LetVec4()` with a
   generated name would remove the last reason for index-suffixed names in the
   cases `scope` cannot cover. Separate plan?

## Out of Scope

- Any change to `loop` / `loopIf` / `break` / `continue`.
- A real statement AST. Everything here stays string-join composition; the
  indentation scheme already makes that correct under nesting.
- `switch`, and the other deferred items from
  [done/shader-control-flow-plan.md](done/shader-control-flow-plan.md).
