# Shader DSL locals: is `ctx.locals` / the `[L]` schema necessary?

Status: **done — the `[L]` schema is removed, ad-hoc locals are the one way.**
The analysis below is kept as the record of why; the removal plan at the end is
what was carried out. Triggered by the milestone 2 loop work
(`uniform-arrays-plan.md`), where the same question came up for loop bodies and
was answered "index-only, ad-hoc locals". This document asks the larger version:
should the `[L]` locals schema exist at all, or is `LetVec3("name")` /
`VarVec3("name")` the one idiomatic way?

## The two styles

```scala
// ad-hoc
program.frag: ctx =>
  val col = VarVec3("col")
  val t = LetFloat("t")
  Block(t := ctx.in.uv.x, col := vec3(t), ctx.out.color := vec4(col, 1.0))

// schema
program.frag[(col: Var[Vec3], t: Float)]: ctx =>
  Block(
    ctx.locals.t := ctx.in.uv.x,
    ctx.locals.col := vec3(ctx.locals.t),
    ctx.out.color := vec4(ctx.locals.col, 1.0),
  )
```

Both produce the **same runtime objects**: `TypedLocalAccessor.selectDynamic`
returns `VarExpr(name)` / `ConstExpr(name)` / `LetExpr(name)`
(`dsl/context.scala:82-94`) — precisely what the ad-hoc constructors build. The
schema is a naming-and-typing front end, nothing more.

## What the codebase actually does

Counted across `trivalibs/{src,examples,test}`, `sketches/` and `src/`:

|                                                | uses    | files  |
| ---------------------------------------------- | ------- | ------ |
| ad-hoc `Let*` / `Var*` / `Const*` constructors | **229** | **27** |
| `ctx.locals.…`                                 | 15      | 6      |

Every `ctx.locals` use is in `trivalibs/examples/` (3 files), `trivalibs/test/`
and two doc comments. **No sketch uses it at all** — 229 ad-hoc constructions
and zero schema fields across the whole sketch repo. The convergence is not a
preference in progress; it already happened.

## Is anything expressible only through the schema?

**No — and the implication runs the other way: the schema is strictly the less
expressive of the two.**

`ToLocal` (`dsl/types.scala:70-97`) enumerates its cases and has **no fallback**,
so an unlisted type does not reduce and the body fails to compile. Its coverage:

- `Let` (plain field): Float, Double, Int, UInt, Vec2-4, IVec2-4, UVec2-4.
- `Var[…]`: Float, Double, Int, UInt, Vec2-4 only.
- `Const[…]`: Float, Double, Int, UInt, Vec2-4 only.

So `[(m: Mat4)]` or `[(b: Boolean)]` is a compile error, while `LetMat4("m")` and
`LetBool("b")` exist and work today. Anything the schema can express, the ad-hoc
constructors can; the reverse is false.

The ad-hoc side has gaps of its own, but they are the **same** gaps: there is no
`VarMat*`, `ConstMat*`, `VarBool`, `VarIVec*`/`VarUVec*`, `ConstIVec*`/`ConstUVec*`
opaque type in `expr.scala` either. Those would be one line each, and are worth
adding regardless of which style survives.

## What the schema buys

Fairly stated, three things:

1. **Types come from the declaration, not from picking the constructor.** You
   write `(col: Var[Vec3])` instead of knowing that the mutable Vec3 local class
   is spelled `VarVec3`.
2. **A declaration list at the top of the body**, which reads as a summary of
   what the body works with.
3. **Field-name checking.** `ctx.locals.colr` is a compile error; a typo inside
   `VarVec3("colr")` is not (it emits a consistently misspelled but valid name).

Against that: every use costs the `ctx.locals.` prefix, which is why real bodies
immediately rebind — `val col = ctx.locals.col` — at which point the schema has
bought one line and cost one line, and (1) is the only remaining benefit.

## What the schema costs — including a live bug

**Repeated selection re-declares.** `selectDynamic` constructs a **fresh**
`VarExpr` per access, and `VarExpr` tracks "already declared" per instance
(`expr.scala:63-68`). So assigning the same `Var` twice through `ctx.locals`
emits two declarations. Verified by building the body of the existing test at
`test/shader/ShaderDsl.test.scala:486`:

```wgsl
  const scale = 2.0;
  var acc = in.position;
  let tmp = (acc + delta);
  var acc = tmp;          // ← second declaration, invalid WGSL
  out.position = vec4<f32>(acc, 0.0, 1.0);
```

`naga` rejects that outright (`error: redefinition of 'acc'`). **The test passes
only because its assertion is `body.contains("acc = tmp;")`, which is a substring
of `var acc = tmp;`.** So the DSL has a shader-breaking footgun on its
"recommended" style, masked by a weak assertion, and the reason no one has hit it
is that nobody writes shaders that way any more.

The ad-hoc style cannot have this bug: the Scala `val` **is** the instance, so
the declared-flag is per local, which is what it is for.

Secondary costs:

- `Var[T]` / `Const[T]` marker classes, `ToLocal`, `buildLocalKinds` /
  `populateKinds`, `TypedLocalAccessor` and the `kinds: Dict[String]` it carries
  exist only for this. The `Dict` and its `selectDynamic` dispatch are runtime
  (shade-build) code in a library where every byte is inlined downstream.
- Every body builder carries an extra type parameter and an extra overload pair
  (`vert`/`vert[L]`, `frag`/`frag[L]`, `WgslFn.dsl[P,R]`/`dsl[L,P,R]`).
- Two documented ways to do one thing, in a DSL whose stated goal is one
  idiomatic way — and `docs/guide/shader-dsl-guide.md:71` even labels the ad-hoc
  style "untyped", which is wrong: `LetFloat("n")` is an opaque type with the
  full `FloatExpr` op set.

## Decision

**Remove the `[L]` schema; ad-hoc `Let*` / `Var*` / `Const*` are the one way.**
It is what the code already does, it is shorter at every use, it is strictly more
expressive today, and it structurally cannot reproduce the re-declaration bug.
Early alpha is exactly when to make this kind of subtraction.

The re-declaration bug goes away with the mechanism that has it — no separate
fix is needed, and the memoising `TypedLocalAccessor` that would have been the
alternative is moot.

Before removing, add the missing opaque local types so nothing regresses:
`VarMat2/3/4`, `ConstMat2/3/4`, `VarBool`, `ConstBool`, `VarIVec2-4`,
`VarUVec2-4`, `ConstIVec2-4`, `ConstUVec2-4` — one line each, same pattern as
their `Let` siblings.

### Removal plan

1. Add the missing `Var*` / `Const*` opaque types (`math/gpu/expr.scala`).
2. Drop the `[L]` overloads: `Program.vert[L]` / `frag[L]`
   (`dsl/program.scala:69-114`), `WgslFn.dsl[L, P, R]` (`dsl/fn.scala:436-452`)
   and `WgslFnCtx.locals` (`dsl/fn.scala:459-470`).
3. Delete `Var` / `Const` markers, `ToLocal`, `buildLocalKinds`,
   `populateKinds` (`dsl/types.scala:23-27, 66-123`) and `TypedLocalAccessor`
   (`dsl/context.scala:76-94`), plus their prelude exports if any are named
   explicitly.
4. Migrate the six call sites: `examples/geometry3d_scene`, `examples/painter_dsl`,
   `examples/deferred`, `examples/uniform_array_gradient`, and the two scaladoc
   examples in `dsl/program.scala:17` and `dsl/fn.scala:430`. Each becomes a few
   `val x = LetVec3("x")` lines.
5. Migrate `test/shader/ShaderDsl.test.scala` and `test/shader/WgslFn.test.scala`
   — and **tighten the assertions while doing it**: the ones that caught nothing
   above should assert on the full emitted body, not on substrings that a
   re-declaration also satisfies.
6. `docs/guide/shader-dsl-guide.md` — collapse the "Two styles" section to one,
   drop the "untyped" mischaracterisation, and fix the `ctx.locals` row in the
   context table (line 60).

### What it came to

Carried out as planned, with two additions the plan did not foresee:

- **`LayerProgram.frag[L]`** had the same overload pair as `Program.frag` and
  went with it (`dsl/layer_program.scala`) — the plan listed only `Program` and
  `WgslFn.dsl`.
- **`sketchlib.utils.bake.TextureBaker`** in the consuming repo types a
  `FragmentCtx` explicitly, so it needed its type-argument list shortened by one
  (`src/utils/bake/Bake.scala:294`). The only downstream signature that named a
  context type directly.

`WgslFn.dsl[L, P, R]` is gone with `WgslFnCtx`; the surviving `dsl[P, R]` form
takes `(params, ret)`, and the three tests that used locals now declare them
ad-hoc inside the body. `Var` / `Const` markers, `ToLocal`, `buildLocalKinds`,
`populateKinds` and `TypedLocalAccessor` are deleted, and the prelude no longer
exports `Var`, `Const`, `TypedLocalAccessor` or `WgslFnCtx`.

Added first, as the plan required: `VarMat2/3/4`, `ConstMat2/3/4`, `VarBool`,
`ConstBool`, `VarIVec2-4`, `VarUVec2-4`, `ConstIVec2-4`, `ConstUVec2-4` — the
local types the schema never covered either, so nothing lost expressiveness.

**The masked bug is now asserted against.** The test that passed on a substring
of its own broken output
(`test/shader/ShaderDsl.test.scala`, the Var/Const/bare-locals program) asserts
the whole emitted body, and a second test pins the one-`val`-per-name rule:
two `VarVec2("dup")` instances each declare, which is what WGSL rejects.

35 test suites green, all examples compile, and the four sketches that exercise
the shader DSL rebuild.

### Knock-on

The `[L]` schema on **loop bodies** (`loop[L]`) was never built and now never
will be: milestone 2 in `uniform-arrays-plan.md` ships `loop` and `unroll` with
index-only bodies, which this decision settles rather than leaves open. Concretely
that means the loop family stays in `math/gpu/expr.scala` beside `when` /
`ifElse`, with no dependency on `shader/dsl` and no new file.
