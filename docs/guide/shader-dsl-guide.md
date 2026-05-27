# Shader DSL Guide

Write shaders as typed Scala that generates WGSL at build time — no `@group` /
`@binding` / `@location` decorations, no struct declarations (those come from
the schemas). For the full pipeline see
[sketch-authoring-guide.md](sketch-authoring-guide.md); for the exhaustive op
list query Metals (`inspect trivalibs.graphics.math.gpu.Expr`, `get-docs`) or
the Scaladoc site.

## Schemas (named tuples)

A shade is parameterised by named-tuple schemas. **Field order = layout index,
field name = WGSL variable name.**

```scala
type Attribs  = (position: Vec3, uv: Vec2)        // A — vertex attributes
type Varyings = (uv: Vec2)                         // V — vertex → fragment
type Uniforms = (                                  // U — uniforms
  mvp:  VertexUniform[Mat4],                        //   vertex-only
  tint: FragmentUniform[Vec3],                      //   fragment-only
  samp: FragmentUniform[Sampler],                   //   a sampler is just a field
  time: Float,                                      //   bare = both stages
)
type Panels = (tex: FragmentPanel)                 // P — panel textures (optional)
```

- `VertexUniform[T]` / `FragmentUniform[T]` set stage visibility; bare `T` =
  both.
- In `layerShade`, every uniform is fragment-stage, so drop the wrappers.
- Panel textures go in a separate `P` schema as `FragmentPanel` (or
  `VertexPanel`/`SharedPanel`) markers, read via `ctx.textures.<name>`.

## vert / frag and the context

`p.shade[A, V, U]` (or `[A, V, U, P]`, `[A, V, U, P, FO]`) takes a `program`
builder. Define `program.vert` and `program.frag`; each returns a `Block`:

```scala
val shade = p.shade[Attribs, Varyings, Uniforms, Panels]: program =>
  program.vert: ctx =>
    Block(
      ctx.out.position := ctx.bindings.mvp * vec4(ctx.in.position, 1.0),
      ctx.out.uv       := ctx.in.uv,
    )
  program.frag: ctx =>
    ctx.out.color := ctx.textures.tex.sample(ctx.in.uv, ctx.bindings.samp) * vec4(ctx.bindings.tint, 1.0)
```

`ctx` exposes:

| accessor       | in `vert`                 | in `frag`                          |
| -------------- | ------------------------- | ---------------------------------- |
| `ctx.in`       | attributes `A`            | varyings `V`                       |
| `ctx.out`      | varyings + `out.position` | fragment output `FO` (`out.color`) |
| `ctx.bindings` | uniforms `U`              | uniforms `U`                       |
| `ctx.textures` | panels `P`                | panels `P`                         |
| `ctx.locals`   | typed locals `L`          | typed locals `L`                   |

`layerShade[U]` (or `[U, P]`, `[U, P, FO]`) is fragment-only; the vertex stage
is a built-in full-screen triangle and `ctx.in.uv: Vec2` is the screen UV in
`[0,1]`.

## Locals

Two styles:

```scala
// untyped, ad-hoc:
program.frag: ctx =>
  val n = LetFloat("n")           // immutable WGSL `let`
  val c = VarVec3("c")            // mutable WGSL `var` (first := declares)
  Block(
    n := ctx.in.uv.x,
    c := vec3(0.0),
    c := c + vec3(n),             // reassign a var
    ctx.out.color := vec4(c, 1.0),
  )

// typed via the [L] schema (gives Vec*/op typing on ctx.locals.*):
program.vert[(pos: Vec2)]: ctx =>
  Block(
    ctx.locals.pos   := ctx.bindings.rotation * ctx.in.position,
    ctx.out.position := vec4(ctx.locals.pos, 0.0, 1.0),
  )
```

### Assignment statements (`:=` and `+= -= *= /=`)

`:=` produces an assignment `Stmt`. Its WGSL depends on the target:

| target                                    | first `:=`     | later `:=`     |
| ----------------------------------------- | -------------- | -------------- |
| `LetFloat` / `LetVec*` … , `ctx.locals.*` | `let n = …;`   | `let n = …;`   |
| `VarFloat` / `VarVec*` …                  | `var n = …;`   | `n = …;`       |
| `ConstFloat` / `ConstVec*` …              | `const n = …;` | `const n = …;` |
| `ctx.out.*`                               | `n = …;`       | `n = …;`       |

A `Var*` local is stateful: the **first** `:=` it sees emits the `var`
declaration, every subsequent `:=` emits a plain reassignment. So declare-then-
mutate just works:

```scala
val col = VarVec3("col")
Block(
  col := vec3(0.1),        // var col = vec3<f32>(0.1);
  col := col + vec3(0.2),  // col = (col + vec3<f32>(0.2));
)
```

**Compound assignment** — `Var*` targets also support `+= -= *= /=`, which emit
native WGSL compound assignment and read better for accumulation:

```scala
col += circle(...)         // col += …;   (same as  col := col + …)
col *= falloff             // col *= …;
```

Only reassignable targets (`Var*`) have the compound forms; `let`/`const` are
immutable so they're `:=`-only. A compound op requires the `var` to be **already
declared** — use it after the initial `:=`, never as the variable's first
statement (WGSL has no `col += …` without a prior `var col`).

Like `:=`, the compound ops are unchecked on the value's category (mirroring the
WGSL it lowers to): `vec3Var += floatExpr` compiles in Scala but WGSL will
reject the type mismatch — keep the operands in the same value category.

## Expressions & ops

GPU expressions mirror the CPU math surface, so shader code reads like CPU code:

- **scalars** (`FloatExpr`): `+ - * /`,
  `.sqrt .pow .sin .cos .tan .abs .floor .ceil .fract .exp .log .min .max .clamp .clamp01 .mix .step .smoothstep .fit0111 .fit1101`.
- **vectors** (`Vec2/3/4Expr`): component-wise `+ - * /` (vector or scalar),
  `.dot .cross(Vec3) .length .distance .normalize .mix .clamp .min .max .abs .fract`,
  swizzles `.xy .xyz .rgb .wzyx …`.
- **constructors**: `vec2(x,y)`, `vec3(xy, z)`, `vec4(rgb, 1.0)`,
  `vec3(scalar)`.
- **comparisons** → `BoolExpr`: `<  <=  >  >=  ===  !==`, combine with
  `&& || !`.
- **matrices**: `Mat*Expr` `*` (matrix or vector), `.determinant`.
- **textures**: `tex.sample(uv, samp)`, `tex.sampleLevel(uv, samp, lod)`,
  `tex.numLevels`.

### Numeric literals

Plain Scala number literals double as GPU expressions — you rarely need to wrap
them. A literal works on **either** side of an operator, with the same result
type as the expression it's combined with:

```scala
expr * 0.5            // FloatExpr
0.5 * expr            // FloatExpr — left side works too
uv + 1.0              // Vec2Expr  — broadcast scalar
2 - uv                // Vec2Expr  — (f32(2) - uv)
0.5 < expr            // BoolExpr  — scalar comparison, either side
```

This covers arithmetic (`+ - * /`) on `FloatExpr` and `Vec2/3/4Expr`, and the
scalar comparisons (`< <= > >= === !==`) on `FloatExpr`.

**A bare literal is always a float.** Both `Double` and `Int` literals convert
to `f32` (an `Int` emits `f32(n)`). This matches WGSL, where most math is `f32`.
For an actual integer expression, opt in explicitly:

- `n.i` → `IntExpr` (WGSL `i32`)
- `n.u` → `UInt` → `UIntExpr` (WGSL `u32`)

```scala
count.i + 1.i         // i32 arithmetic — keep both sides explicit
idx.u * 2.u           // u32 arithmetic
```

Integer expressions don't mix with the float-literal sugar: write `1.i`, not a
bare `1`, when the other operand is an `IntExpr`/`UIntExpr`.

**Vector comparison is not a literal case.** `v < w` is _component-wise_ and
returns a `Vec` mask (1.0 / 0.0 per lane, lowered to WGSL `step`) rather than a
`BoolExpr`, and it takes another vector only — there's no scalar-literal form on
either side. Use a scalar `FloatExpr` comparison when you want a `BoolExpr` for
control flow.

## Control flow

```scala
// branchless select (WGSL select(onFalse, onTrue, cond)):
ctx.out.color := select(blackVec4, color, brightness > threshold)
// or:  (brightness > threshold).select(color, blackVec4)

when(cond, Block(...))                       // if
ifElse(cond, thenBlock, elseBlock)           // if / else
ifChain(c1, b1).elseIf(c2, b2).elseDo(b3)    // if / else if / else
```

## Helper functions (WgslFn)

Reusable WGSL functions. Calling one in a `program.vert`/`frag` body (or in
another **DSL**-authored fn body) auto-registers it — the DSL records the call.

```scala
// DSL-authored (typed body) — its callees are auto-detected:
val addOffset = WgslFn.dsl[(p: Vec2, off: Vec2), Vec2]("add_offset"):
  (args, ret) => ret(args.p + args.off)

// raw WGSL escape hatch (intrinsics / hand-tuned kernels):
val blur9 = WgslFn
  .raw[(tex: Texture2D, s: Sampler, uv: Vec2, dir: Vec2), Vec4]("blur9"):
    """var col = vec4(0.0);
       // … one statement per line …
       return col;"""
  .withDeps(hash, gaussWeight)   // the WGSL body calls these — must declare
```

**What auto-registers vs what you must declare:**

- Calling a helper through its **typed API** in a DSL body — `program.vert`/
  `frag` or another `WgslFn.dsl` body — auto-registers it, whether its own body
  is raw or DSL. Deps are emitted before dependents, deduped.
- A **`WgslFn.dsl`** helper also auto-collects the helpers _it_ calls (they
  become its deps automatically).
- A **`WgslFn.raw`** helper's body is an opaque WGSL string: the DSL can't see
  the functions it calls _inside that string_, so you **must** declare them with
  `.withDeps(dep1, dep2, …)` — then they travel with it (registered before it)
  wherever it's used.
- `program.fn(f)` is the manual escape hatch — force-register a helper that is
  never reached through a typed call (idempotent).

So: dependency wiring is automatic for DSL-authored helpers; for raw-WGSL
helpers you must declare their callees yourself (`withDeps`, or `program.fn`).

Reusable kernels live in `trivalibs.graphics.shader.lib.*` (blur, noise, color,
hashing) — see the `blur` and `noise_tests` examples.

## Raw WGSL bodies

If you prefer raw WGSL, `p.shade[A, V, U](vertWgsl, fragWgsl)` takes strings.
You still skip the decorations/structs and reference `in.<f>`, `out.<f>`, and
uniforms by name. One statement per line (project convention).

## Builtins

Standard vertex/fragment builtins are handled for you (`out.position` is the
clip-space output). To read `@builtin(vertex_index)` etc., use the lower-level
`Shader.full[...]` — see `examples/simple_triangle`.
