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

> Gotcha: a `Double` on the **left** of an operator doesn't auto-convert. Write
> `expr * 0.5` not `0.5 * expr`; for a leading constant use
> `(0.5: FloatExpr) * expr`. See [gotchas.md](gotchas.md).

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
