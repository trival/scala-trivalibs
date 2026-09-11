# Gotchas

Non-obvious traps when writing sketches. Most aren't caught by an obvious error
message, so they're worth knowing up front.

## Shader DSL

### One statement per line in raw WGSL

In `WgslFn.raw` / raw-string shade bodies, never put two statements on one line
(project convention) — each gets its own line.

### Angle constants are `Pi` and `Tau`, capitalised

`trivalibs.utils.numbers` (re-exported by `trivalibs.prelude.core`) defines
`inline val Pi` and `inline val Tau` — not `PI` / `TAU`. `Tau` is 2π, one full
turn, and is the constant to reach for: `t * Tau` is one revolution.

### A `var` declares where its first assignment lands

`VarFloat("t")` and friends carry the declaration on their **first** `:=`, so
where that statement sits is where `var t = …;` is emitted — the Scala `val` can
live anywhere.

That matters around loops. A `var` first assigned inside a `loop` / `loopIf` body
is declared inside the loop's braces: re-initialised every iteration, and gone
afterwards (`no definition in scope for identifier` at shade build). Seed an
accumulator **before** the loop:

```scala
val col = VarVec3("col")
Block(
  col := vec3(0.0),          // declares here, in the enclosing scope
  loop(0, n)(i => col := col + f(i)),
)
```

Two corollaries: "first" means first _built_, so statements pre-built into vals
and emitted in a different order carry the declaration with them; and one Scala
`val` per WGSL name — two `VarVec3("col")` instances emit two declarations and
WGSL rejects the redefinition.

`unroll` is the exception that needs no care here: its iterations share one
scope, so a `var` first assigned in the body declares once and assigns after.
What _unroll_ needs instead is unique names for locals declared per iteration —
`LetVec4(s"cur$i")`.

### Sampling inside a loop

`textureSample` needs uniform control flow. A loop bound read from a uniform
keeps it (verified: a `textureSample` inside `for (… i < i32(taps) …)` passes
`naga`), but a bound or a `break` that depends on per-fragment data does not —
use `.sampleLevel(…)` or `.load(…)` there.

### `Let`-local names can shadow a WGSL builtin

A `LetFloat("mix")` / `VarVec3("step")` emits a WGSL `let mix = …`, which then
shadows the builtin of that name for the rest of the function — and a later
`.mix` / `lerp` in the same body fails to parse. It only bites when something
_after_ the local uses the builtin, so it can survive a long time before an
unrelated edit trips it. Avoid builtin names (`mix`, `step`, `clamp`, `fract`,
`length`, …) for locals.

## Panels, layers & mips

### A layer's first texture slot is auto-injected

A `Layer` with a panel schema `P` normally reads the previous pass's output,
auto-bound to its first panel slot. To read a **different** panel at that slot
(e.g. a threshold/composite pass reading a separate scene panel), bind it
explicitly: `layer.bind("scene" := scenePanel, …)`. Then the painter uses your
binding instead of the auto-injected one.

Leaving the first slot unbound makes the layer _auto-pong_: the painter
ping-pongs it against a scratch target so it can read the previous result and
write the next. Consequence: **an MRT panel (multiple `formats`) cannot host an
auto-pong layer** — ping-pong is single-target by design, so it can't
post-process multiple render targets. The panel throws at construction; compose
a chain of single-format panels instead (each panel does one thing, the next
reads the previous panel's output). A layer that manually binds its first slot
is _not_ auto-pong and is fine on an MRT panel.

### Panel-level bindings fill a shape's _unbound_ slots — reuse a shape across panels

`panel.bind("name" := value)` supplies a uniform / sampler / texture to every
shape the panel draws, but only for slots the shape itself left unbound (shape
bindings always win; the panel only fills nulls). So one shape instance can live
in two panels and read a _different_ value for the same uniform from each — e.g.
a wall shape bound with just its texture, reading `vp` from
`scenePanel.bind("vp" := sceneVp)` in the normal pass and
`mirrorPanel.bind("vp" := mirrorVp)` in a reflection pass. No per-frame
rebinding, no duplicate shapes. (Works for group-0 uniforms, not just textures.)

### Depth textures: bind with `binding(depth = true)`, single-level, lazy-sampleable

Sample a panel's depth attachment by binding `panel.binding(depth = true)` to a
field declared with a `*DepthPanel` marker (`FragmentDepthPanel`, …); it reads as
a `DepthTexture2D`. Two traps: (1) the depth attachment is **single-level** — the
mip pyramid is built on the color texture, so there's no depth `sampleLevel`;
use `.load` (no sampler) or `.sample`. (2) The depth texture is **lazily
recreated as sampleable the first time it's sampled**, so the very first frame
reads an empty depth — a one-frame startup glitch, harmless in an animation loop
(each `paint(panel)` submits separately, so it's not a validation error).
Sampling the depth of a **`multisample = true`** panel works too: the painter
resolves the MSAA depth to a single-sample texture (an internal pass after the
shapes) and hands that back, so you still read a plain `texture_depth_2d` — no
special handling needed (it uses subsample 0, which is fine for depth-driven
effects like fog/DOF).

### `paint` order matters; `show` presents

`paint(a, b, c)` renders in order — list panels so a panel that samples another
comes after it. Nothing reaches the canvas until `show(panel)`. `paintAndShow`
is just `paint` then `show` for one panel.

### HDR needs a float format

For values > 1.0 (bloom, tone-mapping) use
`p.panel(format = TextureFormat.Rgba16Float)`; the default canvas format clamps.
MRT: `formats = Arr(TextureFormat.Rgba8Unorm, TextureFormat.Rgba16Float, …)`
matching the shader's `FO` output schema. Formats are the typed `TextureFormat`
constants — not raw strings.

## CPU math & buffers

### Buffer types are F32; CPU math is Double

`Vec3Buffer`/`Mat4Buffer` (what the GPU sees) are 32-bit floats; the mutable
`Vec3`/`Mat4` classes and tuples are `Double`. Uniform/attribute upload narrows
to F32 automatically. **WGSL has no f64 type** (render _or_ compute), so
anything bound to a shader is F32 — the `…dBuffer` (F64) variants are CPU-side
only (double-precision packed storage), never uploaded to the GPU.

### No quaternion on the GPU

WGSL has no quaternion type. Do quaternion math on the CPU (`Quat`) and upload a
rotation `Mat3`/`Mat4` to the shader.

### Matrices are column-major

`Mat*` store column-major (WGSL convention) — relevant if you index raw buffer
data; the `*` operators and factories handle it for you.

## App loop

### `tpf` is milliseconds

`animate(tpf => …)` passes **milliseconds** since the last frame (the Rust
painter's `frame` gets seconds). Scale accordingly (`time += tpf * 0.001`).

### Resize callback runs immediately

`p.onResize(cb)` invokes `cb` once right away with the current size, then on
every resize — so initialise aspect-dependent uniforms there, don't duplicate
the setup before it.

### Events beyond resize are manual

Only `onResize` is wired. Use `p.input()` (`InputState`) or DOM listeners on
`p.canvas` for pointer/keyboard; keep them out of the `animate` body.

## Tooling

### One `@JSExportTopLevel` name per build input

The sketch build passes a whole sketch **directory** to scala-cli, so every
`.scala` under it is linked together. Two files exporting the same top-level
name — e.g. a sketch directory that ends up nested inside another one — fail at
link time with:

```
Conflicting top level exports for module ModuleID(main), name sketch involving
sketches.a.A$package$, sketches.b.B$package$
```

The message names the two packages but says nothing about directory nesting,
which is usually the actual cause. Check that no sketch directory sits inside
another.

### A fresh Metals MCP / session is needed after `.mcp.json` changes

The Metals MCP server is written to `.mcp.json` on first enable; a Claude Code
session started before it existed won't have the `metals` tools — start a new
session. `.mcp.json` is gitignored (dynamic per-machine port).
