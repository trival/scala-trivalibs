# Gotchas

Non-obvious traps when writing sketches. Most aren't caught by an obvious error
message, so they're worth knowing up front.

## Shader DSL

### One statement per line in raw WGSL

In `WgslFn.raw` / raw-string shade bodies, never put two statements on one line
(project convention) — each gets its own line.

## Panels, layers & mips

### A layer's first texture slot is auto-injected

A `Layer` with a panel schema `P` normally reads the previous pass's output,
auto-bound to its first panel slot. To read a **different** panel at that slot
(e.g. a threshold/composite pass reading a separate scene panel), bind it
explicitly: `layer.bind("scene" := scenePanel, …)`. Then the painter uses your
binding instead of the auto-injected one.

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

### A fresh Metals MCP / session is needed after `.mcp.json` changes

The Metals MCP server is written to `.mcp.json` on first enable; a Claude Code
session started before it existed won't have the `metals` tools — start a new
session. `.mcp.json` is gitignored (dynamic per-machine port).
