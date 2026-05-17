# Rust-Painter → Scala Port Comparison Guide

A reference for porting code written against the Rust `trivalibs_painter`
library to the Scala.js / WebGPU port in this repository.

Companion documents:

- [painter_technical_overview.md](painter_technical_overview.md) — architecture
  of the Rust painter library.
- [repomix-painter.xml](repomix-painter.xml) — full Rust source bundle (painter
  crate + examples).
- [../../CLAUDE.md](../../CLAUDE.md) — Scala project conventions.

This guide is structured for a reader who already knows the Rust painter API.
Scope is API mapping only — not WGSL, not WebGPU fundamentals, not Scala
teaching.

> **Naming flip (read first):** Rust `Layer` is Scala `Panel`; Rust `Effect` is
> Scala `Layer`. Every example that follows honours this flip.

---

## 1. Vocabulary & the Naming Flip

| Rust term            | Scala term               | File                                                                | Role                                                                    |
| -------------------- | ------------------------ | ------------------------------------------------------------------- | ----------------------------------------------------------------------- |
| `Painter`            | `Painter`                | [painter.scala](../../src/graphics/painter/painter.scala)           | Central registry + frame driver.                                        |
| `Shade`              | `Shade[U, P]`            | [shade.scala](../../src/graphics/painter/shade.scala)               | Shader module + bind-group layouts + pipeline layout.                   |
| `Form`               | `Form`                   | [form.scala](../../src/graphics/painter/form.scala)                 | Vertex buffer + topology / front face.                                  |
| `Shape`              | `Shape[U, P]`            | [shape.scala](../../src/graphics/painter/shape.scala)               | Drawable: Form + Shade + bindings + instances.                          |
| `Effect`             | **`Layer[U, P]`**        | [layer.scala](../../src/graphics/painter/layer.scala)               | Fullscreen post-processing pass (no Form, fragment-only Shade).         |
| `Layer`              | **`Panel`**              | [panel.scala](../../src/graphics/painter/panel.scala)               | Render target: textures + ordered shapes + ordered layers.              |
| `BindingBuffer<T>`   | `BufferBinding[T, F]`    | [buffers/binding.scala](../../src/graphics/buffers/binding.scala)   | Typed uniform buffer (CPU `StructRef` + GPU `GPUBuffer`).               |
| `ValueBinding`       | named-tuple bind entry   | n/a                                                                 | Dispatched in `Shape.bind(...)` by runtime type.                        |
| `LayerBinding`       | `PanelBinding` / `Panel` | [panel.scala](../../src/graphics/painter/panel.scala) (lines 13-20) | Texture view bound as shader input.                                     |
| `InstanceBinding`    | `Instance[U, P]`         | [instance.scala](../../src/graphics/painter/instance.scala)         | Per-draw-call binding overrides.                                        |
| `CanvasApp<E>` trait | `animate(...)` + closure | [utils/animate.scala](../../src/graphics/utils/animate.scala)       | Frame loop driver. See [§7](#7-app-loop--events) for the gap on events. |

**Visibility constants.** Rust has the `BINDING_BUFFER_VERT` /
`BINDING_BUFFER_FRAG` / `BINDING_BUFFER_BOTH` / `BINDING_SAMPLER_*` /
`BINDING_LAYER_*` constants that declare a binding's shader-stage visibility
when building a `Shade`. The Scala port derives all visibility information from
the uniform **named-tuple type**: wrap a field in `VertexUniform[T]` or
`FragmentUniform[T]` (or leave it bare, which defaults to vertex+fragment), and
the layout is computed by compile-time reflection. See
[§5](#5-binding-translation) and
[layouts.scala](../../src/graphics/shader/layouts.scala).

---

## 2. Project Setup & Entry Point

### Rust

```rust
struct App { canvas: Layer, /* … */ }

impl CanvasApp<()> for App {
  fn init(p: &mut Painter) -> Self { /* … */ }
  fn resize(&mut self, p: &mut Painter, w: u32, h: u32) { /* … */ }
  fn frame(&mut self, p: &mut Painter, tpf: f32) {
    p.request_next_frame();
    p.paint_and_show(self.canvas);
  }
  fn event(&mut self, _e: Event<()>, _p: &mut Painter) {}
}

pub fn main() {
  App::create()
    .config(AppConfig { show_fps: true, use_vsync: false, ..default() })
    .start();
}
```

### Scala

```scala
@JSExportTopLevel("main", moduleID = "my_example")
def main(): Unit =
  val canvas = document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    // --- init ---
    val shade   = painter.shade[Attribs, Varyings, Uniforms](...)
    val form    = painter.form(vertices = vs)
    val shape   = painter.shape(form, shade).bind("u" := someBinding)
    val canvasP = painter.panel(shape = shape)

    painter.onResize: (w, h) =>
      // viewProj / uniforms that depend on aspect
      ...

    animate: tpf =>
      // per-frame updates, then render:
      painter.paint(canvasP)
      painter.show(canvasP)
```

Differences:

- No trait — `init` runs inside the `Painter.init(canvas) { painter => ... }`
  closure.
- `canvas` is an existing `<canvas>` element passed in (browser owns the DOM,
  not the painter).
- `painter.onResize(cb)` registers a callback invoked both now (with the current
  size) and on every `ResizeObserver` tick. This covers Rust's `resize`.
- No event pipeline. See [§7](#7-app-loop--events).
- `animate(tpf => ...)` from
  [animate.scala](../../src/graphics/utils/animate.scala) drives
  `requestAnimationFrame` and logs FPS. There is no equivalent to
  `painter.request_next_frame()` (WebGPU rendering is always in the browser's
  rAF loop unless you stop the animator).

---

## 3. Core Type-by-Type Mapping

### Painter

Rust: `p.form(...)`, `p.shade(...)`, `p.shade_effect()`, `p.shape(form, shade)`,
`p.effect(shade)`, `p.layer()`, `p.bind_mat4()`, `p.sampler_linear()`,
`p.paint(layer)`, `p.show(layer)`, `p.paint_and_show(layer)`,
`p.request_next_frame()`, `p.canvas_size()`, `p.resize(...)`.

Scala: all factories live on the `Painter` instance too, but names align with
the Scala naming flip.

| Rust                                         | Scala                                                                                                                        |
| -------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `p.form(verts).create()`                     | `painter.form(vertices = verts)` (or with topology/frontFace: `painter.form(vertices = v, topology = ..., frontFace = ...)`) |
| `p.shade([...]).with_bindings(...).create()` | `painter.shade[A, V, U](vertWgsl, fragWgsl)` or DSL overload                                                                 |
| `p.shade_effect()`                           | `painter.layerShade[U]` (or `[U, P]` / `[U, P, FO]`)                                                                         |
| `p.shape(form, shade)`                       | `painter.shape(form, shade)`                                                                                                 |
| `p.effect(shade)`                            | `painter.layer(shade)`                                                                                                       |
| `p.layer()`                                  | `painter.panel()`                                                                                                            |
| `p.single_effect_layer(shade)`               | not yet (construct `painter.panel(layer = painter.layer(shade))`)                                                            |
| `p.sampler_nearest()`                        | `painter.samplerNearest` (lazy val)                                                                                          |
| `p.sampler_linear()`                         | `painter.samplerLinear` (lazy val)                                                                                           |
| `p.sampler()` with options                   | `painter.sampler(magFilter, minFilter, mipmapFilter)`                                                                        |
| `p.bind_mat4()`                              | `painter.binding[Mat4]`                                                                                                      |
| `p.bind_const_vec3(v)`                       | `painter.binding(v)` (single-param overload; auto-infers `Vec3`)                                                             |
| `p.paint(&layer)`                            | `painter.paint(panel)` (varargs: `painter.paint(p1, p2, p3)`)                                                                |
| `p.show(&layer)`                             | `painter.show(panel)`                                                                                                        |
| `p.paint_and_show(&layer)`                   | `painter.paint(panel)` then `painter.show(panel)` — no convenience wrapper                                                   |
| `p.compose(&[&a, &b])`                       | `painter.paint(a, b)` (varargs form of `paint`)                                                                              |
| `p.request_next_frame()`                     | n/a — `animate` is always running while it's not stopped                                                                     |
| `p.canvas_size()`                            | `painter.width` / `painter.height`                                                                                           |

Scala adds one factory not present in Rust:
**`painter.draw(shape, clearColor)`** is a direct-to-canvas shortcut that
renders a Shape straight to the swap chain without needing a Panel. Used by
`painter_triangle`, `painter_dsl`, `painter_typed_bindings`.

### Shade

Rust declares a Shade in three steps: list attribute formats, list bindings,
load a `.spv` module.

```rust
let shade = p
  .shade([Float32x3, Float32x3])          // attribs
  .with_bindings([
    BINDING_BUFFER_VERT, BINDING_BUFFER_VERT, BINDING_BUFFER_FRAG,
  ])
  .with_layers([BINDING_LAYER_FRAG])      // optional
  .create();
load_vertex_shader!(shade, p, "./shader/vs.spv");
load_fragment_shader!(shade, p, "./shader/fs.spv");
```

Scala declares the attribute layout, varyings, uniforms, and panel bindings as
named-tuple type parameters, then provides the shader body either as raw WGSL or
via the DSL.

```scala
type Attribs  = (position: Vec3, color: Vec3)
type Varyings = (color: Vec3)
type Uniforms = (
  viewProj: VertexUniform[Mat4],
  model:    VertexUniform[Mat4],
  tint:     FragmentUniform[Vec3],
)

val shade = painter.shade[Attribs, Varyings, Uniforms](
  vertWgsl = """ out.position = viewProj * model * vec4<f32>(in.position, 1.0);
                 out.color    = in.color; """,
  fragWgsl = """ out.color    = vec4<f32>(in.color * tint, 1.0); """,
)
```

Mapping rules:

- Ordered attributes list (Rust) ↔ ordered fields of the `Attribs` named tuple
  (Scala); field name becomes the WGSL variable name.
- `BINDING_BUFFER_VERT` ↔ `VertexUniform[T]` wrapper; `..._FRAG` ↔
  `FragmentUniform[T]`; `..._BOTH` ↔ plain `T` (default visibility in the Scala
  DSL is vertex+fragment).
- `BINDING_SAMPLER_FRAG` ↔ `FragmentUniform[Sampler]` (Sampler is a marker type
  in the DSL).
- `BINDING_LAYER_FRAG` ↔ a separate `P` (panels) type parameter, declared as
  `type Panels = (nameA: FragmentPanel, ...)`.

Five Scala overloads:

1. `painter.shade[A, V, U](vertWgsl, fragWgsl)` — WGSL strings, no panel
   bindings.
2. `painter.shade[A, V, U, P](vertWgsl, fragWgsl)` — add panel bindings.
3. `painter.shade[A, V, U](program => ...)` — DSL program builder, no panel
   bindings.
4. `painter.shade[A, V, U, P](program => ...)` — DSL + panels.
5. `painter.shade[A, V, U, P, FO](program => ...)` — DSL + panels + custom
   fragment output (MRT).

### Form

Rust:

```rust
let form = p.form(&vertices)
  .with_topology(wgpu::PrimitiveTopology::TriangleList)
  .with_front_face(wgpu::FrontFace::Ccw)
  .create();
```

Scala:

```scala
val form = painter.form(
  vertices  = verts,                          // StructArray[F]
  topology  = PrimitiveTopology.TriangleList,
  frontFace = FrontFace.CCW,
)
// all params have defaults, so the common case is just:
val form = painter.form(vertices = verts)
```

Reassigning geometry after creation: call `form.set(vertices = newVerts)`. The
old GPU buffer is destroyed and a new one allocated (no in-place resize yet).

Index buffers are not supported by `Form` yet — both the
`examples/buffer_triangle` and the raw simple triangle draw non-indexed. If you
need indexed draws today, you fall through to the raw WebGPU path as in
[BufferTriangle.scala](../../examples/buffer_triangle/BufferTriangle.scala).

### Shape

Rust uses builder chains over an `InstanceBinding` map keyed by numeric slot.

```rust
let shape = p.shape(form, shade)
  .with_bindings(map! {
    0 => cam.binding(),
    1 => model_mat.binding(),
    2 => p.bind_const_vec4(color),
  })
  .with_cull_mode(None)
  .with_blend_state(wgpu::BlendState::ALPHA_BLENDING)
  .with_instances(instance_vec)
  .create();
```

Scala binds by **name** (field of the uniform named tuple), using `BindPair`
sugar `"name" := value`:

```scala
val shape = painter.shape(form, shade, cullMode = CullMode.None, blendState = BlendState.Alpha)
  .bind(
    "viewProj" := cam,
    "model"    := modelMat,
    "tint"     := Vec3(1.0, 0.5, 0.2),   // auto-builds a BufferBinding[Vec3]
  )

shape.instances.add("model" := Mat4.fromTranslation(x, y, z))
shape.instances.add("model" := Mat4.fromTranslation(x2, y2, z2))
```

`bind` has overloads for 1–8 pairs; chain `.bind(...)` multiple times for more.
Values may be: `BufferBinding[T, F]`, `GPUSampler`, a raw value matching a
uniform field (auto-boxed into a `BufferBinding`), a `Panel`, or a
`PanelBinding`.

### Panel (Rust `Layer`)

Rust `Layer` constructor options:

| Rust builder method                                     | Scala equivalent                                                                             |
| ------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| `.with_shape(s)`                                        | `panel.set(shape = s)` (singular; plural `shapes = Arr(...)` takes precedence)               |
| `.with_shapes(vs)`                                      | `panel.set(shapes = Arr(s, ...))`                                                            |
| `.with_effect(e)`                                       | `panel.set(layer = l)` (singular; plural `layers = Arr(...)` takes precedence)               |
| `.with_effects()`                                       | `panel.set(layers = Arr(l, ...))`                                                            |
| `.with_size(w, h)`                                      | `panel.set(width = w, height = h)`                                                           |
| `.with_clear_color(c)`                                  | `panel.set(clearColor = (r, g, b, a))`                                                       |
| `.with_multisampling()`                                 | `panel.set(multisample = true)`                                                              |
| `.with_depth_test()`                                    | `panel.set(depthTest = true)`                                                                |
| `.with_mips()`                                          | `panel.set(mipLevels = 0)` (0 = full chain)                                                  |
| `.with_format(fmt)`                                     | `panel.set(format = "rgba16float")` (singular; plural `formats = Arr(...)` takes precedence) |
| `.with_mrt_formats([a, b, c])` / `with_formats`         | `panel.set(formats = Arr("rgba8unorm", "rgba16float", "rgba16float"))`                       |
| `.with_static_texture_data(bytes)`                      | **not yet** (see [§12](#12-known-gaps--workarounds))                                         |
| `.set_layer_bindings(...)` / `.set_value_bindings(...)` | `panel.bind("name" := value, ...)`                                                           |

The Scala Panel's constructor also accepts all of the above as constructor
kwargs:

```scala
val canvas = painter.panel(
  clearColor  = (0.02, 0.02, 0.06, 1.0),
  depthTest   = true,
  multisample = true,
  shape       = someShape,
)
```

### Layer (Rust `Effect`)

Rust:

```rust
let effect = p.effect(shade)
  .with_bindings(map! { 0 => u_time.binding() })
  .with_layers(map!   { 0 => gbuffer_albedo, 1 => gbuffer_normals })
  .with_instances(lights)
  .with_blend_state(additive)
  .with_mip_target(1).with_mip_source(0)
  .create();
```

Scala:

```scala
val lightLayer = painter
  .layer(lightShade, blendState = BlendState.Additive, mipSource = -1, mipTarget = -1)
  .bind(
    "lightDir"    := Vec3(0.5, 0.7, 1.0),
    "texSampler"  := painter.samplerNearest,
    "albedo"      := gBuffer.binding(index = 0),
    "normals"     := gBuffer.binding(index = 1),
  )
// instance-based lighting (one draw per light, accumulating via Additive):
lightLayer.instances.add(
  "lightPos"   := lightPosBinding,
  "lightColor" := lightColorBinding,
)
```

Both `mipSource` and `mipTarget` take an int mip-level or `-1` to disable (no
`Option`). Effects that target a mip level skip ping-pong, just like in the Rust
original.

### Instance

Rust `InstanceBinding` is a struct with `bindings: HashMap<u32, ValueBinding>`
and `layers: HashMap<u32, LayerBinding>`. Scala `Instance` is produced via
`shape.instances.add(...)` or `layer.instances.add(...)`, taking up to 8
`BindPair` entries. Same runtime semantics: per-draw-call overrides of the
shape/layer bindings.

```rust
// Rust — deferred lighting (one instance per light)
let lights = (0..10).map(|_| InstanceBinding {
  bindings: map! { 2 => light_pos, 3 => light_color },
  ..default()
}).collect();
let effect = p.effect(shade).with_instances(lights).create();
```

```scala
// Scala
for i <- 0 until 10 do
  lightLayer.instances.add(
    "lightPos"   := lightPositions(i),
    "lightColor" := lightColors(i),
  )
```

### BufferBinding

| Rust                                  | Scala                                  |
| ------------------------------------- | -------------------------------------- |
| `let m = p.bind_mat4();`              | `val m = painter.binding[Mat4]`        |
| `let m = p.bind_const_mat4(initial);` | `val m = painter.binding(initial)`     |
| `m.update(p, new_value)`              | `m.set(new_value)` or `m := new_value` |
| n/a                                   | `m.update(ref => ref.x = 1.0f)`        |
| `m.binding()` (to pass to Shape)      | pass `m` directly in `"name" := m`     |
| n/a                                   | `m.get` (CPU-side read, no GPU fetch)  |

See [buffers/binding.scala:110-122](../../src/graphics/buffers/binding.scala)
for `set` / `:=` / `update` / `get`.

### Sampler

Rust `p.sampler_linear()` returns a `Sampler` that produces a `ValueBinding` via
`.binding()`. Scala samplers are plain `GPUSampler` values — pass them directly
as bindings.

```scala
val s   = painter.samplerLinear
val s2  = painter.samplerNearest
val s3  = painter.sampler(
  magFilter    = FilterMode.Linear,
  minFilter    = FilterMode.Linear,
  mipmapFilter = FilterMode.Nearest,
)
shape.bind("texSampler" := s)
```

---

## 4. Shader Migration

Rust painter always loads SPIR-V modules compiled from `rust-gpu` / Cargo shader
crates. The Scala port runs WGSL in the browser — there are two migration
routes.

### Route A — keep the body as WGSL

If your Rust shader has a SPIR-V version of a relatively simple WGSL-style
shader, the fastest migration is to transcribe it to WGSL and use the string
overload:

```scala
val shade = painter.shade[Attribs, Varyings, Uniforms](
  vertWgsl = """
    out.position = viewProj * model * vec4<f32>(in.position, 1.0);
    out.color    = in.color;
  """,
  fragWgsl = """
    out.color = vec4<f32>(in.color * tint, 1.0);
  """,
)
```

`in.<name>`, `out.<name>`, and each uniform field (`viewProj`, `model`, `tint`)
are declared automatically. **You do not write any `@group` / `@binding` /
`@location` decorations** — they are emitted by compile-time reflection on the
`Attribs`/`Varyings`/`Uniforms` types. See
[layouts.scala](../../src/graphics/shader/layouts.scala) and
[derive.scala](../../src/graphics/shader/derive.scala).

Examples:
[painter_triangle](../../examples/painter_triangle/PainterTriangle.scala),
[painter_typed_bindings](../../examples/painter_typed_bindings/TypedBindings.scala),
[instances](../../examples/instances/Instances.scala).

### Route B — DSL

The DSL lets you write typed expressions in Scala that generate WGSL at
compile/runtime. Same shader as Route A:

```scala
val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
  program.vert: ctx =>
    Block(
      ctx.out.position :=
        ctx.bindings.viewProj * ctx.bindings.model * vec4(ctx.in.position, 1.0),
      ctx.out.color := ctx.in.color,
    )
  program.frag: ctx =>
    ctx.out.color := vec4(ctx.in.color * ctx.bindings.tint, 1.0)
```

Typed locals:

```scala
program.vert[(pos: Vec2)]: ctx =>
  val pos = ctx.locals.pos
  Block(
    pos := ctx.bindings.rotation * ctx.in.position,
    ctx.out.position := vec4(pos + ctx.bindings.translation, 0.0, 1.0),
  )
```

Or untyped locals with `LetVec2("name")` / `LetFloat(...)` / etc. — see
[panel_layer](../../examples/panel_layer/PanelLayer.scala) for a pattern.

Helper functions come in two flavours:

```scala
// DSL-authored helper (typed, compile-time-checked body)
val applyTransform =
  WgslFn.dsl[(pos: Vec2, mat: Mat2, offset: Vec2), Vec2]("apply_transform"):
    (p, ret) => ret(p.offset + p.mat * p.pos)

// Raw WGSL helper (escape hatch for intrinsics / hand-tuned code)
val gaussianBlur9: WgslFn[
  (tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, dir: Vec2),
  Vec4,
] = WgslFn.raw("gaussian_blur_9"):
  """var color = vec4(0.0);
     let off1 = vec2<f32>(1.3846153846) * dir / res;
     color += textureSample(tex, s, uv)        * 0.2270270270;
     ...
     return color;"""
```

Register a function once with `program.fn(f)` before using it in `program.vert`
/ `program.frag`. Registration is idempotent.

See [examples/painter_dsl](../../examples/painter_dsl/PainterDsl.scala) for
DSL-authored helpers, [examples/blur](../../examples/blur/Blur.scala) for
`WgslFn.raw`, and [examples/deferred](../../examples/deferred/Deferred.scala)
for DSL fragment shaders with typed locals.

### `shade_effect` / `layerShade`

Rust's `p.shade_effect()` drops the vertex shader and uses a built-in fullscreen
triangle. Scala equivalent:

```scala
val shade = painter.layerShade[Uniforms]: program =>
  program.frag: ctx =>
    ctx.out.color := vec4(ctx.in.uv, 0.0, 1.0)
```

`ctx.in.uv` (Vec2) is the screen-space UV in `[0, 1]`. Overloads accept `P`
(panel bindings) and `FO` (custom fragment output) just like `shade`.

### Builtins

Rust implicitly provides `@builtin(position)` etc. through the macro system.
Scala surfaces them through extra type parameters on `ShaderDef` and via
`VertBuiltinIn` / `FragBuiltinIn` named tuples. For standard vertex/fragment
shaders you never touch them; use the defaults. To read `@builtin(vertex_index)`
or similar, use `Shader.full[...]` — see
[SimpleTriangle.scala](../../examples/simple_triangle/SimpleTriangle.scala) for
the only example that does.

---

## 5. Binding Translation

### `bind_*` method table

| Rust                                            | Scala                                                                                                                              |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| `p.bind_f32()`                                  | `painter.binding[Float]`                                                                                                           |
| `p.bind_u32()`                                  | not yet ported (use raw `BufferBinding[Int, U32]`?)                                                                                |
| `p.bind_vec2()` / `bind_vec3()` / `bind_vec4()` | `painter.binding[Vec2]` / `[Vec3]` / `[Vec4]`                                                                                      |
| `p.bind_uvec2()`                                | not yet                                                                                                                            |
| `p.bind_mat2()` / `bind_mat3()` / `bind_mat4()` | `painter.binding[Mat2]` / `[Mat3]` / `[Mat4]`                                                                                      |
| `p.bind_quat()`                                 | won't be ported — WGSL has no quaternion type. Use a rotation matrix binding (`Mat3` / `Mat4`) in shaders.                         |
| `p.bind_buff::<T>(initial)`                     | `painter.binding[T](initial)` if `UniformLayout[T]` given; else construct `BufferBinding[T, F](device)` directly                   |
| `p.bind_const_vec3(v)`                          | `painter.binding(v)` — produces a **mutable** binding pre-set to `v`. To freeze the binding, just don't call `.set` again.         |
| `p.bind_const_f32(f)`                           | `painter.binding(d)` — pass a `Double` directly. Scala.js compiles to JS numbers (doubles), so CPU-side code mainly uses `Double`. |
| `p.sampler_linear().binding()`                  | `painter.samplerLinear` (pass directly)                                                                                            |
| `p.sampler_nearest().binding()`                 | `painter.samplerNearest`                                                                                                           |

**`bind_const_*` semantics difference.** Rust's `bind_const_*` methods return a
`ValueBinding` and the user does not keep the buffer handle. Scala's
`painter.binding(v)` returns a full `BufferBinding` that you can update later —
the "const-ness" is by convention, not type.

### Visibility constants

Rust declares `[BINDING_BUFFER_VERT, BINDING_BUFFER_FRAG, BINDING_SAMPLER_FRAG]`
as an ordered list. Scala encodes visibility on each **field of the uniform
named tuple**:

```scala
type Uniforms = (
  mvp:        VertexUniform[Mat4],        // VERT-only
  tint:       FragmentUniform[Vec3],      // FRAG-only
  texSampler: FragmentUniform[Sampler],   // FRAG-only (sampler)
  time:       Float,                      // VERT+FRAG (default)
)
```

Wrappers live in
[graphics/shader/types.scala](../../src/graphics/shader/types.scala) and
generate the correct `GPUShaderStage` flags. There is no
`VertexFragmentUniform[T]` — a bare `T` is the vertex+fragment default.

**Exception: `layerShade`.** For layer (post-processing) shades there is no
vertex stage the user controls, so every uniform is fragment-only by
construction. `layerShade` applies `NamedTuple.Map[U, WrapFragment]` internally
(see [painter.scala:361](../../src/graphics/painter/painter.scala#L361) and
[derive.scala:247](../../src/graphics/shader/derive.scala#L247)): bare types are
auto-wrapped in `FragmentUniform[T]`, already-wrapped types pass through. So for
layer uniforms you can just write bare types — no `FragmentUniform[...]` wrapper
needed.

### Value-binding map → named-tuple bind

```rust
// Rust: map slot → ValueBinding
.with_bindings(map! {
  0 => cam.binding(),
  1 => model.binding(),
  2 => tint,
})
```

```scala
// Scala: match by field name
.bind(
  "viewProj" := cam,
  "model"    := model,
  "tint"     := tint,    // Vec3 value, BufferBinding[Vec3, Vec4Buffer], or Vec3 literal all work
)
```

### Panel (Rust `LayerBinding`) variants

Rust `LayerBinding` has four variants; Scala uses
`PanelBinding(panel, index, mipLevel, depth)`.

| Rust `LayerBinding`                        | Scala                                                 |
| ------------------------------------------ | ----------------------------------------------------- |
| `LayerBinding::Source(layer)`              | `panel` (bare Panel — wraps to `PanelBinding(panel)`) |
| `LayerBinding::AtIndex(layer, i)`          | `panel.binding(index = i)`                            |
| `LayerBinding::SourceAtMipLevel(layer, m)` | `panel.binding(mipLevel = m)`                         |
| `LayerBinding::Depth(layer)`               | `panel.binding(depth = true)`                         |

Example (deferred lighting reads G-buffer at indices 0 and 1):

```scala
lightLayer.bind(
  "albedo"  := gBuffer.binding(index = 0),
  "normals" := gBuffer.binding(index = 1),
)
```

### Instance bindings

Rust `InstanceBinding` — one slot map per draw call.

```rust
let instances: Vec<InstanceBinding> = models.iter().map(|m| InstanceBinding {
  bindings: map! { 1 => m.binding(), 2 => rand_tint() },
  ..default()
}).collect();
```

Scala — build up `shape.instances` / `layer.instances` with `.add(bindPairs)`:

```scala
var i = 0
while i < N do
  shape.instances.add(
    "model" := models(i),
    "tint"  := tints(i),
  )
  i += 1
```

To mutate an instance per frame (e.g. animation):

```scala
shape.instances(i).bind("model" := newModelMat)
```

---

## 6. Rendering Flow

### Per-frame orchestration

| Rust                       | Scala                                             |
| -------------------------- | ------------------------------------------------- |
| `p.paint(&layer)`          | `painter.paint(panel)`                            |
| `p.compose(&[&a, &b, &c])` | `painter.paint(a, b, c)` (`paint` is varargs)     |
| `p.show(&layer)`           | `painter.show(panel)`                             |
| `p.paint_and_show(&layer)` | `painter.paint(panel)` then `painter.show(panel)` |

Chained passes (deferred rendering):

```scala
animate: tpf =>
  // update uniforms, instances, transforms …
  painter.paint(gBuffer)       // MRT scene pass
  painter.paint(canvasPanel)   // lighting pass that reads gBuffer
  painter.show(canvasPanel)
```

See [examples/deferred](../../examples/deferred/Deferred.scala) for the full
G-buffer + lighting flow and
[examples/panel_tex](../../examples/panel_tex/PanelTex.scala) for panel →
texture feedback across three panels.

### Direct-to-canvas `draw`

Scala-only shortcut for simple demos: render a single shape to the swap chain
with no Panel. Supports an optional clear color; subsequent calls load the
previous contents.

```scala
painter.draw(shape1, clearColor = (0.1, 0.1, 0.1, 1.0))
painter.draw(shape2)   // blends on top (no clear)
```

See [examples/painter_dsl](../../examples/painter_dsl/PainterDsl.scala) and
[examples/painter_triangle](../../examples/painter_triangle/PainterTriangle.scala).

---

## 7. App Loop & Events

| `CanvasApp` hook                       | Scala mechanism                                                         |
| -------------------------------------- | ----------------------------------------------------------------------- |
| `fn init(...)`                         | code inside the `Painter.init(canvas) { painter => ... }` closure       |
| `fn resize(w, h)`                      | `painter.onResize((w, h) => ...)` — callback runs now + on every resize |
| `fn frame(tpf)`                        | `animate(tpf => ...)` callback (tpf is ms, not s — beware units)        |
| `fn event(e)`                          | **gap** — no pipeline; attach raw DOM listeners to `painter.canvas`     |
| `AppConfig.show_fps`                   | `animate` logs FPS automatically; no toggle                             |
| `AppConfig.use_vsync`                  | browser-controlled; no knob                                             |
| `AppConfig.remember_window_dimensions` | **gap** — no persistence                                                |
| `request_next_frame`                   | n/a — `animate` runs continuously until `animator.stop()`               |

### Minimal recipe

```scala
Painter.init(canvas): painter =>
  val ...    // init
  painter.onResize((w, h) => /* aspect-dependent uniforms */)
  animate: tpf =>
    // per-frame
    painter.paint(panel)
    painter.show(panel)
```

### Event recipe (until the pipeline exists)

Use `painter.canvas` (an `HTMLCanvasElement`) with Scala.js DOM events:

```scala
import org.scalajs.dom.PointerEvent

painter.canvas.addEventListener[PointerEvent]("pointermove", (e: PointerEvent) =>
  val rect = painter.canvas.getBoundingClientRect()
  val x = (e.clientX - rect.left) / rect.width
  val y = (e.clientY - rect.top)  / rect.height
  mousePosBinding.set(Vec2(x, y))
)
```

Keyboard, pointer button, wheel — all via standard DOM events on `canvas` or
`window`. This is verbose compared to Rust's `Event<UserEvent>` enum. Until the
library adds a unified event pipeline, keep event handling out of the frame-loop
closure so the `animate` body stays tight.

---

## 8. Math & Transforms

Scala exposes every math type in three representations:

1. **Mutable class** (e.g. `Vec3`, `Mat4`) — CPU workhorse, zero-alloc in-place
   ops (`.rotateSelf`, `.mulSelf`, etc.).
2. **Immutable tuple** (`Vec3Tuple`) — cheap to construct and destructure.
3. **Buffer type** (`Vec3Buffer`, `Mat4Buffer`) — the layout the GPU sees, used
   by `StructRef` / `StructArray` for zero-cost typed access.

All three share method names where it matters; conversions are implicit in most
cases via `given`s in [graphics/math/cpu/](../../src/graphics/math/cpu/).

GPU-side `Expr[T]` in [graphics/math/gpu/](../../src/graphics/math/gpu/) exposes
the same surface (`+`, `-`, `*`, `.normalize`, `.dot`, `.length`, `.sin`,
`.cos`, `.sqrt`, …) so the DSL reads like CPU code — see
[painter_dsl](../../examples/painter_dsl/PainterDsl.scala),
[panel_layer](../../examples/panel_layer/PanelLayer.scala).

### glam → Scala math

| glam                                        | Scala                                                                                                                 |
| ------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| `Vec2` / `Vec3` / `Vec4`                    | `Vec2` / `Vec3` / `Vec4` (mutable class)                                                                              |
| `Mat2` / `Mat3` / `Mat4`                    | `Mat2` / `Mat3` / `Mat4`                                                                                              |
| `Quat`                                      | `Quat` (in `graphics.math.cpu`)                                                                                       |
| `vec3(x, y, z)`                             | `Vec3(x, y, z)` on CPU; `vec3(x, y, z)` (lowercase) in the GPU DSL                                                    |
| `Mat4::perspective(fov, a, n, f)`           | `Mat4.perspective(fov, a, near, far)`                                                                                 |
| `Mat4::look_at_rh`                          | `Mat4.lookAt(...)`                                                                                                    |
| `Transform::from_translation(v)`            | `Transform.fromTranslation(v)`                                                                                        |
| `Transform::rotate_y(angle)`                | `t.rotation.setFromRotationY(angle)` on the mutable `Transform`                                                       |
| `PerspectiveCamera::create(CamProps {...})` | `PerspectiveCamera(fov = ..., aspect = ..., pos = Vec3(...))`                                                         |
| `cam.view_proj_mat()`                       | `cam.viewProjMat`                                                                                                     |
| `cam.set_aspect_ratio(a)`                   | `cam(aspect = a)` — `PerspectiveCamera.apply` is a setter                                                             |
| `SceneObject` trait                         | `SceneObject[T]` given instance (typeclass style) — [scene_object.scala](../../src/graphics/scene/scene_object.scala) |
| `obj.model_mat()`                           | `obj.modelMat` (via the `SceneObject` extension)                                                                      |

Scala `Transform` is a **mutable** TRS triple (`translation`, `rotation`,
`scale`), matching Rust's API shape but without the builder `.with_*` methods —
mutate the field directly or use the `Transform.from*` factories. See
[scene/transform.scala](../../src/graphics/scene/transform.scala).

---

## 9. Geometry & Meshes

Rust examples build vertex data via `#[apply(gpu_data)]` structs and hand it to
`p.form(&data).create()`.

```rust
#[apply(gpu_data)]
struct Vertex { pos: Vec2, uv: Vec2 }
const TRIANGLE: &[Vertex] = &[ /* ... */ ];
let form = p.form(TRIANGLE).create();
```

Scala — `allocateAttribs[T](count)` from
[buffers/attributes.scala](../../src/graphics/buffers/attributes.scala) sizes a
`StructArray` whose layout is derived from the `Attribs` named tuple. Write
vertices by index with `.set0(...)`, `.set1(...)` etc., or with the `:=` ops on
each tuple field.

```scala
type V = (position: Vec2, uv: Vec2)
val verts = allocateAttribs[V](3)
verts(0).set0(-0.7, -0.7)
verts(0).set1(0.0, 1.0)
verts(1).set0(0.7, -0.7)
verts(1).set1(1.0, 1.0)
verts(2).set0(0.0, 0.7)
verts(2).set1(0.5, 0.0)
val form = painter.form(vertices = verts)
```

Mesh / polygon utilities live in
[graphics/geometry/](../../src/graphics/geometry/) with `Mesh[F]` and `Polygon`
helpers. Procedural builders (Grid, 2D lines, Cuboid, sphere) and the surface
helpers that Rust sketches reach for are not yet ported; see
[mesh-geometry-port-plan.md](../mesh-geometry-port-plan.md) for the tracked
status and phased plan.

---

## 10. trivalibs Utility Mapping

For the `trivalibs_nostd::*` rows (hash, color, coords, num_ext, vec_ext,
simplex noise, blur) the tracked status and phased port plan live in
[trivalibs-nostd-port-plan.md](../trivalibs-nostd-port-plan.md).

| Rust path                                                                   | Scala path                                                                     | Status  | Notes                                                                                                                                                                                                                                                    |
| --------------------------------------------------------------------------- | ------------------------------------------------------------------------------ | ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `trivalibs_core::glam::*`                                                   | [graphics/math/cpu/](../../src/graphics/math/cpu/)                             | Ported  | Vec2-4, Mat2-4, Quat; three representations each.                                                                                                                                                                                                        |
| `trivalibs::math::transform::Transform`                                     | [scene/transform.scala](../../src/graphics/scene/transform.scala)              | Ported  | Mutable TRS; `from*` factories; `toMatrix` extension.                                                                                                                                                                                                    |
| `trivalibs::rendering::camera::{PerspectiveCamera, CamProps}`               | [scene/camera.scala](../../src/graphics/scene/camera.scala)                    | Ported  | FPS-style; cached projection; no second `CamProps` type.                                                                                                                                                                                                 |
| `trivalibs::rendering::scene::SceneObject`                                  | [scene/scene_object.scala](../../src/graphics/scene/scene_object.scala)        | Ported  | Scala typeclass: `given SceneObject[T]` with `.modelMat`, `.modelViewProjMat(cam)`.                                                                                                                                                                      |
| `trivalibs_nostd::color::hsv2rgb`                                           | —                                                                              | Not yet | Inline the 6-line WGSL fn where needed.                                                                                                                                                                                                                  |
| `trivalibs_nostd::random::hash::hash`                                       | —                                                                              | Not yet | One-liner WGSL hash, port inline.                                                                                                                                                                                                                        |
| `trivalibs_nostd::num_ext::NumExt`                                          | [trivalibs/utils/numbers.scala](../../trivalibs/src/utils/numbers.scala)       | Done    | CPU-side scalar `NumExt` ported (incl. `fract`, `fit0111`, `fit1101`). GPU DSL has all ops on `Float/Vec2/Vec3/Vec4` Exprs. Integer ops live in `IntExt[Int]` in `numbers.scala`; GPU `IntExt[IntExpr]`/`IntExt[UIntExpr]` deferred to integer DSL work. |
| `trivalibs_nostd::vec_ext::VecExt`                                          | —                                                                              | Not yet | GPU-side vector extensions missing.                                                                                                                                                                                                                      |
| `trivalibs_nostd::blur::{gaussian_blur, gaussian_blur_9, gaussian_blur_13}` | —                                                                              | Not yet | [examples/blur](../../examples/blur/Blur.scala) inlines `WgslFn.raw("gaussian_blur_9", ...)`.                                                                                                                                                            |
| `trivalibs::prelude::*`                                                     | —                                                                              | N/A     | Use explicit imports; no prelude wildcard on Scala side.                                                                                                                                                                                                 |
| `trivalibs::map!` macro                                                     | named-tuple `bind(...)` / `Arr(...)`                                           | N/A     | No macro needed.                                                                                                                                                                                                                                         |
| `Arr`, `Dict`, `Opt`, `Obj.literal`                                         | [trivalibs/utils/js.scala](../../trivalibs/src/utils/js.scala)                 | Ported  | `Arr[T] = js.Array[T]`, `Dict[V] = js.Dictionary[V]`, `Opt[T] = T \| Null`.                                                                                                                                                                              |
| `StructArray`, `StructRef`                                                  | [trivalibs/utils/bufferdata.scala](../../trivalibs/src/utils/bufferdata.scala) | Ported  | Zero-cost typed binary buffers (F32/F64/U8/U16/U32/I8/I16/I32).                                                                                                                                                                                          |
| `Random` (`rand_range`, `rand_sign`, `rand_vec3`)                           | [trivalibs/utils/random.scala](../../trivalibs/src/utils/random.scala)         | Partial | `rand()`, `randInRange(lo, hi)` available; `rand_vec3_range` etc. not yet.                                                                                                                                                                               |

---

## 11. Example-by-Example Mapping

| Rust example      | Scala example                                                                                                                                                        | Status      | Notes                                                                                   |
| ----------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------- | --------------------------------------------------------------------------------------- |
| `simple_triangle` | [simple_triangle](../../examples/simple_triangle/)                                                                                                                   | Ported      | Raw WebGPU (no painter). First-read sanity check.                                       |
| `triangle`        | [painter_triangle](../../examples/painter_triangle/) / [painter_dsl](../../examples/painter_dsl/) / [painter_typed_bindings](../../examples/painter_typed_bindings/) | Ported      | Three flavours; start from `painter_triangle` then branch to `painter_dsl`.             |
| `base_effect`     | [panel_layer](../../examples/panel_layer/)                                                                                                                           | Ported      | Single full-screen `Layer` with time + resolution uniforms.                             |
| `instances`       | [instances](../../examples/instances/)                                                                                                                               | Ported      | 100 rotating triangles w/ per-instance model matrix.                                    |
| `layer_tex`       | [panel_tex](../../examples/panel_tex/)                                                                                                                               | Ported      | Two off-screen panels used as textures on a third canvas panel; camera + `Transform`.   |
| `blur`            | [blur](../../examples/blur/)                                                                                                                                         | Ported      | `WgslFn.raw` inlines the gaussian kernel; `bind_const_f32/vec2` → `binding(...)`.       |
| `deferred_light`  | [deferred](../../examples/deferred/)                                                                                                                                 | Ported      | MRT panel + instance-based lighting layer w/ `BlendState.Additive`.                     |
| `mipmap`          | [mipmaps](../../examples/mipmaps/)                                                                                                                                   | Ported      | `mipLevels = 0` for full chain; `sampleLevel` in DSL.                                   |
| `random_lines`    | —                                                                                                                                                                    | **Not yet** | Would need a line-primitive example + per-frame geometry updates.                       |
| `ball`            | —                                                                                                                                                                    | **Not yet** | 3D sphere + normal-mapped lighting; needs ball geometry generator.                      |
| `geometries`      | —                                                                                                                                                                    | **Not yet** | Depends on pre-built cube/sphere/plane generators (not ported).                         |
| `dynamic_shapes`  | —                                                                                                                                                                    | **Not yet** | Runtime `form.set(vertices = ...)` works, but no example yet.                           |
| `dynamic_texture` | —                                                                                                                                                                    | **Not yet** | Render-to-texture feedback loop.                                                        |
| `shader_image`    | —                                                                                                                                                                    | **Not yet** | Procedural image shader — trivial once samplers/panel-as-source are used.               |
| `mouse_color`     | —                                                                                                                                                                    | Skipped     | Not worth porting — trivial demo of the event-pipeline gap ([§7](#7-app-loop--events)). |
| `render_to_mip`   | partial via [mipmaps](../../examples/mipmaps/)                                                                                                                       | Partial     | `mipSource`/`mipTarget` fields exist on `Layer` but no dedicated example yet.           |
| `noise_tests`     | —                                                                                                                                                                    | **Not yet** | Blocked on `hash` / noise utils not being ported.                                       |

If you're porting your first Rust file and want a richly featured template,
start from [painter_dsl](../../examples/painter_dsl/PainterDsl.scala) — it
exercises the DSL, `WgslFn.dsl` helpers, multiple shapes, per-frame binding
updates, and `painter.draw`.

---

## 12. Known Gaps & Workarounds

### Trivial workaround (port now)

- **`paint_and_show(layer)`** — two calls: `painter.paint(panel)` then
  `painter.show(panel)`.
- **`hsv2rgb`, `hash`** — inline the WGSL fn with `WgslFn.raw`. See
  [examples/blur](../../examples/blur/Blur.scala) for the pattern.
- **`gaussian_blur_9/13`** — inline the kernel as `WgslFn.raw`, same as
  `examples/blur`.
- **`map! { 0 => a, 1 => b }`** — use named-tuple
  `bind("nameA" := a, "nameB" := b)`.

### Blocked on library work

- **`CanvasApp` event pipeline.** Only `onResize` is wired; pointer / keyboard /
  user events must go through raw DOM listeners on `painter.canvas`. A unified
  event abstraction is a future milestone.
- **Shader hot reload / `ShaderReloadEvent`.** Not plumbed. Requires a dev
  WebSocket + module invalidation path — deferred.
- **Compute shaders, indirect draws, async buffer readback, `BufferMapping`.**
  Facades in [webgpu/facades.scala](../../src/webgpu/facades.scala) cover
  render-pass-only code.
- **`with_static_texture_data(bytes)` on Panel.** No direct API — currently must
  create a `GPUTexture` with usage `COPY_DST | TEXTURE_BINDING` through the
  device and bind it manually.
- **`bind_uvec2`, `bind_quat`.** Need `UniformValue[UVec2]` /
  `UniformValue[Quat]` givens. Straightforward to add.
- **Pre-built mesh generators** (sphere, cube, plane, grid, icosphere). Blocks
  `geometries` / `ball` examples.
- **`NumExt` / `VecExt` GPU shader extensions.** `.fit0111()`, `.frct()`, etc.
  not in the DSL. Workaround: write inline expressions.
- **Random helpers.** `rand_vec3_range`, `rand_sign`, `rand_vec4` are missing;
  compose them from `rand()` / `randInRange(a, b)`.
- **`remember_window_dimensions`.** Not planned — the browser already persists
  window dimensions across sessions, so no Scala-side equivalent is needed.
- **`AppConfig.show_fps` toggle.** FPS logging is always on inside `Animator`.

---

## 13. Porting Checklist

Work top-to-bottom for a methodical port:

1. Rename every **`Layer` → `Panel`** and every **`Effect` → `Layer`** in the
   Rust source you're translating.
2. Identify the Rust `struct App`'s fields — these become `val`s inside the
   `Painter.init(canvas) { painter => ... }` closure.
3. Translate the shader:
   - `p.shade([Float32xN, ...])` → declare `type Attribs = (field: VecN, ...)`.
   - `.with_bindings([...])` → declare `type Uniforms = (field: ...[T], ...)`
     with `VertexUniform` / `FragmentUniform` wrappers where needed.
   - `.with_layers([...])` → declare
     `type Panels = (field: FragmentPanel, ...)`.
   - `load_*_shader!` macros → either paste WGSL into `vertWgsl`/`fragWgsl`
     strings or port to the DSL with `.vert` / `.frag`.
4. Translate each `p.bind_*` call to `painter.binding[...]` (or
   `painter.binding(initial)`).
5. Translate each `p.shape(form, shade)` to `painter.shape(form, shade)` and
   map `.with_bindings(map! {...})` to `.bind("name" := value, ...)`.
6. Translate `p.effect(shade)` → `painter.layer(shade)`; `p.layer()` →
   `painter.panel()`. Map each `.with_*` call per the tables in
   [§3](#3-core-type-by-type-mapping).
7. If the Rust code calls `load_vertex_shader!` after `p.shade(...)`, you do
   **not** need an equivalent — the Scala string / DSL is evaluated inline.
8. Move `fn init` body above `animate(tpf => ...)`.
9. Move `fn resize(w, h)` body inside `painter.onResize((w, h) => ...)`.
10. Move `fn frame(tpf)` body inside `animate(tpf => ...)` — remember **`tpf` is
    milliseconds on Scala side**, not seconds; adjust scale.
11. Replace `p.paint_and_show(layer)` with `painter.paint(panel)` then
    `painter.show(panel)`.
12. Move `fn event(...)` body into raw DOM `addEventListener` calls on
    `painter.canvas`. Match on `type` of the DOM event.
13. Replace `trivalibs_nostd::...` imports with either Scala trivalibs imports
    (numbers, random, bufferdata) or inline-WGSL workarounds.
14. Drop `AppConfig { ... }.start()` — `Painter.init` + `animate` handle it.

---

## 14. Glossary

- **Attribs** — named tuple type describing vertex attribute layout.
  Compile-time reflection builds the `GPUVertexBufferLayout`.
- **BindPair** — `BindPair[N, V]` produced by `"name" := value`; consumed by
  `Shape.bind` / `Layer.bind` / `Panel.bind` / `Instance.bind`.
- **Bindable** — trait in [shape.scala](../../src/graphics/painter/shape.scala)
  implemented by `Shape`, `Layer`, `Instance`. Provides the 8 overloads of
  `bind(...)`.
- **Form** — `graphics.painter.Form` — vertex buffer + topology + front face.
- **FragmentPanel** — marker type in the uniform named tuple signalling a
  panel-texture binding readable in fragment stage only.
- **FragmentUniform[T] / VertexUniform[T]** — wrappers in
  [shader/types.scala](../../src/graphics/shader/types.scala) that encode
  shader-stage visibility on a uniform field. Bare `T` defaults to
  vertex+fragment.
- **Layer** (Scala) — Rust's `Effect`. Fullscreen fragment pass attached to a
  `Panel`. File: [layer.scala](../../src/graphics/painter/layer.scala).
- **Panel** (Scala) — Rust's `Layer`. Render target holding shapes + layers.
  File: [panel.scala](../../src/graphics/painter/panel.scala).
- **PanelBinding** — wraps a `Panel` with optional `index`, `mipLevel`, or
  `depth = true` to pick which texture view is bound.
- **Shade** — `graphics.painter.Shade[U, P]`. Contains the shader module, vertex
  buffer layout, and bind-group / pipeline layouts.
- **StructArray / StructRef** — zero-cost typed binary buffers from
  [trivalibs bufferdata](../../trivalibs/src/utils/bufferdata.scala).
- **WgslFn** — `WgslFn[Params, Return]` — a typed reference to a WGSL helper
  function; register once via `program.fn(f)` then call it in a `.vert` /
  `.frag` block.
