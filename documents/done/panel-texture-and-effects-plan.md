# Panel Texture Bindings & Post-Processing Effects — Implementation Plan

Two sequential features, each targeting a specific Rust example port. Feature 1
must be completed before Feature 2, as Feature 2 needs the texture
infrastructure from Feature 1.

---

## Table of Contents

1. [Texture & Sampler API Reference](#1-texture--sampler-api-reference)
2. [Feature 1: Panel Texture Bindings + MSAA (panel_tex draft)](#2-feature-1-panel-texture-bindings--msaa)
   2b. [Feature 1b: Depth Buffer](#2b-feature-1b-depth-buffer)
3. [Feature 2: Post-Processing Stack / Ping-Pong (blur port)](#3-feature-2-post-processing-stack--ping-pong)
4. [Open Questions](#4-open-questions)
5. [Implementation Status](#5-implementation-status)

---

## 1. Texture & Sampler API Reference

### 1.1 The Two GPU Resource Types

`Texture2D` and `Sampler` are unified GPU resource types — they serve double
duty as both the **type used in declarations** (shader contracts, `WgslFn`
parameter types) and the **DSL expression type** returned from context accessors
(`ctx.textures.*`, `ctx.bindings.*`). Unlike `Vec2`, `Float`, etc., they have no
CPU-side representation.

At runtime both are opaque string-wrapping types (like all `*Expr` types), where
`toString` yields the WGSL variable name.
`WGSLType[Texture2D].wgslName = "texture_2d<f32>"`,
`WGSLType[Sampler].wgslName = "sampler"`.

### 1.2 Sampler Bindings (Group 0)

Samplers live in group 0 alongside buffer uniforms. Declare them in `U` using
the standard visibility wrappers:

```scala
type Uniforms = (
  mvp:        VertexUniform[Mat4],
  texSampler: FragmentUniform[Sampler],  // group 0, slot 1
)
```

The `Painter` provides two shared, lazily created samplers:

```scala
painter.samplerNearest   // GPUSampler — nearest-neighbour filtering
painter.samplerLinear    // GPUSampler — bilinear filtering
```

Bind them to a shape or layer by name, exactly like any other uniform:

```scala
val myShape = painter.shape(shade, form)
  .bind(
    "mvp"        := mvpBinding,
    "texSampler" := painter.samplerNearest,
  )
```

`BindingSlots` accepts both `BufferBinding[?, ?]` and `GPUSampler` entries.

### 1.3 Panel Texture Declarations (Group 1)

A shape or layer that samples another panel's output declares it in a `P` named
tuple. Use three visibility wrappers to indicate which shader stage sees the
texture:

```scala
sealed trait FragmentPanel  // texture_2d visible in fragment shader
sealed trait VertexPanel    // texture_2d visible in vertex shader
sealed trait SharedPanel    // texture_2d visible in both shaders
```

```scala
type Panels = (
  colorTex: FragmentPanel,   // @group(1) @binding(0) var colorTex: texture_2d<f32>
  depthTex: FragmentPanel,   // @group(1) @binding(1) var depthTex: texture_2d<f32>
)
```

Pass `P` as the second type argument to `shade`:

```scala
val texShade = painter.shade[Attribs, Varyings, Uniforms, Panels]: program =>
  ...
```

Panel bindings are passed to the same `bind` method as uniforms and samplers —
field names in `U` and `P` are always distinct, so there is no collision.
`processEntry` dispatches at compile time: names found in `U` go to group 0
binding slots, names found in `P` go to the panel binding array:

```scala
val myShape = painter.shape(texShade, form)
  .bind(
    "mvp"        := mvpBinding,
    "texSampler" := painter.samplerNearest,
    "colorTex"   := trianglePanel,   // Panel — dispatched to panelBindings
    "depthTex"   := quadPanel,
  )
```

### 1.4 Accessing Textures and Samplers in the DSL

`ctx.textures` gives typed access to the `P` panel bindings. Each field returns
a `Texture2D` expression:

```scala
ctx.textures.colorTex   // Texture2D
```

`ctx.bindings` returns `Sampler` for any uniform field typed
`*Uniform[Sampler]`:

```scala
ctx.bindings.texSampler   // Sampler
```

### 1.5 Texture2D Methods

`Texture2D` exposes sampling as dot-methods, matching the project's existing DSL
style (`vec.mix(other, t)`, etc.):

```scala
// Primary method — explicit name
tex.sample(uv: Vec2Expr, sampler: Sampler): Vec4Expr
// Emits: textureSample(tex, sampler, uv)

// Shorthand alias — enables tex(uv, sampler) call syntax
tex.apply(uv: Vec2Expr, sampler: Sampler): Vec4Expr
// Same output as .sample()

// Future methods (planned, not in Feature 1):
tex.sampleLevel(uv: Vec2Expr, sampler: Sampler, lod: FloatExpr): Vec4Expr
// Emits: textureSampleLevel(tex, sampler, uv, lod)

tex.sampleGrad(uv: Vec2Expr, sampler: Sampler, ddx: Vec2Expr, ddy: Vec2Expr): Vec4Expr
// Emits: textureSampleGrad(tex, sampler, uv, ddx, ddy)

tex.sampleBias(uv: Vec2Expr, sampler: Sampler, bias: FloatExpr): Vec4Expr
// Emits: textureSampleBias(tex, sampler, uv, bias)
```

### 1.6 Sampling in a Fragment Shader

```scala
type Attribs  = (position: Vec3, uv: Vec2)
type Varyings = (uv: Vec2)
type Uniforms = (mvp: VertexUniform[Mat4], texSampler: FragmentUniform[Sampler])
type Panels   = (colorTex: FragmentPanel)

val texShade = painter.shade[Attribs, Varyings, Uniforms, Panels]: program =>
  program.vert: ctx =>
    Block(
      ctx.out.uv       := ctx.in.uv,
      ctx.out.position := ctx.bindings.mvp * vec4(ctx.in.position, 1.0),
    )
  program.frag: ctx =>
    // Explicit:
    ctx.out.color := ctx.textures.colorTex.sample(ctx.in.uv, ctx.bindings.texSampler)
    // Shorthand:
    ctx.out.color := ctx.textures.colorTex(ctx.in.uv, ctx.bindings.texSampler)
    // Both emit: out.color = textureSample(colorTex, texSampler, in.uv);
```

### 1.7 WgslFn with Texture2D and Sampler Parameters

`Texture2D` and `Sampler` are valid `WgslFn` parameter types. The generated WGSL
function signature uses `texture_2d<f32>` and `sampler` respectively.

**`WgslFn.raw`** — raw WGSL body, typed Scala call site:

```scala
val gaussianBlur9: WgslFn[(tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, dir: Vec2), Vec4] =
  WgslFn.raw("gaussian_blur_9"):
    """
    var color = vec4(0.0);
    let off1 = vec2<f32>(1.3846153846) * dir / res;
    let off2 = vec2<f32>(3.2307692308) * dir / res;
    color += textureSample(tex, s, uv)        * 0.2270270270;
    color += textureSample(tex, s, uv + off1) * 0.3162162162;
    color += textureSample(tex, s, uv - off1) * 0.3162162162;
    color += textureSample(tex, s, uv + off2) * 0.0702702703;
    color += textureSample(tex, s, uv - off2) * 0.0702702703;
    return color;
    """
// Generates:
// fn gaussian_blur_9(tex: texture_2d<f32>, s: sampler, uv: vec2<f32>,
//                    res: vec2<f32>, dir: vec2<f32>) -> vec4<f32> { ... }
```

**`WgslFn.dsl`** — `params.tex` IS a `Texture2D`, has `.sample()` directly:

```scala
val sampleAt: WgslFn[(tex: Texture2D, s: Sampler, uv: Vec2), Vec4] =
  WgslFn.dsl("sample_at"): (p, ret) =>
    ret(p.tex.sample(p.uv, p.s))
// Generates:
// fn sample_at(tex: texture_2d<f32>, s: sampler, uv: vec2<f32>) -> vec4<f32> {
//   return textureSample(tex, s, uv);
// }
```

**Calling a texture WgslFn from a shader** — `Texture2D` and `Sampler` slot
directly into the typed argument positions:

```scala
program.fn(gaussianBlur9)
program.frag: ctx =>
  ctx.out.color := gaussianBlur9(
    ctx.textures.source,
    ctx.bindings.blurSampler,
    ctx.in.uv,
    ctx.bindings.resolution,
    ctx.bindings.dir * ctx.bindings.diameter,
  )
```

### 1.8 MSAA Panels

Enable 4× MSAA on a panel with `multisample = true`. The painter renders into
the MSAA texture and resolves to the regular texture automatically. When another
shape samples the resolved panel, it reads a plain `texture_2d<f32>` — no WGSL
changes needed on the consumer side:

```scala
val msaaPanel = painter.panel().set(
  width = 800, height = 800,
  clearColor = (0.0, 1.0, 0.0, 1.0),
  shapes = Arr(colorShape),
  multisample = true,
)
// msaaPanel.textureView is the resolved output — safe to bind in group 1
```

### 1.9 Post-Processing Layers

For layers in a post-processing stack, `P` is declared the same way as for
shapes. When a layer does **not** manually bind slot 0, the painter
automatically injects the previous render result there — enabling the ping-pong
chain. If a layer **does** bind slot 0 explicitly, that binding is respected and
no ping-pong swap occurs for that layer:

```scala
type LayerUniforms = (blurSampler: FragmentUniform[Sampler], ...)
type LayerPanels   = (source: FragmentPanel)   // slot 0 = auto-filled when unbound

val blurShade = painter.layerShade[LayerUniforms, LayerPanels]: program =>
  program.fn(gaussianBlur9)
  program.frag: ctx =>
    ctx.out.color := gaussianBlur9(
      ctx.textures.source,         // auto-filled with previous render result
      ctx.bindings.blurSampler,
      ctx.in.uv,
      ctx.bindings.resolution,
      ctx.bindings.dir * ctx.bindings.diameter,
    )

// Omit "source" from bind — painter injects the ping-pong source automatically:
val blurLayer = painter.layer(blurShade)
  .bind("blurSampler" := painter.samplerLinear)

// Override slot 0 manually — painter respects it, no ping-pong swap for this layer:
val fixedLayer = painter.layer(blurShade)
  .bind("source" := someFixedPanel, "blurSampler" := painter.samplerLinear)
```

Layers without `P` (procedural effects, plasma, etc.) are unaffected — rendered
in-place with no ping-pong:

```scala
val plasmaShade = painter.layerShade[PlasmaUniforms]: program =>   // P = EmptyTuple
  program.frag: ctx =>
    ctx.out.color := ...
```

---

## 2. Feature 1: Panel Texture Bindings + MSAA

**Goal**: Allow shapes (and layers) to sample another Panel's render output as a
texture. Also implement MSAA on panels so the Rust `layer_tex` example can be
fully ported. The target draft is `drafts/panel_tex/`: two intermediate panels
render colored shapes, a third panel's shapes sample those textures.

### 2.1 What the Rust Example Does

`layer_tex` creates:

- `color_triangle_layer` — red triangle on yellow background, 800×800 fixed size
- `color_quad_layer` — blue quad on green background, 800×800, **with MSAA**
- `canvas` layer — two shapes each sampling one of the above panels:
  - A quad sampling `color_triangle_layer` (nearest sampler)
  - A triangle sampling `color_quad_layer` (linear sampler)

Key patterns in the Rust source:

```rust
// Shader declares a layer binding in group 1
let tex_shade = p
  .shade([Float32x3, Float32x2])
  .with_bindings([BINDING_BUFFER_VERT, BINDING_SAMPLER_FRAG])  // group 0
  .with_layers([BINDING_LAYER_FRAG])                            // group 1
  .create();

// Shape binds a panel texture at group 1, slot 0
let tex_quad_shape = p.shape(tex_shade, quad_form)
  .with_bindings(map! {
    0 => tex_quad_mvp.binding(),
    1 => nearest_sampler.binding(),
  })
  .with_layers(map! { 0 => color_triangle_layer.binding() })
  .create();

// Layer with MSAA enabled
let color_quad_layer = p.layer()
  .with_shape(color_quad_shape)
  .with_multisampling()
  .create();
```

### 2.2 Scala Design

#### 2.2.1 Group 0: Sampler Bindings

`Sampler` serves as both the uniform contract type (in `U` with visibility
wrappers) and the DSL expression type. Each uniform field already gets its own
declaration (the existing generator emits one `var<uniform>` line per field).
Sampler fields use a different address space — `var name: sampler;` instead of
`var<uniform> name: type;`:

```wgsl
@group(0) @binding(0) var<uniform> mvp: mat4x4<f32>;
@group(0) @binding(1) var texSampler: sampler;
```

`WGSLType[Sampler]`: `wgslName = "sampler"`, no `byteSize`/`alignment`. The
generator checks the field type and emits the sampler variant (no address space
qualifier). The bind group layout entry uses `sampler = Obj.literal()` instead
of `buffer = Obj.literal(type = "uniform")`.

At runtime, `BindingSlots` accepts both `BufferBinding[?, ?]` and `GPUSampler`.
`createBindGroup` dispatches on entry type.

**Painter sampler factories** (lazy, shared):

```scala
lazy val samplerNearest: GPUSampler
lazy val samplerLinear: GPUSampler
```

#### 2.2.2 Group 1: Panel Texture Declarations

A new type parameter `P` on `Shade` represents panel textures in group 1. `P` is
a named tuple of `FragmentPanel` / `VertexPanel` / `SharedPanel` fields.

`Shade[U, P = EmptyTuple]` gains `panelBindGroupLayout`:

```scala
class Shade[U, P = EmptyTuple](
  val id: Int,
  val shaderModule: GPUShaderModule,
  val vertexBufferLayout: js.Dynamic | Null,
  val valueBindGroupLayout: GPUBindGroupLayout | Null,   // group 0
  val panelBindGroupLayout: GPUBindGroupLayout | Null,   // group 1 (new)
  val pipelineLayout: GPUPipelineLayout,
  val isLayer: Boolean,
)
```

`derive.scala` additions:

1. `generatePanelDeclarations[P]: String` — emits
   `@group(1) @binding(N) var name: texture_2d<f32>;` for each field in `P`
2. `createPanelBindGroupLayout[P](device): GPUBindGroupLayout` — one `texture`
   entry per field with visibility from the wrapper type

`Texture2D` serves both as the declared field type in `WgslFn` params and as the
DSL expression returned by `ctx.textures.*`.
`WGSLType[Texture2D].wgslName = "texture_2d<f32>"`.

#### 2.2.3 Shape[U, P] and Layer[U, P]

Both classes gain `panelBindings: Arr[Panel | Null]` as internal storage. There
is no separate `bindPanels` method — panel bindings go through the same `bind`
method as uniforms and samplers. `processEntry` resolves the name at compile
time: if found in `U`, it routes to `bindings`; if found in `P`, it routes to
`panelBindings`. A name absent from both is a compile error.

Constructor takes `(painter, shade, form)` — all other properties are `var`s
configured via `set()`:

```scala
class Shape[U, P](
    val painter: Painter,
    val shade: Shade[U, P],
    val form: Form,
) extends Bindable[U, P]:
  var cullMode: CullMode = CullMode.None
  var blendState: Opt[BlendState] = null
  var bindings: BindingSlots = Arr()
  var panelBindings: Arr[Panel | Null] = Arr()

  def set(
      cullMode: Maybe[CullMode] = Maybe.Not,
      blendState: Maybe[Opt[BlendState]] = Maybe.Not,
  ): this.type =
    cullMode.foreach(v => this.cullMode = v)
    blendState.foreach(v => this.blendState = v)
    this
```

`Layer[U, P]` — takes `(painter, shade)`, same `set()` pattern for blendState.

The `processEntry` dispatch (in `Bindable[U, P]` trait):

```scala
protected inline def processEntry[N <: String & Singleton, V](
    pair: BindPair[N, V],
): Unit =
  inline if derive.containsName[N, U] then
    // uniform / sampler path (existing)
    ...
  else inline if derive.containsName[N, P] then
    // panel path — V must be Panel
    inline pair.value match
      case p: Panel =>
        val idx = shade.panelIndices(pair.name)
        while panelBindings.length <= idx do panelBindings.push(null)
        panelBindings(idx) = p
      case _ => error("Panel binding must be a Panel instance")
  else
    error("Name not found in uniforms or panel bindings")
```

#### 2.2.4 Painter Factory Changes

Factories are thin wrappers — all setup lives in entity `set()` methods:

```scala
def form(): Form = Form(this)
def shape[U, P](shade: Shade[U, P], form: Form): Shape[U, P] = Shape(this, shade, form)
def layer[U, P](shade: Shade[U, P]): Layer[U, P] = Layer(this, shade)
def panel(): Panel = Panel(this)
```

Shade factories remain inline:

```scala
inline def shade[A, V, U, P](build: Program[A, V, U, P] => Unit): Shade[U, P]
inline def layerShade[U, P](build: LayerProgram[U, P] => Unit): Shade[U, P]
```

The 4-param `shade[A, V, U, P]` overload generates panel WGSL declarations via
`generatePanelDeclarations[P]` and includes the panel BGL in the pipeline layout
when `P != EmptyTuple`.

#### 2.2.5 Render Path Changes

`renderShapeOnPass` — set group 1 when panelBindings non-empty:

```scala
if shape.panelBindings.length > 0 && shape.shade.panelBindGroupLayout != null then
  val panelGroup = device.createBindGroup(Obj.literal(
    layout = shape.shade.panelBindGroupLayout,
    entries = buildPanelEntries(shape.panelBindings),
  ))
  pass.setBindGroup(1, panelGroup)
```

`buildPanelEntries` maps each non-null slot to the panel's `textureView`.
`renderLayerOnPass` same.

#### 2.2.6 MSAA Support

**Panel changes** — `multisample` is a `var` configured via `set()`:

```scala
class Panel(val painter: Painter):
  var multisample: Boolean = false
  // ... other vars ...

  private var _msaaTexture: Opt[GPUTexture] = null
  private var _msaaView: Opt[GPUTextureView] = null

  def set(
      // ... existing params ...
      multisample: Maybe[Boolean] = Maybe.Not,
  ): this.type =
    // ...
    multisample.foreach(v => this.multisample = v)
    this
```

`ensureSize` allocates a 4× MSAA texture when `multisample = true`. The render
pass targets the MSAA view and resolves into the regular texture view:

```scala
Obj.literal(
  view = panel.msaaView,
  resolveTarget = panel.textureView,
  loadOp = "clear",
  storeOp = "discard",
  clearValue = ...,
)
```

Pipelines for shapes/layers rendered into an MSAA panel include
`multisample = Obj.literal(count = 4)`. The pipeline cache key includes the
sample count.

#### 2.2.7 DSL Context: ctx.textures

`FragmentCtx` and `VertexCtx` gain a `textures: TypedPanelAccessor[P]` field.
`TypedPanelAccessor[P]` wraps each field of `P` as a `Texture2D` keyed by name.

`UniformToExpr` gains `Sampler` cases:

```scala
type UniformToExpr[T] = T match
  case VertexUniform[Sampler]   => Sampler
  case FragmentUniform[Sampler] => Sampler
  case SharedUniform[Sampler]   => Sampler
  case VertexUniform[t]         => ToExpr[t]
  // ... existing cases
```

`ToExpr` gains self-mapping cases:

```scala
type ToExpr[T] = T match
  case Sampler   => Sampler
  case Texture2D => Texture2D
  case Float     => FloatExpr
  // ... existing cases
```

When `P = EmptyTuple`, `textures` is a zero-field accessor — no change to
existing shaders.

#### 2.2.8 Backward Compatibility

`Shade[U, P = EmptyTuple]`, `Shape[U, P = EmptyTuple]`,
`Layer[U, P = EmptyTuple]` — default type parameters mean all existing code
compiles unchanged.

### 2.3 Implementation Steps

**Step 1 — Texture2D + Sampler types + BindingSlots**

- Add `opaque type Texture2D <: Expr = Expr` to `expr.scala` with `.sample()`,
  `.apply()` extension methods
- Add `opaque type Sampler <: Expr = Expr` to `expr.scala`
- Add `WGSLType[Texture2D]` (`wgslName = "texture_2d<f32>"`) and
  `WGSLType[Sampler]` (`wgslName = "sampler"`) to `types.scala`
- Add `ToExpr[Texture2D] = Texture2D` and `ToExpr[Sampler] = Sampler` to
  `dsl/types.scala`
- Add `UniformToExpr` cases for `*Uniform[Sampler]` to `dsl/types.scala`
- Update `derive.scala` / `generateUniforms[U]`: split fields into buffer fields
  (struct) and sampler fields (standalone `var`), consistent binding indices
- Update `layouts.scala` / `bindGroupEntries` to emit `sampler = Obj.literal()`
  for sampler fields
- Expand `BindingSlots` / `BindingEntry` to accept `GPUSampler`
- Update `createBindGroup` in `painter.scala` for sampler entries
- Add `samplerNearest` and `samplerLinear` lazy vals to `Painter`

**Step 2 — Panel bind group (group 1)**

- Add `sealed trait FragmentPanel`, `VertexPanel`, `SharedPanel` to
  `types.scala`
- Add `generatePanelDeclarations[P]` to `derive.scala`
- Add `createPanelBindGroupLayout[P]` to `layouts.scala`
- Update `ShaderDef.generateWGSL` to append panel declarations
- Update `Shade[U, P = EmptyTuple]` with `panelBindGroupLayout` field
- Update `painter.shade[A, V, U, P]` for panel BGL
- Update pipeline layout creation to include group 1 BGL when `P != EmptyTuple`

**Step 3 — Shape / Layer / render path**

- `Shape[U, P]` takes `(painter, shade, form)` — all other props are `var`s with
  `set()` methods; `Bindable[U, P]` trait provides `val painter: Painter` and
  `inline def device` via painter
- `Layer[U, P]` takes `(painter, shade)` — same pattern
- `processEntry` dispatches via `inline if derive.containsName[N, U]` /
  `inline if derive.containsName[N, P]`
- Update `renderShapeOnPass` and `renderLayerOnPass` for group 1
- Add `TypedPanelAccessor[P]` to DSL
- Add `textures: TypedPanelAccessor[P]` to `FragmentCtx`, `VertexCtx`
- Update `Program.frag[L]`, `Program.vert[L]`, `LayerProgram.frag[L]`

**Step 4 — MSAA**

- Add `var multisample: Boolean = false` to `Panel`, settable via
  `panel.set(multisample = true)`
- `Panel.ensureSize` allocates MSAA texture (`Opt[GPUTexture]`) when
  `multisample = true`
- `paint(panel)`: MSAA attachment format when `panel.multisample`
- `createPipeline` / `createLayerPipeline`: `multisample = { count: 4 }` + cache
  key update

**Step 5 — panel_tex draft**

- Create `drafts/panel_tex/`
- `trianglePanel` — red triangle (Mat4 MVP uniform), yellow clear, 800×800
- `quadPanel` — blue quad (Mat4 MVP uniform), green clear, 800×800, MSAA
- `canvasPanel` — two shapes using `texShade`:
  - Quad sampling `trianglePanel`, nearest sampler
  - Triangle sampling `quadPanel`, linear sampler
- Rotating transforms via `animate`
- `setInterval` every 2s to toggle panel sizes and swap clear colors

### 2.4 WGSL Generated (texture shade)

```wgsl
struct VertexInput  { @location(0) position: vec3<f32>, @location(1) uv: vec2<f32> }
struct VertexOutput { @builtin(position) position: vec4<f32>, @location(0) uv: vec2<f32> }
struct FragmentOutput { @location(0) color: vec4<f32> }

@group(0) @binding(0) var<uniform> mvp: mat4x4<f32>;
@group(0) @binding(1) var texSampler: sampler;

@group(1) @binding(0) var colorTex: texture_2d<f32>;

@vertex fn vs_main(in: VertexInput) -> VertexOutput { ... }
@fragment fn fs_main(in: VertexOutput) -> FragmentOutput {
  var out: FragmentOutput;
  out.color = textureSample(colorTex, texSampler, in.uv);
  return out;
}
```

---

## 2b. Feature 1b: Depth Buffer

**Goal**: Allow panels to render 3D geometry with correct depth sorting.
Required for the `panel_tex` 3D upgrade (shapes with perspective projection and
Y-axis rotation will overlap and need depth testing). Depth is orthogonal to
both texture sampling (Feature 1) and post-processing (Feature 2) — it is a
property of the render target, not the shader.

**Prerequisite**: Feature 1 (panel texture sampling) should be stable first, but
Feature 1b can be implemented independently of Feature 2.

### 2b.1 Design

`Panel` gains an optional depth texture. When `depthTest = true` the painter
allocates a matching `depth24plus` texture alongside the color texture and
attaches it to the render pass. Shapes and layers rendered into that panel
automatically use a depth-testing pipeline.

```scala
class Panel(val painter: Painter):
  var depthTest: Boolean = false   // allocate + attach depth texture
  // ...
  private var _depthTexture: Opt[GPUTexture] = null
  private var _depthView: Opt[GPUTextureView] = null
```

No WGSL changes needed — depth testing is fully handled by the pipeline and
render pass descriptors, not the shader.

### 2b.2 Implementation Steps

**Step 1 — Panel depth texture allocation**

- `Panel.depthTest` is a `var` (default `false`), settable via
  `panel.set(depthTest = true)`
- In `Panel.ensureSize`: when `depthTest = true`, create a `depth24plus` texture
  at the same size as the color texture; store as `_depthTexture` / `_depthView`
- On resize, destroy the old depth texture before allocating the new one

**Step 2 — Render pass depth attachment**

- In `paint(panel)` / `beginRenderPass`: when `panel.depthTest`, attach
  `depthStencilAttachment`:
  ```js
  {
    view:           panel.depthView,
    depthLoadOp:    "clear",
    depthStoreOp:   "store",
    depthClearValue: 1.0,
  }
  ```

**Step 3 — Pipeline depth state**

- In `createPipeline` / `createLayerPipeline`: when the target panel has
  `depthTest = true`, include:
  ```js
  depthStencil: {
    format:            "depth24plus",
    depthWriteEnabled: true,
    depthCompare:      "less",
  }
  ```
- The pipeline cache key gains a `|depth` suffix to keep depth-enabled pipelines
  separate from non-depth ones for the same shade.
- The panel must be known at pipeline-creation time. Pass a `depthTest: Boolean`
  flag into `renderShapeOnPass` / `renderLayerOnPass` so it can be forwarded to
  the pipeline cache lookup.

**Step 4 — panel_tex 3D draft**

- Enable `depthTest = true` on the two intermediate panels (`trianglePanel`,
  `quadPanel`) once their shapes use perspective projection
- The canvas panel (flat textured quads/triangles) does not need depth testing
- Update `panel_tex` draft to use `PerspectiveCamera` + `Transform` once the
  `graphics.scene` layer is implemented

### 2b.3 WGSL Impact

None. Depth is implicit — the rasterizer writes and tests fragment depth
automatically when a `depthStencil` attachment is present in the pipeline. The
shader does not need to output a depth value unless overriding the default (not
needed here).

### 2b.4 Implementation Status

✅ Done. Steps 1–3 implemented in `src/graphics/painter/panel.scala` and
`src/graphics/painter/painter.scala`:

- `Panel.depthTest` is a `var` (default `false`), settable via `set()`;
  `ensureSize` allocates/destroys a `depth24plus` texture alongside the color
  texture on resize
- `paint()` attaches `depthStencilAttachment` (clear to 1.0, store) when
  `panel.depthTest`
- `createPipeline` / `createLayerPipeline` conditionally include
  `depthStencil: { format: depth24plus, depthWriteEnabled: true, depthCompare: less }`
- Pipeline cache key includes `|$depthTest` suffix to keep depth/non-depth
  variants separate

Step 4 (`panel_tex` 3D upgrade) done — draft uses `PerspectiveCamera` +
`Transform` with 3D geometry.

---

## 3. Feature 2: Post-Processing Stack / Ping-Pong

**Goal**: When a Panel has layers, each layer in the stack reads from the
previous render result via ping-pong render targets. The target draft is
`drafts/blur/`: a triangle rendered to a panel, then Gaussian blur applied in
multiple passes as layers.

**Prerequisite**: Feature 1 must be implemented first. Feature 2 reuses the
`Layer[U, P]` / `Shade[U, P]` / `ctx.textures` infrastructure directly — no
separate machinery.

### 3.1 What the Rust Example Does

In Rust, "Effect" is the fullscreen post-processing pass (= our Scala "Layer").
The blur example chains multiple effect instances on a panel, each reading from
the previous result. The Rust painter auto-injects the source as a fixed group 1
binding.

In Scala we take a more explicit approach: the layer shader declares its source
texture as part of its `P` type just like any other panel texture. What the
painter automates is **injecting the previous render result at slot 0 of group 1
only when slot 0 has not been manually bound** on that layer. If a layer does
set slot 0 via `layer.bind("source" := somePanel)`, that binding is respected —
the painter skips the ping-pong swap for that layer entirely and renders it
in-place using its explicit source.

### 3.2 Scala Design

#### 3.2.1 Layer[U, P] — Same Structure as Shape[U, P]

`Layer[U, P]` gains the `P` type parameter from Feature 1, exactly mirroring
`Shape[U, P]`. `layerShade[U, P]` becomes a direct parallel of
`shade[A, V, U, P]`:

```scala
def layer[U, P](shade: Shade[U, P]): Layer[U, P] = Layer(this, shade)

inline def layerShade[U, P](build: LayerProgram[U, P] => Unit): Shade[U, P]
```

For layers with `P = EmptyTuple` (e.g. plasma), no panel bind group is created
and no ping-pong source is injected.

#### 3.2.2 LayerProgram[U, P] — ctx.textures

`LayerProgram[U, P]` gains the same `textures` accessor as `FragmentCtx`:

```scala
class LayerFragCtx[U, L, P](
  in:       TypedExprAccessor[...],
  out:      TypedAssignAccessor[...],
  bindings: TypedExprAccessor[...],
  locals:   TypedLocalAccessor[...],
  textures: TypedPanelAccessor[P],
)
```

Blur shader example — slot 0 (`source`) is auto-filled by the painter when not
manually bound:

```scala
type LayerUniforms = (
  diameter:    FragmentUniform[Float],
  resolution:  FragmentUniform[Vec2],
  dir:         FragmentUniform[Vec2],
  blurSampler: FragmentUniform[Sampler],
)
type LayerPanels = (source: FragmentPanel)   // slot 0 = auto-filled when unbound

val blurShade = painter.layerShade[LayerUniforms, LayerPanels]: program =>
  program.fn(gaussianBlur9)
  program.frag: ctx =>
    ctx.out.color := gaussianBlur9(
      ctx.textures.source,
      ctx.bindings.blurSampler,
      ctx.in.uv,
      ctx.bindings.resolution,
      ctx.bindings.dir * ctx.bindings.diameter,
    )
```

#### 3.2.3 Ping-Pong: Painter Injects Slot 0 Only When Unbound

During `paint(panel)`, for each layer with `panelBindGroupLayout != null` the
painter checks whether slot 0 has been manually bound:

- **Slot 0 is empty** (`layer.panelBindings(0) == null`): inject `srcView` as
  slot 0, render to `dstView`, then swap `srcView ↔ dstView` (normal ping-pong).
- **Slot 0 is manually bound**: respect it — render the layer in-place (write
  back to `srcView`), no swap.

```scala
val hasPanelBindings = layer.shade.panelBindGroupLayout != null
if hasPanelBindings then
  val slot0Manual = layer.panelBindings.length > 0 && layer.panelBindings(0) != null
  val entries = Arr[js.Dynamic]()
  if slot0Manual then
    entries.push(Obj.literal(binding = 0, resource = layer.panelBindings(0).textureView))
  else
    entries.push(Obj.literal(binding = 0, resource = srcView))  // ping-pong injection
  var k = 1
  while k < layer.panelBindings.length do
    val p = layer.panelBindings(k)
    if p != null then
      entries.push(Obj.literal(binding = k, resource = p.textureView))
    k += 1
  val panelGroup = device.createBindGroup(Obj.literal(
    layout = layer.shade.panelBindGroupLayout,
    entries = entries,
  ))
  pass.setBindGroup(1, panelGroup)
  (slot0Manual)  // returned: true = in-place, false = ping-pong
```

#### 3.2.4 Ping-Pong Textures on Panel

A second pong texture is allocated when any layer has `P != EmptyTuple`:

```
Main texture:  shapes render here (ping₀)
Pong texture:  layer 0 reads from main,  writes to pong
Main texture:  layer 1 reads from pong,  writes to main
...
Final output:  whichever texture holds the last write
```

Panel gains `_pongTexture: Opt[GPUTexture]`, `_pongView: Opt[GPUTextureView]`,
`_outputView: Opt[GPUTextureView]`. `ensureSize` allocates the pong pair when
needed. `panel.outputView` falls back to `panel.textureView` when no ping-pong
occurred.

#### 3.2.5 paint() Changes

```scala
def paint(panel: Panel): Unit =
  // Step 1: shapes into main texture
  val encoder = device.createCommandEncoder()
  val mainPass = beginRenderPass(encoder, panel.textureView, panel.clearColor)
  var i = 0
  while i < panel.shapes.length do
    renderShapeOnPass(mainPass, panel.shapes(i)); i += 1
  mainPass.end(); queue.submit(Arr(encoder.finish()))

  // Step 2: layer ping-pong chain
  var srcView = panel.textureView
  var dstView = panel.pongView
  var j = 0
  while j < panel.layers.length do
    val layer = panel.layers(j)
    val hasPanels = layer.shade.panelBindGroupLayout != null
    val slot0Manual = hasPanels &&
      layer.panelBindings.length > 0 && layer.panelBindings(0) != null
    val enc2 = device.createCommandEncoder()
    if hasPanels && !slot0Manual then
      // Normal ping-pong: inject srcView at slot 0, write to dstView, then swap
      val layerPass = beginRenderPassNoClear(enc2, dstView)
      renderLayerOnPass(layerPass, layer, srcView)
      layerPass.end(); queue.submit(Arr(enc2.finish()))
      val tmp = srcView; srcView = dstView; dstView = tmp
    else
      // In-place: either no panel bindings, or slot 0 manually bound — no swap
      val layerPass = beginRenderPassNoClear(enc2, srcView)
      renderLayerOnPass(layerPass, layer, null)
      layerPass.end(); queue.submit(Arr(enc2.finish()))
    j += 1
  panel.setOutputView(srcView)
```

`show(panel)` uses `panel.outputView` instead of `panel.textureView`.

#### 3.2.6 Backward Compatibility

- `layerShade[U]` (no `P`) unchanged — returns `Shade[U, EmptyTuple]`
- Existing `panel_layer` draft: plasma uses `P = EmptyTuple`, rendered in-place
- All other drafts: no layers, completely unaffected

### 3.3 Implementation Steps

**Step 1 — Layer[U, P] + layerShade[U, P]**

Already done as part of Feature 1 — `Layer[U, P]` takes `(painter, shade)`, has
`panelBindings: Arr[Panel | Null]`, `processEntry` dispatches across both `U`
and `P`. `layerShade[U, P]` generates panel declarations + layout.

**Step 2 — Ping-pong textures on Panel**

- Add `_pongTexture: Opt[GPUTexture]`, `_pongView: Opt[GPUTextureView]`,
  `_outputView: Opt[GPUTextureView]` to `Panel`
- `ensureSize`: allocate pong pair when any layer has a `panelBindGroupLayout`
- Add `pongView`, `outputView`, `setOutputView`

**Step 3 — paint() + show() changes**

- Restructure `paint(panel)` to ping-pong layers with panel bindings
- `renderLayerOnPass` overrides slot 0 with `srcView` when present
- Update `show(panel)` to use `panel.outputView`

**Step 4 — blur draft**

- Create `drafts/blur/`
- Triangle shape (UV-colored) rendered to a panel, blue clear
- Multiple `Layer` instances sharing `blurShade`, different `dir`/`diameter`
  bindings: logarithmic passes halved while `> 2.0`, horizontal then vertical
- `painter.onResize` updates the `resolution` binding
- `animate` loop with `paint` + `show`

### 3.4 WGSL Generated (blur layer shade)

```wgsl
@group(0) @binding(0) var<uniform> diameter: f32;
@group(0) @binding(1) var<uniform> resolution: vec2<f32>;
@group(0) @binding(2) var<uniform> dir: vec2<f32>;
@group(0) @binding(3) var blurSampler: sampler;

@group(1) @binding(0) var source: texture_2d<f32>;

fn gaussian_blur_9(tex: texture_2d<f32>, s: sampler,
                   uv: vec2<f32>, res: vec2<f32>, dir: vec2<f32>) -> vec4<f32> { ... }

@vertex fn vs_main(...) -> ... { ... }
@fragment fn fs_main(in: ...) -> FragmentOutput {
  var out: FragmentOutput;
  out.color = gaussian_blur_9(source, blurSampler, in.uv,
                               values.resolution, values.dir * values.diameter);
  return out;
}
```

### 3.5 Gaussian Blur Helper

```scala
val gaussianBlur9: WgslFn[(tex: Texture2D, s: Sampler, uv: Vec2, res: Vec2, dir: Vec2), Vec4] =
  WgslFn.raw("gaussian_blur_9"):
    """
    var color = vec4(0.0);
    let off1 = vec2<f32>(1.3846153846) * dir / res;
    let off2 = vec2<f32>(3.2307692308) * dir / res;
    color += textureSample(tex, s, uv)        * 0.2270270270;
    color += textureSample(tex, s, uv + off1) * 0.3162162162;
    color += textureSample(tex, s, uv - off1) * 0.3162162162;
    color += textureSample(tex, s, uv + off2) * 0.0702702703;
    color += textureSample(tex, s, uv - off2) * 0.0702702703;
    return color;
    """
```

---

## 4. Open Questions

1. **`Shade[U, P]` default type parameter and inline shade factories**: Confirm
   that `class Shade[U, P = EmptyTuple]` works cleanly with the inline methods
   in `painter.scala` (`inline erasedValue[P]` etc.). Default type parameters in
   `inline` + `erasedValue` context may require explicit attention.

2. **Sampler variant in `generateUniformGroupFromLists`**: The existing
   generator emits `var<uniform> name: type;` for every field. Sampler fields
   need `var name: sampler;` (no address space qualifier). The simplest fix is
   to check `WGSLType[T]` for a `isSampler` flag (or just match on
   `wgslName == "sampler"`) and emit the appropriate line. Binding indices stay
   consecutive across both kinds, so no index-consistency problem arises.

3. **MSAA + panel texture sampling**: When `color_quad_layer` uses MSAA, its
   resolved `textureView` (not the MSAA view) is what gets bound in group 1. The
   resolved view is always a regular `texture_2d<f32>`, so no WGSL changes
   needed for the sampling shader.

4. **Pipeline cache key for MSAA**: The MSAA sample count must be part of the
   pipeline key. Currently the key string includes shade ID, blend state, cull
   mode, topology, front face, and format. Add `|${panel.sampleCount}` to
   distinguish MSAA vs non-MSAA pipelines for the same shade.

5. **Plasma example with Feature 2**: Plasma uses `P = EmptyTuple` layers, so
   they render in-place without ping-pong. No backward-compat issue. If the user
   later adds a `P`-typed layer after a plasma layer, the ping-pong path kicks
   in with the plasma's output as the source.

---

## 5. Implementation Status

### Feature 1 (Panel Texture Bindings)

**Step 1 — Texture2D + Sampler types** ✅ Done

- `opaque type Texture2D <: Expr` and `opaque type Sampler <: Expr` added to
  `expr.scala` with `.sample()` / `.apply()` extension methods
- `WGSLType[Texture2D]` and `WGSLType[Sampler]` added to `shader/types.scala`
- `ToExpr` and `UniformToExpr` cases added to `shader/dsl/types.scala`
- `derive.scala` / `generateUniformGroupFromLists` emits `var name: sampler;`
  for sampler fields (no address space qualifier) vs `var<uniform>` for buffers,
  using `inline erasedValue[head] match` (not a runtime `.isSampler` check)
- `layouts.scala` / `bindGroupEntriesImpl` emits `sampler = Obj.literal()`
  instead of `buffer = Obj.literal(type = "uniform")` for sampler entries
- `BindingSlots = Arr[BufferBinding[?,?] | GPUSampler | Null]`
- `painter.samplerNearest` and `painter.samplerLinear` lazy vals added

**Step 2 — Panel bind group (group 1)** ✅ Done

- `sealed trait FragmentPanel`, `VertexPanel`, `SharedPanel` added to
  `shader/types.scala`
- `derive.generatePanelDeclarations[P]`, `derive.panelFieldIndex[N, P]`,
  `derive.containsName[N, T]` added to `derive.scala`
- `layouts.createPanelBindGroupLayout[P]` added to `layouts.scala`
- `Shade[U, P]` gains `panelBindGroupLayout: GPUBindGroupLayout | Null`

  > **Note**: `class Shade[U, P = EmptyTuple]` is not supported in Scala 3.8.2
  > (class-level type parameter defaults are a parse error). The class is
  > `Shade[U, P]` with no default; all call sites specify both type params
  > explicitly.

- `painter.shade[A,V,U,P]` and `painter.layerShade[U,P]` overloads added;
  private helpers `shadeFromWgsl` / `layerShadeFromWgsl` generate panel
  declarations and include the panel BGL in the pipeline layout when `P` is
  non-empty

**Step 3 — Shape / Layer / render path** ✅ Done

- `Shape[U, P]` takes `(painter, shade, form)`, `Layer[U, P]` takes
  `(painter, shade)` — all other props are `var`s with `set()` methods
- `Bindable[U, P]` trait: `val painter: Painter`, `inline def device` via
  painter
- `processEntry` dispatches via `inline if derive.containsName[N, U]` /
  `inline if derive.containsName[N, P]` at compile time; `GPUSampler` arm added
  for group-0 sampler slots
- `TypedPanelAccessor[P]` with `Selectable` and `PanelToTexture2D` match type
  added to `shader/dsl/context.scala`
- `VertexCtx` and `FragmentCtx` gain `textures: TypedPanelAccessor[P]`
- `Program[A,V,U,P]` and `LayerProgram[U,P]` updated to pass the accessor
- `renderShapeOnPass` / `renderLayerOnPass` set group 1 when `panelBindings` is
  non-empty

**Step 4 — MSAA** ✅ Done

- `Panel.multisample` is a `var` (default `false`), settable via
  `panel.set(multisample = true)`; `Panel.sampleCount` returns 4 or 1
- `ensureSize` allocates 4× MSAA texture (`_msaaTexture`, `_msaaView`) when
  `multisample = true`; depth texture uses matching `sampleCount`
- `paint()` renders to `panel.msaaView` with `resolveTarget = panel.textureView`
  and `storeOp = "discard"` when MSAA is enabled
- `createPipeline` / `createLayerPipeline` include
  `multisample = { count: sampleCount }` when `sampleCount > 1`
- Pipeline cache keys include `sampleCount` to separate MSAA/non-MSAA variants
- `panel.textureView` is the resolved output — consumers sample it as a plain
  `texture_2d<f32>`, no WGSL changes needed

**Step 5 — panel_tex draft** ✅ Done

Draft at `drafts/panel_tex/PanelTex.scala` compiles cleanly and renders
correctly. Uses new API: `painter.form().set(vertices = ...)`,
`painter.shape(shade, form)`, `painter.panel().set(...)`.

### Open Questions — Resolved

- **Q1 (class type param defaults)**: `class Shade[U, P = EmptyTuple]` fails to
  parse in Scala 3.8.2. Resolution: no default; all factories specify `P`
  explicitly. Scala 3 only supports default type params on `type` aliases and
  `given`/`def` type params, not `class`.

- **Q2 (sampler in generateUniformGroupFromLists)**: Resolved by using
  `inline erasedValue[head] match { case _: *Uniform[Sampler] => ... }` in
  `derive.scala` — compile-time dispatch, not a runtime `.isSampler` flag check
  (which cannot be used as an `inline if` condition).

### Feature 1b (Depth Buffer)

✅ Done (Steps 1–3). `Panel.depthTest` is a `var` settable via `set()`, depth
texture allocation (`Opt[GPUTexture]` with `.nonNull`/`.get` patterns), render
pass attachment, and pipeline depth state all implemented. Step 4 (`panel_tex`
3D upgrade) done — draft uses `PerspectiveCamera` + `Transform`.

### 3D Math Track

✅ Done. All planned work completed:

1. **P1 — Mat4 constructors**: `fromTranslation`, `fromScale`,
   `fromTranslationRotationScale`, `perspective`, `lookAt` — implemented
2. **P2 — Quat class**: `QuatImmutableOps`, `QuatMutableOps`, CPU `Quat` —
   implemented
3. **Scene layer**: `Transform`, `PerspectiveCamera`, `SceneObject` in
   `graphics.scene` — implemented

### Painter API Refactoring

✅ Done. Constructors take only `painter` (+ `shade`/`form` for typed entities);
all rendering properties are `var`s with defaults, configured via `set()` with
`Maybe[T]` params. Nullable fields use `Opt[T] = T | Null`:

- `Form(painter)` with `set(vertices, topology, frontFace)` — buffer lifecycle
  managed internally (destroy old on `set(vertices = ...)`)
- `Shape(painter, shade, form)` with `set(cullMode, blendState)`
- `Layer(painter, shade)` with `set(blendState)`
- `Panel(painter)` with
  `set(width, height, clearColor, depthTest, multisample, shapes, layers)`
- `GPUBuffer.destroy()` added to facade
- All draft files updated to new API

### Feature 2 (Post-Processing / Ping-Pong)

**Step 1 — Ping-pong textures on Panel** ✅ Done

- `_pongTexture: Opt[GPUTexture]`, `_pongView: Opt[GPUTextureView]`,
  `_outputView: Opt[GPUTextureView]` added to `Panel`
- `ensureSize` allocates pong pair when `needsPong` (any layer has
  `panelBindGroupLayout != null`)
- `panel.outputView` falls back to `panel.textureView` when no ping-pong occurred
- `panel.setOutputView(view)` sets `_outputView` from `paint()`

**Step 2 — paint() + show() restructuring** ✅ Done

- `paint()` restructured into two phases: shapes pass (with depth/msaa) then
  layer pass (no depth/msaa — fullscreen quads always render on top)
- Layers rendered in user-specified order; consecutive non-ping-pong layers share
  a pass, ping-pong layers get their own pass with src/dst swap
- `renderLayerOnPass` gains `srcView: Opt[GPUTextureView]` param; when non-null,
  injects `srcView` at slot 0 of the panel bind group (auto ping-pong source)
- Slot 0 manually bound → render in-place to `srcView`, no swap
- Slot 0 unbound → inject `srcView` at slot 0, render to `dstView`, swap
- `show()` uses `panel.outputView` instead of `panel.textureView`

**Step 3 — blur draft** ✅ Done

- `drafts/blur/Blur.scala`: triangle shape (UV-colored) rendered to a panel with
  blue-ish clear color, then Gaussian blur applied via logarithmic passes
  (halve diameter while > 2.0, horizontal then vertical each step)
- `gaussianBlur9` as `WgslFn.raw` — 9-tap optimized linear sampling (5 fetches)
- `layerShade` with bare uniform types (no `FragmentUniform` wrappers) — auto
  fragment visibility inference via `WrapFragment` match type
- `painter.onResize` updates resolution binding
- `drafts/index.html` and `serve.ts` updated with blur route
- DSL fix: `ToExpr` match type uses `case _ => T` fallback so `Texture2D` and
  `Sampler` opaque types reduce correctly
- `checkSamplerFieldImpl` extended to accept bare `Sampler` type
