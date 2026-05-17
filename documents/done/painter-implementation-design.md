# Painter Implementation Design — Scala.js WebGPU

This document describes the plan for porting the Rust `trivalibs_painter`
library to Scala.js, targeting browser-based WebGPU exclusively. It catalogs
existing infrastructure, maps it to Rust painter concepts, and designs
Scala-idiomatic replacements that leverage the type system, GC-based memory
management, and direct browser API access.

The long-term north star is a unified CPU/GPU programming model where Scala
types and math operations work identically on both sides — abstracting the
CPU/GPU boundary the way React abstracts the server/client boundary.

---

## Table of Contents

1. [Existing Scala Infrastructure](#1-existing-scala-infrastructure)
2. [Rust Painter Architecture Summary](#2-rust-painter-architecture-summary)
3. [Mapping: Rust Concepts → Scala Components](#3-mapping-rust-concepts--scala-components)
4. [Design Divergences from Rust](#4-design-divergences-from-rust)
5. [Core Type Designs](#5-core-type-designs)
6. [Implementation Milestones](#6-implementation-milestones)
7. [Long-Term Vision: Unified CPU/GPU Programming](#7-long-term-vision-unified-cpugpu-programming)
8. [WebGPU Facade Extensions](#8-webgpu-facade-extensions)
9. [Open Questions](#9-open-questions)
10. [File Organization](#10-file-organization)
11. [Implementation Status](#11-implementation-status)

---

## 1. Existing Scala Infrastructure

### 1.1 Math Library — `src/gpu/math/`

**Vectors**: Vec2, Vec3, Vec4 — each with multiple representations:

- Mutable classes (`Vec2`, `Vec3`, `Vec4`) with Double components — main working
  types
- Float variants (`Vec2f`, `Vec3f`, `Vec4f`) — for when Float precision suffices
- Immutable tuples (`Vec2Tuple`, `Vec3Tuple`, `Vec4Tuple`) — lightweight,
  allocation-free
- GPU buffer types (`Vec2Buffer = (F32, F32)`, `Vec3Buffer = (F32, F32, F32)`,
  `Vec4Buffer`) — for GPU upload

Trait hierarchy per family: `Base[Num, Vec]` → `Mutable` / `ImmutableOps` /
`MutableOps` with extension methods providing `.x`, `.y`, `.z`, `.w` / `.r`,
`.g`, `.b`, `.a` / `.u`, `.v` aliases.

**Matrices**: Mat2, Mat3, Mat4 — column-major storage with similar variant sets.
Buffer types handle WGSL std140 padding automatically (Vec3→Vec4Buffer in
uniforms, Mat3→Mat3PaddedBuffer).

### 1.2 Shader System — `src/gpu/shader/`

**`ShaderDef[Attribs, Varyings, Uniforms, VBI, VBO, FBI, FO]`** (`shader.scala`)

- Fully type-parametric shader definition
- `inline def generateWGSL: String` — compile-time WGSL generation from type
  parameters
- `inline def vertexBufferLayout: js.Dynamic` — vertex buffer descriptor from
  `Attribs` named tuple
- `inline def createBindGroupLayouts(device): Array[GPUBindGroupLayout]` — from
  `Uniforms`
- `inline def createPipelineLayout(device): (Array[GPUBindGroupLayout], GPUPipelineLayout)`
- Factory: `Shader[A, V, U](vertexBody, fragmentBody)` with defaults for
  builtins

**`WGSLType[T]`** type class (`types.scala`)

- Maps Scala types → WGSL: `wgslName`, `byteSize`, `alignment`, `vertexFormat`
- Associated types: `AttribBuffer` (vertex layout), `UniformBuffer` (uniform
  layout with padding)
- Instances for Float, Vec2–4, Mat2–4

**Visibility wrappers**: `VertexUniform[T]`, `FragmentUniform[T]`,
`SharedUniform[T]` — control shader stage visibility in bind group layouts.
Transparent delegation to inner `WGSLType`.

**Compile-time derivation** (`derive.scala`)

- `fieldNames[T]`, `fieldWgslTypes[T]` — introspect named tuples
- `generateLocationStruct[T](name)`, `generateBuiltinStruct[T](name)` — WGSL
  struct generation
- `generateUniforms[T]` — @group/@binding declarations from nested named tuples

**Layout derivation** (`layouts.scala`)

- `vertexBufferLayout[Attribs]` →
  `{ arrayStride, attributes: [{ location, offset, format }] }`
- `bindGroupLayoutDescriptors[Uniforms]` → nested arrays of bind group entries
- `createPipelineLayoutFromUniforms[Uniforms](device)` →
  `(layouts, pipelineLayout)`

**Builtins** (`builtins.scala`)

- Opaque types: `BuiltinVertexIndex`, `BuiltinInstanceIndex`, `BuiltinPosition`,
  `BuiltinFragCoord`, etc.
- `BuiltinType[T]` type class with `wgslBuiltin` and `wgslType`

### 1.3 Buffer System — `src/gpu/buffers/`

**`BufferBinding[T, F <: Tuple]`** (`binding.scala`)

- GPU uniform buffer with typed CPU↔GPU sync
- `set(value: T)` / `:=(value)` — write to CPU buffer + upload to GPU
- `update(f: StructRef[F] => Unit)` — mutate buffer then upload
- `get: T` — read from CPU buffer
- Created via `BufferBinding[T](device)` or
  `BufferBinding[T](device, initialValue)`

**`UniformValue[T, F]`** type class — marshals Scala values to GPU buffer format

- Handles std140 padding (Vec3→Vec4Buffer, Mat3→Mat3PaddedBuffer)

**`AttribLayout[Attribs, Fields]`** (`attributes.scala`)

- Resolves named tuple attribute types to concrete `StructArray` field layouts
- `transparent inline def allocateAttribs[Attribs](count: Int): Any` — returns
  concrete `StructArray[F]`

### 1.4 Binary Buffer Foundation — `trivalibs/src/utils/bufferdata.scala`

**`StructArray[Fields <: Tuple]`** — zero-cost typed array of structs over
`DataView`

- `allocate[Fields](count)`, `wrap[Fields](buffer)`
- Index access: `arr(i): StructRef[Fields]`
- Iteration: `.indices`, implicit `Iterable` conversion

**`StructRef[Fields]`** — typed reference to single struct

- `apply(n): FieldAccess` — transparent inline, returns `StructRef` for nested
  or `PrimitiveRef` for leaf
- `set(values)` / `:=(values)` — write entire struct

**`PrimitiveRef[T]`** — typed single-field reference with `get`/`set`

**Match types**: `PrimitiveSize[T]`, `TupleSize[Fields]`,
`FieldOffset[Fields, N]` — compile-time byte calculations.

### 1.5 WebGPU Facades — `src/webgpu/facades.scala`

Complete `@js.native` bindings for core WebGPU API:

- `GPU`, `GPUAdapter`, `GPUDevice`, `GPUQueue`, `GPUShaderModule`
- `GPURenderPipeline`, `GPUCommandEncoder`, `GPURenderPassEncoder`
- `GPUBuffer`, `GPUBindGroup`, `GPUBindGroupLayout`, `GPUPipelineLayout`
- `GPUCanvasContext`, `GPUTexture`, `GPUTextureView`
- Constants: `GPUBufferUsage`, `GPUShaderStage`
- Helper: `WebGPU.getGPU()`, `WebGPU.getContext(canvas)`

### 1.6 Key Type-Level Patterns Used

| Pattern                             | Where                                | Purpose                                  |
| ----------------------------------- | ------------------------------------ | ---------------------------------------- |
| Opaque types over `(DataView, Int)` | StructArray, StructRef, PrimitiveRef | Zero-cost GPU buffer abstractions        |
| Match types                         | TupleSize, FieldOffset, ValueType    | Compile-time byte offset calculation     |
| `transparent inline` + `summonFrom` | allocateAttribs, field access        | Concrete return type propagation         |
| Named tuples                        | Attribs, Uniforms types              | Type-safe field names for shader structs |
| `NamedTuple.Names/DropNames`        | derive.scala                         | Separate names from types for code gen   |
| Given type class chains             | WGSLType, UniformValue, AttribLayout | Extensible type-to-GPU mapping           |
| Visibility wrapper types            | VertexUniform, FragmentUniform       | Shader stage metadata in types           |

---

## 2. Rust Painter Architecture Summary

(Full details in `documents/rust-painter/painter_technical_overview.md`)

### Core Abstractions

**Painter** — central GPU resource registry and execution engine. Owns device,
queue, surface, all resource storage vectors, and a pipeline cache.

**Shade** — shader program + input contract. Defines vertex attribute layout,
binding layout, shader code. Two bind group layouts: value bindings
(uniforms/samplers) and layer bindings (textures from other layers).

**Form** — geometry on GPU. Vertex buffer(s) + optional index buffer. Topology +
front face. Supports dynamic updates and buffer reallocation.

**Shape** — drawable object. Combines Form + Shade + bindings + render state
(blend, cull). Generates pipeline keys for caching. Supports instanced rendering
with per-instance binding overrides.

**Effect** — fullscreen post-processing pass. No Form, no vertex shader (uses
built-in fullscreen triangle). Fragment-only Shade + bindings. Enables blur,
deferred lighting, etc.

**Layer** — render target + composition unit. Owns textures, holds ordered
Shapes and Effects. Supports MRT, depth testing, MSAA, mipmaps, auto-sizing to
window, shared bindings, and texture ping-pong for multi-effect chains.

### Binding Hierarchy

Three levels, higher overrides lower at matching slots:

```
Layer bindings → Shape/Effect bindings → Instance bindings
```

**Value bindings**: uniform buffers and samplers. **Layer bindings**: textures
from other layers (Source, AtIndex, SourceAtMipLevel, Depth).

### Rendering Execution

```
painter.paint(layer):
  1. Render pass → layer textures
  2. For each Shape: get/create cached pipeline, resolve merged bindings, draw
  3. For each Effect: ping-pong textures, new render pass, draw fullscreen triangle
  4. Generate mipmaps if configured

painter.show(layer):
  Blit layer's output texture to canvas swap chain via fullscreen triangle
```

### Resource Management

- **Handle-based**: all resources are `usize` indices into storage vectors on
  the Painter
- **Builder pattern**: chained method calls for resource creation
- **Lazy pipeline creation**: created on first render, cached by config hash
- **Pipeline key**: shade + blend + cull + topology + layer formats + depth +
  MSAA

---

## 3. Mapping: Rust Concepts → Scala Components

| Rust Concept             | Existing Scala                      | New Scala                                 | Notes                                         |
| ------------------------ | ----------------------------------- | ----------------------------------------- | --------------------------------------------- |
| Vertex attribute formats | `WGSLType[T].vertexFormat`          | —                                         | Complete, reuse directly                      |
| Vertex buffer layout     | `layouts.vertexBufferLayout[A]`     | —                                         | Complete, reuse directly                      |
| Bind group layout        | `layouts.createBindGroupLayouts[U]` | Extend for samplers/textures              | Currently uniforms only                       |
| WGSL generation          | `ShaderDef.generateWGSL`            | —                                         | Complete for vert+frag structs                |
| CPU vertex data          | `allocateAttribs` + `StructArray`   | —                                         | Complete, feeds into Form                     |
| Uniform buffer           | `BufferBinding[T, F]`               | —                                         | Direct equivalent of Rust `BindingBuffer<T>`  |
| Math types               | Vec2–4, Mat2–4                      | —                                         | Complete                                      |
| WebGPU bindings          | `facades.scala`                     | Extend (see §8)                           | Missing samplers, textures, etc.              |
| Shade                    | `ShaderDef` (compile-time)          | `Shade` (runtime wrapper)                 | Shade materializes ShaderDef into GPU objects |
| Form                     | —                                   | `Form`                                    | New: vertex/index buffers + topology          |
| Shape                    | —                                   | `Shape`                                   | New: Form + Shade + bindings + state          |
| Effect                   | —                                   | **`Layer`**                               | Renamed: fullscreen post-processing pass      |
| Layer                    | —                                   | **`Panel`**                               | Renamed: render target + shapes/layers        |
| Painter                  | —                                   | `Painter`                                 | New: execution engine + pipeline cache        |
| CanvasApp                | —                                   | `CanvasApp`                               | New: application framework                    |
| Handle indices           | N/A                                 | **Not needed**                            | Use direct object references (GC)             |
| Pipeline cache           | —                                   | `HashMap[PipelineKey, GPURenderPipeline]` | Case class key with structural equality       |

> **Naming convention — Rust → Scala:**
>
> | Rust name      | Scala name     | Rationale                                                    |
> | -------------- | -------------- | ------------------------------------------------------------ |
> | **Layer**      | **Panel**      | A self-contained image/surface (painting panel, comic panel) |
> | **Effect**     | **Layer**      | A coat of paint applied over the whole surface (a glaze)     |
> | LayerBinding   | PanelBinding   | Texture sampled from another Panel's render target           |
> | layer bindings | panel bindings | Bind group slot for textures from other Panels               |
>
> The painting metaphor: you paint **shapes** onto a **panel**, then apply
> **layers** (glazes, washes) over the whole surface. This rename is intentional
> and applies throughout the Scala codebase. When this document refers to the
> Rust architecture (§2), it uses the original Rust names.

---

## 4. Design Divergences from Rust

### 4.1 No Handle Indices — Direct Object References

Rust uses `usize` handles because of ownership/lifetime rules. Scala has GC. A
`Shape` holds direct references to its `Form` and `Shade` objects, not indices.
This eliminates invalid-handle bugs and removes the central storage registry.

The `Painter` is still the execution engine and GPU context holder, but does not
own or store all resources. Resources are self-contained GC-managed objects.

### 4.2 Compile-Time Shader Contracts via ShaderDef

Rust's `Shade` takes runtime arrays of `GPUVertexFormat` values and binding type
enums. Scala's `Shade` wraps a `ShaderDef` whose type parameters drive
compile-time WGSL generation, layout derivation, and format computation. Type
errors in shader contracts are caught at compile time.

`ShaderDef` itself remains a general-purpose utility with flexible nested tuple
uniforms. The painter's `shade` factory is a thin inline adapter that accepts
flat named tuples (one per bind group role) and wraps them into `ShaderDef`'s
nested format. See §5.2 for details.

### 4.3 Named Tuples Instead of Positional Arrays

Rust:
`shade([Float32x3, Float32x3, Float32x2]).with_bindings([BUFFER_VERT, SAMPLER_FRAG])`
Scala: `type Attribs = (position: Vec3, normal: Vec3, uv: Vec2)` — field names
become WGSL struct fields.

Uniforms similarly use nested named tuples with visibility wrappers, not
positional binding lists.

### 4.4 Browser-Native Features

- No windowing abstraction — use `HTMLCanvasElement` directly
- `ResizeObserver` for canvas resize detection
- `js.Promise` for async GPU initialization (existing for-comprehension
  extensions)
- `requestAnimationFrame` for render loop
- `dom.Event` for input handling (no custom event enum layer)

### 4.5 Mutable State with Immutable Config

Rust uses builders extensively. Scala design:

- **Immutable configs**: `PanelConfig`, `SamplerConfig` as case classes with
  defaults
- **Mutable runtime state**: `Shape.valueBindings`, `Panel.shapes`, etc. as
  `var`s
- Simple assignment instead of builder ceremony for common cases

### 4.6 Opaque Types for GPU State (No Enums)

Scala 3 `enum` generates significant JS boilerplate (wrapper objects, pattern
match tables). For GPU constants that are just WebGPU string values, we use
**opaque types** backed by `String` — zero runtime overhead, compiles to bare
string constants in JS output.

```scala
opaque type PrimitiveTopology = String
object PrimitiveTopology:
  val TriangleList: PrimitiveTopology = "triangle-list"
  val TriangleStrip: PrimitiveTopology = "triangle-strip"
  val LineList: PrimitiveTopology = "line-list"
  val LineStrip: PrimitiveTopology = "line-strip"
  val PointList: PrimitiveTopology = "point-list"
  extension (t: PrimitiveTopology)
    inline def toJs: js.Any = t.asInstanceOf[js.Any]

opaque type CullMode = String
object CullMode:
  val None: CullMode = "none"
  val Front: CullMode = "front"
  val Back: CullMode = "back"
  extension (c: CullMode) inline def toJs: js.Any = c.asInstanceOf[js.Any]

opaque type FrontFace = String
object FrontFace:
  val CCW: FrontFace = "ccw"
  val CW: FrontFace = "cw"
  extension (f: FrontFace) inline def toJs: js.Any = f.asInstanceOf[js.Any]
```

Blend state uses `js.Object` subclasses for structured data with zero overhead:

```scala
class BlendFn(
    val srcFactor: BlendFactor,
    val dstFactor: BlendFactor,
    val operation: BlendOp = BlendOp.Add,
) extends js.Object

class BlendState(
    val color: BlendFn,
    val alpha: BlendFn,
) extends js.Object

object BlendState:
  val Alpha = BlendState(
    color = BlendFn(BlendFactor.SrcAlpha, BlendFactor.OneMinusSrcAlpha),
    alpha = BlendFn(BlendFactor.One, BlendFactor.OneMinusSrcAlpha),
  )
  val Additive = BlendState(...)
  val Multiply = BlendState(...)
```

The `.toJs` extension on opaque types is needed because `String` does not extend
`js.Any` in Scala.js, so `asInstanceOf` is the only way to pass them into
`js.Dynamic.literal`.

---

## 5. Core Type Designs

### 5.1 Painter

```scala
class Painter(
  val device: GPUDevice,
  val queue: GPUQueue,
  val canvas: HTMLCanvasElement,
  val context: GPUCanvasContext,
  val preferredFormat: String,
):
  private val pipelineCache = mutable.HashMap[PipelineKey, GPURenderPipeline]()
  private var nextShadeId = 0

  lazy val fullscreenVertexModule: GPUShaderModule = // built-in fullscreen triangle

  // Direct-to-canvas convenience (permanent shortcut for quick sketches)
  def draw(shape: Shape, clearColor: Opt[(Double, Double, Double, Double)] = Opt.Null): Unit

  // Panel-based rendering (intermediate render target + blit)
  def paint(panel: Panel): Unit
  def show(panel: Panel): Unit

  // Shader factories (inline to preserve type info for ShaderDef delegation)
  // MVP: values only (single bind group)
  inline def shade[A, V, Values](vertBody: String, fragBody: String): Shade
  // Later: values + textures (two bind groups)
  inline def shade[A, V, Values, Textures](vertBody: String, fragBody: String): Shade
  inline def layerShade[Values](fragBody: String): Shade

  // Resource factories (hide device from user)
  inline def binding[T](value: T): BufferBinding[T, ?]
  inline def binding[T]: BufferBinding[T, ?]
  def form[F <: Tuple](vertices: StructArray[F], ...): Form
  def shape(form: Form, shade: Shade, ...): Shape
  def panel(...): Panel
```

### 5.2 Shade & the ShaderDef Adapter

`ShaderDef` (in `src/gpu/shader/`) is a general-purpose, standalone utility
whose `Uniforms` type parameter accepts arbitrarily nested named tuples — each
nesting level maps to a bind group. This flexibility is correct for
`ShaderDef`'s level of abstraction and should be preserved.

The **Painter** has an opinionated convention: bind group 0 = value/sampler
bindings, bind group 1 = panel/texture bindings (textures from other Panels).
This convention lives in the painter framework, not in `ShaderDef`.

The painter's `shade` factory is an `inline def` adapter that:

1. Accepts the user's **flat** named tuples (one for values, one for textures)
2. Wraps them into the nested structure `ShaderDef` expects
3. Delegates to `ShaderDef`'s compile-time WGSL generation and layout derivation
4. Returns a `Shade` with the runtime GPU objects

```scala
// User writes flat types for painter — no nesting:
type Values = (viewProj: SharedUniform[Mat4], time: VertexUniform[Float])
type Textures = (albedo: FragmentTexture, normal: FragmentTexture)

// MVP (Milestone 1) — values only:
val shade = painter.shade[Attribs, Varyings, Values](vertBody, fragBody)
// Internally constructs: ShaderDef[Attribs, Varyings, (values: Values), ...](...)

// Later (Milestone 3) — values + textures:
val shade = painter.shade[Attribs, Varyings, Values, Textures](vertBody, fragBody)
// Internally constructs: ShaderDef[Attribs, Varyings, (values: Values, textures: Textures), ...](...)
```

The user never sees the nesting. `ShaderDef` stays general-purpose for use
outside of painter.

#### Shade Runtime Object

```scala
class Shade(
  val id: Int,
  val shaderModule: GPUShaderModule,
  val vertexBufferLayout: js.Dynamic | Null,   // null for layers (post-processing)
  val valueBindGroupLayout: GPUBindGroupLayout,
  val panelBindGroupLayout: GPUBindGroupLayout | Null,
  val pipelineLayout: GPUPipelineLayout,
  val isLayer: Boolean,  // true = fullscreen post-processing (Rust "Effect")
)
```

**Pipeline Layout vs Pipeline Instance**: The `GPUPipelineLayout` produced by
`ShaderDef.createPipelineLayout` depends only on the shader's binding
declarations (the `Uniforms` type parameter). It is independent of blend state,
depth/stencil, multisampling, target formats, topology, and cull mode. Those
concerns only affect the `GPURenderPipeline` instance, which the `Painter`
creates and caches using `PipelineKey`. This separation is clean: `Shade` owns
the pipeline _layout_; `Painter` owns pipeline _instances_.

### 5.3 Form

```scala
class Form(
  private val device: GPUDevice,
  private val queue: GPUQueue,
  private var vertexBuffer: GPUBuffer,
  val vertexCount: Int,
  val topology: PrimitiveTopology = PrimitiveTopology.TriangleList,
  val frontFace: FrontFace = FrontFace.CCW,
):
  def updateVertices(data: ArrayBuffer): Unit
```

Index buffers are deferred to a later milestone. MVP only supports non-indexed
draw calls.

**Integration with existing infrastructure:**

```scala
// User allocates typed vertex data using existing allocateAttribs
type MyAttribs = (position: Vec3, color: Vec4)
val vertices = allocateAttribs[MyAttribs](3)
vertices(0).set((Vec3(0, 1, 0), Vec4(1, 0, 0, 1)))
// ...

// Create Form from the StructArray's underlying ArrayBuffer
val form = painter.form(vertices)  // infers count from StructArray.length
```

### 5.4 Shape

```scala
class Shape(
  val form: Form,
  val shade: Shade,
  var valueBindings: BindingSlots = Map.empty,
  var panelBindings: PanelBindingSlots = Map.empty,
  var instances: Seq[InstanceBindings] = Seq.empty,
  var cullMode: CullMode = CullMode.None,
  var blendMode: BlendMode = BlendMode.Replace,
)
```

### 5.5 Layer (Rust: "Effect")

A **Layer** is a fullscreen post-processing pass — what Rust calls an "Effect".

```scala
class Layer(
  val shade: Shade,  // must have isLayer = true
  var valueBindings: BindingSlots = Map.empty,
  var panelBindings: PanelBindingSlots = Map.empty,
  var instances: Seq[InstanceBindings] = Seq.empty,
  var blendMode: BlendMode = BlendMode.Replace,
  var srcMipLevel: Option[Int] = None,
  var dstMipLevel: Option[Int] = None,
)
```

### 5.6 Panel (Rust: "Layer")

A **Panel** is a render target + composition unit — what Rust calls a "Layer".

```scala
case class PanelConfig(
  width: Int = 0,             // 0 = use canvas size
  height: Int = 0,
  clearColor: Option[Vec4Tuple] = Some((0, 0, 0, 1)),
  formats: Seq[TextureFormat] = Seq(TextureFormat.BGRA8Unorm),
  depthTest: Boolean = false,
  multisample: Boolean = false,
  mipLevelCount: Int = 1,
)

class Panel(
  private val painter: Painter,
  val config: PanelConfig,
):
  // Textures (allocated on creation, reallocated on resize)
  private var targets: Array[GPUTexture]
  private var pingPongTargets: Array[GPUTexture] | Null  // allocated when layers exist
  private var depthTexture: GPUTexture | Null
  private var msaaTextures: Array[GPUTexture] | Null
  private var currentTarget: Int = 0  // ping-pong index

  // Content
  val shapes: mutable.ArrayBuffer[Shape] = mutable.ArrayBuffer.empty
  val layers: mutable.ArrayBuffer[Layer] = mutable.ArrayBuffer.empty

  // Shared bindings (inherited by all shapes/layers)
  var valueBindings: BindingSlots = Map.empty
  var panelBindings: PanelBindingSlots = Map.empty

  // API
  def currentTextureView: GPUTextureView
  def textureViewAt(index: Int): GPUTextureView
  def depthTextureView: Option[GPUTextureView]
  def resize(width: Int, height: Int): Unit
  def width: Int
  def height: Int
```

### 5.7 initPainter — Bootstrap Function

```scala
def initPainter(canvas: HTMLCanvasElement)(
    init: Painter => Unit,
): js.Promise[Unit]
```

`initPainter` handles: GPU/adapter/device acquisition, context configuration,
ResizeObserver setup, and initial canvas sizing. It does **not** manage a render
loop — the user drives `requestAnimationFrame` directly, which avoids
unnecessary framework abstraction and lets browser APIs be used naturally.

The callback receives a fully initialized `Painter`. Animation, input handling,
and frame scheduling are the user's responsibility via standard DOM APIs.

---

## 6. Implementation Milestones

The implementation follows two major milestones. Milestone 1 proves the
architecture works end-to-end. Milestone 2 proves Scala's value proposition over
Rust through type-safe named bindings — if we can't deliver a meaningfully
better DX than the Rust version, the port isn't justified.

---

### Milestone 1 — MVP: Minimal Rendering Pipeline

**Goal**: Render triangles with zero manual WebGPU boilerplate, built in two
incremental steps. Each step produces a working draft example, and all previous
examples continue to work.

**Scope — included**:

- `initPainter` bootstrap (GPU init, canvas setup, ResizeObserver)
- `Painter` with `draw(shape)` (direct) + `paint(panel)` / `show(panel)` (panel)
- `Shade` wrapping `ShaderDef` (shader module + pipeline layout)
- `Form` (vertex buffer + topology, no index buffer yet)
- `Shape` (Form + Shade + simple `Map[Int, BufferBinding]` bindings +
  blend/cull)
- `Panel` owns a render texture — `paint()` renders into it, `show()` blits to
  canvas via built-in fullscreen triangle + blit shader
- Pipeline cache with `PipelineKey`
- Opaque type constants: `PrimitiveTopology`, `CullMode`, `FrontFace`,
  `BlendFactor`, `BlendOp`, `BlendState`
- Built-in fullscreen triangle vertex shader + blit fragment shader in Painter

**Scope — excluded (deferred)**:

- Index buffers on Form
- Textures as user-facing bindings, samplers, panel bindings between panels
- Layers (fullscreen post-processing, Rust "Effects")
- Instances
- Depth testing, MSAA, MRT, mipmaps
- Panel-level bindings and the override hierarchy
- Multi-panel compositing (show only blits a single panel)

**Draft example progression** (in `drafts/`):

1. `simple_triangle` — raw WebGPU, hardcoded vertices in shader (exists)
2. `buffer_triangle` — raw WebGPU + typed vertex buffers/uniforms via helpers
   (exists)
3. `painter_triangle` — Painter/Shade/Form/Shape + `draw()` via initPainter
   (**Step A**)
4. `panel_triangle` — Panel render-to-texture + blit (**Step B**)

Each new draft demonstrates the next layer of abstraction. Existing drafts are
never deleted — they remain as reference points and must keep compiling.

#### 6.1 MVP Binding Approach

For Milestone 1, bindings are deliberately simple. Since the painter convention
puts all value/buffer bindings in group 0, the key is just the binding index
(not a `(group, binding)` tuple). Bindings are stored in a sparse `Arr` (JS
array) — null slots for unused binding points, zero Scala collection overhead.

```scala
// MVP: sparse array indexed by binding slot (group 0 is implicit)
type BindingSlots = Arr[BufferBinding[?, ?] | Null]

class Shape(
  val form: Form,
  val shade: Shade,
  var bindings: BindingSlots = Arr(),
  var cullMode: CullMode = CullMode.None,
  var blendState: Opt[BlendState] = Opt.Null,
):
  def bind(slots: (Int, BufferBinding[?, ?])*): Shape
```

The Painter provides factories so users don't pass `device` around:

```scala
// On Painter:
inline def binding[T](value: T): BufferBinding[T, ?]  // with initial value
inline def binding[T]: BufferBinding[T, ?]             // uninitialized

// Usage:
val tintColor = painter.binding[Vec4]
val shape = painter.shape(form, shade).bind(0 -> tintColor)

// Update each render cycle:
tintColor.update: c =>
  c.r = newR; c.g = newG; c.b = newB
```

`Shape.bind()` returns `this` for fluent chaining. No merge hierarchy, no panel
bindings. Shapes hold their own bindings directly.

#### 6.2 initPainter

```scala
def initPainter(canvas: HTMLCanvasElement)(
    init: Painter => Unit,
): js.Promise[Unit]
```

**Responsibilities**:

1. Acquire GPU adapter and device (Promise-based)
2. Configure canvas context with device and preferred format
3. Set up ResizeObserver — automatically updates `canvas.width`/`canvas.height`
4. Set initial canvas dimensions from `clientWidth`/`clientHeight`
5. Call `init(painter)` with fully initialized Painter

**Not managed**: render loop, frame timing, input. The user calls
`requestAnimationFrame` directly, keeping control over animation and enabling
static scenes with no overhead. This replaces the original `CanvasApp`
abstraction, which imposed Rust-style lifecycle management that is unnecessary
in the browser where these APIs are already ergonomic.

---

#### Step A — Direct-to-Canvas Rendering

**Goal**: First working Painter triangle — `painter.draw(shape)` renders one
shape directly to the canvas surface, no intermediate texture.

##### 6.3 `draw(shape)` — Direct Rendering

`draw` is a permanent convenience method for quick sketches. It renders a single
shape directly to the swap chain texture without any intermediate Panel.

```scala
def draw(
  shape: Shape,
  clearColor: Opt[(Double, Double, Double, Double)] = Opt.Null,
): Unit
```

Blend state lives on the `Shape` (via `shape.blendState`), not on `draw()`.

**Algorithm**:

```
1. Create GPUCommandEncoder
2. Get swap chain texture: context.getCurrentTexture()
3. Begin render pass targeting swap chain textureView
   - Load op = clear (if clearColor set) or load
4. Compute pipeline key from shape config + swap chain format
5. Get or create cached pipeline
6. Create bind groups from shape.bindings
7. Set pipeline, vertex buffer, bind groups
8. Draw
9. End render pass
10. Submit command buffer
```

This is the core single-shape rendering logic. In Step B, the same logic is
reused inside `paint(panel)` — the only difference is the render target.

##### 6.4 Step A — Draft Example: `painter_triangle`

```scala
initPainter(canvas): painter =>
  type Attribs = (position: Vec2, color: Vec3)
  type Varyings = (color: Vec3)
  type Uniforms = (tintColor: Vec4)

  val shade = painter.shade[Attribs, Varyings, Uniforms](
    vertBody = """
      |  out.position = vec4<f32>(in.position, 0.0, 1.0);
      |  out.color = in.color;
    """.stripMargin,
    fragBody = """
      |  out.color = vec4<f32>(in.color, 1.0) * tintColor;
    """.stripMargin,
  )

  val vertices = allocateAttribs[Attribs](3)
  // ... fill vertices ...
  val form = painter.form(vertices)
  val tintColor = painter.binding[Vec4]
  val shape = painter.shape(form, shade).bind(0 -> tintColor)

  def render(time: Double): Unit =
    tintColor.update: c =>
      c.r = Math.sin(time / 1000.0) * 0.5 + 0.5
    painter.draw(shape, clearColor = (0.1, 0.1, 0.1, 1.0))
    dom.window.requestAnimationFrame(t => render(t))

  dom.window.requestAnimationFrame(t => render(t))
```

##### 6.5 Step A — Implementation Steps

**Step 1 — Facades & Enums**

- Extend `facades.scala`: `getPreferredCanvasFormat()`, missing render pass
  descriptor fields
- Create `src/gpu/painter/enums.scala`: `PrimitiveTopology`, `CullMode`,
  `FrontFace`, `BlendMode`

**Step 2 — Shade & Form**

- `Shade`: wraps ShaderDef → GPUShaderModule + pipeline layout
- `Form`: vertex buffer (accepts StructArray), topology, vertex count

**Step 3 — Shape & Painter with `draw()`**

- `Shape`: Form + Shade + slot-indexed bindings + blend/cull
- `Painter`: device holder, pipeline cache, `draw(shape)` renders directly to
  canvas swap chain texture

**Step 4 — initPainter**

- Async GPU init, ResizeObserver, canvas setup
- User drives render loop via `requestAnimationFrame`

**Step 5 — `painter_triangle` draft**

- Working example: colored triangle using `painter.draw(shape)` via initPainter
- Validate full pipeline: initPainter → Painter → Shade/Form/Shape → GPU

---

#### Step B — Panel + Blit

**Goal**: Introduce Panel as intermediate render target. Reuse the per-shape
rendering logic from `draw()`, but render into Panel's texture instead of the
swap chain, then blit to canvas.

##### 6.6 Panel — Render-to-Texture

Each Panel owns a GPU texture that shapes render into. `paint(panel)` renders
shapes into the panel's texture. `show(panel)` blits that texture onto the
canvas swap chain.

```scala
class Panel(
  private val painter: Painter,
  val width: Int,                // 0 = use canvas size
  val height: Int,
  var clearColor: Option[Vec4Tuple] = Some((0, 0, 0, 1)),
  val shapes: mutable.ArrayBuffer[Shape] = mutable.ArrayBuffer.empty,
):
  // Allocated on creation, reallocated on resize
  private var texture: GPUTexture
  private var textureView: GPUTextureView

  def currentTextureView: GPUTextureView = textureView
  def resize(w: Int, h: Int): Unit  // destroy + recreate texture
```

The texture is created with `RENDER_ATTACHMENT | TEXTURE_BINDING` usage — it can
be rendered into and sampled from (for the blit).

##### 6.7 `paint(panel)` + `show(panel)` — Panel Rendering

**paint(panel)** — render shapes into panel's texture:

```
1. Create GPUCommandEncoder
2. Begin render pass targeting panel.textureView
   - Load op = clear (if clearColor set) or load
3. For each shape in panel.shapes:
   a. Render shape (same logic as draw(), different target)
4. End render pass
5. Submit command buffer
```

**show(panel)** — blit panel texture to canvas:

```
1. Get swap chain texture: context.getCurrentTexture()
2. Create render pass targeting swap chain texture (clear)
3. Set blit pipeline (built-in fullscreen triangle + sampler)
4. Create bind group: panel.textureView + nearest sampler
5. Draw(3) — fullscreen triangle, no vertex buffer
6. End render pass, submit
```

The blit pipeline and sampler are created once on the Painter and reused. The
bind group is recreated per-show because the panel texture view may change
(resize). This is cheap — one bind group with two entries.

##### 6.8 Step B — Draft Example: `panel_triangle`

```scala
initPainter(canvas): painter =>
  type Attribs = (position: Vec2, color: Vec3)
  type Varyings = (color: Vec3)

  val shade = painter.shade[Attribs, Varyings, EmptyTuple](
    vertBody = """
      |  out.position = vec4<f32>(in.position, 0.0, 1.0);
      |  out.color = in.color;
    """.stripMargin,
    fragBody = """
      |  out.color = vec4<f32>(in.color, 1.0);
    """.stripMargin,
  )
  val vertices = allocateAttribs[Attribs](3)
  // ... fill vertices ...
  val form = painter.form(vertices)
  val shape = painter.shape(form, shade)
  val panel = painter.panel(clearColor = (0.1, 0.1, 0.1, 1.0))
  panel.shapes += shape

  painter.paint(panel)
  painter.show(panel)
  // Static scene — single render. For animation: requestAnimationFrame loop.
```

##### 6.9 Step B — Implementation Steps

**Step 1 — Facade extensions**

- `createTexture`, `GPUTextureUsage` (`RENDER_ATTACHMENT`, `TEXTURE_BINDING`),
  `createSampler`, `GPUSampler`, `GPUTexture.createView()`

**Step 2 — Panel**

- Panel class: texture allocation, resize, shapes list
- `painter.panel(...)` factory

**Step 3 — `paint()` + `show()` + blit shader**

- `paint(panel)`: reuse per-shape rendering from `draw()`, target panel texture
- `show(panel)`: blit panel texture to canvas
- Built-in fullscreen triangle vertex shader + blit fragment shader, created
  once on Painter init

**Step 4 — `panel_triangle` draft**

- Working example: same colored triangle, now via `paint(panel)` + `show(panel)`
- `painter_triangle` stays unchanged — both drafts work side by side

---

### Milestone 2 — Type-Safe Named Bindings

**Goal**: Replace slot-indexed bindings with a name-based, type-inferred API
that validates binding names and value types at compile time. This is the key DX
improvement that justifies the Scala port over staying with Rust.

#### 6.7 The Vision

```scala
type Values = (
  viewProj: SharedUniform[Mat4],
  time: VertexUniform[Float],
  color: FragmentUniform[Vec3],
  attenuation: FragmentUniform[Float],
)

val shade = painter.shade[Attribs, Varyings, Values](vertBody, fragBody)

// External shared binding — reusable across shapes
val timeBinding = painter.binding(0.0f)

// Create shape — Shade carries the Values type
val shape = Shape(form, shade)

// Type-safe named updates:
shape.set(
  "viewProj" -> Mat4.identity,       // auto-managed: Shape creates internal binding
  "color"    -> Vec3(0.5, 0.4, 0.3), // auto-managed
  "attenuation" -> 2.3,              // auto-managed
  "time"     -> timeBinding,          // external: shared across shapes
)

// Compile-time errors:
shape.set("nonexistent" -> 1.0)     // error: "nonexistent" is not a binding name
shape.set("color" -> 1.0)           // error: expected Vec3, got Double
```

#### 6.8 Design Approach

The Shade must carry the `Uniforms` type at the type level so Shape can use it
for validation. This means `Shade` becomes `Shade[U]` (or we use a typed
wrapper).

**Option A — Typed Shade, typed Shape**:

```scala
class Shade[Uniforms](...) // carries Uniforms type
class Shape[U](form: Form, shade: Shade[U], ...)

// shape.set uses inline + summonFrom to validate names against U
inline def set(bindings: (String, Any)*): Unit
```

**Option B — Typed builder on untyped shape**:

```scala
class Shade(uniforms: UniformsMetadata, ...) // runtime metadata
class Shape(form: Form, shade: Shade, ...)

// Typed update via extension on Shape+Shade pair, using Shade's compile-time info
inline def set[U](shade: Shade[U])(bindings: ...): Unit
```

**Option C — Named tuple as binding input**:

```scala
// Flatten the nested Uniforms into a single named tuple of values:
type FlatUniforms = (viewProj: Mat4, time: Float, color: Vec3, attenuation: Float)

// Shape.set accepts a subset of FlatUniforms as a named tuple:
shape.set((color = Vec3(0.5, 0.4, 0.3), attenuation = 2.3))
shape.set((time = timeBinding))  // BufferBinding[Float] accepted where Float expected
```

This option leverages named tuple structural typing. The compiler checks field
names and types. No string keys. Most Scala-idiomatic.

#### 6.9 Binding Storage Model

Each shape maintains:

```scala
// Internal storage: name → either a managed BufferBinding or external reference
private val managedBindings: mutable.Map[String, BufferBinding[?, ?]]
private val externalBindings: mutable.Map[String, GPUBuffer]
```

When `set("color" -> Vec3(...))` is called:

1. If no managed binding exists for "color", create a `BufferBinding[Vec3]`
2. Call `managedBinding.set(Vec3(...))` — writes to CPU buffer + uploads

When `set("time" -> timeBinding)` is called:

1. Store the external `BufferBinding` reference
2. At render time, use `timeBinding.gpuBuffer`

At render time, the Shape resolves all bindings to `GPUBuffer` references and
creates the bind groups. The name→(group, binding) slot mapping is derived from
the `Uniforms` type at compile time and cached in the Shade.

#### 6.10 Compile-Time Name Resolution

We need an inline function that maps a binding name string to its (group,
binding) indices, derived from the `Uniforms` nested named tuple structure:

```scala
// Given:
//   Uniforms = (scene: (viewProj: ..., time: ...), material: (color: ..., attenuation: ...))
// Derive:
//   "viewProj" → (0, 0), "time" → (0, 1), "color" → (1, 0), "attenuation" → (1, 1)

inline def bindingSlot[Uniforms](name: String): (Int, Int)
// Compile-time error if name doesn't match any field in the flattened Uniforms
```

This is achievable with `constValueOpt`, `erasedValue`, and `inline match` on
the named tuple structure. The existing `derive.scala` already walks the nested
named tuple structure for WGSL generation — this is the same traversal,
producing slot indices instead of WGSL code.

#### 6.11 Type Validation

For each binding name, we also validate the value type:

```scala
inline def validateBinding[Uniforms](name: String, value: Any): Unit
// At compile time, resolves "color" to FragmentUniform[Vec3],
// unwraps to Vec3, and checks that the provided value is Vec3 or BufferBinding[Vec3, ?]
```

Using `summonFrom` + match types:

```scala
type UnwrapUniform[T] = T match
  case VertexUniform[t]   => t
  case FragmentUniform[t] => t
  case SharedUniform[t]   => t

type BindingValueAt[Uniforms, Name <: String] = ... // resolve name → type via match
```

#### 6.12 Milestone 2 Implementation Steps

**Step 1 — Shade metadata**

- Add compile-time name→slot resolution to `derive.scala`
- Shade stores runtime `Map[String, (Int, Int)]` for name→slot lookup

**Step 2 — Typed shape bindings**

- Replace `BindingSlots = Map[(Int, Int), GPUBuffer]` with name-based storage
- Internal managed bindings vs external references
- `shape.set(...)` with compile-time name+type validation

**Step 3 — Named tuple binding syntax** (stretch)

- Accept named tuple subsets for fully structural binding updates
- `shape.set((color = Vec3(...), attenuation = 2.3))`

**Step 4 — Panel bindings & merge** (after named bindings work)

- Reintroduce panel-level shared bindings
- Merge hierarchy: panel → shape (by name, not slot index)

---

### Milestone 3 — Shader DSL + Layer/LayerShade: **Done**

See Section 11 for implementation status.

### Future Milestones

#### Milestone 4 — Cross-Panel Composition & Advanced Pipeline

- Texture ping-pong for layer chains
- PanelBinding enum (Source, AtIndex, Depth, etc.) — reading textures from other
  Panels
- Multi-panel compositing in `show()`
- Form: index buffers and multiple vertex buffers
- Depth testing, MSAA, MRT
- Mipmap generation
- Sampler bindings
- Instance rendering with per-instance binding overrides

---

## 7. Long-Term Vision: Unified CPU/GPU Programming

This section documents the north star that should guide all architectural
decisions throughout development, even when we're not directly working on it.

### 7.1 The Problem Across Ecosystems

Every GPU framework suffers from a fundamental split: the shader language and
the host language are different worlds. Types are duplicated, math libraries are
reimplemented, and the contract between CPU and GPU code must be manually kept
in sync. Bugs at this boundary are caught only at runtime (or worse, produce
silent visual glitches).

**Rust trivalibs** improved on this: shaders were written in Rust using the same
math crate, compiled to SPIR-V via `rust-gpu`, and loaded on the CPU side. The
math was shared, the language was shared. But the `Shade` contract (attribute
formats, binding layouts) still had to be manually declared on the CPU side and
kept in sync with the shader — a remaining seam.

**Our Scala port already closes part of that gap**: the `ShaderDef` type
parameters (`Attribs`, `Varyings`, `Uniforms`) are the single source of truth.
They derive both the WGSL struct declarations and the WebGPU layout objects. The
CPU and GPU sides are type-checked against the same definition. But the shader
_body_ is still a raw WGSL string — an opaque, untyped escape hatch.

### 7.2 The Vision: Scala Shader DSL

Replace WGSL string bodies with a Scala DSL that:

1. **Reuses the same math types** — `Vec3`, `Mat4`, etc. work identically in CPU
   code and in shader expressions
2. **Is type-checked by the Scala compiler** — accessing `.xyz` on a `Vec2` is a
   compile error, not a GPU runtime error
3. **Compiles to WGSL** — the DSL is an AST that serializes to WGSL strings at
   compile time or initialization
4. **Shares functions** — a `normalize(v: Vec3): Vec3` function can be used in
   both CPU-side simulation and GPU-side shading

The analogy to React is deliberate:

| React                    | Scala WebGPU                             |
| ------------------------ | ---------------------------------------- |
| Server Components        | CPU-side code                            |
| Client Components        | Shader code (GPU)                        |
| Network boundary         | CPU/GPU boundary                         |
| JSX compiles to both     | Scala DSL compiles to both native + WGSL |
| Shared types (props)     | Shared types (Attribs, Uniforms, math)   |
| `"use client"` directive | Shader context / `@gpu` annotation       |

The developer writes one language, one math library, one set of types. The
compiler figures out which code runs where and generates the appropriate output
for each side.

### 7.3 Progressive Approach

This is not an all-or-nothing feature. We build it incrementally, and each step
delivers standalone value:

**Already done** (current codebase):

- Shared type definitions via `ShaderDef` type parameters
- Compile-time WGSL struct generation from Scala types
- Type-safe attribute/uniform layout derivation

**Milestone 2** (named bindings):

- CPU code refers to shader bindings by name, validated at compile time
- Further tightens the CPU/GPU contract

**Future: Shader expression DSL** (AST-based):

```scala
// Instead of:
val shader = Shader[Attribs, Varyings, Uniforms](
  vertexBody = "output.position = uniforms.viewProj * vec4f(input.position, 1.0);",
  fragmentBody = "output.color = vec4f(input.color * uniforms.albedo.rgb, 1.0);"
)

// Write:
val shader = Shader[Attribs, Varyings, Uniforms](
  vertex = (input, uniforms) => ShaderOut(
    position = uniforms.viewProj * Vec4(input.position, 1.0)
  ),
  fragment = (input, uniforms) => FragOut(
    color = Vec4(input.color * uniforms.albedo.xyz, 1.0)
  ),
)
```

The DSL captures an expression AST (not executing the math) that serializes to
WGSL. The same `Vec4(...)`, `*`, `.xyz` syntax that works on CPU `Vec4` values
also works in shader context — but produces AST nodes instead of computed
values.

**Future: Shared CPU/GPU functions**:

```scala
// This function works on both sides:
def phongLighting(normal: Vec3, lightDir: Vec3, viewDir: Vec3): Vec3 =
  val ambient = 0.1
  val diffuse = max(dot(normal, lightDir), 0.0)
  val reflect = reflect(-lightDir, normal)
  val specular = pow(max(dot(viewDir, reflect), 0.0), 32.0)
  Vec3(ambient + diffuse + specular)

// CPU: called normally, returns a Vec3 value
val cpuResult = phongLighting(n, l, v)

// GPU: captured as AST, emitted as WGSL function
fragment = (input, uniforms) => FragOut(
  color = Vec4(phongLighting(input.normal, uniforms.lightDir, input.viewDir), 1.0)
)
```

### 7.4 Implementation Strategy for the DSL

The DSL can be built using Scala 3's metaprogramming:

- **Opaque GPU expression types**: `GpuFloat`, `GpuVec3`, `GpuMat4` that mirror
  the CPU types but build an AST instead of computing values
- **Implicit conversions**: CPU `Vec3` → `GpuVec3` literal in shader context
- **Operator overloading**: `+`, `*`, `.xyz` on GPU types produce AST nodes
- **Inline macros**: `shader { ... }` block captures the body as AST at compile
  time, type-checks it using Scala's type system, and serializes to WGSL
- **Shared trait hierarchy**: `trait Vec3Ops[V]` implemented by both CPU `Vec3`
  and GPU `GpuVec3`, enabling truly shared function signatures

### 7.5 Design Constraints from the Vision

These constraints should guide decisions in earlier milestones:

1. **Keep math types trait-based**: The trait hierarchy (`Vec3Base`,
   `Vec3ImmutableOps`, etc.) is essential — GPU expression types will implement
   the same traits, enabling shared functions.

2. **Don't hardcode WGSL strings in the framework**: Use string generation from
   structured representations wherever possible, so we can later swap in
   AST-based generation.

3. **Keep shader body separate from contract**: The `ShaderDef` already
   separates the type-level contract (type parameters) from the body (strings).
   This separation is exactly right — the body becomes the DSL output.

4. **Named bindings by name, not index**: Milestone 2's named bindings ensure
   that both CPU binding code and GPU shader code refer to the same names. When
   the DSL arrives, `uniforms.viewProj` in the shader DSL and
   `shape.set("viewProj" -> ...)` on the CPU resolve to the same binding — by
   name, type-checked.

5. **Math functions as pure expressions**: Prefer pure, side-effect-free math
   functions. These are the ones that can be lifted to the GPU. Mutable math
   operations (`.add`, `.set`) are CPU-only by nature.

---

## 8. WebGPU Facade Extensions

Needed across milestones. Only add what's needed per milestone.

**Milestone 1, Step A** (direct-to-canvas):

```scala
// Navigator / GPU
def getPreferredCanvasFormat(): String
// + any missing render pass descriptor fields
```

**Milestone 1, Step B** (render-to-texture + blit):

```scala
// Navigator / GPU
def getPreferredCanvasFormat(): String

// Device
def createTexture(descriptor: js.Dynamic): GPUTexture
def createSampler(descriptor: js.Dynamic): GPUSampler

// Texture
def createView(): GPUTextureView
def createView(descriptor: js.Dynamic): GPUTextureView
def destroy(): Unit

// Constants
object GPUTextureUsage:
  val RENDER_ATTACHMENT: Int
  val TEXTURE_BINDING: Int

@js.native trait GPUSampler extends js.Object
```

**Milestone 3+** (textures, effects):

```scala
@js.native trait GPUSampler extends js.Object

def createSampler(descriptor: js.Dynamic): GPUSampler
def createTexture(descriptor: js.Dynamic): GPUTexture

def width: Int; def height: Int; def mipLevelCount: Int
def destroy(): Unit  // GPUTexture, GPUBuffer

def setIndexBuffer(buffer: GPUBuffer, format: String): Unit
def drawIndexed(indexCount: Int, ...): Unit

object GPUTextureUsage:
  val COPY_SRC, COPY_DST, TEXTURE_BINDING, STORAGE_BINDING, RENDER_ATTACHMENT: Int

def copyTextureToTexture(src: js.Dynamic, dst: js.Dynamic, size: js.Dynamic): Unit
```

---

## 9. Open Questions

1. **Typed binding syntax**: Named tuple subset syntax
   (`shape.set((color = v, time = t))`) vs string key syntax
   (`shape.set("color" -> v, "time" -> t)`)? Named tuples are more
   Scala-idiomatic but may hit the named-tuple-reduction issue in match types.
   String keys are simpler to implement and still type-checkable via inline.

2. **Shade type parameter erasure**: If `Shade[U]` carries `Uniforms` as a type
   parameter, does that complicate storage in collections (`List[Shade[?]]`)?
   May need a non-generic base trait with typed extension.

3. ~~**Form creation ergonomics**~~: **Resolved** — `painter.form(structArray)`
   infers vertex count from `StructArray.length`.

4. **Panel texture format**: When panels get advanced features (Milestone 4),
   should formats be inferred from the shades or always explicit?

---

## 10. File Organization

```
src/graphics/painter/
├── enums.scala        — Opaque types: PrimitiveTopology, CullMode, FrontFace, BlendFactor, BlendOp, BlendState
├── shade.scala        — Shade[U] class + factories
├── form.scala         — Form class
├── shape.scala        — Shape[U] class + typed bind(), processEntry
├── layer.scala        — Layer[U]: fullscreen post-processing (Rust "Effect")
├── panel.scala        — Panel: render target + layers + onResize
├── painter.scala      — Painter class, pipeline cache, draw/paint/show/onResize
└── init.scala         — initPainter bootstrap function

src/graphics/shader/
├── types.scala        — WGSLType type class, VertexUniform/FragmentUniform/SharedUniform
├── derive.scala       — WGSL generation, uniformFieldIndex, checkUniformFieldType
├── layouts.scala      — vertex + bind group layout derivation
├── builtins.scala     — builtin attribute types (BuiltinVertexIndex, etc.)
├── shader.scala       — ShaderDef, Shader.full
└── dsl/
    ├── types.scala        — Expr types: FloatExpr, Vec2/3/4Expr, Mat2/3/4Expr, BoolExpr,
    │                         AssignTarget, Stmt, Block; Local types: LetFloat, LetVec2/3/4,
    │                         VarFloat, VarVec2/3/4, ConstFloat, ConstVec2/3/4
    ├── context.scala      — VertexCtx, FragmentCtx, TypedExprAccessor, TypedLocalAccessor,
    │                         TypedAssignAccessor, VertexOut; ToExpr/ToLocal/UniformToExpr type maps
    ├── program.scala      — Program[A, V, U]: DSL builder for vert/frag shaders with typed locals
    ├── layer_program.scala — LayerProgram[U]: DSL builder for layer (fullscreen) fragment shaders
    └── fn.scala           — WgslFn helpers: WgslFn.raw, WgslFn.dsl for calling raw/DSL functions

src/graphics/math/
├── vec2.scala / vec3.scala / vec4.scala — Vec*ImmutableOps traits: full math including
│                                           lerp, gte/gt/lte/lt, comparison operators < <= > >=
├── mat2.scala / mat3.scala / mat4.scala — matrix types
├── cpu/                                 — CPU concrete implementations
└── gpu/
    ├── expr.scala   — FloatExpr/Vec2/3/4Expr/Mat2/3/4Expr with WGSL-optimized overrides
    │                   (abs, fract, mix/lerp, normalize, dot, length, step/gte/gt/lte/lt,
    │                    smoothstep, min, max, clamp, sign, floor, ceil, round, exp, log, sqrt,
    │                    vector comparison operators using WGSL step())
    └── package.scala — re-exports + GPU math helpers (vec2/3/4, mat2/3/4 constructors)

trivalibs/src/utils/numbers.scala
    — NumExt trait: mix, lerp (alias), step (alias for gte), gte/gt/lte/lt (0/1 result)

src/webgpu/
└── facades.scala      — extended incrementally per milestone

drafts/                        — iterative working examples
├── index.html                 — portal page linking to all drafts
├── out/                       — compiled JS output
├── simple_triangle/           — raw WebGPU, hardcoded vertices
├── buffer_triangle/           — raw WebGPU + typed helpers
├── painter_triangle/          — Painter + draw() (Milestone 1 Step A)
├── panel_triangle/            — Panel + paint/show (Milestone 1 Step B)
├── painter_typed_bindings/    — type-safe named bindings (Milestone 2)
├── painter_dsl/               — Shader DSL with Program builder (Milestone 3)
└── panel_layer/               — fullscreen animated plasma via layerShade + onResize (Milestone 3)
```

---

## 11. Implementation Status

### Milestone 1, Step A — Direct-to-Canvas Rendering: **Done**

All planned components are implemented and working:

- **`initPainter`** (`src/gpu/painter/init.scala`) — Promise-based GPU bootstrap
  with ResizeObserver and canvas setup
- **`Shade`** (`src/gpu/painter/shade.scala`) — wraps `ShaderDef` into
  `GPUShaderModule` + pipeline layout, with unique ID for cache keying
- **`Form`** (`src/gpu/painter/form.scala`) — vertex buffer from `StructArray`,
  topology, front face
- **`Shape`** (`src/gpu/painter/shape.scala`) — Form + Shade + sparse binding
  slots + cull/blend state, with fluent `.bind()` API
- **`Painter`** (`src/gpu/painter/painter.scala`) — device holder, factory
  methods (`shade`, `form`, `binding`, `shape`), `draw()` with pipeline caching
- **Enums** (`src/gpu/painter/enums.scala`) — `PrimitiveTopology`, `CullMode`,
  `FrontFace`, `BlendFactor`, `BlendOp`, `BlendState` as opaque types
- **Draft** (`drafts/painter_triangle/`) — animated colored triangle with
  uniform tint color, validates the full pipeline end-to-end

#### Design Divergences from Original Plan

| Original Design                           | Actual Implementation                                     | Reason                                                                                           |
| ----------------------------------------- | --------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| `CanvasApp` trait with lifecycle          | `initPainter` callback                                    | Browser APIs are already ergonomic; Rust-style lifecycle management adds unnecessary abstraction |
| `enum` for GPU constants                  | `opaque type = String` with `.toJs`                       | Enums generate ~400 lines of JS boilerplate; opaque types compile to bare string constants       |
| `Map[Int, BufferBinding]` for bindings    | `Arr[BufferBinding \| Null]` (sparse JS array)            | Zero Scala collection overhead; aligns with JS bundle size rules                                 |
| `BlendMode` enum (Replace/Alpha/...)      | `BlendState` class + `BlendFactor`/`BlendOp` opaque types | More flexible composition; pre-built constants for common modes                                  |
| `shape.bindings = Map(...)` assignment    | `shape.bind(0 -> b)` fluent API                           | Cleaner chaining: `painter.shape(form, shade).bind(0 -> tintColor)`                              |
| `Option` for optional values              | `Opt` (`js.UndefOr`)                                      | Follows JS bundle size rules                                                                     |
| `Dict` (js.Dictionary) for pipeline cache | `Dict[GPURenderPipeline]` with string keys                | Zero-cost JS object as hashmap                                                                   |

### Milestone 1, Step B — Panel + Blit: **Done**

### Milestone 2 — Type-Safe Named Bindings: **Done**

Type-safe named bindings are implemented and validated with a working draft
example. Both raw values and external `BufferBinding` objects are supported,
with compile-time validation of binding names and value types.

#### What was implemented

**`Shade[U]`** — phantom type parameter carries the `Uniforms` type:

- `Shade` (`src/gpu/painter/shade.scala`) now takes a type parameter `U`
  representing the Uniforms named tuple
- `painter.shade[A, V, U](...)` returns `Shade[U]`, propagating the type

**`Shape[U]`** — typed shape with named `bind()`:

- `Shape` (`src/gpu/painter/shape.scala`) takes type parameter `U` from its
  `Shade[U]`
- Holds `device: GPUDevice` reference for auto-creating managed bindings
- Retains the untyped `bind(slots: (Int, BufferBinding)*)` for backward
  compatibility
- New typed `bind("name" := value)` overloads (1–3 entries) using `BindPair`

**`BindPair[N, V]`** — typed name-value pair:

- `BindPair` class + `:=` extension method on string literals
- `"color" := Vec3(1, 0, 0)` creates a `BindPair["color", Vec3]`
- The string literal type `N` is preserved as a singleton type for compile-time
  name resolution

**Compile-time validation** (`src/gpu/shader/derive.scala`):

- `uniformFieldIndex[Name, U]` — resolves field name to binding slot index at
  compile time, produces error if name not found
- `checkUniformFieldType[Name, V, U]` — validates value type matches the
  expected uniform field type. Uses `summonFrom` to accept both:
  - Raw values: `V =:= UnwrapUniform[field]`
  - BufferBindings: `V <:< BufferBinding[UnwrapUniform[field], ?]`
  - Custom error message: "Binding type mismatch" on failure

**`processEntry` in Shape** — inline dispatch:

- Calls `checkUniformFieldType` first (applies to both raw and binding values)
- Uses `inline value match` to separate `BufferBinding` from raw values at
  compile time (not runtime `match`, which would type-check both branches)
- Raw values: reuses existing binding if slot occupied (calls `.set()`),
  otherwise auto-creates a managed `BufferBinding` via `summonFrom` on
  `UniformValue[V, f]`
- External bindings: stored directly in the slot

**Draft** (`drafts/painter_typed_bindings/`):

- Two animated triangles sharing the same shade/form with different bindings
- Tests 3 uniform types: `Vec3` (fragment color), `Mat2` (vertex rotation),
  `Vec2` (vertex translation)
- Triangle 1: raw values for all bindings, auto-managed internally
- Triangle 2: external `BufferBinding[Mat2]` for rotation (updated via
  `mat1.update(_.rotate(...))`), raw values for color and translation
- Validates compile-time error for wrong binding name, wrong raw value type, and
  wrong `BufferBinding` inner type

#### Design divergences from original plan

| Original Design                                                 | Actual Implementation                                           | Reason                                                                                                                    |
| --------------------------------------------------------------- | --------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `shape.set("name" -> value)` with `->` tuples                   | `shape.bind("name" := value)` with `BindPair`                   | `:=` creates a typed `BindPair[N, V]` preserving the singleton string type; `->` creates `(String, Any)` losing type info |
| Named tuple subset syntax (Option C: `shape.set((color = v))`)  | String-keyed `BindPair` with `summonFrom`                       | Avoids named-tuple-reduction issues in match types; simpler to implement; still fully type-checked                        |
| `Map[String, BufferBinding]` for managed bindings               | Same `Arr[BufferBinding \| Null]` sparse array from Milestone 1 | Name→index resolution happens at compile time; runtime storage remains index-based                                        |
| Separate managed vs external binding maps                       | Single `BindingSlots` array                                     | Both managed and external bindings are `BufferBinding` objects; no need for separate storage                              |
| `Shade` stores runtime `Map[String, (Int, Int)]` name→slot      | Compile-time only via `uniformFieldIndex` inline                | No runtime name resolution needed; all names resolve to indices at compile time                                           |
| Match type `UnwrapBinding[V]` for BufferBinding type extraction | `summonFrom` with `V <:< BufferBinding[Expected, ?]`            | Match types on `final class` don't reliably reduce in Scala 3; `summonFrom` with `<:<` works correctly                    |

---

### Milestone 3 — Shader DSL + Layer/LayerShade: **Done**

#### Shader DSL

The Scala shader DSL replaces raw WGSL string bodies with a type-safe expression
tree that compiles to WGSL. It is split across `src/graphics/shader/dsl/`.

**Expression types** (`dsl/types.scala`):

- `FloatExpr`, `Vec2Expr`, `Vec3Expr`, `Vec4Expr`, `Mat2Expr`, `Mat3Expr`,
  `Mat4Expr` — GPU expression wrappers carrying a `.wgsl: String` that emits the
  WGSL representation
- `BoolExpr` — for WGSL boolean expressions
- `AssignTarget` — an l-value (e.g. `out.color`) that can receive `:=`
- `Stmt` — a single WGSL statement; `Block(stmts*)` — a sequence of statements
- **Local variable types**:
  - `LetFloat/Vec2/Vec3/Vec4` — `let` bindings (immutable in WGSL, reassignable
    by re-declaring; generates `var` in WGSL for flexibility)
  - `VarFloat/Vec2/Vec3/Vec4` — mutable `var` local variables
  - `ConstFloat/Vec2/Vec3/Vec4` — `const` compile-time constants

**Typed accessors** (`dsl/context.scala`):

- `TypedExprAccessor[T]` — provides typed named field access over a named tuple
  `T`, returning the correct expression type per field (e.g. `ctx.in.position`
  returns a `Vec2Expr`)
- `TypedLocalAccessor[T]` — provides typed access to local variables with `:=`
  assignment support
- `TypedAssignAccessor[T]` — provides typed l-values for output fields
- `VertexOut[V]` — vertex output accessor with built-in `position` (`Vec4Expr`)
  plus user-defined varyings

**Contexts**:

- `VertexCtx[A, V, U, L]` — vertex shader context with `in`, `out`, `bindings`,
  `locals`
- `FragmentCtx[V, U, L]` — fragment shader context with `in`, `out`, `bindings`,
  `locals`

**`Program[A, V, U]`** (`dsl/program.scala`) — DSL builder for full vertex +
fragment shaders:

- `program.vert[L](ctx => Block(...))` — define vertex shader; `[L]` is an
  optional named tuple of local variable types
- `program.frag[L](ctx => Block(...))` — define fragment shader
- `program.fn(wgslFn)` — register a helper function (idempotent)
- `painter.shade[A, V, U](program => ...)` accepts a `Program[A, V, U]` builder
  callback

**`LayerProgram[U]`** (`dsl/layer_program.scala`) — DSL builder for fullscreen
layer fragment shaders:

- Fixed vertex stage (fullscreen triangle from `vertex_index`, passes UV
  varying)
- `program.frag[L](ctx => Block(...))` — define only the fragment shader; input
  is `ctx.in.uv: Vec2Expr` in 0..1 range
- `painter.layerShade[U](program => ...)` accepts a `LayerProgram[U]` callback

**`WgslFn` helpers** (`dsl/fn.scala`):

- `WgslFn.raw(name, src)` — register a raw WGSL function string
- `WgslFn.dsl[(Params), Return](name)(body)` — define a helper function using
  the DSL; `body` receives typed parameter accessors and a `ret()` callback
- Registered functions are emitted into the shader before `vs_main`/`fs_main`

**Draft** (`drafts/painter_dsl/`):

- Two animated triangles sharing one shade defined with `Program`
- Uses a `WgslFn.dsl` helper (`apply_transform`) for mat2 rotation + translation
- Vertex shader has a typed local variable `t: Vec2`
- Fragment shader reads `ctx.bindings.color`
- Validates DSL-generated WGSL produces correct visuals end-to-end

---

#### Layer / LayerShade

**`Layer[U]`** (`src/graphics/painter/layer.scala`):

- Typed companion to `Shape[U]` for fullscreen post-processing
- Holds `shade: Shade[U]`, `bindings: BindingSlots`, optional `blendState`
- Typed `bind("name" := value)` overloads — same compile-time validation as
  `Shape` via `processEntry`
- `painter.layer(shade)` factory method

**Fullscreen layer vertex shader**:

- Embedded in `ShaderDef` as `LAYER_VERT_BODY` — generates a full-screen
  triangle from `vertex_index` built-in, computing UV as
  `(x * 0.5, 1.0 - y * 0.5)`
- Uses `VBI = (vertex_index: BuiltinVertexIndex)` as vertex input

**`Shade[U]` for layers**:

- `painter.layerShade[U](program => ...)` — creates a `Shade[U]` using
  `Shader.full` with `VBI`, fixed vert body, and user-defined frag body from
  `LayerProgram[U]`
- Returns the same `Shade[U]` type as regular shade, compatible with `Layer[U]`

**Panel integration**:

- `painter.panel(layers = Arr(layer1, layer2, ...))` — Panel now accepts a
  `layers` list in addition to (or instead of) shapes
- `paint(panel)` renders shapes first, then layers in order into the panel
  texture

**Draft** (`drafts/panel_layer/`):

- Fullscreen animated plasma effect
- Uniforms: `time: FragmentUniform[Float]`, `res: FragmentUniform[Vec2]`
- Fragment shader: aspect-corrected UV
  `p = (uv - 0.5) * vec2(aspect * 2.0, 2.0)`, then three sinusoidal color
  channels using `p.x`, `p.y`, `p.length`, and `time`
- Demonstrates `ctx.in.uv`, `LetFloat`/`LetVec2` local variables, and `Block`
  DSL

---

#### `onResize` — Painter Resize Callback

**`Painter.onResize`** (`src/graphics/painter/painter.scala`):

- `painter.onResize((w, h) => ...)` — register a callback invoked whenever the
  canvas dimensions change
- Resize is detected at the start of `paint()` by comparing canvas size against
  previously stored dimensions
- First call always fires (previous dimensions start at 0×0)
- Implemented with a `resizeCallbacks: Arr[(Int, Int) => Unit]` array and a
  manual `while` loop to avoid Scala collection overhead
- Used in `panel_layer` draft to update the `res` binding when the canvas is
  resized, keeping the aspect ratio correct

---

#### Math Library Extensions

**`NumExt` trait** (`trivalibs/src/utils/numbers.scala`):

- Added `lerp(b, t)` as `inline def lerp = mix(b, t)` — alias for `mix`;
  automatically delegates to whichever `mix` override exists in the given
- Added `gte(edge)`, `gt(edge)`, `lte(edge)`, `lt(edge)` — return `1.0`/`0.0`
  (not boolean) for threshold comparisons
- Changed `step` to `inline def step(edge) = gte(edge)` — step is now an alias
  for `gte` for WGSL familiarity
- CPU implementations in `NumExt[Double]` and `NumExt[Float]` givens use
  `if/else` expressions

**Vec2/3/4 ImmutableOps traits** (`src/graphics/math/vec2.scala` etc.):

- Added `lerp(b, t)` overloads (vec-vec-vec and vec-vec-scalar) as inline
  aliases for `mix`
- Added componentwise comparison operators `<`, `<=`, `>`, `>=` — fill a new
  vector with `0.0`/`1.0` per component using `gte`/`gt`/`lte`/`lt` on scalars

**GPU expression givens** (`src/graphics/math/gpu/expr.scala`):

All operations generate single WGSL built-in calls instead of component-wise
expansion:

- `NumExt[FloatExpr]` given: `gte` → `step(edge, x)`, `lte` → `step(x, edge)`,
  `gt` → `(1.0 - step(x, edge))`, `lt` → `(1.0 - step(edge, x))`
- `Vec*ImmutableOps` givens: `normalize`, `abs`, `sign`, `floor`, `ceil`,
  `round`, `fract`, `exp`, `log`, `log2`, `sqrt` → single WGSL call
- `min`, `max`, `clamp`, `mix`/`lerp`, `step`, `smoothstep` → single WGSL call
- Vector comparison operators `<`, `<=`, `>`, `>=` → WGSL `step()`-based
  expressions on vector types:
  - `v >= other` → `step(other, v)` (1 where component ≥ edge)
  - `v <= other` → `step(v, other)`
  - `v > other` → `(1.0 - step(v, other))`
  - `v < other` → `(1.0 - step(other, v))`
- `Vec*Base` givens: `dot(a, b)` → `dot(a, b)`, `length` → `length(v)`
