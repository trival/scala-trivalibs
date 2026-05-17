# Refactor: Mutable painter entities with unified Painter access

DONE: This refactoring was successfully implemented and tested, resulting in a
more flexible and consistent painter API. The key changes include:

## Context

The painter API has several inconsistencies and rigidities:

- `specWidth`/`specHeight` on Panel and topology/frontFace on Form are `val` —
  can't change at runtime
- Entities mix `Painter`, `GPUDevice`, and `GPUQueue` as constructor args
  inconsistently
- All non-type-level properties get passed at construction, making runtime
  mutation awkward
- The `Bindable` trait hardcodes `device: GPUDevice` directly instead of going
  through Painter

**Goal:** Constructor takes only `painter` (+ `shade`/`form` for typed/required
entities); all rendering properties live as `var` with defaults and are
configured via a `set(...)` method with all-`Maybe` params. Type-level safety
(compile-time bind checks via U, P) stays intact. Missing required GPU resources
(e.g. Form vertices not set) → runtime crash.

## Design Principles

- **Static (type-level):** `shade: Shade[U, P]` on Shape/Layer — required at
  construction so U/P propagate to `bind()`; `form: Form` on Shape — required
  because a shape without geometry is meaningless
- **Dynamic (runtime vars):** everything else — cullMode, blendState,
  clearColor, specSize, shapes/layers arrays, topology, frontFace, vertices
- **All entities access GPU through `painter`** — no direct
  `GPUDevice`/`GPUQueue` args in entity constructors
- **GPU resource cleanup:** Only `GPUBuffer`, `GPUTexture`, `GPUQuerySet`, and
  `GPUDevice` have `destroy()` in the WebGPU spec. `GPURenderPipeline`,
  `GPUSampler`, `GPUShaderModule`, `GPUBindGroup`, `GPUBindGroupLayout`, and
  `GPUPipelineLayout` do NOT — they are GC'd by the browser.

### `Maybe` vs `Opt` — two distinct null-like semantics

- **`Maybe[T]`** = `js.UndefOr[T]` (JS `undefined`-based). Used for **function
  parameters** where "not provided" means "don't change". Empty value:
  `Maybe.Not`. Methods: `.orElse(default)`, `.safe`.
- **`Opt[T]`** = `T | Null` (JS `null`-based). Used for **fields** that can be
  meaningfully absent at runtime (e.g. `blendState`, `clearColor`, GPU resources
  before allocation). Empty value: `Opt.Null` (= `null`). Methods: `.isNull`,
  `.nonNull`, `.getOr(default)`, `.get`.

### Three-state `set()` pattern for resettable fields

For fields that can be explicitly unset (e.g. `blendState`, `clearColor`), the
`set()` parameter type is `Maybe[Opt[T]]`:

- `Maybe.Not` (= `js.undefined`) → don't change the field
- `Opt.Null` (= `null`) → explicitly unset the field
- `T` value → set to that value

Both `null` and `T` values are subtypes of `Opt[T]` which is a subtype of
`Maybe[Opt[T]]`, so the user writes naturally:

- `.set(blendState = BlendState.Alpha)` — set
- `.set(blendState = null)` — unset
- omit parameter — don't change

Inside `set()`, `Maybe.foreach` skips `undefined` but passes both `null` and
real values through:

```scala
blendState.foreach(v => this.blendState = v)
```

### Non-resettable `set()` parameters

For fields that always have a value (e.g. `cullMode`, `topology`, `depthTest`),
the parameter type is simply `Maybe[T]`:

- `Maybe.Not` → don't change
- `T` value → set to that value

## Files to Modify

- `src/graphics/painter/shape.scala` — constructor(painter, shade, form); `set`
  method; `device` via painter
- `src/graphics/painter/layer.scala` — constructor(painter, shade); `set`
  method; `device` via painter
- `src/graphics/painter/panel.scala` — constructor(painter); make all props
  `var`; `set` method; internal textures use `Opt[T]`
- `src/graphics/painter/form.scala` — constructor(painter); `set` method with
  buffer destroy; move buffer allocation from painter factory to `set`
- `src/graphics/painter/painter.scala` — simplify factory methods; update render
  methods for `Opt` field access
- `src/webgpu/facades.scala` — add `destroy()` to `GPUBuffer` trait
- Any draft files that use the old factory API

## Step 1 — `Bindable[U, P]` trait: `device` via `painter`

Change abstract member from `val device: GPUDevice` to `val painter: Painter`,
expose `device` as a def:

```scala
trait Bindable[U, P]:
  val shade: Shade[U, P]
  val painter: Painter
  inline def device: GPUDevice = painter.device
  var bindings: BindingSlots
  var panelBindings: Arr[Panel | Null]
  // bind overloads and processEntry unchanged
```

## Step 2 — `Shape[U, P]`

`form` is required at construction (a shape without geometry is meaningless).
Geometry changes are handled by calling `form.set(vertices = ...)` on the form
in place — the shape's `form` ref stays fixed.

`blendState` is `Opt[BlendState]` — `Opt.Null` means no blending. In `set()`,
the param type is `Maybe[Opt[BlendState]]` for the three-state pattern.

```scala
class Shape[U, P](
    val painter: Painter,
    val shade: Shade[U, P],
    val form: Form,
) extends Bindable[U, P]:
  var cullMode: CullMode = CullMode.None
  var blendState: Opt[BlendState] = Opt.Null
  var bindings: BindingSlots = Arr()
  var panelBindings: Arr[Panel | Null] = Arr()

  def set(
      cullMode: Maybe[CullMode] = Maybe.Not,
      blendState: Maybe[Opt[BlendState]] = Maybe.Not,
  ): this.type =
    cullMode.foreach(v => this.cullMode = v)
    blendState.foreach(v => this.blendState = v)
    this

  // legacy slot-based bind stays as-is
  def bind(slots: (Int, BufferBinding[?, ?])*): Shape[U, P] = ...
```

Note how `blendState.foreach(v => this.blendState = v)` handles all cases:

- `Maybe.Not` → foreach skipped, field unchanged
- `null` (= `Opt.Null`) → foreach runs, field set to `null` (blending disabled)
- `BlendState(...)` → foreach runs, field set to blend state

## Step 3 — `Layer[U, P]`

Same pattern as Shape for blendState.

```scala
class Layer[U, P](
    val painter: Painter,
    val shade: Shade[U, P],
) extends Bindable[U, P]:
  var blendState: Opt[BlendState] = Opt.Null
  var bindings: BindingSlots = Arr()
  var panelBindings: Arr[Panel | Null] = Arr()

  def set(
      blendState: Maybe[Opt[BlendState]] = Maybe.Not,
  ): this.type =
    blendState.foreach(v => this.blendState = v)
    this
```

## Step 4 — `Panel`

Internal texture fields use `Opt[T]` with `Opt.Null` as initial value. Cleanup
uses `.nonNull` / `.get` methods.

`clearColor` is `Opt[ClearColor]` — `Opt.Null` means don't clear. Default is
black `(0.0, 0.0, 0.0, 1.0)`.

```scala
type ClearColor = (Double, Double, Double, Double)

class Panel(val painter: Painter):
  var specWidth: Int = 0
  var specHeight: Int = 0
  var clearColor: Opt[ClearColor] = (0.0, 0.0, 0.0, 1.0)
  var depthTest: Boolean = false
  var shapes: Arr[Shape[?, ?]] = Arr()
  var layers: Arr[Layer[?, ?]] = Arr()

  private var _texture: Opt[GPUTexture] = Opt.Null
  private var _textureView: Opt[GPUTextureView] = Opt.Null
  private var _depthTexture: Opt[GPUTexture] = Opt.Null
  private var _depthView: Opt[GPUTextureView] = Opt.Null
  private var _width: Int = 0
  private var _height: Int = 0

  def textureView: GPUTextureView = _textureView.get
  def depthView: GPUTextureView = _depthView.get

  def set(
      width: Maybe[Int] = Maybe.Not,
      height: Maybe[Int] = Maybe.Not,
      clearColor: Maybe[Opt[ClearColor]] = Maybe.Not,
      depthTest: Maybe[Boolean] = Maybe.Not,
      shapes: Maybe[Arr[Shape[?, ?]]] = Maybe.Not,
      layers: Maybe[Arr[Layer[?, ?]]] = Maybe.Not,
  ): this.type =
    width.foreach(v => this.specWidth = v)
    height.foreach(v => this.specHeight = v)
    clearColor.foreach(v => this.clearColor = v)
    depthTest.foreach(v => this.depthTest = v)
    shapes.foreach(v => this.shapes = v)
    layers.foreach(v => this.layers = v)
    this

  private[painter] def ensureSize(canvasW: Int, canvasH: Int): Unit =
    val targetW = if specWidth == 0 then canvasW else specWidth
    val targetH = if specHeight == 0 then canvasH else specHeight
    if targetW != _width || targetH != _height then
      if _texture.nonNull then _texture.get.destroy()
      if _depthTexture.nonNull then _depthTexture.get.destroy()
      _width = targetW
      _height = targetH
      val tex = painter.device.createTexture(...)
      _texture = tex
      _textureView = tex.createView()
      if depthTest then
        val depthTex = painter.device.createTexture(...)
        _depthTexture = depthTex
        _depthView = depthTex.createView()
```

## Step 5 — `Form`

Buffer allocation moves from painter factory to `Form.set()`. Uses
`Opt[GPUBuffer]` with `.nonNull` / `.get` for destroy:

```scala
class Form(val painter: Painter):
  var vertexBuffer: Opt[GPUBuffer] = Opt.Null
  var vertexCount: Int = 0
  var topology: PrimitiveTopology = PrimitiveTopology.TriangleList
  var frontFace: FrontFace = FrontFace.CCW

  def set[F <: Tuple](
      vertices: Maybe[StructArray[F]] = Maybe.Not,
      topology: Maybe[PrimitiveTopology] = Maybe.Not,
      frontFace: Maybe[FrontFace] = Maybe.Not,
  ): this.type =
    topology.foreach(v => this.topology = v)
    frontFace.foreach(v => this.frontFace = v)
    vertices.foreach: verts =>
      if vertexBuffer.nonNull then vertexBuffer.get.destroy()
      val buf = painter.device.createBuffer(
        Obj.literal(
          size = verts.byteLength,
          usage = GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
        ),
      )
      painter.queue.writeBuffer(buf, 0, verts.arrayBuffer)
      vertexBuffer = buf
      vertexCount = verts.length
    this
```

**Note:** `GPUBuffer.destroy()` must be added to `src/webgpu/facades.scala`:

```scala
@js.native
trait GPUBuffer extends js.Object:
  def destroy(): Unit = js.native
```

## Step 6 — `Painter` factory methods

Thin wrappers — all setup moved to entity `set` methods:

```scala
def shape[U, P](shade: Shade[U, P], form: Form): Shape[U, P] =
  Shape(this, shade, form)

def layer[U, P](shade: Shade[U, P]): Layer[U, P] =
  Layer(this, shade)

def panel(): Panel = Panel(this)

def form(): Form = Form(this)
```

Remove old parameterized factory methods (old `form[F]` with vertices, old
`shape` with bindings/cullMode/blendState args, old `layer` with bindings/
blendState args, old `panel` with all args). `binding[T]` stays as-is.

**`renderShapeOnPass` in painter.scala:**

- `shape.form.vertexBuffer` is `Opt[GPUBuffer]` — use `.get` (crashes if not
  set, by design)
- `!= null` checks on Shade layouts (`valueBindGroupLayout`,
  `panelBindGroupLayout`, `vertexBufferLayout`) remain as-is — these are
  `T | Null` computed at Shade creation time, not user-settable
- `shape.blendState` checks use `.isNull` / `.nonNull` / `.get`

## Step 7 — Pipeline cache and runtime mutation

The pipeline key is computed on every `renderShapeOnPass` call from current
values of `shade.id`, `shape.blendState`, `shape.cullMode`,
`shape.form.topology`, `shape.form.frontFace`. **No change needed** — the cache
already handles key changes transparently by creating a new cached pipeline. Old
pipelines stay in cache (acceptable; WebGPU pipelines don't have a destroy()
method).

**Pipeline key update:** The `blendState` portion of the key uses `.isNull`:

```scala
val key = s"${shade.id}|${shape.blendState.isNull}|${shape.cullMode}|..."
```

## Step 8 — GPU resource cleanup summary

Verified against `@webgpu/types` — only these WebGPU interfaces have
`destroy()`:

- **GPUBuffer** — `destroy(): undefined`
- **GPUTexture** — `destroy(): undefined`
- **GPUQuerySet** — `destroy(): undefined`
- **GPUDevice** — `destroy(): undefined`

Everything else (GPURenderPipeline, GPUSampler, GPUShaderModule, GPUBindGroup,
GPUBindGroupLayout, GPUPipelineLayout) has no destroy() and is GC'd by the
browser.

| Resource                  | When to destroy        | Cleanup pattern                                             |
| ------------------------- | ---------------------- | ----------------------------------------------------------- |
| Panel `_texture`          | On size change         | `if _texture.nonNull then _texture.get.destroy()`           |
| Panel `_depthTexture`     | On size change         | `if _depthTexture.nonNull then _depthTexture.get.destroy()` |
| Form `vertexBuffer`       | On `set(vertices=...)` | `if vertexBuffer.nonNull then vertexBuffer.get.destroy()`   |
| `BufferBinding.gpuBuffer` | When binding replaced  | Leave to user (shared bindings)                             |

## Usage after refactor

```scala
val form = painter.form().set(vertices = myVerts)
val shape = painter.shape(shade, form)
  .set(cullMode = CullMode.Back)
  .bind("color" := Vec3(1, 0, 0))

val layer = painter.layer(layerShade)
  .set(blendState = BlendState.Alpha)
  .bind("time" := timeBinding)

val panel = painter.panel()
  .set(shapes = Arr(shape), layers = Arr(layer))
panel.clearColor = (0.1, 0.1, 0.1, 1.0)

// runtime mutation
shape.set(cullMode = CullMode.Front)
shape.form.set(vertices = updatedVerts)  // destroys old GPU buffer
shape.set(blendState = null)             // explicitly disable blending
panel.set(clearColor = null)             // explicitly disable clear
```

## Files to update (drafts)

Search for all draft files using old `painter.shape(form, shade, ...)` /
`painter.layer(shade, ...)` / `painter.panel(...)` /
`painter.form(vertices, ...)` / `Form(device, queue, ...)` signatures and update
to new API.

## Verification

```bash
bun run build   # all drafts must compile
scala test .    # tests must pass
```

Manually run `drafts/painter_typed_bindings/` and `drafts/panel_layer/` to
verify runtime index lookup still works and rendering is correct.
