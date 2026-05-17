# Mip Levels + Multiple Render Targets — Implementation Plan

## Context

The Scala.js painter library is approaching feature parity with the Rust
`trivalibs_painter`, but two related features are missing:

1. **Mip levels** — automatic mipmap generation, binding specific mip levels,
   rendering to specific mip levels (which disables ping-pong), enhanced sampler
   creation with independent mip/coordinate filtering
2. **Multiple render targets (MRT)** — panels with multiple texture formats for
   techniques like deferred lighting

Both features share a need for richer panel binding descriptors (target index +
mip level), so the binding API is designed as a shared foundation.

---

## Phase 0: Unified PanelBinding API (Foundation)

### New type: `PanelBinding`

In `src/graphics/painter/panel.scala`, add a lightweight JS class:

```scala
class PanelBinding(
    val panel: Panel,
    val index: Int = 0,       // MRT target index (default: 0)
    val mipLevel: Int = -1,   // -1 = all mips / default view
) extends js.Object
```

Add `Panel.binding()` method:

```scala
def binding(index: Int = 0, mipLevel: Int = -1): PanelBinding =
  new PanelBinding(this, index, mipLevel)
```

### Type signature changes

- `PanelBindingValue` in panel.scala:12 — add `PanelBinding` to the union
- `Bindable.panelBindings` in shape.scala:25 — `Arr[Opt[Panel]]` →
  `Arr[Opt[PanelBinding]]`
- `Shape.panelBindings` in shape.scala:257 — same
- `Layer.panelBindings` in layer.scala:11 — same
- `Instance.panelBindings` in instance.scala:10 — same
- `_workPanelBindings` in painter.scala:580 — same

### Update `processEntry` in shape.scala:234-243

Accept both `Panel` (wrap as `PanelBinding(p)`) and `PanelBinding` directly:

```scala
case p: Panel =>
  val idx = shade.panelIndices(pair.name)
  while panelBindings.length <= idx do panelBindings.push(null)
  panelBindings(idx) = PanelBinding(p)
case pb: PanelBinding =>
  val idx = shade.panelIndices(pair.name)
  while panelBindings.length <= idx do panelBindings.push(null)
  panelBindings(idx) = pb
```

Same pattern in `processPanelEntry` (panel.scala:72-73).

### Update `setPanelBindGroup` in painter.scala:670-694

Replace `p.textureView` with resolution through `PanelBinding`:

```scala
// Instead of: resource = p.textureView
// Use: resource = pb.panel.textureViewAt(pb.index, pb.mipLevel)
```

Add `Panel.textureViewAt(index: Int, mipLevel: Int)` — initially delegates to
`textureView` (only index=0, mipLevel=-1 supported until later phases).

### Update helper methods in painter.scala

- `applyPanelRuntimeBindings` (line 597-623): handle `PanelBinding` in the union
  cast
- `applyInstanceBindings` (line 625-641): already uses `panelBindings` array,
  just needs type change
- `copyToWork` (line 582-595): type change for `Arr[Opt[PanelBinding]]`

### User API after this phase

```scala
shape.bind("texture" := panel)                                    // still works
shape.bind("texture" := panel.binding(index = 2, mipLevel = 4))  // detailed form
```

---

## Phase 1: Mip Levels (Milestone 1)

### 1.1 Panel mip configuration

Add to `Panel` in panel.scala:

- Field: `var mipLevels: Int = 1` (1 = no mips; 0 = full chain; N = exact count)
- Add `mipLevels` param to `Panel.set(...)`
- Helper: `def mipLevelCount: Int` — computes actual count from dimensions when
  `mipLevels == 0`

```scala
def mipLevelCount: Int =
  if mipLevels <= 1 then mipLevels
  else if mipLevels == 0 then
    (Math.log(Math.max(_width, _height).toDouble) / Math.log(2.0)).toInt + 1
  else mipLevels
```

### 1.2 Texture allocation with mips

In `Panel.ensureSize` (panel.scala:273), add `mipLevelCount` to `createTexture`
calls for main + pong textures. Depth and MSAA textures stay at mipLevelCount=1.

### 1.3 Mip-level texture views

Add view cache to `Panel`:

```scala
private val _mipViews: js.Dictionary[GPUTextureView] = js.Dictionary()
```

Implement `textureViewAt(index, mipLevel)`:

- `mipLevel < 0` → return default view (all mips)
- `mipLevel >= 0` → create/cache a view with
  `baseMipLevel = mipLevel, mipLevelCount = 1`

Cache key: `s"$index|$mipLevel"`. Clear cache when textures are recreated in
`ensureSize`.

Facade check: `GPUTexture.createView(descriptor: js.Dynamic)` already exists at
facades.scala:156.

### 1.4 Enhanced sampler creation

Add `FilterMode` opaque type (in `src/graphics/painter/enums.scala` or similar):

```scala
opaque type FilterMode = String
object FilterMode:
  val Nearest: FilterMode = "nearest"
  val Linear: FilterMode = "linear"
  extension (x: FilterMode) inline def toJs: js.Any = x.asInstanceOf[js.Any]
```

Add factory method to `Painter` in painter.scala:

```scala
def sampler(
    magFilter: FilterMode = FilterMode.Nearest,
    minFilter: FilterMode = FilterMode.Nearest,
    mipmapFilter: FilterMode = FilterMode.Nearest,
): GPUSampler =
  device.createSampler(Obj.literal(
    magFilter = magFilter.toJs,
    minFilter = minFilter.toJs,
    mipmapFilter = mipmapFilter.toJs,
  ))
```

### 1.5 Automatic mipmap generation

Add private `generateMipmaps(panel: Panel)` to `Painter`:

- Iterates mip levels 1..N, each pass reads mip i-1, writes mip i
- Uses a dedicated mip blit pipeline (reuse BLIT_WGSL shader, cached per format
  string in a `js.Dictionary`)
- Uses a linear-filtering sampler for smooth downsampling
- Called at end of `paint()` (after line 494) when `panel.mipLevelCount > 1`

Key detail: `createView` with `baseMipLevel = N, mipLevelCount = 1` makes the
attachment sized to that mip level — WebGPU auto-sizes the render pass to the
attachment dimensions.

### 1.6 Layer mip source/target

Add to `Layer` in layer.scala:

```scala
var mipSource: Int = -1   // -1 = default (current ping/pong source)
var mipTarget: Int = -1   // -1 = default (ping/pong destination)
```

Add `mipSource` and `mipTarget` params to `Layer.set(...)`.

In `paint()` layer loop (painter.scala:436):

- If `layer.mipTarget >= 0`: render to `panel.textureViewAt(0, layer.mipTarget)`
  instead of `dstView`; skip ping-pong swap
- If `layer.mipSource >= 0`: use `panel.textureViewAt(0, layer.mipSource)` as
  srcView instead of current ping texture

### 1.7 Draft example

Create `drafts/mipmaps/` with a scene that:

- Renders colored geometry to a panel with mips enabled
- Visualizes different mip levels side-by-side using
  `panel.binding(mipLevel = N)`

---

## Phase 2: Multiple Render Targets (Milestone 2)

### 2.1 Panel format configuration

Add to `Panel` in panel.scala:

- Field: `var formats: Arr[String] = Arr()` (empty = use `preferredFormat`,
  single target)
- Add `formats` param to `Panel.set(...)`
- Helper: `def effectiveFormats: Arr[String]` — returns
  `Arr(painter.preferredFormat)` if empty
- Helper: `def targetCount: Int = effectiveFormats.length`

### 2.2 Multi-texture storage

Replace single texture fields with arrays:

```scala
private var _textures: Arr[GPUTexture] = Arr()
private var _textureViews: Arr[GPUTextureView] = Arr()
private var _msaaTextures: Arr[GPUTexture] = Arr()
private var _msaaViews: Arr[GPUTextureView] = Arr()
// depth stays single (shared across all targets)
```

Keep backward-compatible accessors: `textureView` → `_textureViews(0)`, etc.

Update `textureViewAt(index, mipLevel)` to use `_textures(index)`.

### 2.3 Texture allocation

Rewrite `ensureSize` to loop over `effectiveFormats`, creating one texture (+
msaa) per format. Depth texture remains single and shared.

### 2.4 Render pass color attachments

In `paint()` (painter.scala:363), build `colorAttachments` array with one entry
per target instead of the current single attachment.

### 2.5 Pipeline creation — multiple fragment targets

In `createPipeline` (painter.scala:850) and `createLayerPipeline`
(painter.scala:808):

- Build `targets` array from panel's `effectiveFormats` (one entry per format
  with blend state)
- Pass panel format info through — both methods need access to panel's formats
- Update pipeline cache keys to include format list

### 2.6 No ping-pong for MRT (MVP)

For the initial implementation, **MRT panels do not support ping-pong**. This
matches the Rust library's behavior. Layers on MRT panels render directly to the
main textures without swapping.

This simplifies the MVP significantly: no pong textures are allocated for MRT
panels, and the layer loop skips the swap logic when `targetCount > 1`.

See "Future: MRT Ping-Pong" below for analysis of how this could be extended.

### 2.7 Shader DSL: multi-output fragments

Currently `FragmentCtx.out` is hardcoded as
`TypedAssignAccessor[(color: AssignTarget)]` (context.scala:117).

Add type parameter `FO` to `FragmentCtx`, `Program`, and `LayerProgram`:

```scala
class FragmentCtx[V, U, L, P, FO](
    val out: TypedAssignAccessor[NamedTuple.Map[FO & AnyNamedTuple, ToAssign]],
    ...
)
```

Default `FO` to `FragOut = (color: Vec4)` for backward compat. For MRT:

```scala
type GBufferOut = (color: Vec4, normal: Vec4, position: Vec4)
painter.shade[Attribs, Varyings, Uniforms, Panels, GBufferOut] { program =>
  program.frag { ctx =>
    Block(
      ctx.out.color    := ...,
      ctx.out.normal   := ...,
      ctx.out.position := ...,
    )
  }
}
```

The WGSL generation already handles this:
`generateCombinedStruct[FragmentOut, EmptyTuple]("FragmentOutput")` produces
`@location(N)` annotations per field.

The `shade` factory methods in painter.scala need additional overloads accepting
the `FO` type param.

### 2.8 Draft example

Create `drafts/deferred/` with:

- G-buffer panel with 3 formats:
  `Arr("rgba8unorm", "rgba16float", "rgba16float")`
- Scene shapes writing to all 3 targets
- Lighting layer reading all 3 via `panel.binding(index = 0/1/2)`
- Additive blending for light accumulation via instances

---

## Future Enhancement: MRT Ping-Pong

### Analysis

The Rust library disables ping-pong for MRT panels entirely. This is the safe
default: ping-pong requires allocating a second set of all target textures, and
the semantics are non-trivial when layers read/write different subsets of
targets.

However, there are interesting rendering pipelines where MRT ping-pong would be
valuable. Consider a deferred pipeline where:

1. Shapes write to 3 G-buffer targets (color, normal, position)
2. A lighting layer reads all 3 and writes accumulated light to a new set of 3
   targets (or a single output)
3. A post-process layer reads the lighting output

### When MRT ping-pong makes sense

MRT ping-pong is meaningful when:

- A layer (fullscreen effect) has **no explicit panel bindings at slot 0** (the
  auto-inject trigger for ping-pong today)
- The panel has N targets
- The layer's shader expects to read from the "previous pass" — all N targets as
  inputs

In this case, the auto-injected srcView would need to become N auto-injected
texture views (one per MRT target), and the layer would render into the
corresponding N pong textures.

### Design sketch

- Allocate pong textures for MRT panels (one pong per target format)
- During auto-inject, instead of injecting a single `srcView` at binding 0,
  inject N views at bindings 0..N-1
- The layer's panel bind group layout would need N texture entries for the
  auto-injected slots (not just 1)
- Swap all N textures atomically on ping-pong

### Challenges

- **Shader declaration mismatch**: Today, the auto-injected source texture is
  implicit (binding 0 in the panel bind group). For MRT ping-pong, the shader
  would need to declare N source textures. This could be handled by a new panel
  type annotation like `SourcePanel` that auto-expands to match the panel's
  target count.
- **Partial reads**: A layer might only need target 0 and 1 but not 2. Forcing
  all targets into bindings wastes slots. A more flexible approach: let the
  layer declare which source targets it reads, and only those participate in
  ping-pong.
- **Mixed MRT + single output**: After an MRT ping-pong layer, the next layer
  might output to a single target (e.g., tone mapping). The ping-pong state
  needs to track which output configuration is active.

### Recommendation

Defer MRT ping-pong to a future milestone after the basic MRT and mip features
are stable and in use. The MVP (no ping-pong for MRT) covers the most common use
case (deferred lighting with explicit panel bindings) without added complexity.

---

## Files to Modify

| File                                    | Phase | Changes                                                           |
| --------------------------------------- | ----- | ----------------------------------------------------------------- |
| `src/graphics/painter/panel.scala`      | 0,1,2 | PanelBinding type, mip config, multi-texture storage, formats     |
| `src/graphics/painter/painter.scala`    | 0,1,2 | Bind group resolution, sampler factory, mipmap gen, MRT, pipeline |
| `src/graphics/painter/shape.scala`      | 0     | panelBindings type change, processEntry update                    |
| `src/graphics/painter/layer.scala`      | 0,1   | panelBindings type change, mipSource/mipTarget fields             |
| `src/graphics/painter/instance.scala`   | 0     | panelBindings type change                                         |
| `src/graphics/painter/enums.scala`      | 1     | FilterMode opaque type                                            |
| `src/graphics/shader/dsl/context.scala` | 2     | FragmentCtx type param FO                                         |
| `src/graphics/shader/dsl/program.scala` | 2     | Program/LayerProgram FO type param                                |
| `src/webgpu/facades.scala`              | 0     | Verify createView(descriptor) exists (it does)                    |

---

## Implementation Status

### Phase 0: Unified PanelBinding API — COMPLETE

- `PanelBinding` class added to `panel.scala`
- `PanelBindingValue` union extended with `PanelBinding`
- All `panelBindings` arrays changed from `Arr[Opt[Panel]]` to
  `Arr[Opt[PanelBinding]]` in shape, layer, instance, painter
- `processEntry` (shape) and `processPanelEntry` (panel) accept both `Panel`
  (auto-wrapped) and `PanelBinding`
- `setPanelBindGroup` resolves `PanelBinding` →
  `panel.textureViewAt(pb.index, pb.mipLevel)`
- `applyPanelRuntimeBindings` handles both `Panel` and `PanelBinding` in union
- All existing drafts compile and run; all 62 tests pass

### Phase 1: Mip Levels — COMPLETE

- `mipLevels` field + `mipLevelCount` helper on Panel
- `ensureSize` allocates textures with `mipLevelCount`; separate render views
  (single mip via `baseMipLevel=0, mipLevelCount=1`) and sampling views (full
  mip chain) to avoid WebGPU attachment errors
- `_mipViews` cache for per-mip texture views, cleared on resize
- `textureViewAt(index, mipLevel)` returns sampling view for mipLevel < 0,
  cached single-mip view otherwise
- `FilterMode` opaque type in `enums.scala`
- `painter.sampler(magFilter, minFilter, mipmapFilter)` factory
- `generateMipmaps(panel)` blit chain with cached per-format pipelines and
  linear sampler, called at end of `paint()` when `mipLevelCount > 1`
- Layer `mipSource`/`mipTarget` fields; mip-targeted layers skip ping-pong
- `drafts/mipmaps/` example: 4 colored triangles → 512x512 panel with full mip
  chain, displayed at mip levels 0/2/4/6 via `textureSampleLevel`

### Phase 2: Multiple Render Targets — COMPLETE

- `formats` field + `effectiveFormats`/`targetCount` helpers on Panel
- Multi-texture storage: `_textures`, `_textureViews`, `_samplingViews`,
  `_pongTextures`, `_pongViews`, `_msaaTextures`, `_msaaViews` all as `Arr`
- `renderViewAt(index)` accessor for single-mip render views (used by `paint()`
  color attachments to avoid accidentally binding all-mip sampling views as
  render attachments)
- `ensureSize` loops over `effectiveFormats`, one texture set per format
- `paint()` builds `colorAttachments` array with one entry per `targetCount`
- `renderShapeOnPass` and `renderLayerOnPass` accept `formats: Arr[String]`
- `pipelineKeyStr` uses `formats.join(",")` for cache key uniqueness
- `createPipeline` and `createLayerPipeline` build `targets` array from formats
- Shader DSL: `FragmentCtx[V, U, L, P, FO]` with typed `out` accessor via
  `NamedTuple.Map[FO, ToAssign]`
- `Program[A, V, U, P, FO]` and `LayerProgram[U, P, FO]` thread FO through
- New `shade[A, V, U, P, FO]` and `layerShade[U, P, FO]` overloads; existing
  overloads default to `FragOut` for backward compatibility
- `shadeFromWgslFO` and `layerShadeFromWgslFO` use `Shader.full` with custom FO
- `drafts/deferred/` example: G-buffer with 2 targets (`rgba8unorm` albedo +
  `rgba16float` normals), MRT shade with
  `GBufferOut = (color: Vec4, normal: Vec4)`, lighting layer composites with
  animated directional light using typed locals
- No ping-pong for MRT panels (MVP, matches Rust library behavior)
- All 62 tests pass; all existing + new drafts compile

---

## Verification

### After Phase 0

- [x] All existing drafts compile and run identically (`bun run build`)
- [x] `scala test test/` passes (62 tests)

### After Phase 1

- [x] New `drafts/mipmaps/` example renders and shows mip level differences
- [x] Panel with `mipLevels = 0` generates full chain
- [x] `panel.binding(mipLevel = 3)` correctly binds that mip level
- [x] `painter.sampler(mipmapFilter = FilterMode.Linear)` creates correct
      sampler
- [x] Layer with `mipTarget` renders to specific mip without ping-pong

### After Phase 2

- [x] New `drafts/deferred/` example renders G-buffer with 2 targets
- [x] `panel.binding(index = 1)` correctly binds second MRT target
- [x] Fragment shader with `GBufferOut` writes to all locations
- [x] No ping-pong for MRT panels (MVP)
- [x] Combined: `panel.binding(index = 1, mipLevel = 2)` works
