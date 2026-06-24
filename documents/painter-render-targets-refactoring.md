# Panel render-target refactor: pair-array pong + view simplification

## Context

`Panel` (`trivalibs/src/graphics/painter/panel.scala`) today maintains three
parallel lineages of slot-0-indexed texture state:

1. The main render targets — `_textures` / `_textureViews` / `_samplingViews` —
   sized to `formats.length` (1 for typical panels, N for MRT).
2. A pong lineage — `_pongTextures` / `_pongViews` / `_pongSamplingViews` (the
   latter just added to repair the post-pong sampling bug) — also sized to
   `formats.length` because the allocation lives inside the format loop.
3. A `_outputView` override field used to record the final post-pong slot for
   `show()` (now redundant after the recent `swapPongMain` fix).

The pong lineage is per-format only by accident of code structure. The runtime
exclusively touches index 0:

- `pongView` / the pong layer pass attach a **single** colour attachment
  (painter.scala:952–961).
- `swapPongMain` swaps slot 0 only.
- `generateMipmaps` reads/writes slot 0 only.
- `pongViewAt(i)` exists but is never called for `i > 0`.

So on an MRT panel that also has an auto-pong layer:

- slots 1..N-1 of `_pongTextures` are allocated but never read or written;
- the user has no way to express "which target the layer post-processes";
- slot 0 silently gets ping-ponged while the others pass through.

This document plans a refactor that formalises the actual usage:

- **MRT and auto-pong layers are mutually exclusive at the panel level.** If
  post-processing of MRT outputs is ever needed, the user composes it as a chain
  of single-format panels — each panel does one thing.
- Ping-pong storage collapses into the same `_texture*` arrays, sized to **2**
  when pong is needed. Slot 0 is always the current logical result; slot 1 is
  scratch. Swapping slot 0 ↔ slot 1 after every pong layer pass keeps slot 0
  authoritative at all points in time, so external sampling, `show()`, mip
  generation, and the _next_ layer in a chain all read the same slot 0 view.
- Drop the `_outputView` override entirely — slot 0 is always the result.
- Collapse the three per-slot view lineages (`_textureViews`,
  `_samplingViews`, `_mipViews`) into one eager bundle (`SlotViews`) per
  slot. WebGPU still requires both a single-mip attachment view and a
  full-chain sampling view when `mipCount > 1`, so two *kinds* of views
  remain — but they're co-located in one bundle, one array, no lazy Dict,
  no null branch.

## Goals

- One uniform texture model (`_textures` and its lockstep view arrays) for
  single-target, MRT, and ping-pong cases — no dedicated `_pong*` arrays.
- Chained pong layers compose naturally: each layer's auto-bound first panel
  slot resolves to the _previous_ layer's output without any caller bookkeeping.
- External samplers, `show()`, and `generateMipmaps` always read slot 0; no
  output-routing override needed.
- The MRT-vs-pong invariant is checked at configuration time, including after
  post-construction `set(...)` reconfiguration.
- One unified per-slot view bundle replaces today's three lineages
  (`_textureViews`, `_samplingViews`, `_mipViews`). One eager array per
  slot, no lazy Dict cache, no null branches in `textureViewAt`.

## Non-goals

- Allowing layers to write to non-zero MRT slots. (Not blocked forever — could
  be added later with an explicit per-layer "writes target index N" API. Out of
  scope here.)
- Replacing the implicit "manual slot 0 → no pong, omit → auto-pong" Layer
  dispatch with an explicit `Layer.Mode` or named factory. This dispatch is a
  deliberate design decision (one pipeline reusable in or out of a layer stack
  depending on what the caller binds); it stays as-is. Documentation teaches
  the rule.
- Multi-slot auto-injection. The single-binding-pongs-only rule keeps the
  mental model trivial (always exactly one binding is read-or-not) and is the
  same reason MRT + pong is disallowed. Both are coupled non-goals.

## Design

### Invariant: `MRT && auto-pong` is rejected at configuration time

Check inside `Panel.set(...)` (panel.scala:196 onward), after all field updates
have been applied. `set` is the choke point for both initial construction (via
`Painter.panel(...)` → `Panel(this).set(...)`) and post-hoc reconfiguration, so
a single check covers both.

Predicate (reuses the existing slot-0-auto-bind detection used at paint time,
painter.scala:903–906):

```scala
val mrtActive = formats.length > 1
val hasAutoPongLayer =
  var found = false
  var i = 0
  while i < layers.length && !found do
    val l = layers(i)
    val needsAutoSlot0 =
      l.shade.panelBindGroupLayout.notNull &&
      (l.panelBindings.length == 0 || l.panelBindings(0).isNull)
    if needsAutoSlot0 then found = true
    i += 1
  found
if mrtActive && hasAutoPongLayer then
  throw jsError(
    "Panel: MRT (multiple formats) cannot host auto-pong layers. " +
    "Chain a single-format panel for post-processing instead.",
  )
```

`needsPong` (panel.scala:469) remains the runtime predicate for "this panel must
allocate two slots" and is used by the allocation path below. The
configuration-time check is structurally equivalent but raises a clear
build-time error instead of producing a malformed panel.

### Pair-array texture model

One slot-indexed array each, lockstep:

| Field       | Length                | Slot 0                  | Slot 1 (only if `needsPong`)         |
| ----------- | --------------------- | ----------------------- | ------------------------------------ |
| `_textures` | `formats.length` or 2 | current logical result  | scratch / pong target                |
| `_slots`    | `formats.length` or 2 | view bundle for slot 0  | view bundle for slot 1               |

Each `SlotViews` bundle holds the per-mip single-level views (length =
`mipCount`) plus a full-mip sampling view — see [Unify per-slot views] below.

`_msaaTextures` / `_msaaViews` stay sized to `formats.length` (independent of
pong; since MRT + pong is forbidden, the length is 1 in any pong setup). Layers
never render through MSAA (painter.scala:889 comment "Render layers in order —
no depth, no msaa"), so there is no second MSAA texture for the pong scratch
slot.

The pong scratch in slot 1 carries the same `mipLevelCount` as slot 0 so
`textureViewAt(1, mipLevel)` works for hand-built mip chains that target the
scratch slot (none today, but the symmetry costs nothing).

### Allocation changes (`ensureSize`)

In `panel.scala:476`:

- Compute `hasPong = needsPong` early. (Today's variable name.)
- Reject MRT + pong via the configuration-time check (above), so by the time we
  get here we can assume `!hasPong || fmts.length == 1`.
- Iterate `i = 0..fmts.length` as today, pushing the slot-i main texture and
  building its `SlotViews` bundle (see [Unify per-slot views] below): one
  single-level view per mip level + one full-chain sampling view.
- **After** the format loop, if `hasPong`, allocate a second texture into
  `_textures` and a matching `SlotViews` into `_slots` (i.e. slot 1) — same
  `format`, `usage`, `mipLevelCount` as slot 0.
- Drop the in-loop `if hasPong then` pong push.
- Drop the entire `_pongTextures` / `_pongViews` / `_pongSamplingViews` field
  initialisation and destruction.
- Drop `_textureViews`, `_samplingViews`, and `_mipViews` field
  initialisation and the post-loop mip-cache clear — all subsumed by
  `_slots`.
- Change the destruction loop to destroy `_textures` only.

### `Panel` surface changes

Remove (private[painter]):

- fields `_pongTextures`, `_pongViews`, `_pongSamplingViews`, `_outputView`,
  `_textureViews`, `_samplingViews`, `_mipViews`;
- getters `pongView`, `pongViewAt`, `outputView`;
- setters/mutators `setOutputView`, `swapPongMain`.

Add (private[painter]):

- `_slots: Arr[SlotViews]` — the unified per-slot view bundle (see next
  section).
- `altView: GPUTextureView` — returns `_slots(1).attach`. Valid only on
  pong-configured panels; documented as "the scratch render target for the
  next pong layer pass". Callers in the layer loop already key off
  `needsPingPong`, so misuse is structurally prevented.
- `swapPair(): Unit` — swaps index 0 ↔ index 1 in `_textures` and `_slots`.
  No Dict.clear: the slot bundles already point to the right textures because
  they're indexed alongside `_textures`.

Rewrite (private[painter]):

- `textureView` → `_slots(0).attach` (mip-0 view of slot 0).
- `textureViewAt(i, -1)` → `_slots(i).sampling`.
- `textureViewAt(i, mip)` → `_slots(i).perMip(mip)`. No branch, no Dict
  lookup.

### `paintPanel` layer-loop changes

In `trivalibs/src/graphics/painter/painter.scala` around lines 889–1001:

- Remove the `srcView` / `dstView` locals and the `hasPongLayers` accumulator.
- For each layer:
  - **Mip-target** branch (lines 909–941) unchanged. Reads/writes specific mip
    levels via `textureViewAt(0, mipLevel)` — slot 0 is always the live main.
  - **Pong** branch (lines 942–975) becomes:
    - Begin a render pass against `panel.altView` (slot 1).
    - Call `renderLayerOnPass(..., srcView = panel.textureView, panel = panel)`.
      `panel.textureView` is read fresh on this call, so it reflects any prior
      pong layer's swap. `setPanelBindGroup` auto-injects this view at binding 0
      (renderLayerOnPass:1685, applied when slot 0 isn't manually bound).
    - End/submit the pass.
    - `panel.swapPair()`. Now slot 0 is this layer's output; the next pong
      layer's `panel.textureView` call will return it.
  - **Non-pong** branch (lines 976–991) writes to `panel.textureView` (slot 0)
    as today.
- Drop the trailing `setOutputView(srcView)` / `setOutputView(null)` block.
- `generateMipmaps(panel)` keeps working: it reads `textureViewAt(0, …)` which
  is slot 0 — guaranteed to hold the final result after every swap.

### Chained pong layers (worked example)

Three layers attached to a panel, all auto-pong (`A → B → C`), plus a shape
pass. With slot 0 = main, slot 1 = scratch:

| Step | Action                              | Slot 0         | Slot 1         |
| ---- | ----------------------------------- | -------------- | -------------- |
| 0    | Initial                             | —              | —              |
| 1    | Shape pass                          | shape          | (stale)        |
| 2    | Layer A: read slot 0 → write slot 1 | shape          | A(shape)       |
| 3    | `swapPair()`                        | A(shape)       | shape          |
| 4    | Layer B: read slot 0 → write slot 1 | A(shape)       | B(A(shape))    |
| 5    | `swapPair()`                        | B(A(shape))    | A(shape)       |
| 6    | Layer C: read slot 0 → write slot 1 | B(A(shape))    | C(B(A(shape))) |
| 7    | `swapPair()`                        | C(B(A(shape))) | B(A(shape))    |
| 8    | `generateMipmaps` reads slot 0      | C(B(A(shape))) | —              |
| 9    | Sampler / `show()` read slot 0      | C(B(A(shape))) | —              |

The invariant "slot 0 = current live result" holds across the loop. No parity
bookkeeping for even/odd pong counts. The "next layer reads the previous result"
property the user asked for falls out of the per-step swap — Step 4's
`renderLayerOnPass` reads `panel.textureView` again, which is now A's output.

### `show()`

`Painter.show(panel)` (painter.scala ~1221) reads `panel.outputView` today.
After this refactor the override field disappears; replace the call site with
`panel.textureView` (i.e. `_slots(0).attach`).

### Consolidate the pong dispatch predicate

Today the "does this layer auto-pong slot 0?" predicate exists in two places:

- `paintPanel` dispatcher (painter.scala:903–906): decides which branch (pong
  vs non-pong vs mip-target) the layer takes.
- `renderLayerOnPass` `effectiveSrcView` resolution (painter.scala:1673–1676):
  decides whether the auto-injected `srcView` is actually wired into the
  layer's panel bind group slot 0.

These check the same intent but on different inputs — the dispatcher reads
static `layer.panelBindings(0)`; the inner resolver reads the post-merge
`_workPanelBindings(0)` (after applying panel runtime overrides). That second
check is a *legitimate* runtime gate (a panel-level binding can override slot
0 even for a pong-dispatched layer), so the two predicates aren't redundant —
but the *static* portion is duplicated.

Refactor: expose a single `Layer.usesPongSlot0: Boolean` (computed from
`shade.panelBindGroupLayout.notNull && (panelBindings.length == 0 ||
panelBindings(0).isNull)`). The dispatcher consumes it directly; the inner
`effectiveSrcView` resolver still consults the merged bindings on top. No
behaviour change; one source of truth for the static predicate, no risk of
the two sites drifting.

Important: this **keeps** the implicit "manual slot-0 bind → no pong, omit →
pong" dispatch. The predicate's existence as a single accessor on `Layer` is
the only consolidation; the rule itself is the deliberate design.

### Layer bind-group caching

`renderLayerOnPass` (painter.scala:1640+) calls `setValueBindGroup` and
`setPanelBindGroup` on every draw, even for the very common case of "one
layer, no instances, no panel runtime overrides". The Group-0 (uniforms) bind
group references stable GPU buffer handles (uniform value mutation happens
in place in those buffers, not by rebuilding the group). The Group-1 (panel
textures) bind group references stable view objects — except slot 0 of an
auto-pong layer, which after this refactor changes only when `swapPair`
rotates slot 0 ↔ slot 1, or when `ensureSize` reallocates textures.

This refactor narrows the panel-state mutation surface (no more
`_outputView` indirection, no per-format pong allocations) to two single
events: `swapPair` and `ensureSize`-realloc. That makes a clean invalidation
condition for caching the layer's bind groups.

**Design.**

- `Panel` gains `private[painter] var _bindEpoch: Int = 0`. Bumped on:
  - the realloc branch of `ensureSize` (after texture handles change)
  - `swapPair()` (slot 0 view changes)
- `Layer` gains a per-(panel) cache entry capturing both bind groups, keyed
  by the panel's `_bindEpoch` at the time of capture. Concretely a
  `private[painter] var _cache: LayerBindCache | Null = null` (small inner
  class storing `panelId`, `epoch`, `valueGroup`, `panelGroup`).
- The cache is only populated when the draw is *static*: no instances and
  no panel runtime bindings for this layer's shade. The dynamic paths (`if
  instanceCount > 0` or `hasPanelBinds`) keep the existing per-draw merge
  + bind-group build — those are intentionally per-frame and rarely
  bottlenecked.
- `Bindable.bind(...)` (the `.bind(...)` API on `Layer`) invalidates the
  cache (`_cache = null`) when a layer's bindings change post-construction.
  Mip-target layers cache the same way; the swap doesn't affect their
  source/target (which are explicit mip levels), only `ensureSize` does.

**Hit path** (the new fast path inside `renderLayerOnPass`):

```
if instanceCount == 0 && !hasPanelBinds && layer._cache.notNull &&
   layer._cache.panelId == panelId && layer._cache.epoch == panel._bindEpoch
then
  pass.setBindGroup(0, layer._cache.valueGroup)
  pass.setBindGroup(1, layer._cache.panelGroup)
  pass.draw(3)
else
  // existing build path; on completion, if static, store into _cache
```

Cost: two pointer compares + two `setBindGroup` calls per layer per paint,
down from a full bind-group build (work-buffer copy + GPU bind-group
creation). For dense layer stacks (bloom: 5 mip downsample + 5 upsample +
threshold + composite, all static) this avoids ~12 bind-group rebuilds per
frame.

**Lifetime.** `GPUBindGroup` handles outlive the views they reference until
the views (or the underlying textures) are destroyed. We rebuild textures
in `ensureSize` and bump `_bindEpoch` *before* destruction in the natural
order — but to be safe, the cache invalidation should clear `_cache` to
`null` rather than just incrementing the epoch (so we don't dangle a
reference to a destroyed bind group across frames). Implementation note:
`Panel.ensureSize` cannot reach across to all `Layer` caches; instead, the
layer's hit-check compares `_bindEpoch` and treats a mismatch as
"invalidate now, then miss" — i.e. set `_cache = null` and fall through.
This pulls the invalidation into the same code path as the miss and avoids
hanging onto a destroyed `GPUBindGroup` past its frame.

**Non-coverage / future work.**
- Cached groups are layer-panel pairs. If a layer is reused across multiple
  panels (rare but supported), the cache only memoises the last panel; cache
  thrashes if a layer is repeatedly used across more than one panel per
  frame. Not addressed here — the common case is one layer per panel.
- Instanced layers and runtime-overridden layers stay on the per-draw
  build path. The per-instance bind group build is unavoidable; the
  per-frame runtime-binding merge is the bigger overhead and is its own
  optimisation target (not this refactor).

### Unify per-slot views

Today `Panel` carries three view lineages per colour slot:

| Field            | Per slot         | Mip-level scope | Use                                             |
| ---------------- | ---------------- | --------------- | ----------------------------------------------- |
| `_textureViews`  | 1                | mip 0 only      | render-attachment for shape pass + non-pong layer |
| `_samplingViews` | 1 (or null)      | full chain      | external sampling (`textureSampleLevel`)        |
| `_mipViews`      | lazy Dict        | single, any mip | `generateMipmaps`, hand-built mip-target layers |

`_textureViews(i)` and `_mipViews["i|0"]` would be the same view if both
existed simultaneously. The three are managed separately (different fields,
different lifecycle, different invalidation) only because they were added at
different times.

WebGPU forces two *kinds* of views to coexist when `mipCount > 1`:
attachments are required to be `mipLevelCount = 1`, sampling needs the full
mip chain. We can't merge those two kinds. But we *can* collapse the three
storage lineages into one per-slot bundle.

**The `SlotViews` bundle.**

```scala
private[painter] final class SlotViews(
    val perMip: Arr[GPUTextureView],  // length = mipCount, single-level views
    val sampling: GPUTextureView,     // full-chain view for samplers
):
  inline def attach: GPUTextureView = perMip(0)
```

Per slot, eagerly allocated alongside the texture: one
`tex.createView({ baseMipLevel = m, mipLevelCount = 1 })` per mip level plus
one `tex.createView()` (no args = full chain) for sampling. View descriptors
are small JS objects, not GPU memory — bloom's 5-mip panel costs five extra
descriptor allocations per slot at panel-build time, negligible.

**Storage on `Panel`.**

```scala
private var _slots: Arr[SlotViews] = Arr()
```

Length matches `_textures.length` (lockstep with the pair-array model above).
Drops the `_textureViews`, `_samplingViews`, and `_mipViews` fields.

**Accessors rewrite.**

| Old                                          | New                                |
| -------------------------------------------- | ---------------------------------- |
| `textureView` (private[painter])             | `_slots(0).attach`                 |
| `_textureViews(i)` (render-attach in MRT)    | `_slots(i).attach`                 |
| `textureViewAt(i, -1)` (sampling)            | `_slots(i).sampling`               |
| `textureViewAt(i, mip)` (specific mip)       | `_slots(i).perMip(mip)`            |
| `_mipViews.has` / `.at` / `.set` (lazy Dict) | gone — direct array index instead  |

`textureViewAt` collapses to:

```scala
private[painter] def textureViewAt(
    index: Int = 0,
    mipLevel: Int = -1,
): GPUTextureView =
  val s = _slots(index)
  if mipLevel < 0 then s.sampling else s.perMip(mipLevel)
```

**Invalidation.** Only two events change the view bundle's validity:

- **`ensureSize` realloc**: rebuilds `_textures`; rebuilds `_slots` from
  scratch alongside. Same code path as today, simpler — no Dict.clear loop.
- **`swapPair`**: rotates `_textures(0) ↔ _textures(1)` and
  `_slots(0) ↔ _slots(1)` together. Bundle views still match their textures
  because they were created and stored together; the swap rotates the index
  associations, no individual view becomes stale. No Dict.clear needed.

This is a notable simplification over today's `swapPongMain`, which also
needs `js.special.delete`-ing every cached mip view because the Dict was
keyed by index and the index meaning shifted under it. With `_slots`-as-Arr,
swapping array entries automatically keeps each `SlotViews` bundle attached
to its texture.

**`Painter`-side changes.** The two callers of `textureViewAt(i, mipLevel >= 0)`
are unchanged at the call site:

- `generateMipmaps` (painter.scala:1402–1404) still reads
  `panel.textureViewAt(0, i - 1)` / writes `panel.textureViewAt(0, i)`.
- Mip-target layer branch (painter.scala:916–918) still reads/writes via
  the same accessor.

Both now skip the lazy Dict and hit a single array index.

**Cost vs benefit.**

- *Costs*: at panel construction, eager allocation of `mipCount` extra view
  descriptors per slot (vs lazy). Practically: 1–6 descriptors per slot —
  noise on any realistic panel.
- *Benefits*: one storage shape replacing three; one invalidation path;
  one accessor shape; no Dict in the hot path; no null branch in
  `textureViewAt`. Aligns with the pair-array consolidation philosophically:
  one logical thing → one storage thing.

## Files

- `trivalibs/src/graphics/painter/panel.scala`
  - Field changes (drop pong + outputView lineages).
  - New `SlotViews` inner class and `_slots: Arr[SlotViews]` field replacing
    `_textureViews` + `_samplingViews` + `_mipViews`.
  - Allocation loop rewrite — eager per-mip + sampling view bundle per slot.
  - New `altView` and `swapPair` (swaps `_textures` and `_slots`
    index 0 ↔ 1; no Dict.clear).
  - New `_bindEpoch: Int` counter; bumped in `swapPair` and after texture
    realloc in `ensureSize`.
  - Configuration-time MRT + pong rejection in `set(...)`.
  - `textureView` / `textureViewAt(i, -1)` / `textureViewAt(i, mip)` all
    collapse to `_slots(i)` array indexing.
  - Destruction loop simplification in `ensureSize`.

- `trivalibs/src/graphics/painter/layer.scala`
  - New `usesPongSlot0: Boolean` accessor — single source of truth for the
    static portion of the pong-dispatch predicate.
  - New `_cache` field (small inner record holding `panelId`, `epoch`,
    `valueGroup`, `panelGroup`); cleared on `Bindable.bind(...)` mutation.

- `trivalibs/src/graphics/painter/painter.scala`
  - `paintPanel` layer-loop rewrite (drop srcView/dstView locals, hasPongLayers,
    setOutputView; per-layer `swapPair`). Dispatcher consumes
    `layer.usesPongSlot0` instead of its inline predicate.
  - `renderLayerOnPass` fast path: cache hit for static draws (no instances,
    no panel runtime bindings); cache store on miss; cache drop on epoch
    mismatch.
  - `show()` reads `panel.textureView` instead of `panel.outputView`.

- `trivalibs/docs/guide/gotchas.md` (small update)
  - The existing "A layer's first texture slot is auto-injected" note remains
    accurate. Add a one-sentence note that MRT panels cannot host auto-pong
    layers and must chain through a single-target panel for post-processing.

## Reused utilities

- `needsPong` (panel.scala:469) — runtime "must allocate two slots".
- `jsError` (`trivalibs/src/utils/js.scala`) — config-time invariant violation.
- `renderLayerOnPass` + `setPanelBindGroup` (painter.scala:1640+) — already
  resolves the auto-injected slot 0 from the `srcView` argument on every call,
  so per-layer freshness comes for free.
- `hasPanelRuntimeBindings` (painter.scala) — already used to gate the
  work-buffer merge; reused as the "is this draw eligible for the static
  bind-group cache?" predicate.
- `Bindable.bind(...)` mutation hook — `Layer` already routes binding
  changes through this; cache invalidation lives there.

## Tasks (implementation order)

1. **Add the invariant check** inside `Panel.set` and write a small test that
   triggers it. Verifies the check fires at construction _and_ after
   reconfiguration.
2. **Introduce `SlotViews` and `_slots`** replacing `_textureViews` +
   `_samplingViews` + `_mipViews`. Rewrite `ensureSize` view allocation to
   build the bundle eagerly; rewrite `textureView` / `textureViewAt`
   accessors to array-index `_slots`. Should be a green-test no-op refactor:
   semantics unchanged, storage shape unified. (Note: this also subsumes
   the earlier "drop the Opt on `_samplingViews`" step — the bundle has a
   non-nullable `sampling` field.)
3. **Collapse pong into pair arrays**: introduce `altView` + `swapPair`; pull
   pong allocation out of the format loop into a single post-loop branch
   (allocates a slot-1 texture and matching `SlotViews`); delete
   `_pongTextures` / `_pongViews` / `_pongSamplingViews` / `pongView` /
   `pongViewAt` / `swapPongMain`.
4. **Rewrite `paintPanel` layer loop** to use `panel.textureView` /
   `panel.altView` and call `swapPair` per pong layer. Delete the `srcView` /
   `dstView` locals, `hasPongLayers`, the `setOutputView` block.
5. **Drop `_outputView` / `setOutputView` / `outputView`**; update `show()` call
   site.
6. **Consolidate pong predicate**: add `Layer.usesPongSlot0`; rewrite the
   `paintPanel` dispatcher to consume it. No behaviour change.
7. **Add `Panel._bindEpoch`** counter; bump it inside `swapPair` and in
   `ensureSize` after the texture-realloc branch.
8. **Layer bind-group cache**: introduce the `LayerBindCache` record on
   `Layer`; clear in `Bindable.bind(...)`. Wire the fast path in
   `renderLayerOnPass`: skip the build for static draws when
   `(panelId, _bindEpoch)` matches the cache; otherwise build, then store
   when the draw is static. Add a benchmark or before/after frame-time
   spot check on the bloom example to validate the win.
9. **Doc updates**: `gotchas.md` MRT-vs-pong note; scaladoc on `Layer`
   noting the cache + when it is invalidated.

## Verification

1. `cd trivalibs && bun run check` — library type-checks.
2. `cd trivalibs && bun run test` — any panel-touching tests stay green; new
   MRT + auto-pong invariant test passes.
3. From the consumer repo:
   - `bun run sketch rooms/canvases` then reload the running dev server. Each
     wall's painting shadow is visible on top of the baked noise (the pong
     result reaches the external sampler in `wall.createWallShape(tex)`). This
     is the regression `swapPongMain` was added to fix.
   - `bun run sketch rooms/base` — no auto-pong; visual must be unchanged.
4. `cd trivalibs && bun run examples:build` then reload each:
   - `panel_layer/PanelLayer.scala` — single non-pong layer; unchanged.
   - `blur/Blur.scala` — exercises an auto-pong chain; the worked example above
     must hold visually (no flicker between odd/even frames).
   - bloom-style examples — exercise mip-target layers (no pong); confirm they
     still work after `_outputView` removal.
5. Manual sanity check: construct an MRT panel and attach an auto-pong layer in
   a throwaway script; confirm `jsError` fires at panel construction
   (`Panel.set`), not at first paint.
6. **Bind-group cache spot check**: run a bloom-heavy example (`examples/blur`
   or a sketch using `sketchlib.utils.bloom`) before and after task 8; record
   frame time in the browser's perf panel. Expect a measurable drop on the
   layer-stack draw cost (cache hits skip the bind-group rebuild). The visual
   output must be identical.
7. **Cache-invalidation correctness**: trigger `swapPair` (any auto-pong
   sketch) and `ensureSize` realloc (resize the window) while a layer is in
   the cached state; confirm no GPU validation errors (would indicate a
   stale bind-group reference to a destroyed view).

## Risks / open questions

- **Layer chains where one layer explicitly binds slot 0**
  (`slot0Manual = true`) interleaved with auto-pong layers: today this still
  pongs through the local `srcView` / `dstView`. After the refactor a
  manually-bound layer writes to `panel.textureView` (slot 0) and does _not_
  swap — so the next auto-pong layer reads its output via the fresh
  `panel.textureView` read. Need a worked example to confirm semantics are
  preserved; add a test.
- **Resize during a paint** — `ensureSize` rebuilds all textures; the swap state
  is irrelevant after rebuild because both slots are fresh. No special handling.
- **MSAA + pong**: MSAA shape pass resolves into `_slots(0).attach` (slot 0).
  First pong layer reads slot 0, writes slot 1, swap. Subsequent paints
  re-resolve into the new slot 0 — that's the previous-frame's pong result. If
  `clearColor` is set the resolve clobbers it (intended). If `clearColor` is
  null (load semantics), the user observes "load from previous frame result" —
  same semantic as today's no-pong load case. Document this explicitly in the
  panel scaladoc.

## Future work (not in this refactor)

- Per-layer "writes target index N" for MRT post-processing, if the use case
  arises. Would be additive on top of this refactor.
- A `mipBlitSampler`-style helper to copy pong slot 1 → slot 0 explicitly for
  the rare case where a user wants a non-pong layer to _replace_ slot 0 with the
  result of a manually-bound read. Probably never needed.
