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

- `pongView` / the pong layer pass attach a **single** color attachment
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
- Collapse the three per-slot view lineages (`_textureViews`, `_samplingViews`,
  `_mipViews`) into one eager bundle (`SlotViews`) per slot. WebGPU still
  requires both a single-mip attachment view and a full-chain sampling view when
  `mipCount > 1`, so two _kinds_ of views remain — but they're co-located in one
  bundle, one array, no lazy Dict, no null branch.

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
  (`_textureViews`, `_samplingViews`, `_mipViews`). One eager array per slot, no
  lazy Dict cache. The overloaded `textureViewAt` accessor (with its
  `-1`-means-sampling sentinel) is removed — call sites read the bundle's
  `attach` / `sampling` / `perMip(n)` members directly.

## Non-goals

- Allowing layers to write to non-zero MRT slots. (Not blocked forever — could
  be added later with an explicit per-layer "writes target index N" API. Out of
  scope here.)
- Replacing the implicit "manual slot 0 → no pong, omit → auto-pong" Layer
  dispatch with an explicit `Layer.Mode` or named factory. This dispatch is a
  deliberate design decision (one pipeline reusable in or out of a layer stack
  depending on what the caller binds); it stays as-is. Documentation teaches the
  rule.
- Multi-slot auto-injection. The single-binding-pongs-only rule keeps the mental
  model trivial (always exactly one binding is read-or-not) and is the same
  reason MRT + pong is disallowed. Both are coupled non-goals.

## Design

### Invariant: `MRT && auto-pong` is rejected at configuration time

Check inside `Panel.set(...)` (panel.scala:196 onward), after all field updates
have been applied. `set` is the choke point for both initial construction (via
`Painter.panel(...)` → `Panel(this).set(...)`) and post-hoc reconfiguration, so
a single check covers both.

The check reuses `Panel.needsPong` — after the predicate consolidation (see
[Consolidate the pong dispatch predicate]), `needsPong` iterates layers using
`Layer.autoPongsSlot0`, so it captures exactly the layers that would trigger an
auto-pong pass at paint time:

```scala
if formats.length > 1 && needsPong then
  throw jsError(
    "Panel: MRT (multiple formats) cannot host auto-pong layers. " +
    "Chain a single-format panel for post-processing instead.",
  )
```

Single predicate driving all three sites: `Panel.needsPong` (allocation-time in
`ensureSize`), the config-time invariant here, and the paint-time dispatcher in
`paintPanel`. No inline duplication, no drift risk.

### Pair-array texture model

One slot-indexed array each, lockstep:

| Field       | Length                | Slot 0                 | Slot 1 (only if `needsPong`) |
| ----------- | --------------------- | ---------------------- | ---------------------------- |
| `_textures` | `formats.length` or 2 | current logical result | scratch / pong target        |
| `slotViews` | `formats.length` or 2 | view bundle for slot 0 | view bundle for slot 1       |

Each `SlotViews` bundle holds the per-mip single-level views (length =
`mipCount`) plus a full-mip sampling view — see [Unify per-slot views] below.

`_msaaTextures` / `_msaaViews` stay sized to `formats.length` (independent of
pong; since MRT + pong is forbidden, the length is 1 in any pong setup). Layers
never render through MSAA (painter.scala:889 comment "Render layers in order —
no depth, no msaa"), so there is no second MSAA texture for the pong scratch
slot.

The pong scratch in slot 1 carries the same `mipLevelCount` as slot 0 so
`slotViews(1).perMip(mipLevel)` works for hand-built mip chains that target the
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
  `_textures` and a matching `SlotViews` into `slotViews` (i.e. slot 1) — same
  `format`, `usage`, `mipLevelCount` as slot 0.
- Drop the in-loop `if hasPong then` pong push.
- Drop the entire `_pongTextures` / `_pongViews` / `_pongSamplingViews` field
  initialisation and destruction.
- Drop `_textureViews`, `_samplingViews`, and `_mipViews` field initialisation
  and the post-loop mip-cache clear — all subsumed by `slotViews`.
- Change the destruction loop to destroy `_textures` only.

### `Panel` surface changes

Remove (private[painter]):

- fields `_pongTextures`, `_pongViews`, `_pongSamplingViews`, `_outputView`,
  `_textureViews`, `_samplingViews`, `_mipViews`;
- getters `pongView`, `pongViewAt`, `outputView`;
- setters/mutators `setOutputView`, `swapPongMain`.

Add (private[painter]):

- `slotViews: Arr[SlotViews]` — the unified per-slot view bundle, exposed as a
  field (no wrapper accessor); call sites index `panel.slotViews(i)` and read
  `.attach` / `.sampling` / `.perMip(n)` themselves (see next section).
- `pongTargetView: GPUTextureView` — returns `slotViews(1).attach`. Valid only
  on pong-configured panels; documented as "the scratch render target for the
  next pong layer pass". Callers in the layer loop already key off
  `needsPingPong`, so misuse is structurally prevented.
- `swapPair(): Unit` — swaps index 0 ↔ index 1 in `_textures` and `slotViews`.
  No Dict.clear: the slot bundles already point to the right textures because
  they're indexed alongside `_textures`.

Rewrite (private[painter]):

- `textureView` → `slotViews(0).attach` (role name for the live-output view).

Remove (private[painter]):

- `textureViewAt` — its `-1`-means-sampling sentinel is gone; call sites read
  `slotViews(i).sampling` / `.perMip(n)` directly (see [Unify per-slot views]).
- `renderViewAt` — the mechanical MRT render-attach index-wrapper; call sites
  read `slotViews(i).attach`.

### `paintPanel` layer-loop changes

In `trivalibs/src/graphics/painter/painter.scala` around lines 889–1001:

- Remove the `srcView` / `dstView` locals and the `hasPongLayers` accumulator.
- For each layer:
  - **Mip-target** branch (lines 909–941) unchanged. Reads/writes specific mip
    levels via `slotViews(0).perMip(mipLevel)` — slot 0 is always the live main.
  - **Pong** branch (lines 942–975) becomes:
    - Begin a render pass against `panel.pongTargetView` (slot 1).
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
- `generateMipmaps(panel)` keeps working: it reads `slotViews(0).perMip(…)`
  which is slot 0 — guaranteed to hold the final result after every swap.

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

### Blend layers compose with ping-pong (targeted behaviour)

A **non-pong layer** (one with no panel-texture input, or one that manually
binds slot 0 — anything the dispatcher routes to the non-pong branch) that
carries a `blendState` is meant to **render on top of the current slot-0 result
and persist there**, so a subsequent pong layer picks up the composite. Example
use: draw an additive glow / alpha-blended overlay between two ping-pong passes.

This is the **targeted** behaviour. The current (pre-refactor) code happens to
do _something_ here through the `srcView` / `dstView` locals, but that path was
never designed or tested for this case — treat whatever it does today as
accidental, not a contract to preserve.

The pair-array design delivers it cleanly. Non-pong layers write to slot 0 with
`loadOp = "load"` (blend composites over the live result); pong layers read slot
0, write slot 1, then `swapPair`. Worked example — `pong A → blend B → pong C`
(B alpha-blends over the running result):

| Step | Action                                      | Slot 0        | Slot 1     |
| ---- | ------------------------------------------- | ------------- | ---------- |
| 1    | Shape pass                                  | shape         | (stale)    |
| 2    | A (pong): read 0 → write 1, `swapPair`      | A(shape)      | shape      |
| 3    | B (non-pong blend): load slot 0, blend over | B∘A(shape)    | shape      |
| 4    | C (pong): read 0 → write 1, `swapPair`      | C(B∘A(shape)) | B∘A(shape) |

The load-bearing detail: the shared non-pong pass writing B into slot 0 must be
**ended and submitted before** C's pass samples slot 0 (a texture can't be
sampled while it's an open render-attachment). The layer loop already flushes
the open `curPass` when it hits a pong (or mip-target) layer — that flush is
what makes the composite land in slot 0 before C reads it, so it must be
retained in the Stage 4 rewrite (called out there).

Two non-pong sub-cases both compose the same way: (a) a pure blend draw with no
panel input, and (b) a `slot0Manual = true` layer that samples an external panel
and blends its result in. Both write slot 0 with `loadOp = "load"` and do
**not** `swapPair`.

### `show()`

`Painter.show(panel)` (painter.scala ~1221) reads `panel.outputView` today.
After this refactor the override field disappears; replace the call site with
`panel.textureView` (i.e. `slotViews(0).attach`).

### Consolidate the pong dispatch predicate

Today the "does this layer auto-pong slot 0?" predicate exists in **three**
places, each with subtly different logic:

- `Panel.needsPong` (panel.scala:469) — allocation-time predicate. Today too
  eager: `shade.panelBindGroupLayout != null` alone, without checking whether
  slot 0 is manually bound. Panels with a manually-bound layer allocate a pong
  texture that is never touched — pre-existing minor bug, wasted GPU memory
  only, not a correctness issue.
- `paintPanel` dispatcher (painter.scala:903–906) — runtime dispatch. Uses the
  stricter form:
  `panelBindGroupLayout.notNull && (panelBindings.length == 0 || panelBindings(0).isNull)`.
- `renderLayerOnPass` `effectiveSrcView` resolution (painter.scala:1673–1676) —
  decides whether the auto-injected `srcView` is wired into the layer's slot 0.
  Reads the post-merge `_workPanelBindings(0)` (after panel runtime overrides).
  This second check is a _legitimate_ runtime gate (a panel runtime binding can
  override slot 0 even for a pong-dispatched layer), so it stays independent.

Refactor: expose a single `Layer.autoPongsSlot0: Boolean` (computed from
`shade.panelBindGroupLayout.notNull && (panelBindings.length == 0 || panelBindings(0).isNull)`).
Rewrite the two _static_ sites on top of it:

- `Panel.needsPong` iterates layers calling `layer.autoPongsSlot0`. This is the
  stricter form — panels with manually-bound slot-0 layers no longer allocate an
  unused pong texture (free bug fix).
- `paintPanel` dispatcher calls `layer.autoPongsSlot0` directly.
- The config-time invariant (Stage 3) reuses `Panel.needsPong`, so it too picks
  up the shared predicate.

Three static sites → one accessor on `Layer`, called from one predicate on
`Panel`. The runtime-merge check in `renderLayerOnPass` stays as-is.

Important: this **keeps** the implicit "manual slot-0 bind → no pong, omit →
pong" dispatch. The predicate's existence as a single accessor on `Layer` is the
only consolidation; the rule itself is the deliberate design.

### Layer bind-group caching

`renderLayerOnPass` (painter.scala:1640+) calls `setValueBindGroup` and
`setPanelBindGroup` on every draw, even for the very common case of "one layer,
no instances, no panel runtime overrides". The Group-0 (uniforms) bind group
references stable GPU buffer handles (uniform value mutation happens in place in
those buffers, not by rebuilding the group). The Group-1 (panel textures) bind
group references stable view objects — except slot 0 of an auto-pong layer,
which after this refactor changes only when `swapPair` rotates slot 0 ↔ slot 1,
or when `ensureSize` reallocates textures.

This refactor narrows the panel-state mutation surface (no more `_outputView`
indirection, no per-format pong allocations) to two single events: `swapPair`
and `ensureSize`-realloc. That makes a clean invalidation condition for caching
the layer's bind groups.

**Design.**

- `Panel` gains `private[painter] var _bindEpoch: Int = 0`. Bumped on:
  - the realloc branch of `ensureSize` (after texture handles change)
  - `swapPair()` (slot 0 view changes)
- `Layer` gains a per-(panel) cache entry capturing both bind groups, keyed by
  the panel's `_bindEpoch` at the time of capture. Concretely a
  `private[painter] var _cache: LayerBindCache | Null = null` (small inner class
  storing `panelId`, `epoch`, `valueGroup`, `panelGroup`).
- The cache is only populated when the draw is _static_: no instances and no
  panel runtime bindings for this layer's shade. The dynamic paths
  (`if instanceCount > 0` or `hasPanelBinds`) keep the existing per-draw merge
  - bind-group build — those are intentionally per-frame and rarely
    bottlenecked.
- `Bindable.bind(...)` (the `.bind(...)` API on `Layer`) invalidates the cache
  (`_cache = null`) when a layer's bindings change post-construction. Mip-target
  layers cache the same way; the swap doesn't affect their source/target (which
  are explicit mip levels), only `ensureSize` does.

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

Cost: two pointer compares + two `setBindGroup` calls per layer per paint, down
from a full bind-group build (work-buffer copy + GPU bind-group creation). For
dense layer stacks (bloom: 5 mip downsample + 5 upsample + threshold +
composite, all static) this avoids ~12 bind-group rebuilds per frame.

**Lifetime.** `GPUBindGroup` handles outlive the views they reference until the
views (or the underlying textures) are destroyed. We rebuild textures in
`ensureSize` and bump `_bindEpoch` _before_ destruction in the natural order —
but to be safe, the cache invalidation should clear `_cache` to `null` rather
than just incrementing the epoch (so we don't dangle a reference to a destroyed
bind group across frames). Implementation note: `Panel.ensureSize` cannot reach
across to all `Layer` caches; instead, the layer's hit-check compares
`_bindEpoch` and treats a mismatch as "invalidate now, then miss" — i.e. set
`_cache = null` and fall through. This pulls the invalidation into the same code
path as the miss and avoids hanging onto a destroyed `GPUBindGroup` past its
frame.

**Correctness guarantee (why multi-panel reuse can never mis-render).** The
hit-check requires **both** `panelId` and `epoch` to match. A layer reused
across panels degrades to a _guaranteed miss + rebuild_ — never a wrong hit:

- Layer cached against panel A, then drawn in panel B:
  `_cache.panelId (A) == panelId (B)` is false → miss → rebuild correctly for B
  → overwrite the cache slot with B. Back in A next frame → miss again → rebuild
  for A. This is the "thrash": every draw costs exactly what it costs today (a
  full per-draw rebuild), with **correct output**. Panel B never receives panel
  A's bind group (which would reference panel A's textures — the only way this
  scheme could produce undefined rendering).

`panelId` in the key is load-bearing for correctness, not just for cache-hit
efficiency. **It must never be dropped from the key** (e.g. "optimised" down to
epoch-only on the assumption epochs are globally unique — they are per-panel
counters, so two panels routinely share an epoch value). Assert this in the code
comment on `LayerBindCache`.

**Non-coverage / future work.**

- Multi-panel layer reuse is correct (above) but gets no cache benefit — it
  thrashes to a rebuild every draw. Not optimised here (the common case is one
  layer per panel); a future fix would key the cache by a small
  per-(layer,panel) map instead of a single slot.
- Instanced layers and runtime-overridden layers stay on the per-draw build
  path. The per-instance bind group build is unavoidable; the per-frame
  runtime-binding merge is the bigger overhead and is its own optimisation
  target (not this refactor).

### Unify per-slot views

Today `Panel` carries three view lineages per color slot:

| Field            | Per slot    | Mip-level scope | Use                                               |
| ---------------- | ----------- | --------------- | ------------------------------------------------- |
| `_textureViews`  | 1           | mip 0 only      | render-attachment for shape pass + non-pong layer |
| `_samplingViews` | 1 (or null) | full chain      | external sampling (`textureSampleLevel`)          |
| `_mipViews`      | lazy Dict   | single, any mip | `generateMipmaps`, hand-built mip-target layers   |

`_textureViews(i)` and `_mipViews["i|0"]` would be the same view if both existed
simultaneously. The three are managed separately (different fields, different
lifecycle, different invalidation) only because they were added at different
times.

WebGPU forces two _kinds_ of views to coexist when `mipCount > 1`: attachments
are required to be `mipLevelCount = 1`, sampling needs the full mip chain. We
can't merge those two kinds. But we _can_ collapse the three storage lineages
into one per-slot bundle.

**The `SlotViews` bundle.**

```scala
private[painter] final class SlotViews(
    val perMip: Arr[GPUTextureView],  // length = mipCount, single-level views
    val sampling: GPUTextureView,     // full-chain view for samplers
):
  inline def attach: GPUTextureView = perMip(0)
```

Per slot, eagerly allocated alongside the texture: one
`tex.createView({ baseMipLevel = m, mipLevelCount = 1 })` per mip level plus one
`tex.createView()` (no args = full chain) for sampling. View descriptors are
small JS objects, not GPU memory — bloom's 5-mip panel costs five extra
descriptor allocations per slot at panel-build time, negligible.

**Storage on `Panel`.** The bundle array is exposed `private[painter]` directly
— no wrapper accessor. Everything that reads views lives inside the painter
package; the views are never handed to user land where they could be mutated, so
a read-only guard buys nothing. Following the codebase convention
(`private[painter]` fields drop the `_` prefix, like `shapes` / `layers` /
`specWidth`; `_`-prefixed fields stay truly private):

```scala
private[painter] var slotViews: Arr[SlotViews] = Arr()
```

Length matches `_textures.length` (lockstep with the pair-array model above).
Drops the `_textureViews`, `_samplingViews`, and `_mipViews` fields.

**`textureViewAt` is removed entirely.** Today it is one overloaded accessor
carrying a `-1`-means-sampling magic sentinel. With the bundle exposed as a
field, that sentinel — and the accessor itself — have no reason to exist: call
sites index `panel.slotViews(i)` and read the precise member they want.

**Accessors rewrite.**

| Old                                          | New                                       |
| -------------------------------------------- | ----------------------------------------- |
| `textureView` (private[painter])             | `slotViews(0).attach` (see note)          |
| `renderViewAt(i)` (render-attach in MRT)     | `slotViews(i).attach`                     |
| `textureViewAt(i, -1)` (sampling)            | `slotViews(i).sampling`                   |
| `textureViewAt(i, mip)` (specific mip)       | `slotViews(i).perMip(mip)`                |
| `_mipViews.has` / `.at` / `.set` (lazy Dict) | gone — direct array index instead         |
| `textureViewAt` (the accessor itself)        | gone — sentinel branch inlined where real |

`textureView` / `pongTargetView` survive as thin `private[painter]` getters
(`slotViews(0).attach` / `slotViews(1).attach`) — they name a _role_ (the
live-output view / the pong-scratch view) rather than wrapping an index, so they
read well in the layer loop. Prefer them at call sites over spelling out
`slotViews(0).attach` / `slotViews(1).attach`; reach for `slotViews(i)` directly
only when you need a non-attach member (`.sampling`, `.perMip(n)`) or a non-zero
MRT slot. The mechanical `renderViewAt` / `textureViewAt` index-wrappers go
away.

**Invalidation.** Only two events change the view bundle's validity:

- **`ensureSize` realloc**: rebuilds `_textures`; rebuilds `slotViews` from
  scratch alongside. Same code path as today, simpler — no Dict.clear loop.
- **`swapPair`**: rotates `_textures(0) ↔ _textures(1)` and
  `slotViews(0) ↔ slotViews(1)` together. Bundle views still match their
  textures because they were created and stored together; the swap rotates the
  index associations, no individual view becomes stale. No Dict.clear needed.

This is a notable simplification over today's `swapPongMain`, which also needs
`js.special.delete`-ing every cached mip view because the Dict was keyed by
index and the index meaning shifted under it. With `slotViews`-as-Arr, swapping
array entries automatically keeps each `SlotViews` bundle attached to its
texture.

**`Painter`-side changes.** All `textureViewAt` call sites index
`panel.slotViews(i)` directly:

- `generateMipmaps` (painter.scala:1402–1404): `slotViews(0).perMip(i - 1)` /
  `slotViews(0).perMip(i)` — always a concrete mip level, no sentinel.
- Mip-target layer branch (painter.scala:916–918): `slotViews(0).perMip(level)`
  — the `mipSource == -1` "use the running srcView" case is already handled by a
  branch _outside_ the call, so only `>= 0` levels reach here.
- Shape-pass color attachments (the MRT loop, painter.scala:823+):
  `slotViews(t).attach` in place of `renderViewAt(t)`.
- PanelBinding resolution (external sampling): the one site where the binding's
  `mipLevel` is a runtime value that may be `-1`. It keeps a local one-line
  branch:
  `if b.mipLevel < 0 then panel.slotViews(i).sampling else panel.slotViews(i).perMip(b.mipLevel)`.
  Honest and visible at the call site — no hidden sentinel buried in a shared
  accessor.

All of these skip the lazy Dict and hit a single array index.

**Cost vs benefit.**

- _Costs_: at panel construction, eager allocation of `mipCount` extra view
  descriptors per slot (vs lazy). Practically: 1–6 descriptors per slot — noise
  on any realistic panel.
- _Benefits_: one storage shape replacing three; one invalidation path; the
  overloaded `textureViewAt` (and its `-1` sentinel) and the mechanical
  `renderViewAt` index-wrapper both gone — call sites index the exposed
  `slotViews` field directly; no Dict in the hot path. Aligns with the
  pair-array consolidation philosophically: one logical thing → one storage
  thing.

## Files

- `trivalibs/src/graphics/painter/panel.scala`
  - Field changes (drop pong + outputView lineages).
  - New `SlotViews` inner class and `slotViews: Arr[SlotViews]` field replacing
    `_textureViews` + `_samplingViews` + `_mipViews`.
  - Allocation loop rewrite — eager per-mip + sampling view bundle per slot.
  - New `pongTargetView` and `swapPair` (swaps `_textures` and `slotViews` index
    0 ↔ 1; no Dict.clear).
  - New `_bindEpoch: Int` counter; bumped in `swapPair` and after texture
    realloc in `ensureSize`.
  - `needsPong` rewritten on top of `Layer.autoPongsSlot0` — stricter predicate
    that no longer over-allocates pong for manually-bound slot-0 layers.
  - Configuration-time MRT + pong rejection in `set(...)` — one-line reuse of
    `needsPong`.
  - `textureView` → `slotViews(0).attach`; `textureViewAt` removed entirely
    (call sites read `slotViews(i).sampling` / `.perMip(n)` directly, sentinel
    branch inlined only at the PanelBinding resolver).
  - Destruction loop simplification in `ensureSize`.

- `trivalibs/src/graphics/painter/layer.scala`
  - New `autoPongsSlot0: Boolean` accessor — single source of truth for the
    static portion of the pong-dispatch predicate.
  - New `_cache` field (small inner record holding `panelId`, `epoch`,
    `valueGroup`, `panelGroup`); cleared on `Bindable.bind(...)` mutation.

- `trivalibs/src/graphics/painter/painter.scala`
  - `paintPanel` layer-loop rewrite (drop srcView/dstView locals, hasPongLayers,
    setOutputView; per-layer `swapPair`). Dispatcher consumes
    `layer.autoPongsSlot0` instead of its inline predicate.
  - `textureViewAt` call sites (`generateMipmaps`, mip-target layer branch,
    PanelBinding resolver) rewritten against `slotViews(i)` — the resolver keeps
    a local `mipLevel < 0` branch; the other two use `.perMip(n)` directly.
  - `renderLayerOnPass` fast path: cache hit for static draws (no instances, no
    panel runtime bindings); cache store on miss; cache drop on epoch mismatch.
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
- `Bindable.bind(...)` mutation hook — `Layer` already routes binding changes
  through this; cache invalidation lives there.

## Staged implementation plan

Each stage is a self-contained, reviewable, shippable increment. After every
stage the library type-checks, all tests pass, and every example + downstream
sketch renders identically to before — until Stage 5, where a perf delta is
expected (visuals still unchanged). Stages stack but can be merged separately.

**Standard verification gate** run after every stage:

- `cd trivalibs && bun run check` — library type-checks.
- `cd trivalibs && bun run test` — all tests green.
- `cd trivalibs && bun run examples:build` — all examples build.
- `cd trivalibs && bun run examples:dev`, reload each example — visuals
  unchanged (`panel_layer`, `blur`, `bloom`-using, `deferred`, etc.).
- From the consumer repo: `bun run sketch rooms/canvases`,
  `bun run sketch rooms/base`, `bun run sketch rooms/columns`,
  `bun run sketch rooms/grid-ceiling` — each rebuilds and renders identically in
  the running dev server. The canvases sketch's wall shadows are the key
  auto-pong regression check.

Per-stage additions on top of this gate are called out under each **Verify**
bullet.

The refactor's success bar is **currently-working rendering still works** — this
gate, run on the _existing_ examples and consumer sketches. Adding new rendering
examples is explicitly **out of scope**: paths with no example today
(blend-in-pong compose, mip-target layers, MSAA + pong, multi-panel layer reuse)
are covered here by consumer sketches or one-off manual checks, and their
permanent examples are a deferred milestone — see [Deferred milestone: example
coverage gaps].

### Stage 1 — Unified per-slot view bundles

Pure storage reshape. Both main and pong sides switch to the same `SlotViews`
bundle in this stage so the model is consistent after step 1, no half-converted
state.

**Edits.**

- Add `SlotViews` inner class on `Panel` (panel.scala):
  `perMip: Arr[GPUTextureView]`, `sampling: GPUTextureView`, inline
  `attach: GPUTextureView = perMip(0)`.
- Replace `_textureViews` + `_samplingViews` + `_mipViews` with
  `slotViews: Arr[SlotViews]`.
- Replace `_pongViews` + `_pongSamplingViews` with
  `_pongSlotViews: Arr[SlotViews]` (slot 0 only is ever populated, mirroring
  today's pong allocation pattern).
- In `ensureSize`: build each bundle eagerly when the texture is allocated — one
  single-mip view per mip level + one full-chain sampling view. Drop the
  post-loop `_mipViews` clear and the `_samplingViews` Opt fallback.
- Expose `slotViews` as a `private[painter]` field. Remove `textureViewAt` and
  `renderViewAt`; rewrite the role getter + call sites to index the field:
  - `textureView` → `slotViews(0).attach`
  - `renderViewAt(i)` call sites → `slotViews(i).attach`
  - `textureViewAt(i, -1)` call sites → `slotViews(i).sampling`
  - `textureViewAt(i, mip)` call sites → `slotViews(i).perMip(mip)`
  - PanelBinding resolver keeps a local
    `if mipLevel < 0 then …sampling else …perMip(mipLevel)` branch (the only
    site where `mipLevel` is a runtime value that may be `-1`).
- Update `swapPongMain` to swap `slotViews(0) ↔ _pongSlotViews(0)` alongside
  `_textures(0) ↔ _pongTextures(0)`. No `js.special.delete` mip-cache loop any
  more — bundles travel with their textures.
- Also rewrite the pong/output getters that survive until Stage 4 so they read
  the new bundles: `pongView` → `_pongSlotViews(0).attach`, `pongViewAt(i)` →
  `_pongSlotViews(i).attach`, `outputView`'s fallback → `slotViews(0).attach`.
- **Guard the eager pong read in `paintPanel` (isolation-critical).** Today
  `paintPanel` unconditionally evaluates `var dstView = panel.pongView`
  (painter.scala:894) _before_ the layer loop, for **every** panel including
  non-pong ones. Under the old `_pongViews(0)` this returned a harmless JS
  `undefined` (empty array index) that the non-pong branches never touched.
  After this stage `pongView` dereferences the bundle
  (`_pongSlotViews(0) .attach` → `perMip(0)`), so on a non-pong panel
  (`_pongSlotViews` empty) it throws a `TypeError` — crashing `panel_layer`,
  `panel_tex`, `deferred`'s gBuffer, `mipmaps`, i.e. every non-pong example. Fix
  in this same stage: either make `pongView` nullable
  (`if _pongSlotViews.length > 0 then _pongSlotViews(0).attach else null`) or
  move the `panel.pongView` read inside the `needsPingPong` branch of the layer
  loop. This is a small, isolated `paintPanel` tweak; the full loop rewrite
  still lands in Stage 4.

**Out of scope for Stage 1.**

- Pong textures stay in their own fields (`_pongTextures`, `_pongSlotViews`);
  merging into slot 1 is Stage 3.
- MRT + auto-pong still silently allowed (Stage 2).
- `_outputView` still in use (Stage 3).

**Verify.**

- Standard gate.
- Add a panel unit test: `slotViews(0).perMip(mip)` returns the same
  `GPUTextureView` reference across calls — proves the eager allocation is
  stable (the lazy Dict used to return the first-built view too; semantics
  preserved).
- Canvases wall shadows render — the auto-pong regression check via the
  rewritten `swapPongMain` storage path.
- `sketchlib.utils.bloom` (consumer) still renders — interim coverage for the
  `slotViews(0).perMip(n)` / mip-target path (no example yet; see [Deferred
  milestone: example coverage gaps]).

### Stage 2 — Consolidate the pong dispatch predicate

Single source of truth for the static "does this layer auto-pong slot 0?"
predicate. Ships before the invariant and the pair-array pong so both of those
reuse it. Fixes a pre-existing over-allocation bug as a free side effect.

**Edits.**

- Add `Layer.autoPongsSlot0: Boolean` (layer.scala):
  `shade.panelBindGroupLayout.notNull && (panelBindings.length == 0 || panelBindings(0).isNull)`.
- Rewrite `Panel.needsPong` (panel.scala:469) on top of `autoPongsSlot0` —
  iterate layers calling `layer.autoPongsSlot0`. This tightens the predicate:
  today's `needsPong` checks only `shade.panelBindGroupLayout != null` and
  over-allocates pong textures for layers with a manually-bound slot 0. After
  this stage, those panels correctly skip the pong allocation.
- Rewrite the dispatcher in `paintPanel` (painter.scala:903–906) to call
  `layer.autoPongsSlot0` instead of the inline predicate.
- Leave `renderLayerOnPass`'s `effectiveSrcView` resolver alone — it still
  consults the post-merge `_workPanelBindings(0)` (a legitimate runtime gate:
  panel-level runtime bindings can override slot 0 even on a pong-dispatched
  layer).

**Prerequisite.** This stage tightens `needsPong`, so panels that previously
over-allocated a pong (any panel with a manually-bound slot-0 layer, e.g.
`deferred`'s canvasPanel) now have an empty `_pongSlotViews`. That makes the
Stage 1 eager-`pongView` guard **load-bearing**, not optional: without it the
still-old `paintPanel` (`var dstView = panel.pongView`) crashes on those panels.
Confirm the Stage 1 guard is in place before merging this stage.

**Verify.**

- Standard gate. No visual change. No perf change beyond the small memory
  reduction from `needsPong` no longer over-allocating on manually-bound layers.
- `deferred` example (MRT gBuffer + manually-bound-slot-0 light layer on a
  separate single-format panel) renders identically — it is the concrete
  regression check for the stricter predicate + the eager-read guard.
- Optional: add a panel unit test constructing a panel with a layer that binds
  slot 0 manually and asserting no pong texture is allocated (needs a
  private[painter] accessor for testing). Note: this test drives `ensureSize`,
  which calls `device.createTexture`, so it needs a real/stubbed `GPUDevice` —
  not available in the current node test env. If no headless GPU is wired up,
  this check stays a browser/example verification, not a munit test. (Same
  caveat applies to the Stage 1 `slotViews` stability test; the Stage 3
  throw-test is device-free and _can_ be a munit test.)

### Stage 3 — MRT + auto-pong invariant

Isolated config-time rejection reusing the newly-consolidated `needsPong`
predicate. Tiny — the whole check is one `if`.

**Edits.**

- Inside `Panel.set(...)`, after field updates:
  ```scala
  if this.formats.length > 1 && needsPong then
    throw jsError(
      "Panel: MRT (multiple formats) cannot host auto-pong layers. " +
      "Chain a single-format panel for post-processing instead.",
    )
  ```
  Note `this.formats` — inside `set` the bare name `formats` is the `Maybe`
  parameter, not the field.
- No inline predicate scan — `needsPong` is the single source (already the
  stricter form after Stage 2).

**Verify.** ✅ Done.

- Standard gate (no existing example or sketch uses MRT + auto-pong, so the gate
  must stay fully green — if one fails, audit it as a real bug surfaced by the
  invariant). Confirmed green — `deferred`'s MRT gBuffer has no layers, its
  lighting layer lives on a single-format panel.
- Panel unit test added — `test/graphics/painter/MrtPongInvariant.test.scala`.
  Turned out **device-free** after all (the invariant reads only `.notNull` on
  the shade's panel layout + the layer's `panelBindings`, never a GPU handle;
  stub shades with a null painter suffice). Covers: MRT + auto-pong throws;
  MRT + non-pong layer allowed; MRT + manually-bound-slot-0 layer allowed;
  single-format + auto-pong allowed; post-construction `set(formats = twoFmts)`
  re-trips the invariant.

### Stage 4 — Pair-array pong + per-layer swap + drop `_outputView`

The behavior-shifting consolidation. Replaces pong-side storage with slot 1 of
the main arrays; switches the layer loop from `srcView`/`dstView` locals and
end-of-loop `setOutputView` to per-pong-layer `swapPair`; removes `_outputView`
entirely.

These pieces ship together because they're interlocked: the layer loop can't
drop the `srcView`/`dstView` locals until `swapPair` exists; `_outputView` can't
go until the layer loop stops writing it; `show()` can't read
`panel.textureView` until `_outputView` is gone.

Allocation reuses `needsPong` (now stricter, thanks to Stage 2) to decide
whether to allocate slot 1.

**Edits.**

- Remove fields `_pongTextures`, `_pongSlotViews`.
- In `ensureSize`: after the format loop, when `needsPong`, push a slot-1
  texture into `_textures` and a matching `SlotViews` into `slotViews`.
- Add `pongTargetView: GPUTextureView` = `slotViews(1).attach`.
- Add `swapPair(): Unit` — swap index 0 ↔ index 1 in `_textures` and
  `slotViews`. No mip-cache clear (the bundles already point at the right
  textures since they were created together).
- Remove `pongView`, `pongViewAt`, `swapPongMain`.
- Rewrite the `paintPanel` layer loop (painter.scala:889–1001):
  - drop `srcView`/`dstView` locals and `hasPongLayers`;
  - **keep the lazy shared-pass management** (`curPass` / `curEncoder`). This is
    what makes non-pong blend layers compose into the pong chain (see [Blend
    layers compose with ping-pong]) — it is not incidental and must survive the
    rewrite.
  - non-pong branch: lazily open `curPass` on `panel.textureView` (slot 0) with
    `loadOp = "load"` and draw — layers with a `blendState` composite on top of
    the live slot 0, and the result persists there. Consecutive non-pong layers
    share the pass, as today.
  - pong branch: **first end + submit any open `curPass`** (so a preceding
    non-pong blend layer's write to slot 0 is flushed before we sample it), then
    begin a pass against `panel.pongTargetView`, call
    `renderLayerOnPass(..., srcView = panel.textureView, panel = panel)`,
    end/submit, then `panel.swapPair()`. The freshly-read `panel.textureView`
    reflects any prior non-pong composite into slot 0.
  - mip-target branch: same — end + submit any open `curPass` first (unchanged
    from today), then render into the target mip.
- Drop `_outputView`, `outputView`, `setOutputView` and the trailing
  `setOutputView(srcView)` / `setOutputView(null)` block in the paint loop.
- Update `Painter.show(panel)` (painter.scala ~1221) to read `panel.textureView`
  instead of `panel.outputView`.

Note: the interim `resultInPong` parity guard added during Stage 1 verification
(to fix the `62e57d9` hotfix's even-count `swapPongMain` bug) is **removed here**
— per-layer `swapPair` keeps slot 0 authoritative after every pong pass, so
even/odd parity is handled structurally with no end-of-loop reconciliation.

**Verify.** ✅ Done — build gate green (check, tests, all examples, all 7
sketches).

- Standard gate. ✅
- Canvases sketch: wall shadows visible (regression re-validation, now via the
  new pair-array swap path). — build green; visual pending user eyeball.
- `trivalibs/examples/blur` (chained auto-pong H+V): the even-count case that
  regressed in Stage 1 is now correct by construction (traced: slot 0 ends on
  `V(H(shape))`). No parity flicker possible — nothing to reconcile.
- `sketchlib.utils.bloom` and `sketchlib.utils.mirror` (mip-target layers, no
  pong) keep building after `_outputView` removal; mip `else`-source now reads
  `panel.textureView` (slot 0) in place of the dropped `srcView` local.
- MSAA + pong panel (the canvases scene panel feeds the mirror reflection
  + bloom util) keeps working without GPU validation errors in the console.
- The blend-compose target ([Blend layers compose with ping-pong]) has **no
  interim coverage** — no existing example or consumer sketch interleaves a
  blend layer in a pong stack. It ships correct-by-construction here; its
  permanent example (`layer_pong_mixed`) is a deferred milestone (see [Deferred
  milestone: example coverage gaps]).

### Stage 5 — Layer bind-group cache

The performance optimization. Adds the `_bindEpoch` invalidation counter and the
per-`Layer` `LayerBindCache` fast path.

**Prerequisites (two missing building blocks — neither exists today).**

- **A stable `Panel.panelId`.** The cache key needs a per-panel identity, but
  `Panel` has no id field (only `Shade` carries an `id`, shade.scala:17). Add
  `private[painter] val panelId: Int`, assigned from a `Painter`-level counter
  at construction. Without it the `panelId`-in-key correctness guarantee (the
  whole "reuse degrades to a rebuild, never a wrong hit" argument) cannot be
  implemented.
- **Bind-group builders that return the group.** `setValueBindGroup` /
  `setPanelBindGroup` (painter.scala:1520, 1537) currently _build and set_ in
  one call and return `Unit`; the built `GPUBindGroup` is never surfaced. The
  cache must capture the two groups, so split each into a build step that
  returns the `GPUBindGroup | Null` (null when the early-return conditions hold
  — `bindings.length == 0 || layout.isNull` for value, `entries.length == 0` for
  panel) and a trivial `pass.setBindGroup(n, g)`. The hit path must skip
  `setBindGroup` for a null cached group, mirroring today's early return.

**Edits.**

- `Panel._bindEpoch: Int = 0`. Bump in `swapPair` and in `ensureSize` after the
  texture-realloc branch.
- `Layer._cache: LayerBindCache | Null = null` (small inner class storing
  `panelId`, `epoch`, `valueGroup`, `panelGroup`).
- Clear `_cache` in `Bindable.bind(...)` mutations.
- Wire the hit path inside `renderLayerOnPass` (painter.scala:1640+):
  - On entry, if
    `instanceCount == 0 && !hasPanelBinds && _cache.notNull && panelId matches && epoch matches`,
    set the cached bind groups and draw.
  - On miss with the same static-draw predicate, build as today, then capture
    into `_cache` before drawing.
  - On miss with epoch mismatch, set `_cache = null` and fall through to rebuild
    (drops references to any potentially-stale `GPUBindGroup`).
  - Instanced and runtime-overridden draws stay on the existing per-draw build
    path.

**Verify.**

- Standard gate — all visuals unchanged.
- Cache-invalidation correctness: trigger `swapPair` (any auto-pong sketch) and
  `ensureSize` realloc (resize the browser window) while layers are in the
  cached state; confirm no GPU validation errors in the console.
- Multi-panel reuse correctness: bind one static layer instance into two
  different panels painted in the same frame; confirm each panel renders its own
  result (not the other's). This exercises the `panelId`-mismatch miss path —
  the guarantee that reuse degrades to a rebuild rather than a wrong hit. Do
  this as a one-off manual/throwaway check for this stage; the permanent
  `layer_reuse` example is a deferred milestone (see [Deferred milestone:
  example coverage gaps]).
- Frame-time spot check on a bloom-heavy sketch (e.g. `rooms/canvases`, which
  feeds `sketchlib.utils.bloom`): record frame time in the browser perf panel
  before and after Stage 5. Expect a measurable drop on the layer-stack draw
  cost. The visual output must be identical.

### Stage 6 — Documentation

Doc-only stage. No code changes.

**Edits.**

- `trivalibs/docs/guide/gotchas.md`: extend the existing "A layer's first
  texture slot is auto-injected" note with a sentence saying MRT panels cannot
  host auto-pong layers and must chain through a single-target panel for
  post-processing.
- `Layer` scaladoc: document the static bind-group cache and the two
  invalidation triggers (`Bindable.bind` mutation; `Panel._bindEpoch` mismatch
  from `swapPair` / `ensureSize` realloc).
- `Panel` scaladoc: MSAA + auto-pong load-semantics note (see [Risks / open
  questions]) — a subsequent paint with `clearColor = null` loads the previous
  frame's _post-layer_ slot 0, same semantic as a no-pong load.

**Verify.**

- `cd trivalibs && bun run docs` — generated API site builds clean.
- Visual spot-check of the gotchas page in the docs site.

## Deferred milestone: example coverage gaps

**Not part of this refactor.** The refactor's bar is _currently-working
rendering still works_ — verified through the existing examples and downstream
consumer sketches in the standard gate. Adding new examples is a **separate
milestone after the refactor lands**, to be designed then with concrete visuals
/ effects in mind (an actual glow, an actual blur cascade, etc.) rather than
synthetic render-correctness probes.

This section only **documents the gaps** so they aren't lost.
`trivalibs/examples` should eventually exercise every rendering feature so a
future refactor can't silently break it; today several paths this refactor
touches are covered only by downstream consumer sketches
(`sketchlib.utils.bloom`, the `rooms/canvases` scene) or by nothing at all. The
"Verified during the refactor by" column is the interim safety net; the example
is the eventual permanent coverage.

| Path                                                                              | Verified during the refactor by                                                                                         | Deferred example (design later)                                                                                                                                                                                                                                                                         |
| --------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Single + chained auto-pong layers                                                 | `blur` (chained), `rooms/canvases` (single)                                                                             | already covered by examples — none needed                                                                                                                                                                                                                                                               |
| MRT panel + manually-bound-slot-0 layer chained to a single-format panel          | `deferred`                                                                                                              | already covered by examples — none needed                                                                                                                                                                                                                                                               |
| **Mip-target layers** (`layer.mipTarget >= 0`, hand-built mip chain via `perMip`) | `sketchlib.utils.bloom` (consumer)                                                                                      | `mip_layers`: one panel, `mips = true`, `mipTarget`/`mipSource` layers that downsample-then-upsample; the mip chain is hand-built (auto-`generateMipmaps` skipped). Exercises `slotViews(0).perMip(n)` + the mip-target branch. Design around a real down/up cascade effect.                            |
| **Blend (non-pong) layer interleaved with auto-pong layers on the same panel**    | nothing — new targeted behaviour (see [Blend layers compose with ping-pong]); only exercised once a real effect uses it | `layer_pong_mixed`: stack `pong A → blend B → pong C`, B carrying an alpha/additive `blendState` compositing over the running result; B persists _and_ is transformed by C (C sampled `B∘A`). Cover both sub-cases: pure blend draw, and `slot0Manual` blend. Design around a real overlay/glow effect. |
| **MSAA + auto-pong**                                                              | `rooms/canvases` (consumer)                                                                                             | `layer_pong_msaa`: `multisample = true` panel + shape + one auto-pong layer; MSAA resolve → slot-0 → pong → swap, plus the `clearColor = null` load-from-previous-frame semantic ([Risks] MSAA + pong).                                                                                                 |
| **Multi-panel reuse of one `Layer` instance**                                     | Stage 5 manual/throwaway check (see its Verify)                                                                         | `layer_reuse`: one static layer instance bound into two panels painted in the same frame; each renders its own result. Proves the Stage 5 `panelId`-mismatch miss path (reuse degrades to a rebuild, never a wrong hit).                                                                                |

When this milestone is picked up, each new example follows the standard example
rules (compiles standalone, ships `index.html` + `main.js`, never deleted once
added) and joins the standard verification gate.

## Risks / open questions

- **Non-pong layers interleaved in a pong stack — resolved, see [Blend layers
  compose with ping-pong].** (This is targeted behaviour, not a risk to preserve
  — the current behaviour is untested and accidental.)
- **Resize during a paint** — `ensureSize` rebuilds all textures; the swap state
  is irrelevant after rebuild because both slots are fresh. No special handling.
- **MSAA + pong**: MSAA shape pass resolves into `textureView` (slot 0). First
  pong layer reads slot 0, writes slot 1, swap. Subsequent paints re-resolve
  into the new slot 0 — that's the previous-frame's pong result. If `clearColor`
  is set the resolve clobbers it (intended). If `clearColor` is null (load
  semantics), the user observes "load from previous frame result" — same
  semantic as today's no-pong load case. Document this explicitly in the panel
  scaladoc.

## Future work (not in this refactor)

- **Example-coverage milestone.** Add the targeted rendering examples catalogued
  in [Deferred milestone: example coverage gaps], designed then around concrete
  visuals/effects. Separate follow-up; the refactor itself relies on existing
  examples + consumer sketches for its safety net.
- **Naming pass — drop "slot".** Once the refactor is landed and confirmed,
  rename `SlotViews` / `slotViews` / `_pongSlotViews` to drop the "slot" prefix.
  Two reasons: the slot dimension is already implicit in the index access
  (`views(i)`), and — now that the bundle is an exposed `private[painter]` field
  read directly in the rendering logic (`panel.slotViews(i).attach`,
  `.perMip(n)`, `.sampling`) — the name is evaluated at its usage sites, where
  it should describe its _function_ (the panel's texture views), not the storage
  layout. The type's own API (`attach`, `sampling`, `perMip`) already conveys
  what it is and how it's used. Deferred to a post-landing pass so the refactor
  diffs stay easy to review against the old `_textureViews` / `_samplingViews` /
  `_mipViews` names. Likely target: `TextureViews` / `views` / `_pongViews`.
- Per-layer "writes target index N" for MRT post-processing, if the use case
  arises. Would be additive on top of this refactor.
- A `mipBlitSampler`-style helper to copy pong slot 1 → slot 0 explicitly for
  the rare case where a user wants a non-pong layer to _replace_ slot 0 with the
  result of a manually-bound read. Probably never needed.
