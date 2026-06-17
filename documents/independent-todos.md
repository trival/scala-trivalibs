# TODOs that don't require full feature documents.

## Form

### 🔄 `Form.set()` — destroy-always is wrong for dynamic geometry

**Current Scala** (`src/graphics/painter/form.scala`):

```scala
if vertexBuffer.notNull then vertexBuffer.get.destroy()
// always creates a new GPUBuffer
val buf = painter.device.createBuffer(...)
painter.queue.writeBuffer(buf, ...)
```

**Rust approach** (`form.rs` — `update_form` fn):

- Tracks `vertex_buffer_max_size` (allocated GPU capacity) separately from
  `vertex_buffer_current_size` (bytes actually used).
- On update: if new data fits within `max_size`, calls `queue.write_buffer` into
  the existing buffer — **no destroy, no realloc**.
- Only destroys + recreates when new data **exceeds** `max_size`.

**Required Scala changes in `Form.set()`:**

- Add `var bufferCapacity: Int = 0` to `Form` alongside `vertexCount`.
- Replace the unconditional destroy+create with a reuse-or-grow branch:
  ```scala
  val newSize = verts.arrayBuffer.byteLength
  if vertexBuffer.isNull || newSize > bufferCapacity then
    if vertexBuffer.notNull then vertexBuffer.get.destroy()
    vertexBuffer = painter.device.createBuffer(...)
    bufferCapacity = newSize
  painter.queue.writeBuffer(vertexBuffer.get, 0, verts.arrayBuffer)
  vertexCount = verts.length
  ```
- Ensure the render pass draws only `vertexCount` vertices, not capacity, so
  using a smaller-than-capacity buffer produces correct output.

**Why this matters:** Any per-frame geometry update (particles, procedural
animation, future `dynamic_shapes` example) will thrash the GPU allocator with
the current implementation. With the Rust strategy the buffer is allocated once
to the high-water mark and reused every frame with a plain `writeBuffer` call.

**Doc follow-up:** Remove the sentence in §3 (Form) — _"The old GPU buffer is
destroyed and a new one allocated (no in-place resize yet)"_ — once this is
fixed.

---

## Panel / Textures

### 🔄 `with_static_texture_data(bytes)` on Panel

**Gap:** Rust's `Panel::with_static_texture_data(bytes)` is the canonical way to
load an image / baked texture into a panel's render target. The Scala Panel has
no direct equivalent — callers must build a `GPUTexture` with usage
`COPY_DST | TEXTURE_BINDING` via the raw device and bind it by hand.

**Required changes:**

- Add a `staticTextureData` param (or a dedicated
  `setStaticTextureData(bytes, format, width, height)` method) on `Panel` /
  `painter.panel(...)`.
- Internally: create the `GPUTexture` with `COPY_DST | TEXTURE_BINDING`, call
  `queue.writeTexture`, and wire the texture view as the panel's output so
  downstream `Layer`s sampling the panel see the baked pixels.
- Default format / dimensions from `bytes` or require explicit args — match
  Rust's signature.

**Doc follow-up:** Once added, remove the gap bullet from
[scala-port-comparison.md §12](rust-painter/scala-port-comparison.md) and add
the param row to the Panel table in §3.

**Priority:** Medium — unblocks any example that needs a baked image texture
(image-based sketches, reference LUTs, etc.).

---

## Random helpers

### 🔄 Port missing `Random` helpers from `trivalibs_core`

**Current state:**
[trivalibs/src/utils/random.scala](../trivalibs/src/utils/random.scala) only
exposes `rand()` and `randInRange(min, max)`. Rust
`trivalibs_core::rendering::scene::Random` additionally provides `rand_sign`,
`rand_vec2` / `rand_vec2_range`, `rand_vec3` / `rand_vec3_range`, `rand_vec4` /
`rand_vec4_range`.

**Required additions** to `trivalibs/src/utils/random.scala`:

- `randSign(): Double` — `if rand() < 0.5 then -1.0 else 1.0`.
- `randVec2()` / `randVec3()` / `randVec4()` — components in `[0, 1)`.
- `randVec2InRange(min: Vec2, max: Vec2)` and the Vec3 / Vec4 analogues —
  componentwise `randInRange`.

**Doc follow-up:** Once added, remove the "Random helpers" bullet in
[scala-port-comparison.md §12](rust-painter/scala-port-comparison.md) and update
the `Random` row in §10 to `Ported`.

**Priority:** Low — one-liners, do when a sketch first needs them.

---

## GPU resources

### 🔄 Explicit GPU resource freeing across the painter

Independent, tangential workstream — a generally useful library capability for
any sketch that allocates transient GPU resources (e.g. the texture baker's
per-bake `model` binding). trivalibs is still work-in-progress, so filling this
gap is fair game. **Not scheduled** — prebaking will not free its binding for
now; capture the design here until it's worth doing.

Add a public `destroy(): Unit` to the painter classes that own GPU resources:

- **`Form`** (`form.scala`) — destroy `vertexBuffer` + `indexBuffer`. The
  teardown already exists internally (it runs on `set`, `form.scala:59/72`);
  expose it.
- **`BufferBinding`** (`buffers/…`) — destroy the underlying `GPUBuffer` (the
  WebGPU facade already has `GPUBuffer.destroy()`, `webgpu.scala:139`). This is
  the one the baker would actually use (free the transient `model` binding after
  `p.paint`).
- **`Panel`** (`panel.scala`) — destroy color / pong / msaa / depth textures;
  reuse the existing texture-teardown logic (`panel.scala:133-134, 450-458`).

Each class carries a `private var destroyed = false` flag:

- `destroy()` is **idempotent** (no-op if already destroyed) and sets the flag.
- Use-after-destroy **throws** `throw jsError("<Resource>: use after destroy")`
  (plain JS `Error`, per the trivalibs no-Scala-exceptions rule) from the public
  use/mutation entry points — `Form` render/buffer access, `BufferBinding`
  `set`/`update`/bind, `Panel` paint/bind/show/`binding`. The guard is a single
  boolean branch.

**Needs deeper thought — who owns which bindings:** the simple "destroy what I
hold" rule above is only correct for resources the object allocated itself. The
painter has several cases where ownership is conditional:

- **`Shape` / instances** allocate and manage their **own** bindings when handed
  a **raw value** instead of a pre-existing `BufferBinding`. Those self-allocated
  bindings are owned by the shape/instance and are candidates to destroy on its
  teardown; a binding **passed in** by the caller is not — destroying it would
  pull the rug out from under the owner. So `destroy()` must track provenance
  (allocated-here vs. supplied) and only free the former.
- **`Panel` bindings** (not only its textures) fall under the same rule — a panel
  similarly owns bindings it created from raw values but not ones it was given.

So the full design needs an ownership/provenance flag per binding before any
class can safely cascade `destroy()` into its bindings. Until that's worked out,
keep `destroy()` scoped to unambiguously-owned resources.

Feasibility / cost notes:

- Guarding only the **public** entry points (not every internal access) keeps it
  cheap and avoids per-inner-loop checks. If a guard would land on a genuine hot
  path, limit it to the outermost call.
- Bundle-size discipline applies (library code): boolean field +
  `throw jsError(...)`, no Scala exception types, no stdlib.
- A small test can assert `destroy()` is idempotent and that a guarded call
  after destroy throws.

**Priority:** Low — transient resources are negligible for the handful-of-bakes
scale; revisit when a sketch churns GPU resources at runtime.

---

## ✅ Completed

---

### ✅ Documentation — `painter.shape` arg order in docs

Doc note about the arg-order flip has been removed now that the API and all
examples are updated to `painter.shape(form, shade)`.

---

### ✅ API Design — `painter.shape(shade, form)` arg order should match Rust

Swapped to `painter.shape(form, shade)` in `painter.scala` and `shape.scala`.
All example call sites updated.

---

### ✅ Examples — `painter.form().set()` / `.panel().set()` / `.layer().set()`

Collapsed chained `.set()` calls into the factory constructor across all example
files. Pattern: `painter.form().set(vertices = v)` →
`painter.form(vertices = v)`.

---

### ✅ API Design — singular param shortcuts for plural `Arr` params

Added `shape`, `layer`, `format` singular params to `Panel.set()` and
`painter.panel()`. Plural wins when both are supplied. All single-item `Arr(x)`
call sites in examples simplified to use the singular form.

---

### ✅ Math — `IntExt` dedicated Int extension trait

Added `trait IntExt[P]` with `min`, `max`, `clamp`, and step predicates to
`trivalibs/src/utils/numbers.scala`. `given IntExt[Int]` provides the CPU
instance. Standalone `Int` extensions for `abs` and `sign` co-located.

---

### ✅ Math — vector overloads for `fit0111` / `fit1101`

Added `fit0111` / `fit1101` to `Vec2/3/4ImmutableOpsG` (shared trait), CPU
`Vec2/3/4ImmutableOps` (componentwise scalar delegation), and all three GPU
`Vec_Expr` given instances (direct WGSL `v * 2.0 - 1.0` / `v * 0.5 + 0.5`).
Port-comparison doc updated.
