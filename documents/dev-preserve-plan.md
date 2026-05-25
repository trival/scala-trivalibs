# Plan: `devPreserve` — preserve dev state across Vite reloads

## Status: ✅ Implemented & working

Shipped as designed. Camera pose now survives live-coding reloads in
`rooms/base`; full test suite + `bun run check` green; sketch builds.

**Delivered:**

- `src/dev/dev.scala` — `object devPreserve` (camera + generic `apply`
  overloads), `DevVar[T]`, `DevHandle`, `DevCodec[T]` + givens (Double, Float,
  Int, Boolean, String, Vec2/3/4, Quat), private storage/registry/dev-guard.
- `test/dev/DevCodec.test.scala` — 10 passing round-trip / malformed-input
  tests.
- `../sketches/rooms/base/Base.scala` — `import trivalibs.dev.*` +
  `devPreserve(cam)`.

**Deviations / fixes made during implementation:**

- **Dev guard via typed facade, not `selectDynamic`.** A
  `@js.native trait ImportMeta` (`hot`, `url`) is cast from `js.import.meta`, so
  member access compiles to literal `import.meta.hot` (dot), which Vite/Rollup
  can statically replace + tree-shake. `selectDynamic` would have emitted
  `import.meta["hot"]` and defeated that. Verified `import.meta.hot` /
  `import.meta.url` (and zero bracket accesses) in the built sketch JS.
- **Storage key strips the URL query/hash.** Vite appends a cache-busting
  `?t=<timestamp>` to `import.meta.url` that changes on every reload — that made
  the save key and restore key differ, so nothing was ever restored. `metaUrl`
  now truncates at the first `?`/`#`. This was _the_ bug that initially made
  preservation appear broken.
- **Codecs encode as `Arr[Double]`** (JSON arrays), not `Dict` — simpler and
  matches the camera snapshot `[x, y, z, rotH, rotV]`.
- **`pagehide` handler logging note:** logs emitted inside `pagehide` are
  routinely dropped by DevTools (context teardown) even with "Preserve log" on.
  The handler does fire — successful restore is the proof. (A temporary
  diagnostic `log` was added then removed once confirmed.)

**Not done (intentionally out of scope, see notes at bottom):** named-tuple /
case-class `DevCodec` derivation; `localStorage` backend option.

## Context

For live-coding sketches, Vite does a **full page reload** after each scala-cli
recompile (we deliberately avoid HMR module patching, because the Painter lib
can't yet hot-swap GPU resources safely — a full reload guarantees clean
teardown/recreate). The downside: every reload resets all runtime state. The
most painful loss is the **camera transform** — you re-navigate to the same
viewpoint after every edit.

We want a tiny, dev-only API that snapshots a piece of state to browser storage
and re-applies it on the next load, overriding the sketch's initial value. It
must be trivial to opt out (`.reset`) to get a clean start from the sketch's
initial state, and it must vanish (be inert) in production builds.

Target ergonomics:

```scala
devPreserve(cam)          // restore cam from storage if present; save on reload
devPreserve(cam).reset    // preservation OFF: wipe stored cam, always start from initial
val speed = devPreserve("speed", 3.0)   // generic simple values (returns a cell)
```

### Confirmed decisions

- **Backend:** `sessionStorage` (survives reloads; auto-clears on tab close).
- **Camera fields:** only `pos`, `rotH` (yaw), `rotV` (pitch). `fov/near/far`
  are sketch config; `aspect` is recomputed by `onResize` — preserving them
  fights config/resize.
- **`.reset` semantics:** a **stateless toggle**, not a one-shot. While `.reset`
  is present in the source, _every_ reload: (1) removes the stored entry
  (idempotent), (2) starts from the sketch's initial value (no restore), (3)
  does **not** register the save hook (nothing persisted). I.e. `.reset` present
  ≡ "preservation OFF + storage kept clean"; remove `.reset` to resume
  preserving.
  - No memorized "already reset" flag, so no remove→reload→re-add dance to reset
    again — leaving `.reset` in keeps every run fresh.
  - "Skip saving" rather than "save then wipe on next init" because the two are
    observably identical (always lands on initial); skipping avoids pointless
    write→delete churn.
  - `.reset` is **not** a no-op — it actively frees the storage entry.

## Key findings from exploration (reuse these)

- **Camera** `src/graphics/scene/camera.scala`:
  - State: `var pos: Vec3`, `var rotH: Double`, `var rotV: Double` (+
    fov/aspect/near/far).
  - Restore via the existing setter: `cam(pos = ..., rotH = ..., rotV = ...)`
    (lines ~23–41; normalizes/clamps rotH/rotV automatically).
  - `BasicFirstPersonCameraController` (`camera_controller.scala`) is
    **stateless** — only the camera needs preserving.
- **Module/dev environment:**
  - `project.scala`: `jsModuleKind es`; `index.html` loads
    `<script type="module" src="./main.js">`.
  - `import.meta` is reachable from Scala.js via `scala.scalajs.js.import.meta`.
  - Dev guard: `import.meta.hot` is truthy in Vite dev, `undefined` in prod
    build → use it to gate all logic (prod = inert no-op).
  - `import.meta.url` → per-sketch key namespacing (avoids cross-sketch
    collisions).
- **JS interop** `src/utils/js.scala`: `Opt`, `Maybe`, `Dict` (`.at/.set/.has`),
  `Obj.literal`, `log`. Use `scala.scalajs.js.JSON.stringify/parse` and
  `org.scalajs.dom.window.sessionStorage` (`getItem/setItem/removeItem`).
- **No existing storage/serialization** — clean slate. Follow typeclass
  conventions from `Position` (geometry/package.scala) and `UniformValue`
  (buffers/binding.scala): trait + anonymous givens in companion, new given
  syntax, `inline` on hot paths.
- **Bundle-size rules** (CLAUDE.md): no `Option`/`enum`/`scala.collection`; use
  `Opt`/`Maybe`/`Arr`/`Dict`, `while` loops, opaque types. This is dev-only
  tooling but lives in `src/`, so keep it lean and gated so prod stays clean.

## Design

New top-level module **`trivalibs.dev`** (file `src/dev/dev.scala`). It sits
above scene/math/utils and may depend on all of them (no layering inversion).
`project.scala` already compiles all of `src`, so no config change.

Sketches add: `import trivalibs.dev.*`.

### 1. Internal primitives (private to the module)

- `inline def devMode: Boolean` — truthiness of
  `js.import.meta.selectDynamic("hot")`.
- `storageKey(label): String` = `s"trivalibs:dev:${import.meta.url}:$label"`.
- sessionStorage read/write/remove wrappers returning `Opt[String]`.
- A module-level `Arr[() => Unit]` flush registry + a one-time `pagehide`
  listener (`dom.window.addEventListener("pagehide", …)`) that runs all
  flushers. Registering returns an unregister handle. Only attach the listener
  once (guard flag). `pagehide` fires on Vite's full reload → latest live state
  is captured exactly once, no per-frame writes.

### 2. `DevCodec[T]` — JSON codec typeclass for simple values

```scala
trait DevCodec[T]:
  def encode(t: T): js.Any
  def decode(json: js.Any): Opt[T]
```

Givens (companion): `Double`, `Float`, `Int`, `Boolean`, `String`, `Vec2`,
`Vec3`, `Vec4`, `Quat` (encode vecs/quat as `Dict` of numbers or a small
`Arr[Double]`). Named-tuple derivation is a noted future extension, not in
scope.

### 3. Public API — `object devPreserve` with overloaded `apply`

Single object → guaranteed overload resolution + one import name.

- **Generic (simple values):**

  ```scala
  def apply[T: DevCodec](key: String, init: T): DevVar[T]
  ```

  - Dev: `value = decode(storage.get(key)).getOr(init)`; register flusher
    `storage.set(key, JSON.stringify(encode(value)))`.
  - `DevVar[T]`: `var value: T`; `def reset: DevVar[T]` → `removeItem(key)`,
    `value = init`, unregister flusher, return self.
  - Prod (`!devMode`): `DevVar(value = init)` with no storage, no registration.

- **Camera convenience:**

  ```scala
  def apply(cam: PerspectiveCamera, label: String = "camera"): DevHandle
  ```

  - Dev: capture initial snapshot `(pos, rotH, rotV)` (for reset); if stored
    entry exists, apply it via `cam(pos=, rotH=, rotV=)`; register flusher that
    reads the **live** cam (mutable ref) and writes the snapshot.
  - `DevHandle.reset`: `removeItem(key)`, re-apply the captured initial snapshot
    into `cam`, unregister flusher. (Because this runs during `init()` before
    the first paint, the transient "restore then reset" is never rendered.)
  - Prod: no-op handle.

The camera snapshot uses a small private encode/decode (5 doubles in a `Dict`) —
independent of the public `DevCodec` set, but the same storage/registry
primitives.

## Files

- **NEW** `src/dev/dev.scala` — `object devPreserve` (apply overloads),
  `DevVar[T]`, `DevHandle`, `DevCodec[T]` + givens, private
  storage/registry/guard.
- **NEW** `test/dev/DevCodec.test.scala` — pure round-trip tests for the codecs
  (storage/dom can't run headless; codec logic is pure and testable):
  encode→decode identity for Double/Int/Boolean/String/Vec3/Quat, and `decode`
  of malformed/`null` JSON → `Opt` empty (`.getOr(init)` falls back).
- **EDIT** `../sketches/rooms/base/Base.scala` — first consumer / demonstration:
  `import trivalibs.dev.*` and `devPreserve(cam)` right after the camera is
  created (the controller stays as-is). Shows the intended one-liner usage.

## Verification

1. **Library compiles in isolation:** `cd trivalibs && bun run check`.
2. **Codec unit tests:** `cd trivalibs && bun run test` → new
   `DevCodec.test.scala` passes (round-trips + malformed-input fallback).
3. **Sketch compiles:** from repo root `bun run sketch rooms/base`.
4. **End-to-end dev behavior** (`bun run dev`, open rooms/base):
   - Navigate the camera somewhere; edit `Base.scala` to force a
     recompile/reload; confirm the camera returns to the navigated viewpoint
     (not the initial pose).
   - In DevTools → Application → Session Storage, confirm a
     `trivalibs:dev:…:camera` entry exists and updates on reload.
   - Append `.reset` → `devPreserve(cam).reset`, reload: camera starts at the
     sketch's initial pose AND the session-storage entry is removed; further
     reloads (with `.reset` still present) do not re-create it. Remove `.reset`,
     reload: preservation resumes.
5. **Production inertness:** `bun run build`; confirm the dev branches are gated
   by `import.meta.hot` (undefined in prod → no storage access). Note: full
   dead-code elimination of the gated blocks is best-effort (depends on Vite/
   Rollup recognizing the pattern in Scala.js output); functional inertness is
   guaranteed by the runtime guard regardless.

## Open questions / notes

- Production erasure: the dev guard is emitted as literal `import.meta.hot` (dot
  access, via a typed `@js.native ImportMeta` facade — NOT `selectDynamic`,
  which would emit `import.meta["hot"]` and defeat static analysis). Verified in
  the built sketch JS. Vite replaces `import.meta.hot` → `undefined` in prod, so
  the gated branches are statically removable; the runtime guard guarantees
  inertness regardless.
- `DevCodec` derivation for named tuples / case classes is a natural follow-up
  (mirrors the `Position` named-tuple derivation) but is out of scope here.
- Multiple `devPreserve` calls in one sketch must use distinct labels/keys; the
  generic overload requires an explicit `key`, the camera overload defaults to
  `"camera"` (override via the `label` arg when preserving more than one).
