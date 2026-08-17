# Non-browser hosts — Deno first, NativeScript additive

**Status: planned, not implemented.** No code in `src/` has been changed for
this. This document records the design so that the first implementation step
does not have to re-derive it, and so that the later NativeScript target is a
pure addition rather than a second refactoring.

## Motivation

Sketches in the consuming repo were refactored from `@main` to
`@JSExportTopLevel("sketch")` taking the canvas as a parameter, precisely so a
sketch never fetches its own canvas from `document`. That removed the _sketch_
side of the browser coupling. The library side is still browser-only: three
spots in the painter and one in `animate` assume a DOM.

Two hosts are wanted, in this order:

1. **Deno** — a real desktop WebGPU runtime with `navigator.gpu` built in
   ([Deno WebGPU docs](https://docs.deno.com/runtime/desktop/webgpu/)). It runs
   on the development machine with no mobile toolchain, so it is the cheap
   place to do the porting work and to get a rendered frame into CI.
2. **NativeScript** — `@nativescript/canvas` exposes WebGPU on Android/iOS
   (currently the `2.0.0-webgpu.*` prerelease line). This is the actual
   deployment interest; Deno is the proving ground.

The design constraint that shapes everything below: **adding the NativeScript
host later must not touch `Painter`, `RenderHost`, or the Deno host.** A new
file and a new constructor call in the app glue, nothing else.

---

## What is browser-coupled today

Verified against the current sources:

| Where                   | Line              | Assumption                                           |
| ----------------------- | ----------------- | ---------------------------------------------------- |
| `painter/webgpu.scala`  | 209               | `dom.window.navigator.gpu` — needs `window`          |
| `painter/webgpu.scala`  | 216               | `canvas.getContext("webgpu")`                        |
| `painter/painter.scala` | 1918–1921         | `canvas.clientWidth/Height`, writes `.width/.height` |
| `painter/painter.scala` | 1923–1936         | `new ResizeObserver(...)`                            |
| `painter/painter.scala` | 94, 108, 117, 120 | `Painter.canvas` is the size source of truth         |
| `painter/painter.scala` | 1211              | `context.getCurrentTexture()`, no explicit present   |
| `utils/animate.scala`   | 39, 43            | `requestAnimationFrame` on the global object         |

Of these, only #1 is an outright bug on a non-browser host that otherwise has
the API: Deno and NativeScript both define `navigator.gpu` but neither defines
`window`. The rest are genuine capability differences.

**Deliberately out of scope** — these stay browser-only and are the reason
`base-triangle` (not a room sketch) is the pilot:

- `utils/events/*` (`CanvasInput`, pointer, keyboard) — DOM event targets
  throughout, reached via `Painter.input`.
- `dev/dev.scala` — `sessionStorage` plus `window` event listeners.
- `preact/*` — DOM by definition.

A sketch that calls `p.input` or `trivalibs.dev.*` is not portable, and this
plan does not try to make it so. Input abstraction is a separate, larger
design; note that NativeScript will eventually want it (touch), Deno-with-SDL2
would want it too, and that is the point at which it becomes worth doing.

---

## Design: `RenderHost`

`Painter` stops talking to a canvas and talks to a host.

```scala
trait RenderHost:
  /** Swap-chain-ish context. `show()` renders into its current texture. */
  def context: GPUCanvasContext
  /** Drawable size in physical pixels. */
  def width: Int
  def height: Int
  /** Texture format for the context; hosts may not implement
    * `getPreferredCanvasFormat()`, so the host decides, with a fallback. */
  def preferredFormat: String
  /** Configure the context once the device exists. */
  def configure(device: GPUDevice, format: String): Unit
  /** End-of-frame present. No-op in the browser. */
  def present(): Unit
  /** Register a resize callback; may never fire on fixed-size hosts. */
  def onResize(cb: (Double, Double) => Unit): Unit
  /** Schedule the next frame. */
  def requestFrame(cb: Double => Unit): Unit
```

`Painter` gains a `host: RenderHost` field; `width`/`height` delegate to it,
`show()` ends with `host.present()`, and `Painter.init` no longer knows what a
canvas is.

### Why no auto-detecting registry

The tempting shape is `Painter.init(surface)` with a capability-detection chain
that picks the host. **Reject it on bundle-size grounds**: detection means every
host is reachable from the entry point, so Scala.js DCE cannot drop the ones a
given bundle never uses, and every sketch pays for every host forever. That is
exactly the kind of cost the library's size discipline exists to prevent.

Instead the host is **explicit and constructed by the app glue**. A sketch that
only ever runs in a browser links `BrowserHost` and nothing else; the Deno
entry point links `DenoHost` and nothing else. Additivity falls out for free —
`NativeScriptHost` is a file nobody references until an app references it.

### Host implementations

| Host               | `context`                               | `present()`         | `onResize`             | `requestFrame`             |
| ------------------ | --------------------------------------- | ------------------- | ---------------------- | -------------------------- |
| `BrowserHost`      | `canvas.getContext("webgpu")`           | no-op               | `ResizeObserver`       | `requestAnimationFrame`    |
| `DenoWindowHost`   | `surface.getContext("webgpu")`          | `surface.present()` | SDL2 window event      | driven by the SDL2 loop    |
| `DenoHeadlessHost` | fake context over an owned `GPUTexture` | no-op               | never fires            | immediate / N-frame driver |
| `NativeScriptHost` | `canvas.getContext("webgpu")`           | **verify**          | canvas `layoutChanged` | rAF polyfill (**verify**)  |

`BrowserHost` is today's `Painter.init` body moved verbatim behind the trait —
no behavior change, no sketch change, and it is the whole of phase 0.

`DenoHeadlessHost` is worth the small hack: rather than teaching `show()` about
a target that has no swap chain, the host hands back an object whose
`getCurrentTexture()` returns a texture it owns. `show()` stays untouched, and
the host exposes `readPixels(): js.Promise[Uint8Array]` (via
`copyTextureToBuffer` + `mapAsync`) so a test can write a PNG. This is what
makes an automated visual smoke test possible at all.

### `getGPU()` — the one fix that is unconditionally right

```scala
def getGPU(): js.UndefOr[GPU] =
  js.Dynamic.global.navigator
    .asInstanceOf[js.Dynamic]
    .gpu
    .asInstanceOf[js.UndefOr[GPU]]
```

`globalThis.navigator` resolves in browsers, Deno, and NativeScript alike.
Independent of the rest of this plan and safe to land on its own.

### The frame loop

`animate` reaches for the global `requestAnimationFrame`, which Deno does not
define. Add `painter.animate(frame)` routing through `host.requestFrame`, and
leave the free `animate` as the browser convenience it already is. Sketches
migrate from `animate: tpf =>` to `p.animate: tpf =>` — mechanical, and only
required for sketches that want to be portable.

### Entry-point signature — the one cross-repo decision

Sketches are currently `def sketch(canvas: HTMLCanvasElement)`. `HTMLCanvasElement`
is a browser type that a Deno surface will never be. Two options:

1. **Sketch takes a `RenderHost`** (recommended). The glue constructs the host:
   browser `index.html` does `sketch(new BrowserHost(canvasEl))`, the Deno
   entry does `sketch(new DenoWindowHost(surface))`, NativeScript does its own.
   Sketch bodies become genuinely host-agnostic and `Painter.init(host)` is the
   only init overload that matters. Cost: one more line of glue per host, and a
   second pass over the sketches in the consuming repo.
2. **Sketch keeps a widened `opaque type Surface = js.Any`** and `Painter.init`
   branches internally. Less churn now, but it either reintroduces the
   detection registry rejected above or pushes the branch into every sketch.

Recommend (1), and note that the `@JSExportTopLevel("sketch")` refactor already
paid most of the migration cost — the remaining change is the parameter type
and the glue line.

Keep `Painter.init(canvas: HTMLCanvasElement)` as a browser overload regardless,
so existing sketches and all of `examples/` keep compiling untouched.

`Painter.input` needs a real DOM canvas. It should live on `BrowserHost` (or
require one), not on `Painter`, so that a portable sketch cannot silently
depend on it.

---

## Phasing

Each phase is independently landable and leaves the library working.

**Phase 0 — host seam, browser only.** Introduce `RenderHost`, implement
`BrowserHost` from the existing init body, switch `getGPU()` to `globalThis`,
add `painter.animate`. No behavior change; `examples/` is the regression test.
Nothing about Deno yet.

**Phase 1 — Deno headless.** `DenoHeadlessHost` + a script that imports a built
sketch bundle, renders one frame, reads pixels back, writes a PNG. This is the
cheapest possible proof that the whole pipeline runs with no DOM, and it is the
piece that can run in CI. Pilot: the triangle example.

**Phase 2 — Deno windowed.** `DenoWindowHost` over
[`Deno.UnsafeWindowSurface`](https://docs.deno.com/api/deno/~/Deno.UnsafeWindowSurface),
obtained from SDL2 (`jsr:@divy/sdl2`, `win.windowSurface()`). Adds the explicit
`present()` and a real resize path, which is where the trait earns itself.

**Phase 3 — NativeScript (later).** New file `NativeScriptHost`, an app under
the consuming repo, nothing else. If this phase turns out to require edits to
`RenderHost`, the trait was wrong and that is the signal to revisit.

---

## To verify at implementation time

Claims below come from documentation, not from a run on this machine:

- Deno's current flag requirements for WebGPU (`--unstable-webgpu` historically;
  check against the Deno version in use).
- Whether `Deno.UnsafeWindowSurface` supports resizing in place or must be
  recreated when the SDL2 window resizes. This decides how ugly
  `DenoWindowHost.onResize` is.
- Whether Deno implements `getPreferredCanvasFormat()`; fall back to
  `"bgra8unorm"` if not.
- NativeScript: whether presenting is implicit or an explicit call, whether
  `requestAnimationFrame` is polyfilled by `@nativescript/canvas`, and whether
  the canvas exposes a usable resize/layout event.
- `@nativescript/canvas` WebGPU is a prerelease line — pin an exact version and
  expect API drift.

## Related

- Consuming-repo entry-point convention (`@JSExportTopLevel("sketch")`, canvas
  passed in) — the graphics repo's `CLAUDE.md`.
- [NativeScript Canvas installation](https://canvas.nativescript.org/canvas/installation)
