# Multi-pointer input + layered pointer/gesture architecture

## Context

On touch devices we lack primitives for richer gestures. Two things are wrong
today, and we fix both together (the lib is WIP — only lib examples + the
graphics sketches consume it — so breaking API changes are fine):

1. **No multi-touch.** `InputState`
   ([src/utils/events/input_state.scala](../src/utils/events/input_state.scala))
   models the pointer as a _single_ live position (`pointerX/pointerY`,
   overwritten on every move). We need to track several concurrent pointers
   (default cap 4): for each, _since when_ it is down and _where_ it is, plus a
   "main" pointer and access to the rest.

2. **Gesture detection sits one layer too low.** `pointerRelay`
   ([src/utils/events/pointer.scala](../src/utils/events/pointer.scala)) is
   supposed to be a thin DOM-sanitizing translator, but it currently embeds the
   whole drag/hold state machine (`PointerTracker`, a hold `setTimeout`,
   `lastX/lastY`, `primaryActive`). Gesture interpretation is _stateful_ and
   belongs above the state snapshot, not inside the event source. Tellingly,
   `PointerTracker` is already a DOM-free, unit-tested state machine — its
   design intent is a gesture-interpreter layer; it's just wired into the relay.

**Concrete driving use case**: the first-person camera should walk _backward_
when **two pointers are down** (a real two-finger touch) **or** a secondary
mouse button is held, while look/rotation keeps following the drag — and a
multi-touch look must not jump when the first finger lifts while a second keeps
moving.

## Target architecture — three layers

```
DOM events
   │
   ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 1  relay  (pointer.scala / keyboard.scala)              │
│   stateless: sanitize raw DOM → typed callbacks, no storage   │
│   onDown(button,id,x,y,isPrimary)  onMove(id,x,y)             │
│   onUp(button,id,x,y)  onCancel(id)                           │
└──────────────────────────────────────────────────────────────┘
   │ typed events
   ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 2  InputState  (input_state.scala)                      │
│   the pollable snapshot: keysDown, buttonsDown, the ordered   │
│   active Pointer list. Records state only — NO gesture logic, │
│   purely polled (no event re-broadcast).                      │
└──────────────────────────────────────────────────────────────┘
   │ polled queries
   ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 3  gestures  (gestures.scala)                           │
│   DOM-free stateful interpreters over an InputState, all      │
│   poll-based: DragGesture, HoldGesture, later Pan/Pinch/      │
│   Rotate. Composable, unit-testable.                          │
└──────────────────────────────────────────────────────────────┘

interactiveCanvas / painter.inputState  = opinionated preset:
   builds InputState + default DragGesture + HoldGesture, returns a bundle.
```

Key consequence: **drag delta is position-based per frame**
(`currentDriverPos − lastFrameDriverPos`), which is exact displacement
regardless of how many move events fired between frames — so no per-event
accumulators are needed in InputState, and the **hand-off "just works"**: when
the driving pointer lifts, the gesture reseeds its last-position to the
newly-promoted driver, so the next delta flows continuously from that pointer
with no jump.

## Layer 1 — `pointer.scala`: make the relay stateless

Strip all gesture/state from `pointerRelay`. Remove `PointerTracker` usage, the
hold `setTimeout`, `lastX/lastY`, `primaryActive`, `endPrimary`, and the
`onDragStart/onDrag/onDragEnd/onHold` + `holdDelay/holdRadius` params. The relay
becomes pure translation:

```scala
def pointerRelay(
    el: dom.EventTarget,
    moveTarget: dom.EventTarget = dom.window,
    onDown:   (PointerButton, Double, Double, Double, Boolean) => Unit = ..., // btn,id,x,y,isPrimary
    onMove:   (Double, Double, Double) => Unit = ...,                         // id,x,y
    onUp:     (PointerButton, Double, Double, Double) => Unit = ...,          // btn,id,x,y
    onCancel: (Double) => Unit = ...,                                         // id
    suppressContextMenu: Boolean = true,
): () => Unit
```

- `downFn/moveFn/upFn/cancelFn` just forward `e.button` (typed), `e.pointerId`,
  `e.clientX/Y`, `e.isPrimary`. Same listener targets as today (press on `el`,
  move/up/cancel on `moveTarget`) and contextmenu suppression unchanged.
- `PointerTracker` (the DOM-free state machine) **moves to Layer 3** as the
  basis of the gesture interpreters; its existing unit test is superseded by
  `DragGesture`/`HoldGesture` tests.

## Layer 2 — `input_state.scala`: state + ordered pointer list (+ refactor)

A lightweight `Pointer` (plain Scala class first per lib guidance; switch to
`js.Object` only if runtime field names / size regressions demand it):

```scala
final class Pointer:
  var button: PointerButton = PointerButton.Primary // initiating button
  var downSince: Double = 0.0                       // ms timestamp of press
  var downX: Double = 0.0                           // press origin (for hold radius)
  var downY: Double = 0.0
  var x: Double = 0.0                               // current position
  var y: Double = 0.0
  // future: pressure, tiltX, tiltY
```

State (new ctor param `maxPointers: Int = 4`):

- Preallocated reusable `Arr[Pointer]` storage of size `maxPointers`, each with
  a private `var id: Opt[Double]` (`null` = free) — stable identity, no
  per-touch allocation.
- An **appearance-ordered active view** `Arr[Pointer]` (`order`), front = oldest
  still-down = main. Promotion on lift = splice the lifted one out.
- `keysDown` (code→downSince) and `buttonsDown` (button→downSince, a ms
  timestamp like `keysDown` for consistency) — retained to keep `isKeyDown` /
  `isDown(button)` (the secondary-mouse-button query the camera still needs).
- `lastMainX/lastMainY` so `pointerX/Y` return the last main position when
  nothing is down.

Relay wiring (records state only, then re-broadcasts):

- **onDown**: claim a free slot if `order.length < maxPointers`, set
  `button/id/downSince/downX,downY/x,y`, push onto `order`; also
  `buttonsDown.set`; `fireActivity()`; then notify pointer-listeners. Over
  capacity → ignore.
- **onMove**: find slot by `id` (linear scan ≤4), update `x,y`; if front, update
  `lastMain*`; notify listeners. No `fireActivity` (matches "not on bare
  moves").
- **onUp / onCancel**: find slot by `id`, clear `id`, splice out of `order`
  (promotes next); `buttonsDown.delete` on up; `fireActivity()`; notify
  listeners.

InputState is **purely polled** — it exposes no pointer-event subscription.
Layer 3 gestures read the snapshot each frame (timestamps + positions) and keep
whatever small internal state they need. If we later want genuinely event-based
gesture logic, the cleaner path is to give that consumer access to the **raw
relay** (Layer 1) rather than re-broadcasting from InputState — left as a future
follow-up, not built now.

Polled queries (replacing the public `pointerX/pointerY` vars):

- `def pointers: Arr[Pointer]` — read-only main-first view of currently-down
  pointers.
- `def pointersDown: Int`.
- `def pointer(i: Int): Opt[Pointer]` — `order(i)`, or `null` when `i` is out of
  range (not currently pressed). Callers null-check.
- `def pointerX/pointerY: Double` — main (`order(0)`) with `lastMain*` fallback.
- `def pointerDownMs(i, now = js.Date.now()): Double` and
  `def buttonHeldMs(button, now = js.Date.now()): Double` — analogous to
  `keyHeldMs` (all three read their respective `downSince` timestamp).
- `isIdle` becomes `no keys && pointersDown == 0` (drag/hold no longer live
  here; they require a pointer down anyway, so this stays equivalent).

Refactor (folded in, approved): class + per-query scaladoc (top banner → class
scaladoc); extract listener wiring into a `private def install()` called from
the ctor so the body reads fields → `install()` → a contiguous query block;
pointer position is read-only; raw slots stay private behind
`pointers`/`pointer(i)`.

## Layer 3 — `gestures.scala`: composable gesture interpreters

New file `src/utils/events/gestures.scala`. DOM-free, each constructed over an
`InputState`. Each gesture is **ticked once per render frame** with `update()`;
the query members (`delta`, `holding`) are then **pure, idempotent reads** of the
last frame's result — safe to call any number of times and from multiple
consumers (no consume-on-read footgun). "Drag-eligible driver" = the front-most
pointer in `order` whose `button == Primary` (covers left-mouse and every touch
finger; middle/secondary never drive drag).

**DragGesture(input)** — position-based per-frame delta:

```scala
class DragGesture(input: InputState):
  private var lastId: Opt[Double] = null
  private var lastX, lastY, _dx, _dy = 0.0
  def dragging: Boolean = driver.notNull
  /** The driver's movement during the last update() frame (pure read). */
  def delta: (dx: Double, dy: Double) = (dx = _dx, dy = _dy)
  /** Advance one frame: recompute delta + baseline. */
  def update(): Unit = ...
```

`update` finds the driver; if its id differs from `lastId` (fresh press or
hand-off) it reseeds `last = driver pos`, making that frame's delta **zero** (no
jump); otherwise `_dx,_dy = driver.pos − last` and the baseline advances. `delta`
just returns `_dx,_dy`.

**HoldGesture(input, holdDelay = 400, holdRadius = 5)** — no timer, no
subscription; time is measured by **accumulating `tpf`** (so it pauses/resumes
with the render loop and uses no wall clock); `holding` reflects the last
`update()`:

```scala
class HoldGesture(input: InputState, holdDelay = 400.0, holdRadius = 5.0):
  private var lastId: Opt[Double] = null
  private var heldMs = 0.0      // accumulated time the current driver was down
  private var strayed = false   // strayed during the init window
  private var activated = false // hold activated (until release)
  def holding: Boolean = ...
  def update(tpf: Double): Unit = ...
```

`update(tpf)` reads the current Primary driver; if none → `holding = false`
(state resets). A fresh press / driver change (id differs from `lastId`) restarts
the timer. It accumulates `heldMs += tpf`. The stray check only gates the
**initialization window** (`heldMs < holdDelay`): a sample of the driver's
distance from its origin (`downX/downY`) beyond `holdRadius` there sets `strayed`
and disqualifies the press. Once `heldMs` reaches `holdDelay` without having
strayed, the hold **activates** and stays held until release **regardless of
further movement** — so the consumer can drive movement and look/drag
simultaneously (only the short initial press must stay still).

No wake is needed: a pointer being down keeps `isIdle` false, so the consumer's
loop is already running and ticks the hold each frame. Stray is sampled at frame
cadence rather than per move event — ample for the camera use case and far
simpler. The radius origin uses `Pointer.downX/downY` (positional); `downSince`
is no longer read by hold (it remains only for the wall-clock `pointerDownMs`
query).

Both are unit-testable by driving a fake InputState (the relocated
`PointerTracker` test coverage lands here).

## Opinionated preset — `interactiveCanvas` returns a bundle

`interactiveCanvas` (and `painter.inputState`) build the default gesture preset
and return a small bundle instead of a bare `InputState`:

```scala
class CanvasInput(
    val input: InputState,
    val drag: DragGesture,
    val hold: HoldGesture,
):
  export input.{dispose => _, *} // isKeyDown, isDown, pointersDown, isIdle, ...
  def update(tpf: Double): Unit   // ticks drag + hold; call once per frame
  def dispose(): Unit             // input.dispose() (gestures hold none)
```

`interactiveCanvas(canvas, …, holdDelay, holdRadius, onActivity)` constructs
`InputState` (passing `onActivity`), `DragGesture(input)`, and
`HoldGesture(input, holdDelay, holdRadius)` and returns `CanvasInput`. The
consumer calls `ci.update(tpf)` **once per render frame** (explicitly, in the
sketch/render loop) before reading `ci.drag.delta` / `ci.hold.holding` or driving
a controller. Bare `InputState` + hand-rolled gestures remain available for
non-opinionated consumers.

**Time rule**: per-frame stepping methods (`CanvasInput.update`,
`HoldGesture.update`, the camera controller) all take `tpf` (ms since last
frame), matching `animate`; this pauses/resumes with the loop and avoids any wall
clock. Event-age queries (`keyHeldMs` / `pointerDownMs` / `buttonHeldMs`) keep
their wall-clock `now = Date.now()` default — a separate concern (real time since
an event), and they never collide with stepping since the loop only idles when
nothing is held.

## Consumer changes

### `camera_controller.scala`

`BasicFirstPersonCameraController(cam, in: CanvasInput, sensitivity, speed)` binds
the camera + input at construction and exposes `update(tpf)`:

- backward: `in.isDown(PointerButton.Secondary) || in.pointersDown >= 2` (keeps
  the secondary mouse button for non-touch input, adds genuine two-finger
  touch).
- forward-on-hold: `in.hold.holding`.
- look: `in.drag.delta` — continuous across hand-off by construction. The
  controller reads only; the consumer must have called `in.update(tpf)` this
  frame.
- update the banner comment (the "secondary-button press walks backward" line).

### sketches (4 call sites) + `painter.scala`

`painter.input(...)` now returns `CanvasInput`. The sketch loops construct
`BasicFirstPersonCameraController(cam, input, …)` and per frame call
`input.update(tpf)` then `controller.update(tpf)`, and
`input.dispose()` still works via the bundle. Files:
[rooms/base/Base.scala](../../sketches/rooms/base/Base.scala),
[rooms/columns/Columns.scala](../../sketches/rooms/columns/Columns.scala),
[rooms/canvases/Canvases.scala](../../sketches/rooms/canvases/Canvases.scala),
[rooms/grid-ceiling/GridCeiling.scala](../../sketches/rooms/grid-ceiling/GridCeiling.scala).
`interactive_canvas.scala` already sets `touch-action: none` /
`user-select: none`, so concurrent touch points reach the relay.

## Forward-compatibility (pan / scale / rotate — out of scope now)

New multi-finger gestures are just more Layer-3 detectors over the same ordered
`pointers` list — **no relay or InputState change**:

- centroid = mean of positions, scale = distance between two pointers, rotation
  = angle between them, pan = centroid delta — all from `pointer(i).x/y` polled
  per frame, mirroring Drag/Hold.
- capacity (`maxPointers`) and per-pointer fields (pressure/tilt) extend in
  place.
- the preset in `interactiveCanvas` can later include a default Pan/Pinch when
  wanted, without touching lower layers.

This plan ships only the layering + multi-pointer list + Drag/Hold relocation +
the two-finger backward-walk consumer.

## Breaking changes / migration

- `pointerRelay` signature changes (gesture params removed; ids added). Only
  caller is `InputState`.
- `InputState.pointerX/pointerY` become read-only defs; `dragging`/`holding`/
  `consumeDragDelta` **leave** `InputState` and live on
  `DragGesture`/`HoldGesture` (reached via `CanvasInput.drag`/`.hold`).
- `interactiveCanvas` / `painter.input` return `CanvasInput` (was `InputState`);
  `BasicFirstPersonCameraController` binds `cam` + `CanvasInput` at construction
  and exposes `update(tpf)`.
- `PointerTracker` (and its test) relocate into the gesture layer.

## Verification

- `bun run check` — type-check the library in isolation.
- `bun run test` — gesture tests (relocated/expanded from PointerTracker) green.
- Manual multi-touch (the real acceptance test): from the graphics repo root
  rebuild a touch-capable camera sketch (`bun run sketch rooms/base`) and on a
  touch device / browser touch emulation confirm: one-finger drag rotates;
  adding a second finger walks **backward** while one-finger drag still only
  rotates; **lifting the first finger while the second keeps moving continues
  the look with no jump**; secondary mouse button still walks backward on
  desktop. Spot-check `pointersDown` / per-pointer `x/y`. (Dev server on :3000
  assumed running.)
- Bundle-size sanity: no Scala-stdlib leak — slot scans are `while` loops,
  ordering uses native `Arr` push/splice, `buttonsDown` via `Dict`. Optional
  `jsModuleSplitStyleStr smallestmodules` diff if the events module grows
  unexpectedly.

## Implementation status

**Implemented & verified** (2026-06-24). Differs from the original sketch above in
two iterated decisions, both reflected in the sections above:

1. **No event subscription on InputState.** Gestures are pure-poll; `InputState`
   exposes no `addPointerListener`. Future event-based logic would tap the raw
   relay instead.
2. **Explicit per-frame `update(tpf)` + pure reads.** Gestures don't consume on
   read. Each frame the consumer calls `CanvasInput.update(tpf)`; then
   `drag.delta` / `hold.holding` are pure, idempotent reads. All stepping is
   driven by **`tpf`** (no wall clock); `HoldGesture` accumulates `heldMs` from
   `tpf` (init-window stray check only — movement after activation is allowed, so
   move + look work together).

Files:

- Layer 1 — [src/utils/events/pointer.scala](../src/utils/events/pointer.scala):
  stateless relay, per-pointer ids; `PointerTracker` removed.
- Layer 2 —
  [src/utils/events/input_state.scala](../src/utils/events/input_state.scala):
  `Pointer` type, reusable ordered slots, `pointers`/`pointersDown`/`pointer(i):
  Opt`/`pointerX,Y`/`pointerDownMs`/`buttonHeldMs` (timestamped `buttonsDown`),
  `install()` refactor, scaladoc.
- Layer 3 — [src/utils/events/gestures.scala](../src/utils/events/gestures.scala):
  `DragGesture` (position-based per-frame `delta`, jump-free hand-off) +
  `HoldGesture` (tpf-accumulated, init-window stray latch).
- Preset —
  [src/utils/events/interactive_canvas.scala](../src/utils/events/interactive_canvas.scala):
  `CanvasInput(input, drag, hold)` with `update(tpf)` / `dispose()`.
- Consumers —
  [src/graphics/painter/painter.scala](../src/graphics/painter/painter.scala)
  (`input()` → `CanvasInput`),
  [src/graphics/scene/camera_controller.scala](../src/graphics/scene/camera_controller.scala)
  (`(cam, in, sensitivity, speed)` + `update(tpf)`; backward = secondary mouse
  **or** `pointersDown >= 2`).
- Tests — [test/utils/Gestures.test.scala](../test/utils/Gestures.test.scala)
  (8 cases, DOM-free via the `() => Arr[Pointer]` constructor).

**Status of checks**: `bun run check` clean; `bun run test` green (8 gesture
tests + full suite); all four `rooms/*` sketches relink (`main.js` rebuilt); all
touched sources `scalafmt`-formatted.

**Remaining**: the manual multi-touch acceptance test (above) on a real touch
device — the only thing unit tests can't cover. The hand-off no-jump path is unit
tested; the device run validates two-finger backward + simultaneous
hold-move/look feel.
