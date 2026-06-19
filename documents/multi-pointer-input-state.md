# Multi-pointer input + layered pointer/gesture architecture

## Context

On touch devices we lack primitives for richer gestures. Two things are wrong
today, and we fix both together (the lib is WIP — only lib examples + the graphics
sketches consume it — so breaking API changes are fine):

1. **No multi-touch.** `InputState`
   ([src/utils/events/input_state.scala](../src/utils/events/input_state.scala))
   models the pointer as a *single* live position (`pointerX/pointerY`,
   overwritten on every move). We need to track several concurrent pointers
   (default cap 4): for each, *since when* it is down and *where* it is, plus a
   "main" pointer and access to the rest.

2. **Gesture detection sits one layer too low.** `pointerRelay`
   ([src/utils/events/pointer.scala](../src/utils/events/pointer.scala)) is
   supposed to be a thin DOM-sanitizing translator, but it currently embeds the
   whole drag/hold state machine (`PointerTracker`, a hold `setTimeout`,
   `lastX/lastY`, `primaryActive`). Gesture interpretation is *stateful* and
   belongs above the state snapshot, not inside the event source. Tellingly,
   `PointerTracker` is already a DOM-free, unit-tested state machine — its design
   intent is a gesture-interpreter layer; it's just wired into the relay.

**Concrete driving use case**: the first-person camera should walk *backward* when
**two pointers are down** (a real two-finger touch) **or** a secondary mouse button
is held, while look/rotation keeps following the drag — and a multi-touch look must
not jump when the first finger lifts while a second keeps moving.

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
│   active Pointer list. Records state only — NO gesture logic. │
│   Also re-broadcasts pointer events via addPointerListener().  │
└──────────────────────────────────────────────────────────────┘
   │ polled queries  +  optional event hooks
   ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 3  gestures  (gestures.scala)                           │
│   DOM-free stateful interpreters over an InputState:          │
│   DragGesture (poll), HoldGesture (event+timer), later        │
│   Pan/Pinch/Rotate. Composable, unit-testable.                │
└──────────────────────────────────────────────────────────────┘

interactiveCanvas / painter.inputState  = opinionated preset:
   builds InputState + default DragGesture + HoldGesture, returns a bundle.
```

Key consequence: **drag delta is position-based per frame** (`currentDriverPos −
lastFrameDriverPos`), which is exact displacement regardless of how many move
events fired between frames — so no per-event accumulators are needed in
InputState, and the **hand-off "just works"**: when the driving pointer lifts, the
gesture reseeds its last-position to the newly-promoted driver, so the next delta
flows continuously from that pointer with no jump.

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
- `PointerTracker` (the DOM-free state machine) **moves to Layer 3** as the basis
  of the gesture interpreters; its existing unit test is superseded by
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
- Preallocated reusable `Arr[Pointer]` storage of size `maxPointers`, each with a
  private `var id: Opt[Double]` (`null` = free) — stable identity, no per-touch
  allocation.
- An **appearance-ordered active view** `Arr[Pointer]` (`order`), front = oldest
  still-down = main. Promotion on lift = splice the lifted one out.
- `keysDown` (code→downSince) and `buttonsDown` (button→true) as today, retained
  to keep `isKeyDown` / `isDown(button)` (the secondary-mouse-button query the
  camera still needs).
- `lastMainX/lastMainY` so `pointerX/Y` return the last main position when nothing
  is down.

Relay wiring (records state only, then re-broadcasts):
- **onDown**: claim a free slot if `order.length < maxPointers`, set
  `button/id/downSince/downX,downY/x,y`, push onto `order`; also `buttonsDown.set`;
  `fireActivity()`; then notify pointer-listeners. Over capacity → ignore.
- **onMove**: find slot by `id` (linear scan ≤4), update `x,y`; if front, update
  `lastMain*`; notify listeners. No `fireActivity` (matches "not on bare moves").
- **onUp / onCancel**: find slot by `id`, clear `id`, splice out of `order`
  (promotes next); `buttonsDown.delete` on up; `fireActivity()`; notify listeners.

Thin event hooks for Layer 3 (InputState stays gesture-free — it only relays its
own already-processed events):
```scala
def addPointerListener(
    onDown: Pointer => Unit = _ => (),
    onMove: Pointer => Unit = _ => (),
    onUp:   Pointer => Unit = _ => (),  // fired for up AND cancel
): () => Unit   // returns unsubscribe
```

Polled queries (replacing the public `pointerX/pointerY` vars):
- `def pointers: Arr[Pointer]` — read-only main-first view of currently-down
  pointers.
- `def pointersDown: Int`.
- `def pointer(i: Int): Pointer` — `order(i)`, valid for `i ∈ [0, pointersDown)`.
- `def pointerX/pointerY: Double` — main (`order(0)`) with `lastMain*` fallback.
- `def pointerDownMs(i, now = js.Date.now()): Double` — analogous to `keyHeldMs`.
- `isIdle` becomes `no keys && pointersDown == 0` (drag/hold no longer live here;
  they require a pointer down anyway, so this stays equivalent).

Refactor (folded in, approved): class + per-query scaladoc (top banner → class
scaladoc); extract listener wiring into a `private def install()` called from the
ctor so the body reads fields → `install()` → a contiguous query block; pointer
position is read-only; raw slots stay private behind `pointers`/`pointer(i)`.

## Layer 3 — `gestures.scala`: composable gesture interpreters

New file `src/utils/events/gestures.scala`. DOM-free, each constructed over an
`InputState`. "Drag-eligible driver" = the front-most pointer in `order` whose
`button == Primary` (covers left-mouse and every touch finger; middle/secondary
never drive drag).

**DragGesture(input)** — pure poll, no subscription:
```scala
class DragGesture(input: InputState):
  private var lastId: Opt[Double] = null
  private var lastX, lastY, accX, accY = 0.0
  def dragging: Boolean = driver.notNull
  /** Accumulated driver movement since last call; reseeds on hand-off (no jump). */
  def consumeDelta(): (dx: Double, dy: Double) = ...
```
`consumeDelta` finds the driver; if its id differs from `lastId` (fresh press or
hand-off) it reseeds `last = driver pos` contributing **zero** for the switch;
otherwise it adds `driver.pos − last`, updates `last`, returns and clears the
accumulator. Called once per frame by the consumer — this *is* the tick.

**HoldGesture(input, holdDelay = 400, holdRadius = 5, onActivity = Maybe.Not)** —
event-driven, keeps the timer (in Layer 3, not the relay):
- subscribes via `input.addPointerListener`: on a Primary driver press → arm
  `setTimeout(holdDelay)`; on move → track max stray from `downX/downY`, cancel if
  it exceeds `holdRadius`; on up → clear timer + `holding=false`.
- timer fire → if still the stationary driver: `holding = true` and fire
  `onActivity` (wakes an idle loop so a poll observes the hold). This preserves
  today's max-stray behavior exactly.
- `def holding: Boolean`.

Both are unit-testable by driving a fake InputState (the relocated `PointerTracker`
test coverage lands here).

## Opinionated preset — `interactiveCanvas` returns a bundle

`interactiveCanvas` (and `painter.inputState`) build the default gesture preset and
return a small bundle instead of a bare `InputState`:

```scala
class CanvasInput(
    val input: InputState,
    val drag: DragGesture,
    val hold: HoldGesture,
):
  export input.*            // isKeyDown, isDown, pointersDown, hasFocus, isIdle, ...
  def dispose(): Unit       // unsubscribe gestures + input.dispose()
```

`interactiveCanvas(canvas, …, holdDelay, holdRadius, onActivity)` constructs
`InputState`, `DragGesture(input)`, `HoldGesture(input, holdDelay, holdRadius,
onActivity)` and returns `CanvasInput`. Bare `InputState` + hand-rolled gestures
remain available for non-opinionated consumers.

## Consumer changes

### `camera_controller.scala`
`BasicFirstPersonCameraController.updateCamera(cam, in: CanvasInput, tpf)`:
- backward: `in.isDown(PointerButton.Secondary) || in.pointersDown >= 2` (keeps the
  secondary mouse button for non-touch input, adds genuine two-finger touch).
- forward-on-hold: `in.hold.holding`.
- look: `in.drag.consumeDelta()` — continuous across hand-off by construction.
- update the banner comment (the "secondary-button press walks backward" line).

### sketches (4 call sites) + `painter.scala`
`painter.inputState(...)` now returns `CanvasInput`; the sketch loops keep
`controller.updateCamera(cam, input, tpf)` unchanged in shape (only the type of
`input` changes) and `input.dispose()` still works via the bundle. Files:
[rooms/base/Base.scala](../../sketches/rooms/base/Base.scala),
[rooms/columns/Columns.scala](../../sketches/rooms/columns/Columns.scala),
[rooms/canvases/Canvases.scala](../../sketches/rooms/canvases/Canvases.scala),
[rooms/grid-ceiling/GridCeiling.scala](../../sketches/rooms/grid-ceiling/GridCeiling.scala).
`interactive_canvas.scala` already sets `touch-action: none` / `user-select: none`,
so concurrent touch points reach the relay.

## Forward-compatibility (pan / scale / rotate — out of scope now)

New multi-finger gestures are just more Layer-3 detectors over the same ordered
`pointers` list — **no relay or InputState change**:
- centroid = mean of positions, scale = distance between two pointers, rotation =
  angle between them, pan = centroid delta — all from `pointer(i).x/y` per frame
  (poll) or via `addPointerListener` (event), mirroring Drag/Hold.
- capacity (`maxPointers`) and per-pointer fields (pressure/tilt) extend in place.
- the preset in `interactiveCanvas` can later include a default Pan/Pinch when
  wanted, without touching lower layers.

This plan ships only the layering + multi-pointer list + Drag/Hold relocation + the
two-finger backward-walk consumer.

## Breaking changes / migration

- `pointerRelay` signature changes (gesture params removed; ids added). Only caller
  is `InputState`.
- `InputState.pointerX/pointerY` become read-only defs; `dragging`/`holding`/
  `consumeDragDelta` **leave** `InputState` and live on `DragGesture`/`HoldGesture`
  (reached via `CanvasInput.drag`/`.hold`).
- `interactiveCanvas` / `painter.inputState` return `CanvasInput` (was
  `InputState`); `BasicFirstPersonCameraController.updateCamera` takes `CanvasInput`.
- `PointerTracker` (and its test) relocate into the gesture layer.

## Verification

- `bun run check` — type-check the library in isolation.
- `bun run test` — gesture tests (relocated/expanded from PointerTracker) green.
- Manual multi-touch (the real acceptance test): from the graphics repo root
  rebuild a touch-capable camera sketch (`bun run sketch rooms/base`) and on a
  touch device / browser touch emulation confirm: one-finger drag rotates; adding a
  second finger walks **backward** while one-finger drag still only rotates;
  **lifting the first finger while the second keeps moving continues the look with
  no jump**; secondary mouse button still walks backward on desktop. Spot-check
  `pointersDown` / per-pointer `x/y`. (Dev server on :3000 assumed running.)
- Bundle-size sanity: no Scala-stdlib leak — slot scans are `while` loops, ordering
  uses native `Arr` push/splice, `buttonsDown`/listeners via `Dict`/`Arr`. Optional
  `jsModuleSplitStyleStr smallestmodules` diff if the events module grows
  unexpectedly.
