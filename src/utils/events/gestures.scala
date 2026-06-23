package trivalibs.utils.events

import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

// ---------------------------------------------------------------------------
// Gestures — Layer 3: DOM-free interpreters built on the pointer snapshot of an
// `InputState`. Each is constructed over an InputState and advanced once per
// render frame via `update()`; the query members (`delta`, `holding`) are then
// pure, idempotent reads of the last frame's result. Driving `update()` from the
// render loop (rather than mutating on read) keeps the reads safe to call any
// number of times and from multiple consumers. Adding new multi-finger gestures
// (pan / pinch / rotate) means adding more of these, with no change to the relay
// or InputState.
//
// The "driver" of single-pointer gestures is the front-most pointer in the
// appearance-ordered active list whose initiating button is `Primary` (which
// covers the left mouse button and every touch contact). This gives free,
// jump-free hand-off: when the driving pointer lifts while others remain down,
// the next one is promoted and the gesture continues from its current position.
// ---------------------------------------------------------------------------

/** The current drag/hold driver: the first `Primary`-button pointer in the
  * active list, or `null` when none is down.
  */
private[events] def drivingPointer(pointers: Arr[Pointer]): Opt[Pointer] =
  var i = 0
  while i < pointers.length do
    val p = pointers(i)
    if p.button == PointerButton.Primary then return p
    i += 1
  null

/** Drag gesture: the movement of the driving pointer during the last [[update]]
  * frame. Hand-off is seamless — when the driver changes (lift or promote),
  * that frame's delta is zero (a reseed) and movement resumes from the new
  * driver, so a hand-off never produces a jump.
  *
  * Call [[update]] once per render frame, then read [[delta]] / [[dragging]].
  * Construct with `DragGesture(input)`.
  */
final class DragGesture private[events] (pointersOf: () => Arr[Pointer]):
  def this(input: InputState) = this(() => input.pointers)

  private var lastId: Opt[Double] = null
  private var lastX: Double = 0.0
  private var lastY: Double = 0.0
  private var _dx: Double = 0.0
  private var _dy: Double = 0.0

  /** Whether a drag-eligible pointer is currently down. */
  def dragging: Boolean = drivingPointer(pointersOf()).notNull

  /** The driving pointer's movement during the last [[update]] frame; `(0, 0)`
    * when no driver is down or on the frame a driver change reseeded.
    */
  def delta: (dx: Double, dy: Double) = (dx = _dx, dy = _dy)

  /** Advance one frame: recompute [[delta]] from the driver's movement and
    * advance the baseline. Call once per render frame before reading [[delta]].
    */
  def update(): Unit =
    val d = drivingPointer(pointersOf())
    if d.isNull then
      lastId = null
      _dx = 0.0
      _dy = 0.0
    else
      val p = d.get
      val sameDriver =
        lastId.notNull && p.id.notNull && lastId.get == p.id.get
      _dx = if sameDriver then p.x - lastX else 0.0
      _dy = if sameDriver then p.y - lastY else 0.0
      lastId = p.id
      lastX = p.x
      lastY = p.y

/** Hold gesture: the driving pointer stayed within `holdRadius` px of its
  * origin for the initial `holdDelay` ms (measured by accumulating `tpf`),
  * which activates the hold. The stray check only gates that initialization
  * window — once activated, the hold stays held until release no matter how far
  * the pointer then moves, so a consumer can drive movement and look/drag
  * simultaneously. Straying during the init window disqualifies the hold for
  * that press. A fresh press or driver hand-off restarts the timer.
  *
  * Call [[update]] once per render frame with the frame's `tpf`, then read
  * [[holding]]. Construct with `HoldGesture(input, holdDelay, holdRadius)`.
  */
final class HoldGesture private[events] (
    pointersOf: () => Arr[Pointer],
    holdDelay: Double,
    holdRadius: Double,
):
  def this(
      input: InputState,
      holdDelay: Double = 400.0,
      holdRadius: Double = 5.0,
  ) =
    this(() => input.pointers, holdDelay, holdRadius)

  private var lastId: Opt[Double] = null
  private var heldMs: Double = 0.0 // time the current driver has been down
  private var strayed: Boolean = false // strayed during the init window
  private var activated: Boolean = false // hold has activated (until release)
  private var _holding: Boolean = false

  /** Whether the driver is currently held, as of the last [[update]]. */
  def holding: Boolean = _holding

  /** Advance one frame by `tpf` (ms): accumulate the driver's held-time and
    * resolve the init-window stray / activation. Call once per render frame
    * before reading [[holding]].
    */
  def update(tpf: Double): Unit =
    val d = drivingPointer(pointersOf())
    if d.isNull then
      lastId = null
      heldMs = 0.0
      strayed = false
      activated = false
      _holding = false
    else
      val p = d.get
      val pid = p.id
      val sameDriver = lastId.notNull && pid.notNull && lastId.get == pid.get
      if !sameDriver then
        // fresh press or hand-off: restart the hold timer
        lastId = pid
        heldMs = 0.0
        strayed = false
        activated = false
      heldMs += tpf
      if activated then _holding = true
      else if heldMs < holdDelay then
        // init window: stray beyond the radius disqualifies this press
        val dx = p.x - p.downX
        val dy = p.y - p.downY
        if (dx * dx + dy * dy).sqrt > holdRadius then strayed = true
        _holding = false
      else if strayed then _holding = false
      else
        activated = true
        _holding = true
