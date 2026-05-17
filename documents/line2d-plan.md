# 2D Line Geometry

Port of `trivalibs_core::rendering::line_2d` — a variable-width polyline builder
with mitre joins, smoothing, vertex cleanup, and a typed `BufferedGeometry`
exporter that feeds directly into the painter.

Companion documents:

- [documents/mesh-geometry-port-plan.md](/home/trival/code/personal/scala/webgpu/documents/mesh-geometry-port-plan.md)
  — original combined plan (now an index); all prerequisites listed there are
  done.
- [documents/geometry3d-plan.md](/home/trival/code/personal/scala/webgpu/documents/geometry3d-plan.md)
  — sibling plan for Grid / Cuboid / Sphere (independent feature).
- [documents/rust-painter/repomix-trivalibs-core.xml](/home/trival/code/personal/scala/webgpu/documents/rust-painter/repomix-trivalibs-core.xml)
  — Rust source bundle. Key section: lines 3136–3946 (`rendering/line_2d`).

---

## 1. Context

The Rust painter's brush / stroke work all routes through `line_2d`. A `Line` is
a sequence of `LineVertex` values, each carrying position, width, accumulated
length, direction, and optional user data. `toBufferedGeometry` expands the line
into a triangle-strip quad mesh (two verts per input vertex, joined at mitres)
and emits it as a typed `BufferedGeometry[LineAttribs]` that the existing
painter pipeline consumes unchanged via `Form` / `Shape`.

### Design decisions vs. Rust

| Rust pattern                          | Scala replacement                                                                                                          |
| ------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `LineData<EmptyData>` type alias      | `Line[Unit]` — `EmptyData` is `Unit`                                                                                       |
| `Lerp<f32>` bound on `T` everywhere   | `using Lerp[T]` only at operations that need it (smooth, cleanup, `toBufferedGeometry`)                                    |
| `NeighbourList<T>` doubly-linked list | Inlined `forEachWithNeighbours` helper (~15 lines) — the only consumer                                                     |
| Hard-coded `VertexData` struct        | `type LineAttribs = (position: Vec2, width: F32, length: F32, uv: Vec2, localUv: Vec2)` — `allocateAttribs` derives layout |

### Non-goals

- `data/neighbour_list/` — the container is not ported; only its iterator
  combinators are needed, inlined in `line2d.scala`.
- Round / square / butt cap styles — Rust does not have them either. Mitre join
  - ratio-cap only.

---

## 2. Implementation

**New file:** `src/graphics/geometry/line2d.scala`  
**New test:** `test/geometry/Line2d.test.scala`

### 2.1 Inline neighbour iterator

Private to `line2d.scala`. Replaces `map_with_prev_next` / `with_neighbours`
from `neighbour_list/traits.rs`:

```scala
inline def forEachWithNeighbours[T](arr: Arr[T])(
    inline f: (prev: Opt[T], curr: T, next: Opt[T]) => Unit,
): Unit =
  var i = 0
  val n = arr.length
  while i < n do
    val prev: Opt[T] = if i == 0     then null else arr(i - 1)
    val next: Opt[T] = if i == n - 1 then null else arr(i + 1)
    f(prev, arr(i), next)
    i += 1
```

A `flatMapWithNeighbours` variant that accumulates results into a fresh `Arr` is
also needed — both helpers are ~10 lines each.

### 2.2 `LineVertex[T]` and `Line[T]`

```scala
class LineVertex[T](
    val pos:   Vec2,
    var width: Double,
    var len:   Double,
    var dir:   Vec2,
    val data:  T,
):
  def pointTo(next: Vec2): Unit
  def smoothEdge(
      prev: LineVertex[T],
      next: LineVertex[T],
      ratio: Double,
      angleThreshold: Double,
  )(using Lerp[T]): Arr[LineVertex[T]]

object LineVertex:
  def apply[T](pos: Vec2, width: Double, data: T): LineVertex[T]
  def apply(pos: Vec2, width: Double): LineVertex[Unit]

class Line[T](val defaultWidth: Double, val offset: Double = 0.0):
  private val list: Arr[LineVertex[T]] = Arr()
  var totalLength: Double = 0.0

  def vertCount: Int
  def iter: Arr[LineVertex[T]]
  def first: LineVertex[T]
  def last:  LineVertex[T]
  def get(i: Int): LineVertex[T]

  // Building
  def add(pos: Vec2): Unit
  def add(pos: Vec2, width: Double): Unit
  def add(pos: Vec2, width: Double, data: T): Unit
  def addVert(v: LineVertex[T]): Unit       // auto-links prev.pointTo(v.pos)
  def addVertRaw(v: LineVertex[T]): Unit    // no linkage

  // Transformations — all return a new Line[T]
  def smoothEdges(ratio: Double, minDist: Double, angleThreshold: Double = 0.0)(using Lerp[T]): Line[T]
  def cleanup(minLenWidRatio: Double, widthThreshold: Double, angleThreshold: Double)(using Lerp[T]): Line[T]
  def splitAtAngle(angleThreshold: Double): Arr[Line[T]]

object Line:
  def apply[T](defaultWidth: Double): Line[T]
  def apply[T](defaultWidth: Double, offset: Double): Line[T]
  def fromPoints(defaultWidth: Double, points: Arr[Vec2]): Line[Unit]
```

### 2.3 Line → `BufferedGeometry[LineAttribs]`

Fixed vertex layout matching Rust `VertexData` at lines 3154–3162:

```scala
type LineAttribs = (position: Vec2, width: F32, length: F32, uv: Vec2, localUv: Vec2)

class LineGeometryProps(
    val smoothDepth:            Int     = 0,
    val smoothAngleThreshold:   Double  = 0.05,
    val smoothMinLength:        Double  = 3.0,
    val capWidthLengthRatio:    Double  = 1.0,
    val totalLength:            Opt[Double] = null,
    val prevDirection:          Opt[Vec2]   = null,
    val nextDirection:          Opt[Vec2]   = null,
    val swapTextureOrientation: Boolean = false,
)
object LineGeometryProps:
  val Default: LineGeometryProps = LineGeometryProps()

extension [T](line: Line[T])
  def toBufferedGeometry(
      props: LineGeometryProps = LineGeometryProps.Default,
  )(using Lerp[T]): BufferedGeometry[LineAttribs]

extension [T](lines: Arr[Line[T]])
  def toBufferedGeometries(
      props: LineGeometryProps = LineGeometryProps.Default,
  )(using Lerp[T]): Arr[BufferedGeometry[LineAttribs]]
```

Implementation follows Rust `buffered_geometry.rs` verbatim: mitre join math,
top/bottom vert generation, zig-zag index emission, conditional cap adjustment
via `prevDirection` / `nextDirection`. Uses `allocateAttribs[LineAttribs](n)`.

`toBufferedGeometries` threads `prevDirection` / `nextDirection` between
adjacent lines (Rust lines 3438–3453) and alternates `swapTextureOrientation`
per segment so stroke continuity is the default.

### 2.4 Test coverage

| Case                                       | Assertion                                                            |
| ------------------------------------------ | -------------------------------------------------------------------- |
| `LineVertex.pointTo`                       | mirrors Rust `vert_point_to` test (lines 3856–3872)                  |
| `Line.totalLength`                         | mirrors Rust `line_length` test (lines 3875–3887)                    |
| `Line.fromPoints`                          | mirrors Rust `from_vecs` test (lines 3889–3903)                      |
| `Line.cleanup` thresholds                  | mirrors Rust `cleanup_vertices` test (lines 3906–3945) — 5 sub-cases |
| `Line.splitAtAngle`                        | corner detection splits at expected segment boundaries               |
| `toBufferedGeometry` vert count            | `2N + cap adjustments` for N input verts                             |
| `toBufferedGeometry` index count           | correct zig-zag triangle-strip pattern                               |
| `toBufferedGeometry` UV                    | first / last vertex `v = 0.5` (cap centre); midpoints alternate      |
| `toBufferedGeometries` direction threading | segment-N's `prevDirection` equals segment-(N-1)'s last `dir`        |

---

## 3. Example — `examples/line2d_stroke/`

An animated 2D canvas that draws several polyline strokes using `Line` +
`toBufferedGeometry`.

### 3.1 Scene description

Three stroke tracks drawn each frame with updated vertex widths:

**Sine ribbon** — 60-point `Line[Unit]` following `y = sin(x · f + t)` across
the canvas width, with width pulsing between 4 and 14 px via
`sin(t · 1.3) · 5 + 9`. Re-built from `Line.fromPoints` each frame.

**Spiral** — A 120-point `Line[Unit]` tracing a shrinking Archimedean spiral
`(r·cos θ, r·sin θ)` centred on the canvas, radius decaying from 0.4 to 0.02
over the arc. Constant width 6 px, `smoothEdges(0.5, 3.0)` applied once.

**Branching path** — Three short `Line[Unit]` segments sharing a junction point,
rendered via `toBufferedGeometries` so UV continuity threads across the join
automatically.

### 3.2 Rendering

All strokes share one `Shade` that takes `position: Vec2`, `uv: Vec2`, and
`localUv: Vec2` as vertex attributes and a time uniform. Fragment shader reads
`localUv.y` for the cross-section gradient and `uv.x` for along-stroke colour:

```wgsl
let t = uniforms.time;
let band = smoothstep(0.0, 0.15, localUv.y) * smoothstep(0.0, 0.15, 1.0 - localUv.y);
let hue = uv.x + t * 0.1;
fragColor = vec4(hsv2rgb(vec3(hue, 0.7, 0.9)) * band, band);
```

The stroke widths and vertex counts are small enough that the `Form` is
re-uploaded from CPU each frame (testing the `Form.set` buffer-reuse path).

### 3.3 Vertex layout

`LineAttribs` is used directly — no additional per-vertex data needed. A single
`uniforms` bind group carries `time: f32`.

---

## 4. Implementation order

1. **`line2d.scala`** — `LineVertex`, `Line`, `forEachWithNeighbours`, all
   transformation methods.
2. **`toBufferedGeometry`** — mitre join algorithm + index emission.
3. **`Line2d.test.scala`** — all §2.4 cases passing.
4. **Example** — `examples/line2d_stroke/`, verified visually with
   `bun run dev`.

---

## 5. Critical files

| File                                                                        | Action  |
| --------------------------------------------------------------------------- | ------- |
| [src/graphics/geometry/line2d.scala](../src/graphics/geometry/line2d.scala) | **New** |
| [test/geometry/Line2d.test.scala](../test/geometry/Line2d.test.scala)       | **New** |
| [examples/line2d_stroke/](../examples/line2d_stroke/)                       | **New** |

### Existing files to reuse

- [src/graphics/geometry/buffers.scala](../src/graphics/geometry/buffers.scala)
  — `BufferedGeometry[F]`
- [src/graphics/geometry/package.scala](../src/graphics/geometry/package.scala)
  — `Lerp[Unit]` given, `Lerp[Vec2]` given
- [src/graphics/buffers/attributes.scala](../src/graphics/buffers/attributes.scala)
  — `allocateAttribs[LineAttribs]`
- [trivalibs/src/utils/js.scala](../trivalibs/src/utils/js.scala) — `Arr`, `Opt`
- [trivalibs/src/utils/numbers.scala](../trivalibs/src/utils/numbers.scala) —
  `NumExt` (`.sin`, `.cos`, `.sqrt`)

---

## 6. Verification

```bash
bun run build   # zero errors
bun run test    # Line2d.test.scala passes
bun run dev     # open :3000/line2d_stroke — three animated strokes visible
```
