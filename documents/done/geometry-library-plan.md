# Geometry Library Plan

## Context

The painter abstraction is complete. The next milestone adds a geometry
processing library at `src/graphics/geometry/` that provides type-safe
primitives (Triangle, Quad), a Mesh container with position-sharing index, and
several buffer-generation strategies. At the end, index buffer support is added
to the painter's Form and draw path.

The design is informed by the existing Rust `trivalibs_core` mesh geometry
implementation (same author), adapted to Scala 3 idioms: type classes with
extension methods, opaque types, no stdlib collections, no enums,
`Arr`/`js.Dictionary` everywhere.

---

## File Organisation

```
src/graphics/geometry/
  package.scala  — Position[T], Lerp[T], Vec3 givens, Plane, Vec3 extension methods
  polygon.scala  — Triangle[T] and Quad[T] opaque types (one file, related concepts)
  mesh.scala     — Face[T] type alias, Mesh[T] class, MeshBufferType
  buffers.scala  — toBufferedGeometry (all strategies)

src/graphics/math/gpu/expr.scala     — add lerp aliases for all VecNExpr types
src/graphics/painter/form.scala      — add index buffer fields + setIndices overloads
src/graphics/painter/painter.scala   — branch on form.indexBuffer in draw path
src/webgpu/facades.scala             — add setIndexBuffer + drawIndexed to GPURenderPassEncoder

test/geometry/
  Plane.test.scala
  Polygon.test.scala  — Triangle and Quad tests
  Mesh.test.scala
  Buffers.test.scala
```

---

## Phase 0 — GPU DSL lerp aliases (`expr.scala`)

`Vec3ImmutableOpsG` already defines `inline def lerp = v.mix(b, t)` for CPU
types (vec3.scala lines 65-67). The GPU `expr.scala` overrides `mix` for
`Vec2Expr`, `Vec3Expr`, `Vec4Expr`, and `FloatExpr` but has no `lerp` aliases —
GPU DSL users can only write `mix`. Add explicit aliases next to every `mix`
override:

```scala
// Example for Vec3Expr block — same pattern for Vec2Expr, Vec4Expr, FloatExpr:
@annotation.targetName("lerpVec3G")
inline def lerp(b: Vec3Expr, t: Vec3Expr): Vec3Expr = v.mix(b, t)
@annotation.targetName("lerpScalar3G")
inline def lerp(b: Vec3Expr, t: FloatExpr): Vec3Expr = v.mix(b, t)
```

---

## Phase 1 — Type Classes & Plane (`package.scala`)

```scala
package graphics.geometry

trait Position[T]:
  extension (t: T) def pos: Vec3

trait Lerp[T]:
  extension (a: T) def lerp(b: T, t: Double): T

// Generic given for ANY type with Vec3Base — covers Vec3, Vec3Tuple, Vec3Buffer,
// and future custom types. Allocates a Vec3; fine since pos is used for
// comparisons, not in hot render loops.
given [V] => Vec3Base[V] => Position[V]:
  extension (v: V) def pos: Vec3 = Vec3(v.x, v.y, v.z)

// Override for Vec3 itself — returns v directly, no allocation
given Position[Vec3]:
  extension (v: Vec3) def pos: Vec3 = v

// Generic Lerp via Vec3ImmutableOps.lerp (already inline alias for mix)
given [V: {Vec3Base, Vec3ImmutableOps}] => Lerp[V]:
  extension (a: V) def lerp(b: V, t: Double): V = a.lerp(b, t)

// Plane: normal · p - d = signed distance (positive = in front)
class Plane(val normal: Vec3, val d: Double) extends js.Object:
  inline def signedDist(p: Vec3): Double = normal.dot(p) - d

private inline val ROUNDING = 10000.0
private inline val DELTA    = 1.0 / ROUNDING

extension (v: Vec3)
  def approxEq(w: Vec3): Boolean =
    (v.x * ROUNDING).toInt == (w.x * ROUNDING).toInt &&
    (v.y * ROUNDING).toInt == (w.y * ROUNDING).toInt &&
    (v.z * ROUNDING).toInt == (w.z * ROUNDING).toInt
  def onPlane(p: Plane): Boolean  = math.abs(p.signedDist(v)) < DELTA
  def inFront(p: Plane): Boolean  = p.signedDist(v) >  DELTA
  def behind(p: Plane): Boolean   = p.signedDist(v) < -DELTA
```

---

## Phase 2 — Triangle and Quad (`polygon.scala`)

Both opaque types in one file — they're closely related (`toTriangles`,
`splitByPlane` result type).

### Triangle

```scala
opaque type Triangle[T] <: Arr[T] = Arr[T]

object Triangle:
  def apply[T](a: T, b: T, c: T): Triangle[T] =
    Arr(a, b, c).asInstanceOf[Triangle[T]]

  extension [T](tri: Triangle[T])
    def a: T = tri(0);  def b: T = tri(1);  def c: T = tri(2)

    def normal(using pos: Position[T]): Vec3 =
      // Cross product of two edges, CCW → outward normal
      val pa = tri.a.pos; val pb = tri.b.pos; val pc = tri.c.pos
      val e1 = Vec3(pb.x - pa.x, pb.y - pa.y, pb.z - pa.z)
      val e2 = Vec3(pc.x - pa.x, pc.y - pa.y, pc.z - pa.z)
      e1.cross(e2).normalize

    def splitByPlane(plane: Plane)
        (using pos: Position[T], lerp: Lerp[T]): Arr[Triangle[T]] =
      // Sutherland-Hodgman: clip 3 edges, collect output polygon (0-4 verts),
      // then fan-triangulate (see shared algorithm below)
```

### Quad

Winding convention: **CCW viewed from front** — `bl(0), br(1), tr(2), tl(3)`.

```scala
opaque type Quad[T] <: Arr[T] = Arr[T]

object Quad:
  def apply[T](bl: T, br: T, tr: T, tl: T): Quad[T] =
    Arr(bl, br, tr, tl).asInstanceOf[Quad[T]]

  extension [T](q: Quad[T])
    def bl: T = q(0);  def br: T = q(1)
    def tr: T = q(2);  def tl: T = q(3)

    def normal(using pos: Position[T]): Vec3 =
      // Both diagonals cross product: (tr-bl) × (tl-br), normalised.
      // Handles non-planar quads; mirrors Rust's calculate_normal fallback.
      val a = q.bl.pos; val b = q.br.pos
      val c = q.tr.pos; val d = q.tl.pos
      val d1 = Vec3(c.x - a.x, c.y - a.y, c.z - a.z)  // bl→tr
      val d2 = Vec3(d.x - b.x, d.y - b.y, d.z - b.z)  // br→tl
      d1.cross(d2).normalize

    def subdivideH(using Lerp[T]): (Quad[T], Quad[T]) =
      // horizontal split → bottom half and top half
      val ml = q.bl.lerp(q.tl, 0.5);  val mr = q.br.lerp(q.tr, 0.5)
      (Quad(q.bl, q.br, mr, ml), Quad(ml, mr, q.tr, q.tl))

    def subdivideV(using Lerp[T]): (Quad[T], Quad[T]) =
      // vertical split → left half and right half
      val mb = q.bl.lerp(q.br, 0.5);  val mt = q.tl.lerp(q.tr, 0.5)
      (Quad(q.bl, mb, mt, q.tl), Quad(mb, q.br, q.tr, mt))

    def toTriangles: (Triangle[T], Triangle[T]) =
      (Triangle(q.bl, q.br, q.tr), Triangle(q.bl, q.tr, q.tl))

    def splitByPlane(plane: Plane)
        (using pos: Position[T], lerp: Lerp[T]): Arr[Triangle[T] | Quad[T]] =
      // Same algorithm, 4 input verts, output 0-5 verts:
      // 0 → empty;  3 → Triangle;  4 → Quad;  5 → Quad(0..3) + Triangle(0,3,4)
```

### splitByPlane shared algorithm (N = 3 for Triangle, 4 for Quad)

```
val out: Arr[T] = Arr()
for i in 0 until N:
  val curr = face(i);  val next = face((i+1) % N)
  val currIn = !curr.pos.behind(plane)
  val nextIn = !next.pos.behind(plane)
  if currIn then out.push(curr)
  if currIn != nextIn then
    val dc = plane.signedDist(curr.pos)
    val dn = plane.signedDist(next.pos)
    out.push(curr.lerp(next, dc / (dc - dn)))  // t in [0,1] along edge

// Fan-triangulate output polygon:
// 0 verts → empty
// 3 verts → Triangle(out(0), out(1), out(2))
// 4 verts → two Triangles (Triangle) or Quad (Quad)
// 5 verts → Quad(out(0..3)) + Triangle(out(0), out(3), out(4))  [Quad only]
```

---

## Phase 3 — Mesh (`mesh.scala`)

Modelled after Rust `MeshGeometry<V>`. Faces are unified (triangle OR quad) via
the union type alias. The position registry is separate from face-vertex data,
matching Rust's `positions: Vec<VertexPosition>`.

```scala
// Union type alias — Face can hold either 3 or 4 vertices; check .length to discriminate
type Face[T] = Triangle[T] | Quad[T]

// Which vertex slot within a face a shared position appears at
class PositionFaceRef(val faceIndex: Int, val vertexSlot: Int) extends js.Object

// Position registry entry (mirrors Rust's VertexPosition)
class VertexPosition[T](val position: Vec3, val faces: Arr[PositionFaceRef]) extends js.Object

// Per-face metadata; normal is computed lazily via ensureFaceNormals()
class FaceData(var normal: Opt[Vec3], val section: Int) extends js.Object

// Buffer strategy — opaque Int with companion val constants (no enum)
opaque type MeshBufferType = Int
object MeshBufferType:
  val FaceVertices:                 MeshBufferType = 0  // no normals
  val FaceVerticesWithFaceNormal:   MeshBufferType = 1  // same normal for all verts of a face
  val FaceVerticesWithVertexNormal: MeshBufferType = 2  // averaged from adjacent faces
  val CompactVertices:              MeshBufferType = 3  // deduped by position, always indexed
  val CompactVerticesWithNormal:    MeshBufferType = 4  // compact + averaged normals

class Mesh[T](using pos: Position[T]):
  val faces:       Arr[Face[T]]           = Arr()   // all faces, unified
  val faceData:    Arr[FaceData]          = Arr()   // parallel array of metadata
  val positions:   Arr[VertexPosition[T]] = Arr()   // unique position registry
  val positionMap: js.Dictionary[Int]     = js.Dictionary()  // posKey → positions index

  // section defaults to 0; caller can supply a pre-computed normal
  def addFace(face: Face[T], normal: Opt[Vec3] = Opt.Null, section: Int = 0): Unit
  def removeFace(faceIdx: Int): Unit  // swap-remove + rewrite position refs

  def getPosition(v: Vec3): Opt[VertexPosition[T]]
  def adjacentFaces(v: Vec3): Arr[Face[T]]

  // Lazily compute face normals; returns true if any quads present (signals index needed)
  def ensureFaceNormals(): Boolean

  // Functional mesh transforms (mirrors Rust map / flat_map / flat_map_data)
  def map[U: Position](f: Face[T] => Face[U]): Mesh[U]
  def flatMap[U: Position](f: Face[T] => Arr[Face[U]]): Mesh[U]
  def flatMapWithData[U: Position](f: (Face[T], FaceData) => Arr[(Face[U], FaceData)]): Mesh[U]

  def newFromSection(section: Int): Mesh[T]
```

**Position key**:
`s"${(v.x*10000).toInt},${(v.y*10000).toInt},${(v.z*10000).toInt}"`

**`addFace`** flow: for each vertex `v` in the face, compute its key, look it up
in `positionMap`; if missing, append a new `VertexPosition` to `positions` and
record the key. Push a `PositionFaceRef(faceIndex, slot)` onto the matching
`VertexPosition.faces`.

**`removeFace`** uses swap-remove and rewrites position refs, mirroring Rust's
`unregister_face_refs` + `rewrite_face_refs`.

### Deferred: Lazy Position Key Caching

`posKey(v: Vec3)` allocates a string on every vertex lookup. For large meshes
this could matter. Design change options — defer until profiling shows it:

**Option A — extend `Position[T]`** with an overridable default:

```scala
trait Position[T]:
  extension (t: T) def pos: Vec3
  extension (t: T) def posKey: String =  // default; users can override
    val p = t.pos
    s"${(p.x*10000).toInt},${(p.y*10000).toInt},${(p.z*10000).toInt}"
```

**Option B — separate `PositionKey[T]` type class** with a default given that
bridges from `Position[T]`:

```scala
trait PositionKey[T]:
  extension (t: T) def posKey: String

given [T] => Position[T] => PositionKey[T]:
  extension (t: T) def posKey: String =
    val p = t.pos
    s"${(p.x*10000).toInt},${(p.y*10000).toInt},${(p.z*10000).toInt}"
```

`Mesh` would then require `PositionKey[T]`.

**Option C — three-level nested `Dict` tree** (no string allocation):

Replace `Dict[Int]` with `Dict[Dict[Dict[Int]]]` — each dimension is a separate
dictionary level keyed by the rounded integer coordinate:

```scala
type PosMap = Dict[Dict[Dict[Int]]]

def posLookup(map: PosMap, v: Vec3): Opt[Int] =
  val xi = (v.x * 10000).toInt.toString
  val yi = (v.y * 10000).toInt.toString
  val zi = (v.z * 10000).toInt.toString
  val xd = map.get(xi).orNull
  if xd == null then null
  else
    val yd = xd.get(yi).orNull
    if yd == null then null
    else yd.get(zi).orNull

def posInsert(map: PosMap, v: Vec3, idx: Int): Unit =
  val xi = (v.x * 10000).toInt.toString
  val yi = (v.y * 10000).toInt.toString
  val zi = (v.z * 10000).toInt.toString
  if !map.contains(xi) then map(xi) = Dict()
  val xd = map(xi)
  if !xd.contains(yi) then xd(yi) = Dict()
  xd(yi)(zi) = idx
```

Three short integer strings instead of one concatenated string — avoids the
`s"$x,$y,$z"` interpolation overhead (extra allocation + string copy in V8).
Still does three dictionary lookups. Worth benchmarking against Option A/B
before adopting.

**Option D — cache inside `Mesh`:** Two sub-variants, both deferred:

- _`WeakMap` by object identity_ — fails because the default
  `given [V] => Vec3Base[V] => Position[V]` creates a fresh `Vec3` on every
  `.pos` call, so the same coordinate produces different instances and the cache
  always misses.
- _`FaceWrapper[T]`_ holding `(face: Face[T], vertexMeta: Arr[VertexMeta])` with
  pre-computed keys per slot — avoids recomputation but adds an extra object
  allocation per face and pointer indirection on every vertex access (normal
  computation, buffer generation, adjacency queries). Indirection cost on hot
  paths likely outweighs key savings that only occur during construction.

Profile Options A–C before revisiting.

---

## Phase 4 — Buffer Generation (`buffers.scala`)

### Writer types

```scala
// Basic: write vertex data into a pre-allocated StructRef slot
type VertexWriter[T, F <: Tuple]  = (T, StructRef[F]) => Unit

// With normal: used when strategy includes normals
type VertexWriterN[T, F <: Tuple] = (T, Vec3, StructRef[F]) => Unit
```

### Strategies

**`FaceVertices`** — no normals. Emit vertices per-face in order. If quads
present (signalled by `ensureFaceNormals()` return value), automatically emit a
per-face local index buffer so quads triangulate correctly: for a quad starting
at offset `base`, indices are `[base, base+1, base+2, base, base+2, base+3]`.
Triangles get no local index.

**`FaceVerticesWithFaceNormal`** — same layout, face normal repeated for all
vertices of a face. Uses `VertexWriterN`.

**`FaceVerticesWithVertexNormal`** — vertex normal averaged from adjacent faces
in the same section. Look up per-position normals from `mesh.positions`; sum
face normals → normalize. Uses `VertexWriterN`.

**`CompactVertices`** — walk `mesh.positions` in order; emit one vertex per
unique position using `VertexWriter`. Then emit index buffer from each face's
position indices (always has index buffer).

**`CompactVerticesWithNormal`** — same, but compute and append averaged vertex
normal per position.

### Output

```scala
class BufferedGeometry[F <: Tuple](
  val vertices: StructArray[F],
  val indices:  Opt[scala.scalajs.js.typedarray.Uint16Array | scala.scalajs.js.typedarray.Uint32Array],
) extends js.Object
```

Auto-select Uint16 vs Uint32 based on vertex count (>65535 → Uint32).

### Top-level entry point

```scala
def toBufferedGeometry[T, F <: Tuple](
  mesh:         Mesh[T],
  bufferType:   MeshBufferType,
  writer:       VertexWriter[T, F],
  writerN:      Opt[VertexWriterN[T, F]] = Opt.Null,
): BufferedGeometry[F]
```

---

## Phase 5 — Index Buffer in Painter

### `src/webgpu/facades.scala`

Add to `GPURenderPassEncoder`:

```scala
def setIndexBuffer(buffer: GPUBuffer, format: String): Unit = js.native
def drawIndexed(indexCount: Int): Unit = js.native
```

### `src/graphics/painter/form.scala`

```scala
// New fields alongside existing vertexBuffer / vertexCount:
var indexBuffer: Opt[GPUBuffer] = null
var indexFormat: String = "uint16"
var indexCount: Int = 0

// Two overloads — union type can't be pattern-matched on erased JS arrays
def setIndices(data: scala.scalajs.js.typedarray.Uint16Array): this.type = ...
def setIndices(data: scala.scalajs.js.typedarray.Uint32Array): this.type = ...
// Each: destroy old buffer, create GPUBuffer(INDEX | COPY_DST), writeBuffer(data.buffer), set fields
```

### `src/graphics/painter/painter.scala`

In `renderShapeOnPass` (and any other `pass.draw` call site):

```scala
// replace:
pass.draw(shape.form.vertexCount)
// with:
if shape.form.indexBuffer.notNull then
  pass.setIndexBuffer(shape.form.indexBuffer.get, shape.form.indexFormat)
  pass.drawIndexed(shape.form.indexCount)
else
  pass.draw(shape.form.vertexCount)
```

---

## Future: Derive Shader Attrib Contract from Mesh Vertex Type

### Core idea

`T` in `Mesh[T]` can be a case class. `NamedTuple.From[T]` (SIP-58, shipped in
Scala 3.8) converts any case class to its named-tuple equivalent at compile
time:

```scala
case class MyVertex(position: Vec3, uv: Vec2, color: Vec3)
// NamedTuple.From[MyVertex] = (position: Vec3, uv: Vec2, color: Vec3)
```

Using this, we derive the shader attrib contract automatically, with an optional
normal appended:

```scala
type AttribsOf[T]         = NamedTuple.From[T]
type AttribsWithNormal[T] = NamedTuple.Concat[NamedTuple.From[T], (normal: Vec3)]
```

### Auto-derive VertexWriter

The existing `allocateAttribs` + `AttribLayout` machinery already maps a named
tuple → `StructArray[F]`. A `VertexWriter` for case class `T` would:

1. Convert `t: T` to its named tuple via `NamedTuple.From[T]`
2. Walk fields inline (same pattern as `derive.scala`)
3. Write each to the corresponding `StructRef` slot

```scala
transparent inline given [T] => VertexWriter[T, AttribBuffer[AttribsOf[T]]]:
  // inline field loop over NamedTuple.From[T],
  // same approach as allocateAttribs derivation
  ...

transparent inline given [T] => VertexWriterN[T, AttribBuffer[AttribsWithNormal[T]]]:
  // write T fields first, then write normal to last slot
  ...
```

### Usage

```scala
case class MyVertex(position: Vec3, uv: Vec2, color: Vec3)
// Position[MyVertex] given: v.position is Vec3, Vec3Base[Vec3] available
// → generic Vec3Base given kicks in, or user writes one-liner manually

val mesh = Mesh[MyVertex]()
// ... build mesh ...

val geom = toBufferedGeometryAuto(mesh, FaceVerticesWithVertexNormal)
// geom: BufferedGeometry[AttribBuffer[AttribsWithNormal[MyVertex]]]
//     ≡ BufferedGeometry[AttribBuffer[(position: Vec3, uv: Vec2, color: Vec3, normal: Vec3)]]

type FinalAttribs = AttribsWithNormal[MyVertex]
val form  = painter.form[FinalAttribs]().set(vertices = geom.vertices)
val shade = painter.shade[FinalAttribs, Varyings, Uniforms]: program => ...
```

This closes the loop: the mesh vertex type IS the shader attribute type.
Separate milestone after the base geometry library is working.

---

## Implementation Order

1. `expr.scala` — `lerp` aliases for Vec2/3/4Expr and FloatExpr
2. `package.scala` — Position (generic via Vec3Base), Lerp (generic via
   Vec3ImmutableOps), Plane, Vec3 extensions → `test/geometry/Plane.test.scala`
3. `polygon.scala` — Triangle and Quad → `test/geometry/Polygon.test.scala`
4. `mesh.scala` → `test/geometry/Mesh.test.scala`
5. `buffers.scala` → `test/geometry/Buffers.test.scala`
6. facades + form + painter (index buffer) → verify existing painter tests pass
7. _(Future)_ `NamedTuple.From[T]`-based attrib auto-derivation

Phases 1–5 are pure CPU, testable without WebGPU.

---

## Tests

Add `//> using file ../src/graphics/geometry` to `test/test-setup.scala`.

| File                 | Key cases                                                                                                                                                  |
| -------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Plane.test.scala`   | signedDist signs; approxEq boundary (0.00009 → equal, 0.00012 → not); onPlane/inFront/behind                                                               |
| `Polygon.test.scala` | Triangle: normal +Z for CCW XY; splitByPlane 0/1/2/3 in-front. Quad: normal +Z; subdivideH/V midpoints; toTriangles CCW; splitByPlane 0/3/4/5-vert outputs |
| `Mesh.test.scala`    | addFace builds positionMap; adjacentFaces; removeFace keeps refs; ensureFaceNormals                                                                        |
| `Buffers.test.scala` | vertex/index counts per strategy; CompactVertices deduplicates; mixed tri+quad FaceVertices index output                                                   |

---

## Open Questions / Deferred

- **splitByPlane on Mesh** — add later as a `flatMap` call once primitives are
  solid.
- **Polygon trait/type class** — not needed; `Face[T] = Triangle[T] | Quad[T]`
  union serves the role.
- **`section` support** — include from the start as a plain `Int` field on
  `FaceData`, default 0.
- **Normal in addFace** — caller can supply (e.g. from `Triangle.normal`) or
  leave for lazy computation.
- **Lazy position key caching** — defer; extend `Position[T]` or add
  `PositionKey[T]` when profiling shows it matters.
- **Shader attrib auto-derivation** — separate milestone; needs
  `NamedTuple.From[T]` + transparent inline writer derivation.
