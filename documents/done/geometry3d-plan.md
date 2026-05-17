# 3D Geometry Helpers — Grid, Cuboid, Sphere ✅ Done

Port of `trivalibs_core` 3D geometry helpers: a coordinate grid with wrap-around
strategies, Cuboid / Quad factories, and a sphere mesh generator. Culminates in
a rendered scene that exercises all three features together.

Companion documents:

- [documents/mesh-geometry-port-plan.md](/home/trival/code/personal/scala/webgpu/documents/mesh-geometry-port-plan.md)
  — original combined plan (now an index); prerequisites listed there are all
  done.
- [documents/line2d-plan.md](/home/trival/code/personal/scala/webgpu/documents/line2d-plan.md)
  — sibling plan for 2D line geometry (independent feature).
- [documents/rust-painter/repomix-trivalibs-core.xml](/home/trival/code/personal/scala/webgpu/documents/rust-painter/repomix-trivalibs-core.xml)
  — Rust source bundle. Key sections: lines 119–1755 (`data/grid`), 5307–5893
  (`rendering/mesh_geometry/utils`, `rendering/shapes/*`).

---

## 1. Context

`src/graphics/geometry/` already has the mesh core (`Mesh[T]`, `Quad[T]`,
`Triangle[T]`, five buffer strategies, `Lerp` givens, `Int` extensions). The
missing pieces are the helpers that actually _build_ geometry:

- **`Grid[T]`** — 2D grid with four wrap strategies, `map` / `flatMap`, column /
  row mutation, subdivision via `Lerp[T]`, direct export to `Arr[Quad[T]]`.
- **`Quad` 3D factories** — dimensioned construction, from-corners,
  from-three-corners.
- **`Cuboid`** — eight corners, six typed face extractors with optional UV
  mappers.
- **`sphereMesh`** — body quads + pole triangles, UV-mapped.

All three feed `Mesh[T]` and the existing `toBufferedGeometry` pipeline with no
new GPU concepts.

### Design principles

| Rust pattern                                           | Scala replacement                                                         |
| ------------------------------------------------------ | ------------------------------------------------------------------------- |
| `CoordOpsFn` trait + 4 zero-sized structs              | `opaque type CoordOps = Int` + `val` constants — same as `MeshBufferType` |
| `Quad3D<P>` (separate type)                            | Reuse existing `Quad[T]` opaque; add factories to companion               |
| `Position3D` trait                                     | Reuse `Position[T]` from `geometry/package.scala`                         |
| `{front,back,...}_face` + `_face_f` pairs (12 methods) | Six `*Face` methods with optional mapper                                  |

---

## 2. Phase 1 — Grid

**New file:** `src/graphics/geometry/grid.scala`  
**New test:** `test/geometry/Grid.test.scala`

### 2.1 Coordinate-overflow strategy (`CoordOps`)

```scala
opaque type CoordOps = Int
object CoordOps:
  val ClampToEdge: CoordOps = 0  // bit 0 = circle cols, bit 1 = circle rows
  val CircleCols:  CoordOps = 1
  val CircleRows:  CoordOps = 2
  val CircleAll:   CoordOps = 3

  extension (c: CoordOps)
    inline def circleCols: Boolean = (c & 1) != 0
    inline def circleRows: Boolean = (c & 2) != 0

    def adjust(x: Int, y: Int, w: Int, h: Int): (Int, Int) =
      val nx = if c.circleCols then ((x % w) + w) % w else x.clamp(0, w - 1)
      val ny = if c.circleRows then ((y % h) + h) % h else y.clamp(0, h - 1)
      (nx, ny)
```

### 2.2 `Grid[T]` class

```scala
class Grid[T](val coordOps: CoordOps = CoordOps.ClampToEdge):
  private val cols: Arr[Arr[T]] = Arr()
  def width:  Int = cols.length
  def height: Int = if cols.length == 0 then 0 else cols(0).length

  def get(x: Int, y: Int): T
  def set(x: Int, y: Int, v: T): Unit
  def vertex(x: Int, y: Int): Vertex[T]

  def col(x: Int): Arr[T]
  def row(y: Int): Arr[T]
  def firstCol: Arr[T]; def lastCol: Arr[T]
  def firstRow: Arr[T]; def lastRow: Arr[T]

  def addCol(vals: Arr[T]): Unit   // panics if length mismatches height
  def addRow(vals: Arr[T]): Unit   // panics if length mismatches width

  def map[B](f: Vertex[T] => B): Grid[B]
  def flatMapCols[B](f: Arr[Vertex[T]] => Arr[Arr[B]]): Grid[B]
  def flatMapRows[B](f: Arr[Vertex[T]] => Arr[Arr[B]]): Grid[B]

  def quadCount: (Int, Int)         // (w, h) accounting for circular wrap
  def ccwQuads: Arr[Quad[T]]
  def cwQuads:  Arr[Quad[T]]
  inline def quads: Arr[Quad[T]] = ccwQuads

extension [T: Lerp](g: Grid[T])
  def subdivide(countX: Int, countY: Int): Grid[T]
```

`subdivide` is an extension so the `Lerp[T]` constraint stays local to that one
operation and does not infect `map` / `flatMapCols` / `flatMapRows`.

`Vertex[T]` is a minimal view with a back-pointer to the grid:

```scala
class Vertex[T](val x: Int, val y: Int, val value: T, grid: Grid[T]):
  def next(dx: Int, dy: Int): Opt[Vertex[T]] =
    val v = grid.vertex(x + dx, y + dy)
    if v.x == x && v.y == y then null else v
  inline def left   = next(-1,  0)
  inline def right  = next( 1,  0)
  inline def top    = next( 0, -1)
  inline def bottom = next( 0,  1)
```

### 2.3 Factories

```scala
object Grid:
  // Empty: Grid[T](CoordOps.CircleAll) works via creator application — no apply needed.
  def fromCols[T](cols: Arr[Arr[T]], coordOps: CoordOps = CoordOps.ClampToEdge): Grid[T]
  def fromRows[T](rows: Arr[Arr[T]], coordOps: CoordOps = CoordOps.ClampToEdge): Grid[T]
```

### 2.4 Quad winding (top-left origin)

Grid `(0, 0)` is top-left; y increases downward. Quad corner order is
`tl(0), bl(1), br(2), tr(3)` — CCW viewed from front, matching the current
`Quad.apply` signature.

CCW quad at grid cell `(x, y)`:

```scala
Quad(tl = get(x, y), bl = get(x, y+1), br = get(x+1, y+1), tr = get(x+1, y))
```

Quad indices wrap through `CoordOps.adjust` so circular grids produce
seam-closing quads automatically (mirrors Rust lines 384–396 / 437–472).

### 2.5 Test coverage

| Case                             | Assertion                                                          |
| -------------------------------- | ------------------------------------------------------------------ |
| `get` / `set` clamp              | negative and oversized indices clamp correctly                     |
| `get` / `set` circle_all         | wrap on both axes; `(4,4)` on 3×3 grid affects `(1,1)`             |
| `addCol` / `addRow` length check | mismatched length throws                                           |
| `Vertex.left/right/top/bottom`   | ClampToEdge returns `null` at edge; CircleAll wraps                |
| `map`                            | shape preserved, values transformed, neighbour access in `f` works |
| `flatMapCols` / `flatMapRows`    | expand factor honoured; returned grid dimensions correct           |
| `subdivide(1, 1)`                | 2×2 → 3×3 with interpolated midpoints via `Lerp`                   |
| `subdivide(3, 0)`                | rows unchanged, cols multiplied by 4                               |
| `ccwQuads` on 2×2                | single `Quad` with correct `tl/bl/br/tr`                           |
| `ccwQuads` with `CircleAll`      | seam-closing quads included; count matches `quadCount`             |

---

## 3. Phase 2 — Shape builders (Cuboid, Quad factories, Sphere)

**New file:** `src/graphics/geometry/shapes.scala`  
**New test:** `test/geometry/Shapes.test.scala`

### 3.1 Quad construction factories

Extend the existing `object Quad` in `polygon.scala`:

```scala
object Quad:
  // already: def apply[T](tl, bl, br, tr): Quad[T]

  // Dimensioned construction around a pivot.
  // uvAtPivot: (0,0) = top-left, (0.5,0.5) = center, (1,1) = bottom-right.
  def fromDimensions[T](
      width: Double,
      height: Double,
      normal: Vec3,
      pivot: Vec3,
      uvAtPivot: Vec2 = Vec2(0.5, 0.5),
  )(f: (Vec3, Vec2) => T): Quad[T]

  inline def fromDimensionsCenter[T](w: Double, h: Double, n: Vec3, center: Vec3)(f: (Vec3, Vec2) => T): Quad[T] =
    fromDimensions(w, h, n, center, Vec2(0.5, 0.5))(f)
  inline def fromDimensionsTopLeft[T](w: Double, h: Double, n: Vec3, tl: Vec3)(f: (Vec3, Vec2) => T): Quad[T] =
    fromDimensions(w, h, n, tl, Vec2(0.0, 0.0))(f)

  // From 4 explicit corner positions — derives normal from diagonals.
  def fromCorners[T](tl: Vec3, bl: Vec3, br: Vec3, tr: Vec3)(f: (Vec3, Vec2) => T): Quad[T]

  // Infer missing 4th corner from 3 adjacent corners.
  // Replaces Rust's from_tl_bl_tr_f / from_tl_bl_br_f / from_tl_br_tr_f / from_br_bl_tr_f.
  def fromThreeCorners[T](a: Vec3, b: Vec3, c: Vec3, missing: QuadCorner)(f: (Vec3, Vec2) => T): Quad[T]

opaque type QuadCorner = Int
object QuadCorner:
  val TopLeft:     QuadCorner = 0
  val BottomLeft:  QuadCorner = 1
  val BottomRight: QuadCorner = 2
  val TopRight:    QuadCorner = 3
```

### 3.2 `Cuboid`

```scala
class Cuboid(
    val center: Vec3,
    val size: Vec3,
    val frontTopLeft:     Vec3, val frontTopRight:    Vec3,
    val frontBottomLeft:  Vec3, val frontBottomRight: Vec3,
    val backTopLeft:      Vec3, val backTopRight:     Vec3,
    val backBottomLeft:   Vec3, val backBottomRight:  Vec3,
):
  // Six face methods — each has two overloads:
  //   no args  → Quad[Vec3]  (position only)
  //   with f   → Quad[T]     (f receives (pos: Vec3, uvw: Vec3))
  // uvw is (0..1)³ in cube space; uvw.z == 0 for front, 1 for back, etc.
  def frontFace: Quad[Vec3]
  def frontFace[T](f: (Vec3, Vec3) => T): Quad[T]
  // back / left / right / top / bottom — same pair each

  // All six faces with shared mapper. Normal is pre-computed (±X/±Y/±Z).
  def faces: Arr[(Quad[Vec3], Vec3)]
  def faces[T](f: (Vec3, Vec3) => T): Arr[(Quad[T], Vec3)]

object Cuboid:
  def apply(center: Vec3, width: Double, height: Double, depth: Double): Cuboid
  def unit: Cuboid = apply(Vec3.zero, 1.0, 1.0, 1.0)
```

Winding: Rust `front_face_f` returns
`Quad3D { top_left, bottom_left, bottom_right, top_right }` — exactly
`(tl, bl, br, tr)`. The face → corner table is a verbatim transliteration of
Rust lines 5436–5524.

### 3.3 Sphere generator

```scala
// Top-level helper in shapes.scala:
def sphereMesh[T: Position](
    verticalSegments:   Int,
    horizontalSegments: Int,
)(f: (pos: Vec3, uv: Vec2) => T): Mesh[T]
```

Mirrors Rust `create_sphere_mesh` (lines 5839–5893). Builds body quads and pole
triangles, then calls `Mesh(faces)`. The mapper:

- `pos` — unit-sphere position; scale / translate inside `f`.
- `uv` — `(u / 2π, (v + π/2) / π)` normalised to `[0,1]²`, top-left origin.

### 3.4 Test coverage

| Case                                       | Assertion                                                     |
| ------------------------------------------ | ------------------------------------------------------------- |
| `Cuboid(center, 1, 1, 1)` corners          | all 8 corners match expected `Vec3`s                          |
| `frontFace` positions                      | `tl/bl/br/tr` match `frontTopLeft` / `frontBottomLeft` / etc. |
| `frontFace(f)` UVW                         | `uvw.z == 0` for front; `uvw.z == 1` for back                 |
| all six faces — correct normals            | table-driven `(face, expectedNormal)` pairs                   |
| `Quad.fromDimensionsCenter`                | reproduces Rust test at lines 5793–5808                       |
| `Quad.fromDimensions` w/ `uvAtPivot=(1,1)` | reproduces Rust test at lines 5810–5829                       |
| `Quad.fromCorners`                         | produces correct normal from cross product                    |
| `Quad.fromThreeCorners(BottomRight)`       | recovers known cuboid face from 3 corners                     |
| `sphereMesh(4, 6)` face count              | body quads + pole triangles = expected total                  |
| `sphereMesh` vertex positions              | sampled points lie on the unit sphere                         |

---

## 4. Example — `examples/geometry3d_scene/`

A lit 3D scene that exercises Grid, Cuboid, and `sphereMesh` together.

### 4.1 Scene description

**Wave terrain** — a 24×24 `Grid[Vec3]` whose y-displacement follows
`sin(x · 0.45) · cos(y · 0.35) · 0.9`. Converted to a `Mesh[Vec3]` via
`Mesh(grid.quads)`. Rendered with vertex normals computed by
`ensureFaceNormals` + per-position averaging.

**Cuboid structures** — three boxes of varying proportions placed above the
terrain:

```scala
val tower    = Cuboid(Vec3(3.0,  1.5,  3.0),  1.0, 3.0, 1.0)
val platform = Cuboid(Vec3(8.0,  0.5,  4.5),  3.0, 1.0, 2.0)
val wall     = Cuboid(Vec3(4.5,  0.5, 10.0),  1.5, 1.0, 4.0)
```

Each uses `cuboid.faces(f)` with a position-only vertex, producing six
`Quad[Vec3]` faces that are batch-added to a `Mesh[Vec3]` via `addFaces`.

**Spheres** — two spheres at different scales:

```scala
val largeSphere = sphereMesh(14, 20)((pos, _) => pos * 0.9 + Vec3(6.0, 2.5, 6.0))
val smallSphere = sphereMesh(10, 14)((pos, _) => pos * 0.5 + Vec3(10.0, 1.5, 9.0))
```

### 4.2 Rendering

All geometry shares one `Shade` that takes `position: Vec4` and `normal: Vec3`
as vertex attributes. The fragment shader computes diffuse + ambient lighting
from a single directional light:

```wgsl
let light = normalize(vec3(1.0, 2.0, 1.0));
let diffuse = max(dot(normal, light), 0.0);
let color = vec3(0.75) * (0.15 + 0.85 * diffuse);
```

Vertex normals for the terrain are computed on the CPU by averaging adjacent
face normals at each shared position. Cuboid and sphere normals come directly
from the geometry generators.

The scene is static (no animation) so it serves as a clean integration smoke
test: every major API path — `Grid`, `Cuboid.faces`, `sphereMesh`, `addFaces`,
`Mesh(faces)`, `toBufferedGeometry` — is exercised end-to-end and visible in the
rendered output.

### 4.3 Vertex layout

```scala
type SceneAttribs = (position: Vec3, normal: Vec3)
```

Each sub-mesh (terrain, each cuboid, each sphere) is a separate `Form` / `Shape`
drawn in the same render pass.

---

## 5. Implementation order

1. ✅ **Grid** — `grid.scala` + `Grid.test.scala`. Pure CPU; no GPU. Done when §2.5
   passes.
2. ✅ **Shapes** — `shapes.scala` + `Shapes.test.scala`. Extend `object Quad`; add
   `Cuboid`; add `sphereMesh`. Done when §3.4 passes.
3. ✅ **Scene example** — `examples/geometry3d_scene/`. Verify visually with
   `bun run dev`. Grid wave surface, three cuboids, two spheres, diffuse
   lighting.

Phases 1 and 2 are independent at the source level.

---

## 6. Critical files

| File                                                                          | Action                                         |
| ----------------------------------------------------------------------------- | ---------------------------------------------- |
| [src/graphics/geometry/grid.scala](../src/graphics/geometry/grid.scala)       | **New**                                        |
| [src/graphics/geometry/shapes.scala](../src/graphics/geometry/shapes.scala)   | **New**                                        |
| [src/graphics/geometry/polygon.scala](../src/graphics/geometry/polygon.scala) | **Edit** — extend `object Quad` with factories |
| [test/geometry/Grid.test.scala](../test/geometry/Grid.test.scala)             | **New**                                        |
| [test/geometry/Shapes.test.scala](../test/geometry/Shapes.test.scala)         | **New**                                        |
| [examples/geometry3d_scene/](../examples/geometry3d_scene/)                   | **New**                                        |

### Existing files to reuse

- [src/graphics/geometry/polygon.scala](../src/graphics/geometry/polygon.scala)
  — `Quad[T]`, `Triangle[T]`, winding convention
- [src/graphics/geometry/mesh.scala](../src/graphics/geometry/mesh.scala) —
  `Mesh[T]`, `addFace`, `addFaces`, `Mesh.apply`
- [src/graphics/geometry/buffers.scala](../src/graphics/geometry/buffers.scala)
  — `toBufferedGeometry`
- [src/graphics/geometry/package.scala](../src/graphics/geometry/package.scala)
  — `Lerp` givens, `Position` typeclass
- [trivalibs/src/utils/numbers.scala](../trivalibs/src/utils/numbers.scala) —
  `NumExt`, `IntExt`
- [trivalibs/src/utils/js.scala](../trivalibs/src/utils/js.scala) — `Arr`, `Opt`

---

## 7. Verification

```bash
bun run build   # zero errors
bun run test    # all geometry tests pass
bun run dev     # open :3000/geometry3d_scene — wave terrain + boxes + spheres visible
```

**Status: ✅ All phases complete.** Build passes, tests pass, scene renders at
`/geometry3d_scene/`. Notable deviations from the plan:

- Indexed rendering was added as a prerequisite (`Form` now holds an index
  buffer; `GPURenderPassEncoder` gained `setIndexBuffer` / `drawIndexed`).
- `toBufferedGeometry` generates index buffers automatically when quads are
  present — no manual pre-triangulation needed in user code.
- `painter.panel` / `Panel.set` gained a `[S <: Shape[?, ?]]` type parameter so
  `Arr(s1, s2, ...)` can be passed without explicit type annotation.
- `type AnyShape = Shape[?, ?]` added to painter package for empty-array construction.
- Scene shader uses the Scala DSL (`program.vert` / `program.frag`) rather than
  raw WGSL strings.
