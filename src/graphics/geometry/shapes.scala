package trivalibs.graphics.geometry

import trivalibs.graphics.math.*
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.given
import trivalibs.utils.js.*

// ---------------------------------------------------------------------------
// Box
// ---------------------------------------------------------------------------

/** An axis-aligned box, defined by its eight corners and convenience accessors
  * for each face as a [[Quad]].
  *
  * The templated face accessors (e.g. `frontFace[T](f)`) call `f(corner, uvw)`
  * for each corner. `uvw` is a normalized box-local coordinate in `[0,1]^3`,
  * where `(0,0,0)` is the front-top-left corner and `(1,1,1)` is the
  * back-bottom-right corner (x = left→right, y = top→bottom, z = front→back).
  */
class Box(
    val center: Vec3,
    val size: Vec3,
    val frontTopLeft: Vec3,
    val frontTopRight: Vec3,
    val frontBottomLeft: Vec3,
    val frontBottomRight: Vec3,
    val backTopLeft: Vec3,
    val backTopRight: Vec3,
    val backBottomLeft: Vec3,
    val backBottomRight: Vec3,
):
  def frontFace: Quad[Vec3] =
    Quad(frontTopLeft, frontBottomLeft, frontBottomRight, frontTopRight)

  /** Front face. `f` is called as `f(corner, uvw)` per corner, where `uvw` is
    * the normalized box-local coordinate in `[0,1]^3` (front-top-left =
    * `(0,0,0)`, back-bottom-right = `(1,1,1)`). For this face `uvw.z = 0`.
    */
  def frontFace[T](f: (Vec3, Vec3) => T): Quad[T] =
    Quad(
      f(frontTopLeft, Vec3(0, 0, 0)),
      f(frontBottomLeft, Vec3(0, 1, 0)),
      f(frontBottomRight, Vec3(1, 1, 0)),
      f(frontTopRight, Vec3(1, 0, 0)),
    )

  def backFace: Quad[Vec3] =
    Quad(backTopRight, backBottomRight, backBottomLeft, backTopLeft)

  /** Back face. `f` is called as `f(corner, uvw)` per corner, where `uvw` is
    * the normalized box-local coordinate in `[0,1]^3` (front-top-left =
    * `(0,0,0)`, back-bottom-right = `(1,1,1)`). For this face `uvw.z = 1`.
    */
  def backFace[T](f: (Vec3, Vec3) => T): Quad[T] =
    Quad(
      f(backTopRight, Vec3(1, 0, 1)),
      f(backBottomRight, Vec3(1, 1, 1)),
      f(backBottomLeft, Vec3(0, 1, 1)),
      f(backTopLeft, Vec3(0, 0, 1)),
    )

  def leftFace: Quad[Vec3] =
    Quad(backTopLeft, backBottomLeft, frontBottomLeft, frontTopLeft)

  /** Left face. `f` is called as `f(corner, uvw)` per corner, where `uvw` is
    * the normalized box-local coordinate in `[0,1]^3` (front-top-left =
    * `(0,0,0)`, back-bottom-right = `(1,1,1)`). For this face `uvw.x = 0`.
    */
  def leftFace[T](f: (Vec3, Vec3) => T): Quad[T] =
    Quad(
      f(backTopLeft, Vec3(0, 0, 1)),
      f(backBottomLeft, Vec3(0, 1, 1)),
      f(frontBottomLeft, Vec3(0, 1, 0)),
      f(frontTopLeft, Vec3(0, 0, 0)),
    )

  def rightFace: Quad[Vec3] =
    Quad(frontTopRight, frontBottomRight, backBottomRight, backTopRight)

  /** Right face. `f` is called as `f(corner, uvw)` per corner, where `uvw` is
    * the normalized box-local coordinate in `[0,1]^3` (front-top-left =
    * `(0,0,0)`, back-bottom-right = `(1,1,1)`). For this face `uvw.x = 1`.
    */
  def rightFace[T](f: (Vec3, Vec3) => T): Quad[T] =
    Quad(
      f(frontTopRight, Vec3(1, 0, 0)),
      f(frontBottomRight, Vec3(1, 1, 0)),
      f(backBottomRight, Vec3(1, 1, 1)),
      f(backTopRight, Vec3(1, 0, 1)),
    )

  def topFace: Quad[Vec3] =
    Quad(backTopLeft, frontTopLeft, frontTopRight, backTopRight)

  /** Top face. `f` is called as `f(corner, uvw)` per corner, where `uvw` is
    * the normalized box-local coordinate in `[0,1]^3` (front-top-left =
    * `(0,0,0)`, back-bottom-right = `(1,1,1)`). For this face `uvw.y = 0`.
    */
  def topFace[T](f: (Vec3, Vec3) => T): Quad[T] =
    Quad(
      f(backTopLeft, Vec3(0, 0, 1)),
      f(frontTopLeft, Vec3(0, 0, 0)),
      f(frontTopRight, Vec3(1, 0, 0)),
      f(backTopRight, Vec3(1, 0, 1)),
    )

  def bottomFace: Quad[Vec3] =
    Quad(frontBottomLeft, backBottomLeft, backBottomRight, frontBottomRight)

  /** Bottom face. `f` is called as `f(corner, uvw)` per corner, where `uvw` is
    * the normalized box-local coordinate in `[0,1]^3` (front-top-left =
    * `(0,0,0)`, back-bottom-right = `(1,1,1)`). For this face `uvw.y = 1`.
    */
  def bottomFace[T](f: (Vec3, Vec3) => T): Quad[T] =
    Quad(
      f(frontBottomLeft, Vec3(0, 1, 0)),
      f(backBottomLeft, Vec3(0, 1, 1)),
      f(backBottomRight, Vec3(1, 1, 1)),
      f(frontBottomRight, Vec3(1, 1, 0)),
    )

  /** All six faces paired with their outward unit normal (axis-aligned). */
  def faces: Arr[(Quad[Vec3], Vec3)] = Arr(
    (frontFace, Vec3(0, 0, 1)),
    (backFace, Vec3(0, 0, -1)),
    (leftFace, Vec3(-1, 0, 0)),
    (rightFace, Vec3(1, 0, 0)),
    (topFace, Vec3(0, 1, 0)),
    (bottomFace, Vec3(0, -1, 0)),
  )

  /** All six faces paired with their outward unit normal (axis-aligned). `f` is
    * applied to each (corner, uv) to build the per-vertex value.
    */
  def faces[T](f: (Vec3, Vec3) => T): Arr[(Quad[T], Vec3)] = Arr(
    (frontFace(f), Vec3(0, 0, 1)),
    (backFace(f), Vec3(0, 0, -1)),
    (leftFace(f), Vec3(-1, 0, 0)),
    (rightFace(f), Vec3(1, 0, 0)),
    (topFace(f), Vec3(0, 1, 0)),
    (bottomFace(f), Vec3(0, -1, 0)),
  )

object Box:
  def apply(
      center: Vec3,
      width: Double,
      height: Double,
      depth: Double,
  ): Box =
    val hw = width / 2
    val hh = height / 2
    val hd = depth / 2
    val cx = center.x
    val cy = center.y
    val cz = center.z
    new Box(
      center = center,
      size = Vec3(width, height, depth),
      frontTopLeft = Vec3(cx - hw, cy + hh, cz + hd),
      frontTopRight = Vec3(cx + hw, cy + hh, cz + hd),
      frontBottomLeft = Vec3(cx - hw, cy - hh, cz + hd),
      frontBottomRight = Vec3(cx + hw, cy - hh, cz + hd),
      backTopLeft = Vec3(cx - hw, cy + hh, cz - hd),
      backTopRight = Vec3(cx + hw, cy + hh, cz - hd),
      backBottomLeft = Vec3(cx - hw, cy - hh, cz - hd),
      backBottomRight = Vec3(cx + hw, cy - hh, cz - hd),
    )

  def unit: Box = apply(Vec3.zero, 1.0, 1.0, 1.0)

// ---------------------------------------------------------------------------
// Sphere mesh
// ---------------------------------------------------------------------------

/** Build a UV-sphere [[Mesh]] over the unit sphere with the given segment
  * counts. `f(pos, uv)` builds each vertex: `pos` is on the unit sphere
  * (scale/translate inside `f`), `uv.x` ∈ [0,1] is longitude and `uv.y` ∈ [0,1]
  * goes south-pole → north-pole. Feed the result to [[toBufferedGeometry]].
  */
def sphereMesh[T: Position](
    verticalSegments: Int,
    horizontalSegments: Int,
)(f: (Vec3, Vec2) => T): Mesh[T] =
  val PI = Math.PI
  val TAU = 2 * PI

  def mkVert(u: Double, v: Double): T =
    val cv = Math.cos(v)
    val pos = Vec3(cv * Math.cos(u), Math.sin(v), cv * Math.sin(u))
    val uv = Vec2(u / TAU, (v + PI / 2) / PI)
    f(pos, uv)

  val allFaces = Arr[Face[T]]()
  var lastCol = Arr[T]()

  var j = 0
  while j <= horizontalSegments do
    val u = TAU * (j.toDouble / horizontalSegments)
    val southPole = mkVert(u, -PI / 2)
    val northPole = mkVert(u, PI / 2)

    val col = Arr[T]()
    var i = 1
    while i < verticalSegments - 1 do
      val v = PI * (i.toDouble / verticalSegments) - PI / 2
      col.push(mkVert(u, v))
      i += 1

    if lastCol.length > 0 then
      var k = 0
      while k < col.length - 1 do
        allFaces.push(Quad(col(k + 1), col(k), lastCol(k), lastCol(k + 1)))
        k += 1
      allFaces.push(Triangle(southPole, lastCol(0), col(0)))
      allFaces.push(
        Triangle(col(col.length - 1), lastCol(lastCol.length - 1), northPole),
      )

    lastCol = col
    j += 1

  Mesh(allFaces)
