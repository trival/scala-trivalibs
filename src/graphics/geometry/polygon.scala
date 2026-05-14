package trivalibs.graphics.geometry

import trivalibs.graphics.math.*
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.given
import trivalibs.utils.js.*

// Winding convention for all polygons: CCW viewed from front.

opaque type Triangle[T] <: Arr[T] = Arr[T]

object Triangle:
  def apply[T](a: T, b: T, c: T): Triangle[T] = Arr(a, b, c)

  extension [T: Position](tri: Triangle[T])
    def a: T = tri(0)
    def b: T = tri(1)
    def c: T = tri(2)

    def normal: Vec3 =
      val pa = tri.a.pos
      val pb = tri.b.pos
      val pc = tri.c.pos
      val e1 = Vec3(pb.x - pa.x, pb.y - pa.y, pb.z - pa.z)
      val e2 = Vec3(pc.x - pa.x, pc.y - pa.y, pc.z - pa.z)
      e1.cross(e2).normalize

  extension [T: {Position, Lerp}](tri: Triangle[T])
    // Returns 0-2 triangles (clipped away, or split into 1 or 2 triangles).
    def splitByPlane(
        plane: Plane,
    ): Arr[Triangle[T]] =
      val out = clipPolygon(tri, 3, plane)
      fanTriangulate(out)

// Quad winding: tl(0), bl(1), br(2), tr(3) — CCW viewed from front.
opaque type Quad[T] <: Arr[T] = Arr[T]

opaque type QuadCorner = Int

object QuadCorner:
  val TopLeft: QuadCorner = 0
  val BottomLeft: QuadCorner = 1
  val BottomRight: QuadCorner = 2
  val TopRight: QuadCorner = 3

object Quad:
  def apply[T](tl: T, bl: T, br: T, tr: T): Quad[T] = Arr(tl, bl, br, tr)

  // Dimensioned construction.  uvAtPivot: (0,0)=top-left, (0.5,0.5)=center, (1,1)=bottom-right.
  def fromDimensions[T](
      width: Double,
      height: Double,
      normal: Vec3,
      pivot: Vec3,
      uvAtPivot: Vec2 = Vec2(0.5, 0.5),
  )(f: (Vec3, Vec2) => T): Quad[T] =
    val n = normal.normalize
    val up = if math.abs(n.y) > 0.999 then -Vec3.Z else Vec3.Y
    val uDir = up.cross(n)
    val uVec = uDir * width
    val vVec = -(n.cross(uDir)) * height
    val tlPos = pivot - uVec * uvAtPivot.x - vVec * uvAtPivot.y
    val trPos = tlPos + uVec
    val blPos = tlPos + vVec
    val brPos = blPos + uVec
    Quad(
      f(tlPos, Vec2(0.0, 0.0)),
      f(blPos, Vec2(0.0, 1.0)),
      f(brPos, Vec2(1.0, 1.0)),
      f(trPos, Vec2(1.0, 0.0)),
    )

  inline def fromDimensionsCenter[T](
      w: Double,
      h: Double,
      n: Vec3,
      center: Vec3,
  )(f: (Vec3, Vec2) => T): Quad[T] =
    fromDimensions(w, h, n, center, Vec2(0.5, 0.5))(f)

  inline def fromDimensionsTopLeft[T](
      w: Double,
      h: Double,
      n: Vec3,
      tl: Vec3,
  )(f: (Vec3, Vec2) => T): Quad[T] =
    fromDimensions(w, h, n, tl, Vec2(0.0, 0.0))(f)

  // From 4 explicit corner positions.  Normal is readable via Quad.normal on the result.
  def fromCorners[T](
      tl: Vec3,
      bl: Vec3,
      br: Vec3,
      tr: Vec3,
  )(f: (Vec3, Vec2) => T): Quad[T] =
    Quad(
      f(tl, Vec2(0.0, 0.0)),
      f(bl, Vec2(0.0, 1.0)),
      f(br, Vec2(1.0, 1.0)),
      f(tr, Vec2(1.0, 0.0)),
    )

  // Infer missing 4th corner from 3 adjacent corners (parallelogram rule).
  // a, b, c are the three known corners in the order tl(0) bl(1) br(2) tr(3),
  // skipping the missing slot.
  def fromThreeCorners[T](
      a: Vec3,
      b: Vec3,
      c: Vec3,
      missing: QuadCorner,
  )(f: (Vec3, Vec2) => T): Quad[T] =
    import QuadCorner.*
    val (tl, bl, br, tr) = missing match
      case TopLeft     => (a + c - b, a, b, c) // a=bl b=br c=tr
      case BottomLeft  => (a, a + b - c, b, c) // a=tl b=br c=tr
      case BottomRight => (a, b, b + c - a, c) // a=tl b=bl c=tr
      case _           => (a, b, c, a + c - b) // TopRight: a=tl b=bl c=br
    fromCorners(tl, bl, br, tr)(f)

  extension [T: Position](q: Quad[T])
    def tl: T = q(0)
    def bl: T = q(1)
    def br: T = q(2)
    def tr: T = q(3)

    // Both-diagonals cross product handles non-planar quads.
    def normal: Vec3 =
      val a = q.tl.pos
      val b = q.bl.pos
      val c = q.br.pos
      val d = q.tr.pos
      val d1 = Vec3(c.x - a.x, c.y - a.y, c.z - a.z) // tl→br
      val d2 = Vec3(d.x - b.x, d.y - b.y, d.z - b.z) // bl→tr
      d1.cross(d2).normalize

    def toTriangles: (Triangle[T], Triangle[T]) =
      (Triangle(q.tl, q.bl, q.br), Triangle(q.tl, q.br, q.tr))

  extension [T: {Position, Lerp}](q: Quad[T])
    // Horizontal split: bottom half and top half.
    def subdivideH: (Quad[T], Quad[T]) =
      val ml = q.tl.lerp(q.bl, 0.5)
      val mr = q.tr.lerp(q.br, 0.5)
      (Quad(ml, q.bl, q.br, mr), Quad(q.tl, ml, mr, q.tr))

    // Vertical split: left half and right half.
    def subdivideV: (Quad[T], Quad[T]) =
      val mb = q.bl.lerp(q.br, 0.5)
      val mt = q.tl.lerp(q.tr, 0.5)
      (Quad(q.tl, q.bl, mb, mt), Quad(mt, mb, q.br, q.tr))

    // Returns 0, 1 Triangle, 1 Quad, or 1 Quad + 1 Triangle (5-vert clip output).
    def splitByPlane(
        plane: Plane,
    ): Arr[Triangle[T] | Quad[T]] =
      val out = clipPolygon(q, 4, plane)
      val n = out.length
      if n == 0 then Arr()
      else if n == 3 then Arr(Triangle(out(0), out(1), out(2)))
      else if n == 4 then Arr(Quad(out(0), out(1), out(2), out(3)))
      else
        // n == 5: fan into Quad(0..3) + Triangle(0, 3, 4)
        Arr(
          Quad(out(0), out(1), out(2), out(3)),
          Triangle(out(0), out(3), out(4)),
        )

// ---------------------------------------------------------------------------
// Shared Sutherland-Hodgman clipping core
// ---------------------------------------------------------------------------

private def clipPolygon[T: {Position, Lerp}](
    verts: Arr[T],
    n: Int,
    plane: Plane,
): Arr[T] =
  val out = Arr[T]()
  var i = 0
  while i < n do
    val curr = verts(i)
    val next = verts((i + 1) % n)
    val currIn = !curr.pos.behind(plane)
    val nextIn = !next.pos.behind(plane)
    if currIn then out.push(curr)
    if currIn != nextIn then
      val dc = plane.signedDist(curr.pos)
      val dn = plane.signedDist(next.pos)
      val t = dc / (dc - dn)
      out.push(curr.lerp(next, t))
    i += 1
  out

// Fan-triangulate a convex polygon (3+ verts) — returns triangles.
private def fanTriangulate[T](out: Arr[T]): Arr[Triangle[T]] =
  val n = out.length
  if n < 3 then Arr()
  else
    val tris = Arr[Triangle[T]]()
    var i = 1
    while i < n - 1 do
      tris.push(Triangle(out(0), out(i), out(i + 1)))
      i += 1
    tris
