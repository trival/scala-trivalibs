package trivalibs.graphics.geometry

import munit.FunSuite
import trivalibs.graphics.math.cpu.{Vec2, Vec3}
import trivalibs.graphics.math.cpu.given
import trivalibs.utils.js.*

class ShapesTest extends FunSuite:

  private val eps = 1e-9

  // ---------------------------------------------------------------------------
  // Box corners
  // ---------------------------------------------------------------------------

  test("Box(center, 1, 1, 1) unit cube corners"):
    val c = Box(Vec3(0, 0, 0), 1.0, 1.0, 1.0)
    assert(c.frontTopLeft.approxEq(Vec3(-0.5, 0.5, 0.5)))
    assert(c.frontTopRight.approxEq(Vec3(0.5, 0.5, 0.5)))
    assert(c.frontBottomLeft.approxEq(Vec3(-0.5, -0.5, 0.5)))
    assert(c.frontBottomRight.approxEq(Vec3(0.5, -0.5, 0.5)))
    assert(c.backTopLeft.approxEq(Vec3(-0.5, 0.5, -0.5)))
    assert(c.backTopRight.approxEq(Vec3(0.5, 0.5, -0.5)))
    assert(c.backBottomLeft.approxEq(Vec3(-0.5, -0.5, -0.5)))
    assert(c.backBottomRight.approxEq(Vec3(0.5, -0.5, -0.5)))

  test("Box off-center corners"):
    val c = Box(Vec3(1, 2, 3), 2.0, 4.0, 6.0)
    assert(c.frontTopLeft.approxEq(Vec3(0, 4, 6)))
    assert(c.backBottomRight.approxEq(Vec3(2, 0, 0)))

  // ---------------------------------------------------------------------------
  // Face positions
  // ---------------------------------------------------------------------------

  test("frontFace positions match frontXxx corners"):
    val c = Box.unit
    val q = c.frontFace
    assert(q.tl.approxEq(c.frontTopLeft))
    assert(q.bl.approxEq(c.frontBottomLeft))
    assert(q.br.approxEq(c.frontBottomRight))
    assert(q.tr.approxEq(c.frontTopRight))

  test("backFace positions match backXxx corners"):
    val c = Box.unit
    val q = c.backFace
    // back face: tl=backTopRight, bl=backBottomRight, br=backBottomLeft, tr=backTopLeft
    assert(q.tl.approxEq(c.backTopRight))
    assert(q.bl.approxEq(c.backBottomRight))
    assert(q.br.approxEq(c.backBottomLeft))
    assert(q.tr.approxEq(c.backTopLeft))

  // ---------------------------------------------------------------------------
  // Face UVW
  // ---------------------------------------------------------------------------

  test("frontFace uvw.z == 0 for all vertices"):
    val c = Box.unit
    val q = c.frontFace[(Vec3, Vec3)]((pos, uvw) => (pos, uvw))
    var i = 0
    while i < 4 do
      assertEqualsDouble(q(i)._2.z, 0.0, eps)
      i += 1

  test("backFace uvw.z == 1 for all vertices"):
    val c = Box.unit
    val q = c.backFace[(Vec3, Vec3)]((pos, uvw) => (pos, uvw))
    var i = 0
    while i < 4 do
      assertEqualsDouble(q(i)._2.z, 1.0, eps)
      i += 1

  test("leftFace uvw.x == 0 for all vertices"):
    val c = Box.unit
    val q = c.leftFace[(Vec3, Vec3)]((pos, uvw) => (pos, uvw))
    var i = 0
    while i < 4 do
      assertEqualsDouble(q(i)._2.x, 0.0, eps)
      i += 1

  test("rightFace uvw.x == 1 for all vertices"):
    val c = Box.unit
    val q = c.rightFace[(Vec3, Vec3)]((pos, uvw) => (pos, uvw))
    var i = 0
    while i < 4 do
      assertEqualsDouble(q(i)._2.x, 1.0, eps)
      i += 1

  test("topFace uvw.y == 0 for all vertices"):
    val c = Box.unit
    val q = c.topFace[(Vec3, Vec3)]((pos, uvw) => (pos, uvw))
    var i = 0
    while i < 4 do
      assertEqualsDouble(q(i)._2.y, 0.0, eps)
      i += 1

  test("bottomFace uvw.y == 1 for all vertices"):
    val c = Box.unit
    val q = c.bottomFace[(Vec3, Vec3)]((pos, uvw) => (pos, uvw))
    var i = 0
    while i < 4 do
      assertEqualsDouble(q(i)._2.y, 1.0, eps)
      i += 1

  // ---------------------------------------------------------------------------
  // faces() normals
  // ---------------------------------------------------------------------------

  test("faces returns 6 faces with correct normals"):
    val c = Box.unit
    val expected = Arr(
      Vec3(0, 0, 1),
      Vec3(0, 0, -1),
      Vec3(-1, 0, 0),
      Vec3(1, 0, 0),
      Vec3(0, 1, 0),
      Vec3(0, -1, 0),
    )
    val fs = c.faces
    assertEquals(fs.length, 6)
    var i = 0
    while i < 6 do
      assert(fs(i)._2.approxEq(expected(i)), s"face $i normal mismatch")
      i += 1

  // ---------------------------------------------------------------------------
  // Quad.fromDimensions
  // ---------------------------------------------------------------------------

  test("fromDimensionsCenter normal=(0,0,1) center=(0,3,0) width=4 height=2"):
    // Mirrors Rust test at lines 5793-5808
    val q =
      Quad.fromDimensionsCenter[Vec3](4.0, 2.0, Vec3(0, 0, 1), Vec3(0, 3, 0)):
        (pos, _) => pos
    assert(q.tl.approxEq(Vec3(-2, 4, 0)))
    assert(q.bl.approxEq(Vec3(-2, 2, 0)))
    assert(q.br.approxEq(Vec3(2, 2, 0)))
    assert(q.tr.approxEq(Vec3(2, 4, 0)))
    // UVs — access via index (no Position needed for Vec2)
    val quv = Quad.fromDimensionsCenter[(Vec3, Vec2)](
      4.0,
      2.0,
      Vec3(0, 0, 1),
      Vec3(0, 3, 0),
    ): (pos, uv) =>
      (pos, uv)
    assertEqualsDouble(quv(0)._2.x, 0.0, eps) // tl uv = (0,0)
    assertEqualsDouble(quv(0)._2.y, 0.0, eps)
    assertEqualsDouble(quv(2)._2.x, 1.0, eps) // br uv = (1,1)
    assertEqualsDouble(quv(2)._2.y, 1.0, eps)

  test("fromDimensions with uvAtPivot=(1,1) bottom-right pivot"):
    // Mirrors Rust test at lines 5810-5829
    // normal=(0,3.3,0) ≈ (0,1,0), pivot=(0,0,0), uvAtPivot=(1,1), w=h=1
    val q = Quad.fromDimensions[Vec3](
      1.0,
      1.0,
      Vec3(0, 3.3, 0),
      Vec3(0, 0, 0),
      Vec2(1.0, 1.0),
    ): (pos, _) =>
      pos
    assert(q.tl.approxEq(Vec3(-1, 0, -1)))
    assert(q.bl.approxEq(Vec3(-1, 0, 0)))
    assert(q.br.approxEq(Vec3(0, 0, 0)))
    assert(q.tr.approxEq(Vec3(0, 0, -1)))

  // ---------------------------------------------------------------------------
  // Quad.fromCorners
  // ---------------------------------------------------------------------------

  test("fromCorners produces correct normal via Quad.normal"):
    val q = Quad.fromCorners[Vec3](
      Vec3(0, 1, 0),
      Vec3(0, 0, 0),
      Vec3(1, 0, 0),
      Vec3(1, 1, 0),
    )((pos, _) => pos)
    val n = q.normal
    assertEqualsDouble(n.x, 0.0, eps)
    assertEqualsDouble(n.y, 0.0, eps)
    assertEqualsDouble(n.z, 1.0, eps)

  // ---------------------------------------------------------------------------
  // Quad.fromThreeCorners
  // ---------------------------------------------------------------------------

  test("fromThreeCorners(BottomRight) recovers box front face"):
    val c = Box.unit
    val tl = c.frontTopLeft
    val bl = c.frontBottomLeft
    val tr = c.frontTopRight
    val q = Quad.fromThreeCorners[Vec3](tl, bl, tr, QuadCorner.BottomRight)(
      (pos, _) => pos,
    )
    assert(q.br.approxEq(c.frontBottomRight))

  test("fromThreeCorners(TopLeft) recovers missing tl"):
    // a=bl, b=br, c=tr
    val bl = Vec3(0, 0, 0)
    val br = Vec3(1, 0, 0)
    val tr = Vec3(1, 1, 0)
    val expected = Vec3(0, 1, 0) // tl = bl + tr - br
    val q = Quad.fromThreeCorners[Vec3](bl, br, tr, QuadCorner.TopLeft)(
      (pos, _) => pos,
    )
    assert(q.tl.approxEq(expected))

  test("fromThreeCorners(TopRight) recovers missing tr"):
    // a=tl, b=bl, c=br
    val tl = Vec3(0, 1, 0)
    val bl = Vec3(0, 0, 0)
    val br = Vec3(1, 0, 0)
    val expected = Vec3(1, 1, 0) // tr = tl + br - bl
    val q = Quad.fromThreeCorners[Vec3](tl, bl, br, QuadCorner.TopRight)(
      (pos, _) => pos,
    )
    assert(q.tr.approxEq(expected))

  // ---------------------------------------------------------------------------
  // sphereMesh
  // ---------------------------------------------------------------------------

  test("sphereMesh(4, 6) face count"):
    val m = sphereMesh(4, 6)((pos, _) => pos)
    // body quads = (V-1-1)*(H) = (4-3)*6 = 6, pole triangles = 2*H = 12, total = 18
    assertEquals(m.faces.length, 18)

  test("sphereMesh vertex positions lie on unit sphere"):
    val m = sphereMesh(6, 8)((pos, _) => pos)
    var allOnSphere = true
    var i = 0
    while i < m.faces.length do
      val arr = m.faces(i).asInstanceOf[Arr[Vec3]]
      var s = 0
      while s < arr.length do
        val len = arr(s).length
        if math.abs(len - 1.0) > 1e-6 then allOnSphere = false
        s += 1
      i += 1
    assert(allOnSphere)
