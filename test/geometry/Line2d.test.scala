package trivalibs.graphics.geometry

import munit.FunSuite
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.given
import trivalibs.utils.js.*

import scala.scalajs.js.typedarray.Uint16Array

class Line2dTest extends FunSuite:

  private def assertVec2(actual: Vec2, x: Double, y: Double): Unit =
    assertEqualsDouble(actual.x, x, 1e-9)
    assertEqualsDouble(actual.y, y, 1e-9)

  // -------------------------------------------------------------------------
  // LineVertex
  // -------------------------------------------------------------------------

  test("LineVertex.pointTo sets len and dir"):
    val vert = LineVertex(Vec2.zero, 1.0)

    vert.pointTo(Vec2(2.0, 0.0))
    assertEqualsDouble(vert.len, 2.0, 1e-9)
    assertVec2(vert.dir, 1.0, 0.0)

    vert.pointTo(Vec2(0.0, 3.0))
    assertEqualsDouble(vert.len, 3.0, 1e-9)
    assertVec2(vert.dir, 0.0, 1.0)

    vert.pointTo(Vec2(0.0, -4.0))
    assertEqualsDouble(vert.len, 4.0, 1e-9)
    assertVec2(vert.dir, 0.0, -1.0)

  // -------------------------------------------------------------------------
  // Line building
  // -------------------------------------------------------------------------

  test("Line accumulates totalLength while adding"):
    val line = Line(10.0)
    line.add(Vec2(0.0, 0.0))
    line.add(Vec2(2.0, 0.0))
    line.add(Vec2(2.0, 1.0))
    line.add(Vec2(2.0, 3.0))

    assertEqualsDouble(line.totalLength, 5.0, 1e-9)
    assertEquals(line.vertCount, 4)
    assertVec2(line.last.dir, 0.0, 1.0)
    assertEqualsDouble(line.last.len, 0.0, 1e-9)

  test("Line.fromPoints"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(2.0, 0.0), Vec2(2.0, 1.0), Vec2(2.0, 3.0)),
    )

    assertEqualsDouble(line.totalLength, 5.0, 1e-9)
    assertEquals(line.vertCount, 4)
    assertEqualsDouble(line.first.width, 2.0, 1e-9)

  test("Line.getOpt is null out of bounds"):
    val line = Line.fromPoints(1.0, Arr(Vec2(0.0, 0.0), Vec2(1.0, 0.0)))
    assert(line.getOpt(0).notNull)
    assert(line.getOpt(1).notNull)
    assert(line.getOpt(2).isNull)
    assert(line.getOpt(-1).isNull)

  // -------------------------------------------------------------------------
  // cleanup
  // -------------------------------------------------------------------------

  test("cleanup drops collinear same-width vertices"):
    val line = Line.fromPoints(
      2.0,
      Arr(
        Vec2(0.0, 0.0),
        Vec2(1.0, 0.0),
        Vec2(2.0, 0.0),
        Vec2(3.0, 0.0),
      ),
    )

    val cleaned = line.cleanup(0.5, 0.001, 0.001)
    assertEquals(cleaned.vertCount, 2)
    assertVec2(cleaned.get(0).pos, 0.0, 0.0)
    assertVec2(cleaned.get(1).pos, 3.0, 0.0)

  test("cleanup min-length thresholds"):
    val zigzag = Line.fromPoints(
      10.0,
      Arr(
        Vec2(0.0, 0.0),
        Vec2(1.0, 0.0),
        Vec2(2.0, 1.0),
        Vec2(3.0, 0.0),
        Vec2(4.0, 1.0),
        Vec2(5.0, 0.0),
      ),
    )

    assertEquals(zigzag.cleanup(1.0, 0.001, 0.001).vertCount, 2)
    assertEquals(zigzag.cleanup(0.5, 0.001, 0.001).vertCount, 3)
    assertEquals(zigzag.cleanup(0.2, 0.001, 0.001).vertCount, 5)
    assertEquals(zigzag.cleanup(0.1, 0.001, 0.001).vertCount, 6)

  test("cleanup leaves the source line untouched"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(1.0, 0.0), Vec2(2.0, 0.0), Vec2(3.0, 0.0)),
    )
    line.cleanup(0.5, 0.001, 0.001)
    assertEquals(line.vertCount, 4)
    assertEqualsDouble(line.totalLength, 3.0, 1e-9)

  // -------------------------------------------------------------------------
  // smoothEdges
  // -------------------------------------------------------------------------

  test("smoothEdges bevels a corner into two vertices"):
    val line = Line.fromPoints(
      1.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(10.0, 10.0)),
    )
    // the middle vertex turns by 90°, the two ends are always kept as-is
    val smoothed = line.smoothEdges(0.25, 1.0, 0.001)
    assertEquals(smoothed.vertCount, 4)
    assertVec2(smoothed.get(1).pos, 7.5, 0.0)
    assertVec2(smoothed.get(2).pos, 10.0, 2.5)

  test("smoothEdges leaves a straight line alone"):
    val line = Line.fromPoints(
      1.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(20.0, 0.0)),
    )
    assertEquals(line.smoothEdges(0.25, 1.0, 0.001).vertCount, 3)

  test("smoothEdges skips corners on segments below minDist"):
    val line = Line.fromPoints(
      1.0,
      Arr(Vec2(0.0, 0.0), Vec2(1.0, 0.0), Vec2(1.0, 1.0)),
    )
    assertEquals(line.smoothEdges(0.25, 5.0, 0.001).vertCount, 3)

  // -------------------------------------------------------------------------
  // splitAtAngle
  // -------------------------------------------------------------------------

  test("splitAtAngle splits at a sharp corner"):
    // a hairpin: right, then straight back left
    val line = Line.fromPoints(
      1.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(0.0, 1.0)),
    )
    val fragments = line.splitAtAngle(math.Pi * 3.0 / 4.0)

    assertEquals(fragments.length, 2)
    assertEquals(fragments(0).vertCount, 2)
    assertEquals(fragments(1).vertCount, 2)
    // the corner vertex is duplicated into both fragments
    assertVec2(fragments(0).last.pos, 10.0, 0.0)
    assertVec2(fragments(1).first.pos, 10.0, 0.0)
    // the ending fragment points back along its incoming segment
    assertVec2(fragments(0).last.dir, 1.0, 0.0)

  test("splitAtAngle keeps a gentle line in one piece"):
    val line = Line.fromPoints(
      1.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(20.0, 1.0)),
    )
    val fragments = line.splitAtAngle(math.Pi * 3.0 / 4.0)
    assertEquals(fragments.length, 1)
    assertEquals(fragments(0).vertCount, 3)

  test("splitAtAngle threads lenOffset through the fragments"):
    // two hairpins, 10 units apart each
    val line = Line.fromPoints(
      1.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(0.0, 0.0), Vec2(10.0, 0.0)),
    )
    val fragments = line.splitAtAngle(math.Pi * 3.0 / 4.0)
    assertEquals(fragments.length, 3)
    assertEqualsDouble(fragments(0).lenOffset, 0.0, 1e-9)
    assertEqualsDouble(fragments(1).lenOffset, 10.0, 1e-9)
    assertEqualsDouble(fragments(2).lenOffset, 20.0, 1e-9)

  // -------------------------------------------------------------------------
  // toBufferedGeometry
  // -------------------------------------------------------------------------

  // Field offsets in LineAttribsBuffer, in bytes:
  // position 0..8, width 8, length 12, uv 16..24, localUv 24..32
  private inline val Stride = 32

  private def readF(g: BufferedGeometry[LineAttribsBuffer], i: Int, off: Int) =
    g.vertices.dataView.getFloat32(i * Stride + off, true).toDouble

  test("toBufferedGeometry emits 2 verts per line vertex plus two caps"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(20.0, 0.0)),
    )
    val geom = line.toBufferedGeometry()

    // top and bottom outline each get one vertex per input vertex plus a
    // duplicated cap vertex at each end => 2 * (3 + 2) = 10
    assertEquals(geom.vertices.length, 10)
    assert(geom.indices.notNull)
    // the strip walks both outlines in lockstep: 2 indices per iteration
    assertEquals(geom.indices.asInstanceOf[Uint16Array].length, 10)

  test("toBufferedGeometry uv: caps at v=0.5, sides at 0 / 1, u spans 0..1"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(20.0, 0.0)),
    )
    val geom = line.toBufferedGeometry()

    // vertex 0 is the top start cap, vertex 1 the bottom start cap
    assertEqualsDouble(readF(geom, 0, 20), 0.5, 1e-6)
    assertEqualsDouble(readF(geom, 1, 20), 0.5, 1e-6)
    // vertex 2 / 3 are the first real outline pair
    assertEqualsDouble(readF(geom, 2, 20), 0.0, 1e-6)
    assertEqualsDouble(readF(geom, 3, 20), 1.0, 1e-6)
    // u runs 0 at the start, 1 at the end
    assertEqualsDouble(readF(geom, 0, 16), 0.0, 1e-6)
    assertEqualsDouble(readF(geom, 9, 16), 1.0, 1e-6)
    // localUv.x matches uv.x when the line is the whole stroke
    assertEqualsDouble(readF(geom, 9, 24), 1.0, 1e-6)

  test("toBufferedGeometry swapTextureOrientation flips uv.y"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(20.0, 0.0)),
    )
    val geom = line.toBufferedGeometry(swapTextureOrientation = true)
    assertEqualsDouble(readF(geom, 2, 20), 1.0, 1e-6)
    assertEqualsDouble(readF(geom, 3, 20), 0.0, 1e-6)

  test("toBufferedGeometry writes position and width"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0)),
    )
    val geom = line.toBufferedGeometry()
    // start cap sits on the centre line
    assertEqualsDouble(readF(geom, 0, 0), 0.0, 1e-6)
    assertEqualsDouble(readF(geom, 0, 4), 0.0, 1e-6)
    assertEqualsDouble(readF(geom, 0, 8), 2.0, 1e-6)
    // the outline pair is offset by ±width along the normal (0, -1)
    assertEqualsDouble(readF(geom, 2, 4), -2.0, 1e-6)
    assertEqualsDouble(readF(geom, 3, 4), 2.0, 1e-6)

  test("toBufferedGeometries threads directions and a shared total length"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(0.0, 0.0), Vec2(10.0, 0.0)),
    )
    val fragments = line.splitAtAngle(math.Pi * 3.0 / 4.0)
    assertEquals(fragments.length, 3)

    val geoms = fragments.toBufferedGeometries()
    assertEquals(geoms.length, 3)

    // uv.x is continuous across fragments: the first starts at 0, the last
    // ends at 1 — total stroke length is 30
    assertEqualsDouble(readF(geoms(0), 0, 16), 0.0, 1e-6)
    val last = geoms(2)
    assertEqualsDouble(readF(last, last.vertices.length - 1, 16), 1.0, 1e-6)
    // fragment 1 starts a third of the way in
    assertEqualsDouble(readF(geoms(1), 0, 16), 10.0 / 30.0, 1e-6)
    // localUv.x still runs 0..1 within each fragment
    assertEqualsDouble(readF(geoms(1), 0, 24), 0.0, 1e-6)

  test("toBufferedGeometries alternates swapTextureOrientation"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(0.0, 0.0), Vec2(10.0, 0.0)),
    )
    val geoms = line.splitAtAngle(math.Pi * 3.0 / 4.0).toBufferedGeometries()
    // vertex 2 is the first non-cap top vertex of each fragment
    assertEqualsDouble(readF(geoms(0), 2, 20), 0.0, 1e-6)
    assertEqualsDouble(readF(geoms(1), 2, 20), 1.0, 1e-6)
    assertEqualsDouble(readF(geoms(2), 2, 20), 0.0, 1e-6)

  test("toBufferedGeometry with smoothDepth adds outline vertices"):
    val line = Line.fromPoints(
      2.0,
      Arr(Vec2(0.0, 0.0), Vec2(10.0, 0.0), Vec2(10.0, 10.0)),
    )
    val plain = line.toBufferedGeometry()
    val smoothed = line.toBufferedGeometry(
      smoothDepth = 2,
      smoothAngleThreshold = 0.001,
      smoothMinLength = 1.0,
    )
    assert(smoothed.vertices.length > plain.vertices.length)
