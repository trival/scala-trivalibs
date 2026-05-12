package trivalibs.graphics.geometry

import munit.FunSuite
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.given
import trivalibs.bufferdata.{F32, StructRef}
import trivalibs.utils.js.*

class BuffersTest extends FunSuite:

  // Vertex layout: (x: F32, y: F32, z: F32)
  type XYZ = (F32, F32, F32)
  type XYZXYZ = (F32, F32, F32, F32, F32, F32)

  def writePos(v: Vec3, ref: StructRef[XYZ]): Unit =
    ref.setAt(0)(v.x)
    ref.setAt(1)(v.y)
    ref.setAt(2)(v.z)

  def writePosNorm(v: Vec3, n: Vec3, ref: StructRef[XYZXYZ]): Unit =
    ref.setAt(0)(v.x)
    ref.setAt(1)(v.y)
    ref.setAt(2)(v.z)
    ref.setAt(3)(n.x)
    ref.setAt(4)(n.y)
    ref.setAt(5)(n.z)

  // A simple unit-square mesh: 2 triangles sharing 4 vertices
  //   (0,1)----(1,1)
  //     |  \    |
  //     |   \   |
  //   (0,0)----(1,0)
  def squareMesh(): Mesh[Vec3] =
    val m = new Mesh[Vec3]()
    m.addFace(Triangle(Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(1, 1, 0)))
    m.addFace(Triangle(Vec3(0, 0, 0), Vec3(1, 1, 0), Vec3(0, 1, 0)))
    m

  def quadMesh(): Mesh[Vec3] =
    val m = new Mesh[Vec3]()
    m.addFace(Quad(Vec3(0, 1, 0), Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(1, 1, 0)))
    m

  // ---------------------------------------------------------------------------
  // FaceVertices
  // ---------------------------------------------------------------------------

  test("FaceVertices: pure triangles — no index buffer"):
    val m = squareMesh()
    val bg = buildFaceVertices(m, writePos)
    assertEquals(bg.vertices.length, 6) // 2 triangles × 3 verts
    assert(bg.indices == null)

  test("FaceVertices: quad mesh — emits index buffer"):
    val m = quadMesh()
    val bg = buildFaceVertices(m, writePos)
    assertEquals(bg.vertices.length, 4) // 1 quad = 4 verts
    assert(bg.indices != null)

  test("FaceVertices: quad index buffer has 6 entries (2 triangles)"):
    val m = quadMesh()
    val bg = buildFaceVertices(m, writePos)
    val indices = bg.indices
    assert(indices != null)
    val ui = indices.asInstanceOf[scala.scalajs.js.typedarray.Uint16Array]
    assertEquals(ui.length, 6)

  test("FaceVertices: mixed tri+quad index buffer"):
    val m = new Mesh[Vec3]()
    m.addFace(Triangle(Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(0, 1, 0)))
    m.addFace(Quad(Vec3(0, 1, 1), Vec3(0, 0, 1), Vec3(1, 0, 1), Vec3(1, 1, 1)))
    val bg = buildFaceVertices(m, writePos)
    assertEquals(bg.vertices.length, 7) // 3 + 4
    val indices = bg.indices
    assert(indices != null)
    val ui = indices.asInstanceOf[scala.scalajs.js.typedarray.Uint16Array]
    assertEquals(ui.length, 9) // 3 + 6

  // ---------------------------------------------------------------------------
  // FaceVerticesWithFaceNormal
  // ---------------------------------------------------------------------------

  test("FaceVerticesWithFaceNormal: vertex count"):
    val m = squareMesh()
    val bg = buildFaceVerticesWithFaceNormal(m, writePosNorm)
    assertEquals(bg.vertices.length, 6)

  test("FaceVerticesWithFaceNormal: normals point +Z"):
    val m = squareMesh()
    val bg = buildFaceVerticesWithFaceNormal(m, writePosNorm)
    // Normal z component is at slot index 5
    assertEqualsDouble(bg.vertices(0).getAt(5): Double, 1.0, 1e-5)

  // ---------------------------------------------------------------------------
  // CompactVertices
  // ---------------------------------------------------------------------------

  test("CompactVertices: deduplicates shared positions"):
    val m = squareMesh()
    val bg = buildCompactVertices(m, writePos)
    assertEquals(bg.vertices.length, 4) // 4 unique corners

  test("CompactVertices: always has index buffer"):
    val m = squareMesh()
    val bg = buildCompactVertices(m, writePos)
    assert(bg.indices != null)

  test("CompactVertices: 2 triangles → 6 indices"):
    val m = squareMesh()
    val bg = buildCompactVertices(m, writePos)
    val indices = bg.indices
    assert(indices != null)
    val ui = indices.asInstanceOf[scala.scalajs.js.typedarray.Uint16Array]
    assertEquals(ui.length, 6)

  test("CompactVertices: quad → 4 vertices, 6 indices"):
    val m = quadMesh()
    val bg = buildCompactVertices(m, writePos)
    assertEquals(bg.vertices.length, 4)
    val indices = bg.indices
    assert(indices != null)
    val ui = indices.asInstanceOf[scala.scalajs.js.typedarray.Uint16Array]
    assertEquals(ui.length, 6)

  // ---------------------------------------------------------------------------
  // toBufferedGeometry dispatch
  // ---------------------------------------------------------------------------

  test("toBufferedGeometry dispatches to FaceVertices"):
    val m = squareMesh()
    val bg = toBufferedGeometry(m, MeshBufferType.FaceVertices)
    assertEquals(bg.vertices.length, 6)

  test("toBufferedGeometry dispatches to CompactVertices"):
    val m = squareMesh()
    val bg = toBufferedGeometry(m, MeshBufferType.CompactVertices)
    assertEquals(bg.vertices.length, 4)
