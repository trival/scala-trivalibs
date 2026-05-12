package trivalibs.graphics.geometry

import munit.FunSuite
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.given
import trivalibs.utils.js.*

class MeshTest extends FunSuite:

  def makeMesh(): Mesh[Vec3] =
    val m = new Mesh[Vec3]()
    // Unit cube face (XY plane z=0): CCW from front
    m.addFace(Triangle(Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(0, 1, 0)))
    m.addFace(Triangle(Vec3(1, 0, 0), Vec3(1, 1, 0), Vec3(0, 1, 0)))
    m

  // ---------------------------------------------------------------------------
  // addFace / position registry
  // ---------------------------------------------------------------------------

  test("addFace registers correct face count"):
    val m = makeMesh()
    assertEquals(m.faces.length, 2)

  test("addFace registers unique positions"):
    val m = makeMesh()
    // 4 unique corners on a unit square
    assertEquals(m.positions.length, 4)

  test("addFace builds positionMap correctly"):
    val m   = makeMesh()
    val key = posKey(Vec3(1, 0, 0))
    assert(m.positionMap.contains(key))

  test("adjacentFaces returns faces sharing a vertex"):
    val m   = makeMesh()
    val adj = m.adjacentFaces(Vec3(1, 0, 0))
    assertEquals(adj.length, 2) // both triangles share (1,0,0)

  test("adjacentFaces at shared vertex returns both faces"):
    val m = makeMesh()
    // (0,1,0) is shared by both triangles
    val adj = m.adjacentFaces(Vec3(0, 1, 0))
    assertEquals(adj.length, 2)

  test("adjacentFaces at unknown vertex returns empty"):
    val m = makeMesh()
    assertEquals(m.adjacentFaces(Vec3(99, 99, 99)).length, 0)

  // ---------------------------------------------------------------------------
  // ensureFaceNormals
  // ---------------------------------------------------------------------------

  test("ensureFaceNormals fills missing normals"):
    val m = makeMesh()
    m.ensureFaceNormals()
    assert(m.faceData(0).normal != null)
    assert(m.faceData(1).normal != null)

  test("ensureFaceNormals returns false for pure triangle mesh"):
    val m = makeMesh()
    assert(!m.ensureFaceNormals())

  test("ensureFaceNormals returns true when quads present"):
    val m = new Mesh[Vec3]()
    m.addFace(Quad(Vec3(0, 1, 0), Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(1, 1, 0)))
    assert(m.ensureFaceNormals())

  test("ensureFaceNormals does not overwrite pre-supplied normal"):
    val m      = new Mesh[Vec3]()
    val custom = Vec3(0, 0, -1)
    m.addFace(Triangle(Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(0, 1, 0)), normal = custom)
    m.ensureFaceNormals()
    val n = m.faceData(0).normal
    assert(n != null)
    assertEqualsDouble(n.asInstanceOf[Vec3].z, -1.0, 1e-9)

  // ---------------------------------------------------------------------------
  // removeFace
  // ---------------------------------------------------------------------------

  test("removeFace reduces face count"):
    val m = makeMesh()
    m.removeFace(0)
    assertEquals(m.faces.length, 1)

  test("removeFace keeps position refs consistent"):
    val m = makeMesh()
    m.removeFace(0)
    // After swap-remove, the surviving face's refs should point to index 0
    val face = m.faces(0)
    val arr  = face.asInstanceOf[Arr[Vec3]]
    var ok   = true
    var i    = 0
    while i < arr.length do
      val key = posKey(arr(i).pos)
      if m.positionMap.contains(key) then
        val vp = m.positions(m.positionMap(key))
        // All refs in vp.faces should have faceIndex == 0 (the only remaining face)
        var j = 0
        while j < vp.faces.length do
          if vp.faces(j).faceIndex != 0 then ok = false
          j += 1
      i += 1
    assert(ok)

  // ---------------------------------------------------------------------------
  // map / flatMap
  // ---------------------------------------------------------------------------

  test("map transforms all faces"):
    val m   = makeMesh()
    val m2  = m.map(f => f) // identity
    assertEquals(m2.faces.length, 2)

  test("flatMap can split each face"):
    val m  = new Mesh[Vec3]()
    m.addFace(Quad(Vec3(0, 2, 0), Vec3(0, 0, 0), Vec3(2, 0, 0), Vec3(2, 2, 0)))
    val m2 = m.flatMap: face =>
      val q = face.asInstanceOf[Quad[Vec3]]
      val (t1, t2) = q.toTriangles
      Arr(t1, t2)
    assertEquals(m2.faces.length, 2)

  // ---------------------------------------------------------------------------
  // newFromSection
  // ---------------------------------------------------------------------------

  test("newFromSection filters by section"):
    val m = new Mesh[Vec3]()
    m.addFace(Triangle(Vec3(0, 0, 0), Vec3(1, 0, 0), Vec3(0, 1, 0)), section = 0)
    m.addFace(Triangle(Vec3(0, 0, 1), Vec3(1, 0, 1), Vec3(0, 1, 1)), section = 1)
    val s0 = m.newFromSection(0)
    val s1 = m.newFromSection(1)
    assertEquals(s0.faces.length, 1)
    assertEquals(s1.faces.length, 1)
