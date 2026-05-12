package trivalibs.graphics.geometry

import munit.FunSuite
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.given

class PlaneTest extends FunSuite:

  val up    = Vec3(0, 1, 0)
  val plane = Plane(up, 0.0) // y = 0 plane, normal pointing up

  test("signedDist positive above plane"):
    assertEqualsDouble(plane.signedDist(Vec3(0, 1, 0)), 1.0, 1e-9)

  test("signedDist negative below plane"):
    assertEqualsDouble(plane.signedDist(Vec3(0, -1, 0)), -1.0, 1e-9)

  test("signedDist zero on plane"):
    assertEqualsDouble(plane.signedDist(Vec3(5, 0, 3)), 0.0, 1e-9)

  test("inFront above plane"):
    assert(Vec3(0, 1, 0).inFront(plane))
    assert(!Vec3(0, -1, 0).inFront(plane))

  test("behind below plane"):
    assert(Vec3(0, -1, 0).behind(plane))
    assert(!Vec3(0, 1, 0).behind(plane))

  test("onPlane very close to plane"):
    assert(Vec3(0, 0.00005, 0).onPlane(plane))
    assert(!Vec3(0, 0.001, 0).onPlane(plane))

  test("approxEq within tolerance"):
    val a = Vec3(1.0, 2.0, 3.0)
    val b = Vec3(1.00005, 2.00005, 3.00005)
    assert(a.approxEq(b))

  test("approxEq outside tolerance"):
    val a = Vec3(1.0, 2.0, 3.0)
    val b = Vec3(1.0002, 2.0, 3.0)
    assert(!a.approxEq(b))

  test("approxEq same point"):
    val a = Vec3(1.0, -2.5, 0.0)
    assert(a.approxEq(a))
