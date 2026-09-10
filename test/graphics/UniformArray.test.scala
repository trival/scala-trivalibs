package trivalibs.graphics.buffers

import munit.FunSuite
import trivalibs.bufferdata.StructArray
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.Expr.*
import trivalibs.graphics.math.gpu.given
import trivalibs.graphics.shader.*
import trivalibs.graphics.shader.given
import trivalibs.utils.js.Arr

import scala.compiletime.testing.typeChecks

class UniformArrayTest extends FunSuite:

  type Stops = UniformArray[Vec4, 4]

  // ---------------------------------------------------------------------------
  // WGSL emission
  // ---------------------------------------------------------------------------

  test("WGSLType emits an array of the element's WGSL type"):
    val t = WGSLType[Stops]
    assertEquals(t.wgslName, "array<vec4<f32>, 4>")
    assertEquals(t.byteSize, 64)

  test("Vec3 elements keep the 16-byte uniform stride"):
    assertEquals(WGSLType[UniformArray[Vec3, 3]].byteSize, 48)

  test("Mat3 elements pad to a 48-byte stride"):
    assertEquals(WGSLType[UniformArray[Mat3, 2]].byteSize, 96)

  // WGSL would reject these at shade creation; UniformArrayElem turns that into
  // a compile error at the declaration instead.
  test("element types with a sub-16-byte stride do not compile"):
    // Positive control — otherwise the negatives below could pass vacuously.
    assert(
      typeChecks("summon[WGSLType[UniformArray[Vec4, 4]]]"),
      "Vec4 should be a legal uniform array element",
    )
    assert(
      !typeChecks("summon[WGSLType[UniformArray[Float, 4]]]"),
      "Float should not be a legal uniform array element",
    )
    assert(
      !typeChecks("summon[WGSLType[UniformArray[Vec2, 4]]]"),
      "Vec2 should not be a legal uniform array element",
    )

  test("a uniform array declares as a var<uniform> of array type"):
    type Uniforms = (values: (stops: FragmentUniform[Stops], count: Double))
    val shader = Shader.full[
      (position: Vec2),
      None,
      Uniforms,
      VertIn,
      VertOut,
      FragIn,
      FragOut,
    ](
      vertexBody = "  out.position = vec4(in.position, 0.0, 1.0);",
      fragmentBody = "  out.color = stops[0];",
    )
    val wgsl = shader.generateWGSL
    assert(
      wgsl.contains("var<uniform> stops: array<vec4<f32>, 4>;"),
      s"Missing array uniform declaration:\n$wgsl",
    )

  // ---------------------------------------------------------------------------
  // DSL indexing
  // ---------------------------------------------------------------------------

  test("constant index"):
    assertEquals(ArrayExpr[Vec4Expr]("stops")(2).toString, "stops[2]")

  test("computed index"):
    assertEquals(
      ArrayExpr[Vec4Expr]("stops")(IntExpr("i")).toString,
      "stops[i]",
    )

  // ---------------------------------------------------------------------------
  // CPU-side buffer layout
  // ---------------------------------------------------------------------------

  test("UniformValue reports N rows so the binding allocates the whole array"):
    val uv = summon[UniformValue[Stops, Vec4Buffer]]
    assertEquals(uv.rows, 4)
    assertEquals(uv.rowBytes, 16)

  test("single values still report one row"):
    assertEquals(summon[UniformValue[Vec4, Vec4Buffer]].rows, 1)

  test("elements are written one row apart and read back"):
    val rows = StructArray.allocate[Vec4Buffer](4)
    val uv = summon[UniformValue[Stops, Vec4Buffer]]
    uv.write(
      rows(0),
      UniformArray[Vec4, 4](Arr(Vec4(1, 2, 3, 4), Vec4(5, 6, 7, 8))),
    )
    // Written through the array…
    assertEquals(rows(0).x, 1.0)
    assertEquals(rows(1).y, 6.0)
    // …untouched rows stay zero, which is what a `count` uniform masks off.
    assertEquals(rows(2).x, 0.0)

    val back = uv.read(rows(0)).values
    assertEquals(back.length, 4)
    assertEquals(
      (back(1).x, back(1).y, back(1).z, back(1).w),
      (5.0, 6.0, 7.0, 8.0),
    )
    assertEquals(
      (back(3).x, back(3).y, back(3).z, back(3).w),
      (0.0, 0.0, 0.0, 0.0),
    )

  test("more values than the capacity throws"):
    val rows = StructArray.allocate[Vec4Buffer](2)
    val uv = summon[UniformValue[UniformArray[Vec4, 2], Vec4Buffer]]
    intercept[Exception]:
      uv.write(
        rows(0),
        UniformArray[Vec4, 2](Arr(Vec4(1), Vec4(2), Vec4(3))),
      )
