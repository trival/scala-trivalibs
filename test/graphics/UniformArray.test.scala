package trivalibs.graphics.buffers

import munit.FunSuite
import trivalibs.bufferdata.F32
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

  // Sub-16-byte elements are lane-packed into vec4 rows: N still counts
  // elements, and the declared row count is ceil(N / lanes).
  test("scalar elements pack four per vec4 row"):
    val t = WGSLType[UniformArray[Double, 8]]
    assertEquals(t.wgslName, "array<vec4<f32>, 2>")
    assertEquals(t.byteSize, 32)

  test("Vec2 elements pack two per vec4 row"):
    val t = WGSLType[UniformArray[Vec2, 4]]
    assertEquals(t.wgslName, "array<vec4<f32>, 2>")
    assertEquals(t.byteSize, 32)

  test("a capacity that is not a multiple of the lane count rounds up"):
    assertEquals(WGSLType[UniformArray[Double, 6]].wgslName, "array<vec4<f32>, 2>")
    assertEquals(WGSLType[UniformArray[Double, 6]].byteSize, 32)
    assertEquals(WGSLType[UniformArray[Double, 7]].byteSize, 32)
    assertEquals(WGSLType[UniformArray[Vec2, 5]].wgslName, "array<vec4<f32>, 3>")
    assertEquals(WGSLType[UniformArray[Vec2, 5]].byteSize, 48)

  test("element types without a CPU write path do not compile"):
    // Positive controls — otherwise the negatives below could pass vacuously.
    assert(
      typeChecks("summon[WGSLType[UniformArray[Vec4, 4]]]"),
      "Vec4 should be a legal uniform array element",
    )
    assert(
      typeChecks("summon[WGSLType[UniformArray[Double, 4]]]"),
      "Double should be a legal uniform array element",
    )
    assert(
      typeChecks("summon[WGSLType[UniformArray[Vec2, 4]]]"),
      "Vec2 should be a legal uniform array element",
    )
    assert(
      !typeChecks("summon[WGSLType[UniformArray[Int, 4]]]"),
      "Int has no UniformValue, so it cannot be an array element",
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

  test("a packed array declares over vec4 rows"):
    type Uniforms = (values: (curves: FragmentUniform[UniformArray[Double, 6]]))
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
      fragmentBody = "  out.color = vec4(curves[0][0]);",
    )
    val wgsl = shader.generateWGSL
    assert(
      wgsl.contains("var<uniform> curves: array<vec4<f32>, 2>;"),
      s"Missing packed array uniform declaration:\n$wgsl",
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

  // Packed elements address a row and then a part of it. Constant indices fold
  // in Scala; computed ones emit the division and remainder.
  test("scalar element, constant index, folds to row and component"):
    assertEquals(ArrayExpr[FloatExpr]("curves")(0).toString, "curves[0][0]")
    assertEquals(ArrayExpr[FloatExpr]("curves")(3).toString, "curves[0][3]")
    assertEquals(ArrayExpr[FloatExpr]("curves")(6).toString, "curves[1][2]")

  test("scalar element, computed index"):
    assertEquals(
      ArrayExpr[FloatExpr]("curves")(IntExpr("i")).toString,
      "curves[i / 4][i % 4]",
    )

  test("Vec2 element, constant index, picks the row half"):
    assertEquals(ArrayExpr[Vec2Expr]("uvs")(0).toString, "uvs[0].xy")
    assertEquals(ArrayExpr[Vec2Expr]("uvs")(1).toString, "uvs[0].zw")
    assertEquals(ArrayExpr[Vec2Expr]("uvs")(5).toString, "uvs[2].zw")

  test("Vec2 element, computed index, selects between the halves"):
    assertEquals(
      ArrayExpr[Vec2Expr]("uvs")(IntExpr("i")).toString,
      "select(uvs[i / 2].xy, uvs[i / 2].zw, (i % 2) == 1)",
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

  test("scalar rows round up to a whole vec4"):
    val uv = summon[UniformValue[UniformArray[Double, 6], F32 *: EmptyTuple]]
    assertEquals(uv.rowBytes, 4)
    // 6 doubles => 2 vec4 rows => 8 F32 rows of allocation
    assertEquals(uv.rows, 8)

  test("Vec2 rows round up to a whole vec4"):
    val uv = summon[UniformValue[UniformArray[Vec2, 5], Vec2Buffer]]
    assertEquals(uv.rowBytes, 8)
    // 5 vec2s => 3 vec4 rows => 6 Vec2Buffer rows of allocation
    assertEquals(uv.rows, 6)

  test("scalars are written densely, four to a row"):
    val rows = StructArray.allocate[F32 *: EmptyTuple](8)
    val uv = summon[UniformValue[UniformArray[Double, 8], F32 *: EmptyTuple]]
    uv.write(rows(0), UniformArray[Double, 8](Arr(1.0, 2.0, 3.0, 4.0, 5.0)))
    // element 4 is lane 0 of row 1 — byte 16
    assertEquals(rows(4).getAt(0).toDouble, 5.0)
    assertEquals(rows(3).getAt(0).toDouble, 4.0)
    assertEquals(rows(5).getAt(0).toDouble, 0.0)
    val back = uv.read(rows(0)).values
    assertEquals(back.length, 8)
    assertEquals(back(4), 5.0)

  test("Vec2s are written densely, two to a row"):
    val rows = StructArray.allocate[Vec2Buffer](4)
    val uv = summon[UniformValue[UniformArray[Vec2, 4], Vec2Buffer]]
    uv.write(rows(0), UniformArray[Vec2, 4](Arr(Vec2(1, 2), Vec2(3, 4))))
    // element 1 is the .zw half of row 0 — byte 8
    assertEquals(rows(1).x, 3.0)
    assertEquals(rows(1).y, 4.0)

  test("a ragged capacity still rejects an extra value"):
    val rows = StructArray.allocate[F32 *: EmptyTuple](8)
    val uv = summon[UniformValue[UniformArray[Double, 7], F32 *: EmptyTuple]]
    intercept[Exception]:
      uv.write(rows(0), UniformArray[Double, 7](Arr(1, 2, 3, 4, 5, 6, 7, 8)))

  // ---------------------------------------------------------------------------
  // Binding API — an Arr goes in directly
  // ---------------------------------------------------------------------------

  test("asUniform names only the capacity"):
    val values = Arr(Vec4(1, 2, 3, 4), Vec4(5, 6, 7, 8))
    val ua: UniformArray[Vec4, 4] = values.asUniform[4]
    assertEquals(ua.values.length, 2)

    // the same bytes as the explicit wrapper
    val a = StructArray.allocate[Vec4Buffer](4)
    val b = StructArray.allocate[Vec4Buffer](4)
    val uv = summon[UniformValue[Stops, Vec4Buffer]]
    uv.write(a(0), values.asUniform[4])
    uv.write(b(0), UniformArray[Vec4, 4](values))
    assertEquals(a(1).y, b(1).y)

  test("set and := both take a bare Arr on an array binding"):
    assert(
      typeChecks(
        "(??? : BufferBinding[UniformArray[Vec4, 8], Vec4Buffer]).set(Arr(Vec4(1)))",
      ),
      "set should take an Arr of the element type",
    )
    assert(
      typeChecks(
        "(??? : BufferBinding[UniformArray[Vec4, 8], Vec4Buffer]) := Arr(Vec4(1))",
      ),
      ":= should accept whatever set accepts",
    )
    assert(
      !typeChecks(
        "(??? : BufferBinding[UniformArray[Vec4, 8], Vec4Buffer]) := Arr(1.0)",
      ),
      ":= must not take an Arr of the wrong element type",
    )
    assert(
      !typeChecks("(??? : BufferBinding[Vec4, Vec4Buffer]) := Arr(Vec4(1))"),
      ":= must not take an Arr on a non-array binding",
    )

  test("an Arr of the wrong element type is not silently accepted"):
    assert(
      typeChecks("Arr(Vec4(1, 2, 3, 4)).asUniform[4]: UniformArray[Vec4, 4]"),
      "an Arr[Vec4] should view as a UniformArray[Vec4, 4]",
    )
    assert(
      !typeChecks("Arr(1.0).asUniform[4]: UniformArray[Vec4, 4]"),
      "an Arr[Double] should not view as a UniformArray[Vec4, 4]",
    )

  test("more values than the capacity throws"):
    val rows = StructArray.allocate[Vec4Buffer](2)
    val uv = summon[UniformValue[UniformArray[Vec4, 2], Vec4Buffer]]
    intercept[Exception]:
      uv.write(
        rows(0),
        UniformArray[Vec4, 2](Arr(Vec4(1), Vec4(2), Vec4(3))),
      )

  // ---------------------------------------------------------------------------
  // Values-sized buffers — what `panel.bind("name" := arr)` relies on
  // ---------------------------------------------------------------------------

  test("rows round up to a whole vec4 for any lane count"):
    assertEquals(UniformArray.rowsFor(3, 1), 3)
    assertEquals(UniformArray.rowsFor(3, 2), 4)
    assertEquals(UniformArray.rowsFor(3, 4), 4)
    assertEquals(UniformArray.rowsFor(5, 4), 8)
    assertEquals(UniformArray.rowsFor(8, 4), 8)

  test("a values-sized UniformValue allocates and writes for its own length"):
    val elem = summon[UniformValue[Vec4, Vec4Buffer]]
    val uv = UniformArray.valuesOnly[Vec4, Vec4Buffer](elem, 2)
    assertEquals(uv.rows, 2)
    assertEquals(uv.rowBytes, 16)

    val rows = StructArray.allocate[Vec4Buffer](2)
    uv.write(rows(0), Arr(Vec4(1, 2, 3, 4), Vec4(5, 6, 7, 8)))
    assertEquals(rows(1).y, 6.0)
    assertEquals(uv.read(rows(0)).length, 2)

  test("a values-sized buffer rejects a later, longer array"):
    val elem = summon[UniformValue[Vec4, Vec4Buffer]]
    val uv = UniformArray.valuesOnly[Vec4, Vec4Buffer](elem, 2)
    val rows = StructArray.allocate[Vec4Buffer](2)
    intercept[Exception]:
      uv.write(rows(0), Arr(Vec4(1), Vec4(2), Vec4(3)))

  // ---------------------------------------------------------------------------
  // Schema-driven adaptation — what `.bind("name" := arr)` relies on
  // ---------------------------------------------------------------------------

  type BindSchema = (
      stops: FragmentUniform[UniformArray[Vec4, 8]],
      count: FragmentUniform[Double],
  )

  test("the field-type check accepts an Arr of the element type, and only that"):
    assert(
      typeChecks(
        """derive.checkUniformFieldType["stops", Arr[Vec4], BindSchema]""",
      ),
      "an Arr[Vec4] should be bindable to an array<vec4> field",
    )
    assert(
      !typeChecks(
        """derive.checkUniformFieldType["stops", Arr[Double], BindSchema]""",
      ),
      "an Arr[Double] must not be bindable to an array<vec4> field",
    )
    assert(
      !typeChecks(
        """derive.checkUniformFieldType["count", Arr[Double], BindSchema]""",
      ),
      "an Arr must not be bindable to a scalar field",
    )
