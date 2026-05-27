package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.shader.{FragOut, FragmentUniform, VertexUniform}
import munit.FunSuite

class ShaderDslTest extends FunSuite:

  // =========================================================================
  // Expression types — verify WGSL string output
  // =========================================================================

  test("FloatExpr arithmetic produces correct WGSL"):
    val a = FloatExpr("a")
    val b = FloatExpr("b")
    assertEquals((a + b).toString, "(a + b)")
    assertEquals((a - b).toString, "(a - b)")
    assertEquals((a * b).toString, "(a * b)")
    assertEquals((a / b).toString, "(a / b)")

  test("FloatExpr from Double/Int literal conversions"):
    val f: FloatExpr = 3.14
    assertEquals(f.toString, "3.14")
    val i: FloatExpr = 42
    assertEquals(i.toString, "f32(42)")

  test("Vec2Expr arithmetic produces native WGSL"):
    val a = Vec2Expr("a")
    val b = Vec2Expr("b")
    assertEquals((a + b).toString, "(a + b)")
    assertEquals((a - b).toString, "(a - b)")
    assertEquals((a * b).toString, "(a * b)")
    assertEquals((a / b).toString, "(a / b)")

  test("Vec2Expr scalar arithmetic"):
    val v = Vec2Expr("v")
    val s = FloatExpr("s")
    assertEquals((v + s).toString, "(v + s)")
    assertEquals((v * s).toString, "(v * s)")

  test("Vec3Expr arithmetic produces native WGSL"):
    val a = Vec3Expr("a")
    val b = Vec3Expr("b")
    assertEquals((a + b).toString, "(a + b)")
    assertEquals((a - b).toString, "(a - b)")
    assertEquals((a * b).toString, "(a * b)")
    assertEquals((a / b).toString, "(a / b)")

  test("Vec4Expr arithmetic produces native WGSL"):
    val a = Vec4Expr("a")
    val b = Vec4Expr("b")
    assertEquals((a + b).toString, "(a + b)")
    assertEquals((a - b).toString, "(a - b)")

  test("Mat2Expr * Vec2Expr produces native WGSL"):
    val m = Mat2Expr("m")
    val v = Vec2Expr("v")
    assertEquals((m * v).toString, "(m * v)")

  test("Mat3Expr * Vec3Expr produces native WGSL"):
    val m = Mat3Expr("m")
    val v = Vec3Expr("v")
    assertEquals((m * v).toString, "(m * v)")

  test("Mat4Expr * Vec4Expr produces native WGSL"):
    val m = Mat4Expr("m")
    val v = Vec4Expr("v")
    assertEquals((m * v).toString, "(m * v)")

  test("vec2/vec3/vec4 constructors produce correct WGSL"):
    val x = FloatExpr("x")
    val y = FloatExpr("y")
    val z = FloatExpr("z")
    val w = FloatExpr("w")
    assertEquals(vec2(x, y).toString, "vec2<f32>(x, y)")
    assertEquals(vec3(x, y, z).toString, "vec3<f32>(x, y, z)")
    assertEquals(vec4(x, y, z, w).toString, "vec4<f32>(x, y, z, w)")

  test("vec4 overloads with vec2/vec3 args"):
    val xy = Vec2Expr("xy")
    val xyz = Vec3Expr("xyz")
    val s = FloatExpr("s")
    assertEquals(vec4(xy, s, s).toString, "vec4<f32>(xy, s, s)")
    assertEquals(vec4(xyz, s).toString, "vec4<f32>(xyz, s)")

  // =========================================================================
  // Stmt and Block
  // =========================================================================

  test("Stmt.assign produces assignment"):
    val s = Stmt.assign("out.position", Vec4Expr("v"))
    assertEquals(s.toString, "  out.position = v;")

  test("Stmt.let produces let declaration"):
    val s = Stmt.let("rotated", Vec2Expr("expr"))
    assertEquals(s.toString, "  let rotated = expr;")

  test("Block joins statements with newlines"):
    val b = Block(
      Stmt.let("a", FloatExpr("1.0")),
      Stmt.assign("out.x", FloatExpr("a")),
    )
    assertEquals(
      Block.unwrap(b),
      "  let a = 1.0;\n  out.x = a;",
    )

  // =========================================================================
  // AssignTarget — ctx.out.:= produces assignment statements
  // =========================================================================

  test("AssignTarget := produces assignment Stmt"):
    val target = AssignTarget("out.position")
    val stmt = target := Vec4Expr("v")
    assertEquals(stmt.toString, "  out.position = v;")

  // =========================================================================
  // LetExpr — := produces let statements, can be used in expressions
  // =========================================================================

  test("LetExpr := produces let Stmt"):
    val local = LetExpr("rotated")
    val stmt = local := Vec2Expr("expr")
    assertEquals(stmt.toString, "  let rotated = expr;")

  test("LetVec2 supports both := and arithmetic"):
    val local = LetVec2("rotated")
    val other = Vec2Expr("translation")

    // Assignment
    val assignStmt = local := Vec2Expr("value")
    assertEquals(assignStmt.toString, "  let rotated = value;")

    // Arithmetic — should produce native WGSL, not string concatenation
    val addResult = local + other
    assertEquals(addResult.toString, "(rotated + translation)")

  // =========================================================================
  // TypedExprAccessor — ctx.in / ctx.bindings
  // =========================================================================

  test("TypedExprAccessor with prefix generates qualified names"):
    type Fields = (position: Vec2Expr, color: Vec4Expr)
    val accessor = TypedExprAccessor[Fields]("in")

    val pos: Vec2Expr = accessor.position
    val col: Vec4Expr = accessor.color
    assertEquals(pos.toString, "in.position")
    assertEquals(col.toString, "in.color")

  test("TypedExprAccessor without prefix generates bare names"):
    type Fields = (rotation: Mat2Expr, translation: Vec2Expr)
    val accessor = TypedExprAccessor[Fields]("")

    val rot: Mat2Expr = accessor.rotation
    val trans: Vec2Expr = accessor.translation
    assertEquals(rot.toString, "rotation")
    assertEquals(trans.toString, "translation")

  test("TypedExprAccessor values can be used in expressions"):
    type Fields = (position: Vec2Expr, offset: Vec2Expr)
    val accessor = TypedExprAccessor[Fields]("in")

    val result = accessor.position + accessor.offset
    assertEquals(result.toString, "(in.position + in.offset)")

  // =========================================================================
  // TypedAssignAccessor — ctx.out
  // =========================================================================

  test("TypedAssignAccessor := produces assignment statements"):
    type Fields = (position: AssignTarget, color: AssignTarget)
    val accessor = TypedAssignAccessor[Fields]("out")

    val stmt = accessor.position := Vec4Expr("v")
    assertEquals(stmt.toString, "  out.position = v;")

  test("TypedAssignAccessor color assignment"):
    type Fields = (color: AssignTarget)
    val accessor = TypedAssignAccessor[Fields]("out")

    val stmt = accessor.color := vec4(
      FloatExpr("r"),
      FloatExpr("g"),
      FloatExpr("b"),
      1.0,
    )
    assertEquals(stmt.toString, "  out.color = vec4<f32>(r, g, b, 1.0);")

  // =========================================================================
  // TypedLocalAccessor — ctx.locals
  // =========================================================================

  test("TypedLocalAccessor returns typed local with := and math ops"):
    import scala.NamedTuple.Map as NTMap

    type Locals = (rotated: Vec2)
    type LocalFields = NTMap[Locals, ToLocal]
    val accessor = TypedLocalAccessor[LocalFields]()

    val rotated: LetVec2 = accessor.rotated

    // := produces let statement
    val letStmt = rotated := Vec2Expr("value")
    assertEquals(letStmt.toString, "  let rotated = value;")

    // Can be used in arithmetic
    val sum = rotated + Vec2Expr("offset")
    assertEquals(sum.toString, "(rotated + offset)")

    // Scalar multiplication
    val scaled = rotated * FloatExpr("2.0")
    assertEquals(scaled.toString, "(rotated * 2.0)")
    // Vec multiplication
    val mulVec = rotated * Vec2Expr("other")
    assertEquals(mulVec.toString, "(rotated * other)")
    // Implicit conversion
    val scaledI = rotated * (2.0: FloatExpr)
    assertEquals(scaledI.toString, "(rotated * 2.0)")

  // =========================================================================
  // Full Program builder — end-to-end integration
  // =========================================================================

  test("Program.vert generates correct vertex body"):
    type Attribs = (position: Vec2)
    type Varyings = EmptyTuple
    type Uniforms = (
        rotation: VertexUniform[Mat2],
        translation: VertexUniform[Vec2],
    )

    val program = Program[Attribs, Varyings, Uniforms, EmptyTuple, FragOut]()

    program.vert[(rotated: Vec2)]: ctx =>
      Block(
        ctx.locals.rotated := ctx.bindings.rotation * ctx.in.position,
        ctx.out.position := vec4(
          ctx.locals.rotated + ctx.bindings.translation,
          0.0,
          1.0,
        ),
      )

    val body = program.vertBodyStr
    assert(
      body.contains("let rotated = (rotation * in.position);"),
      s"Missing let rotated statement:\n$body",
    )
    assert(
      body.contains(
        "out.position = vec4<f32>((rotated + translation), 0.0, 1.0);",
      ),
      s"Missing out.position assignment:\n$body",
    )

  test("Program.frag generates correct fragment body"):
    type Attribs = (position: Vec2)
    type Varyings = EmptyTuple
    type Uniforms = (color: FragmentUniform[Vec3])

    val program = Program[Attribs, Varyings, Uniforms, EmptyTuple, FragOut]()

    program.frag[EmptyTuple]: ctx =>
      Block(
        ctx.out.color := vec4(ctx.bindings.color, 1.0),
      )

    val body = program.fragBodyStr
    assert(
      body.contains("out.color = vec4<f32>(color, 1.0);"),
      s"Missing out.color assignment:\n$body",
    )

  test("Program with locals can alias and reuse local variables"):
    type Attribs = (position: Vec2, normal: Vec3)
    type Varyings = EmptyTuple
    type Uniforms = (scale: VertexUniform[Float])

    val program = Program[Attribs, Varyings, Uniforms, EmptyTuple, FragOut]()

    program.vert[(scaled: Vec2)]: ctx =>
      val scaled = ctx.locals.scaled
      Block(
        scaled := ctx.in.position * ctx.bindings.scale,
        ctx.out.position := vec4(scaled, 0.0, 1.0),
      )

    val body = program.vertBodyStr
    assert(
      body.contains("let scaled = (in.position * scale);"),
      s"Missing let scaled statement:\n$body",
    )
    assert(
      body.contains("out.position = vec4<f32>(scaled, 0.0, 1.0);"),
      s"Missing out.position with scaled:\n$body",
    )

  test("Program with multiple locals"):
    type Attribs = (position: Vec2)
    type Varyings = EmptyTuple
    type Uniforms = (offset: VertexUniform[Vec2])

    val program = Program[Attribs, Varyings, Uniforms, EmptyTuple, FragOut]()

    program.vert[(moved: Vec2, final_pos: Vec2)]: ctx =>
      val moved: LetVec2 = ctx.locals.moved
      val scaled = moved * 2.0
      Block(
        ctx.locals.moved := ctx.in.position + ctx.bindings.offset,
        ctx.locals.final_pos := scaled,
        ctx.out.position := vec4(ctx.locals.final_pos, 0.0, 1.0),
      )

    val body = program.vertBodyStr
    assert(
      body.contains("let moved = (in.position + offset);"),
      s"Missing let moved:\n$body",
    )
    assert(
      body.contains("let final_pos = (moved * 2.0);"),
      s"Missing let final_pos:\n$body",
    )
    assert(
      body.contains("out.position = vec4<f32>(final_pos, 0.0, 1.0);"),
      s"Missing out.position:\n$body",
    )

  test("Program frag with EmptyTuple locals compiles"):
    type Attribs = (position: Vec2)
    type Varyings = (color: Vec4)
    type Uniforms = EmptyTuple

    val program = Program[Attribs, Varyings, Uniforms, EmptyTuple, FragOut]()

    program.frag[EmptyTuple]: ctx =>
      Block(
        ctx.out.color := ctx.in.color,
      )

    val body = program.fragBodyStr
    assert(
      body.contains("out.color = in.color;"),
      s"Missing passthrough:\n$body",
    )

  test("Full vert + frag program"):
    type Attribs = (position: Vec2)
    type Varyings = EmptyTuple
    type Uniforms = (
        color: FragmentUniform[Vec3],
        rotation: VertexUniform[Mat2],
        translation: VertexUniform[Vec2],
    )

    val program = Program[Attribs, Varyings, Uniforms, EmptyTuple, FragOut]()

    program.vert[(rotated: Vec2)]: ctx =>
      Block(
        ctx.locals.rotated := ctx.bindings.rotation * ctx.in.position,
        ctx.out.position := vec4(
          ctx.locals.rotated + ctx.bindings.translation,
          0.0,
          1.0,
        ),
      )

    program.frag[EmptyTuple]: ctx =>
      Block(
        ctx.out.color := vec4(ctx.bindings.color, 1.0),
      )

    val vertBody = program.vertBodyStr
    val fragBody = program.fragBodyStr

    // Verify vertex body
    assert(
      vertBody.contains("let rotated = (rotation * in.position);"),
      s"Vert missing rotated:\n$vertBody",
    )
    assert(
      vertBody.contains(
        "out.position = vec4<f32>((rotated + translation), 0.0, 1.0);",
      ),
      s"Vert missing position:\n$vertBody",
    )

    // Verify fragment body
    assert(
      fragBody.contains("out.color = vec4<f32>(color, 1.0);"),
      s"Frag missing color:\n$fragBody",
    )

  // =========================================================================
  // VarExpr — stateful first-:= becomes var decl, subsequent become assign
  // =========================================================================

  test("VarExpr first := generates var decl"):
    val v = new VarExpr("acc")
    assertEquals(
      (v := Vec2Expr("vec2<f32>(0.0, 0.0)")).toString,
      "  var acc = vec2<f32>(0.0, 0.0);",
    )

  test("VarExpr subsequent := generates reassignment"):
    val v = new VarExpr("acc")
    v := Vec2Expr("init") // first
    val reassign = v := Vec2Expr("next")
    assertEquals(reassign.toString, "  acc = next;")

  test("ConstExpr := generates const decl"):
    val c = new ConstExpr("scale")
    assertEquals(
      (c := FloatExpr("2.0")).toString,
      "  const scale = 2.0;",
    )

  test("VarVec2 supports arithmetic and stateful :="):
    val acc = VarVec2("acc")
    val delta = Vec2Expr("delta")
    // First := → var decl
    assertEquals(
      (acc := Vec2Expr("vec2<f32>(0.0, 0.0)")).toString,
      "  var acc = vec2<f32>(0.0, 0.0);",
    )
    // Math works
    assertEquals((acc + delta).toString, "(acc + delta)")
    // Second := → reassign
    assertEquals((acc := (acc + delta)).toString, "  acc = (acc + delta);")

  test("VarExpr compound assignment emits native WGSL compound ops"):
    val acc = VarVec3("col")
    val delta = Vec3Expr("delta")
    assertEquals((acc += delta).toString, "  col += delta;")
    assertEquals((acc -= delta).toString, "  col -= delta;")
    assertEquals((acc *= delta).toString, "  col *= delta;")
    assertEquals((acc /= delta).toString, "  col /= delta;")

  test("ConstFloat supports arithmetic and const decl"):
    val s = ConstFloat("scale")
    assertEquals(
      (s := FloatExpr("2.0")).toString,
      "  const scale = 2.0;",
    )
    assertEquals((s * FloatExpr("x")).toString, "(scale * x)")

  // =========================================================================
  // TypedLocalAccessor with kinds dict — dispatches var/const/let
  // =========================================================================

  test("TypedLocalAccessor dispatches var/const/let from kinds dict"):
    import trivalibs.utils.js.Dict
    import scala.NamedTuple.Map as NTMap

    type Locals = (acc: Var[Vec2], scale: Const[Float], tmp: Vec2)
    type LocalFields = NTMap[Locals, ToLocal]

    val kinds = Dict[String]()
    kinds("acc") = "v"
    kinds("scale") = "c"
    val accessor = TypedLocalAccessor[LocalFields](kinds)

    val acc: VarVec2 = accessor.acc
    val scale: ConstFloat = accessor.scale
    val tmp: LetVec2 = accessor.tmp

    assertEquals((acc := Vec2Expr("v")).toString, "  var acc = v;")
    acc := Vec2Expr("v2") // mark as declared
    assertEquals((acc := Vec2Expr("v2")).toString, "  acc = v2;")
    assertEquals((scale := FloatExpr("2.0")).toString, "  const scale = 2.0;")
    assertEquals((tmp := Vec2Expr("t")).toString, "  let tmp = t;")

  // =========================================================================
  // Full program integration — mixed Var, Const, and bare locals
  // =========================================================================

  test("Program.vert with Var and Const locals generates correct WGSL"):
    type Attribs = (position: Vec2)
    type Varyings = EmptyTuple
    type Uniforms = (delta: VertexUniform[Vec2])

    val program = Program[Attribs, Varyings, Uniforms, EmptyTuple, FragOut]()

    program.vert[(acc: Var[Vec2], scale: Const[Float], tmp: Vec2)]: ctx =>
      Block(
        ctx.locals.scale := 2.0,
        ctx.locals.acc := ctx.in.position,
        ctx.locals.tmp := ctx.locals.acc + ctx.bindings.delta,
        ctx.locals.acc := ctx.locals.tmp,
        ctx.out.position := vec4(ctx.locals.acc, 0.0, 1.0),
      )

    val body = program.vertBodyStr
    assert(body.contains("const scale = 2.0;"), s"Missing const:\n$body")
    assert(body.contains("var acc = in.position;"), s"Missing var decl:\n$body")
    assert(body.contains("let tmp = (acc + delta);"), s"Missing let:\n$body")
    assert(body.contains("acc = tmp;"), s"Missing reassign:\n$body")
    assert(
      body.contains("out.position = vec4<f32>(acc, 0.0, 1.0);"),
      s"Missing position:\n$body",
    )

  // =========================================================================
  // Scala helper function with VarExpr — inlines without WgslFn
  // =========================================================================

  test("Scala helper function with VarExpr inlines into shader"):
    def accumulate(acc: VarVec2, value: Vec2Expr): Stmt =
      acc := acc + value

    type Attribs = (position: Vec2)
    type Varyings = EmptyTuple
    type Uniforms = (delta: VertexUniform[Vec2])

    val program = Program[Attribs, Varyings, Uniforms, EmptyTuple, FragOut]()

    program.vert[(acc: Var[Vec2])]: ctx =>
      Block(
        ctx.locals.acc := ctx.in.position,
        accumulate(ctx.locals.acc, ctx.bindings.delta),
        ctx.out.position := vec4(ctx.locals.acc, 0.0, 1.0),
      )

    val body = program.vertBodyStr
    assert(body.contains("var acc = in.position;"), s"Missing var decl:\n$body")
    assert(body.contains("acc = (acc + delta);"), s"Missing accumulate:\n$body")
    assert(
      body.contains("out.position = vec4<f32>(acc, 0.0, 1.0);"),
      s"Missing position:\n$body",
    )
