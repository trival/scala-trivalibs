package trivalibs.graphics.shader.dsl

import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.shader.{FragOut, FragmentUniform, VertexUniform, given}
import munit.FunSuite

class WgslFnTest extends FunSuite:

  // =========================================================================
  // WgslFn.raw — source generation
  // =========================================================================

  test("WgslFn.raw with 1 param generates correct WGSL source"):
    val fn: WgslFn[Float *: EmptyTuple, Float] =
      WgslFn.raw("double_it")("  return x * 2.0;")
    val data = fn.asInstanceOf[WgslFnData]
    assertEquals(data.name, "double_it")
    assert(
      data.src.contains("fn double_it("),
      s"Missing fn declaration:\n${data.src}",
    )
    assert(
      data.src.contains("-> f32"),
      s"Missing return type:\n${data.src}",
    )
    assert(
      data.src.contains("return x * 2.0;"),
      s"Missing body:\n${data.src}",
    )

  test("WgslFn.raw generates correct param list for named tuple"):
    val fn: WgslFn[(v: Vec2, angle: Float), Vec2] =
      WgslFn.raw("rotate")("  return v;")
    val data = fn.asInstanceOf[WgslFnData]
    assert(
      data.src.contains("v: vec2<f32>"),
      s"Missing v param:\n${data.src}",
    )
    assert(
      data.src.contains("angle: f32"),
      s"Missing angle param:\n${data.src}",
    )
    assert(
      data.src.contains("-> vec2<f32>"),
      s"Missing return type:\n${data.src}",
    )

  test("WgslFn.raw with 3 params"):
    val fn: WgslFn[(pos: Vec2, mat: Mat2, offset: Vec2), Vec2] =
      WgslFn.raw("apply_transform")("  return mat * pos + offset;")
    val data = fn.asInstanceOf[WgslFnData]
    assert(data.src.contains("pos: vec2<f32>"), data.src)
    assert(data.src.contains("mat: mat2x2<f32>"), data.src)
    assert(data.src.contains("offset: vec2<f32>"), data.src)

  test("WgslFn.raw with Unit return type generates void"):
    val fn: WgslFn[Float *: EmptyTuple, Unit] =
      WgslFn.raw("no_op")("  // nothing")
    val data = fn.asInstanceOf[WgslFnData]
    assert(data.src.contains("-> void"), s"Missing void return:\n${data.src}")

  // =========================================================================
  // WgslFn apply — call expression generation
  // =========================================================================

  test("WgslFn arity-1 apply generates correct call expression"):
    val fn: WgslFn[Float *: EmptyTuple, Float] =
      WgslFn.raw("double_it")("  return x * 2.0;")
    val a = FloatExpr("myVal")
    val result = fn(a)
    assertEquals(result.toString, "double_it(myVal)")

  test("WgslFn arity-2 apply generates correct call expression"):
    val fn: WgslFn[(v: Vec2, angle: Float), Vec2] =
      WgslFn.raw("rotate")("  return v;")
    val v = Vec2Expr("pos")
    val a = FloatExpr("theta")
    val result = fn(v, a)
    assertEquals(result.toString, "rotate(pos, theta)")

  test("WgslFn arity-3 apply generates correct call expression"):
    val fn: WgslFn[(pos: Vec2, mat: Mat2, offset: Vec2), Vec2] =
      WgslFn.raw("transform")("  return mat * pos + offset;")
    val result = fn(Vec2Expr("p"), Mat2Expr("m"), Vec2Expr("o"))
    assertEquals(result.toString, "transform(p, m, o)")

  test("WgslFn apply return type is correct Expr subtype"):
    val fn: WgslFn[(v: Vec3, s: Float), Vec3] =
      WgslFn.raw("scale_vec")("  return v * s;")
    val result: Vec3Expr = fn(Vec3Expr("v"), FloatExpr("s"))
    assertEquals(result.toString, "scale_vec(v, s)")

  test("WgslFn apply can be used in larger DSL expressions"):
    val fn: WgslFn[(v: Vec2, angle: Float), Vec2] =
      WgslFn.raw("rotate")("  return v;")
    val rotated = fn(Vec2Expr("pos"), FloatExpr("angle"))
    val result = vec4(rotated, 0.0, 1.0)
    assertEquals(result.toString, "vec4<f32>(rotate(pos, angle), 0.0, 1.0)")

  // =========================================================================
  // WgslFn.dsl — DSL body constructor
  // =========================================================================

  test("WgslFn.dsl generates correct source"):
    val fn: WgslFn[(a: Float, b: Float), Float] =
      WgslFn.dsl("add"): (p, ret) =>
        ret(p.a + p.b)
    val data = fn.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn add("), data.src)
    assert(data.src.contains("a: f32"), data.src)
    assert(data.src.contains("b: f32"), data.src)
    assert(data.src.contains("-> f32"), data.src)
    assert(data.src.contains("return (a + b);"), data.src)

  test("WgslFn.dsl with Vec2 params and return"):
    val fn: WgslFn[(v: Vec2, offset: Vec2), Vec2] =
      WgslFn.dsl("add_offset"): (p, ret) =>
        ret(p.v + p.offset)
    val data = fn.asInstanceOf[WgslFnData]
    assert(data.src.contains("v: vec2<f32>"), data.src)
    assert(data.src.contains("offset: vec2<f32>"), data.src)
    assert(data.src.contains("return (v + offset);"), data.src)

  // =========================================================================
  // Program.fn — registration and deduplication
  // =========================================================================

  test("program.fn registers helper function"):
    type Attribs = (position: Vec2)
    type Uniforms = (angle: VertexUniform[Float])
    val program = Program[Attribs, EmptyTuple, Uniforms, EmptyTuple, FragOut]()

    val rotate: WgslFn[(v: Vec2, angle: Float), Vec2] =
      WgslFn.raw("rotate")("  return v;")
    program.fn(rotate)

    assert(
      program.helperFnsStr.contains("fn rotate("),
      s"Helper not registered:\n${program.helperFnsStr}",
    )

  test("program.fn is idempotent — registering twice emits once"):
    type Attribs = (position: Vec2)
    val program = Program[Attribs, EmptyTuple, EmptyTuple, EmptyTuple, FragOut]()

    val fn: WgslFn[Float *: EmptyTuple, Float] =
      WgslFn.raw("my_fn")("  return x;")
    program.fn(fn)
    program.fn(fn)

    val count = program.helperFnsStr.split("fn my_fn").length - 1
    assertEquals(count, 1)

  test("program.fn accumulates multiple functions in order"):
    type Attribs = (position: Vec2)
    val program = Program[Attribs, EmptyTuple, EmptyTuple, EmptyTuple, FragOut]()

    val fn1: WgslFn[Float *: EmptyTuple, Float] =
      WgslFn.raw("fn_one")("  return x;")
    val fn2: WgslFn[Float *: EmptyTuple, Float] =
      WgslFn.raw("fn_two")("  return x * 2.0;")
    program.fn(fn1)
    program.fn(fn2)

    val helpers = program.helperFnsStr
    val pos1 = helpers.indexOf("fn_one")
    val pos2 = helpers.indexOf("fn_two")
    assert(pos1 < pos2, "fn_one should appear before fn_two")

  // =========================================================================
  // Integration — function used in vertex body
  // =========================================================================

  test("WgslFn call expression integrates into vertex body"):
    type Attribs = (position: Vec2)
    type Uniforms = (angle: VertexUniform[Float])
    val program = Program[Attribs, EmptyTuple, Uniforms, EmptyTuple, FragOut]()

    val rotate: WgslFn[(v: Vec2, angle: Float), Vec2] =
      WgslFn.raw("rotate")("  return v;")
    program.fn(rotate)

    program.vert[EmptyTuple]: ctx =>
      Block(
        ctx.out.position := vec4(
          rotate(ctx.in.position, ctx.bindings.angle),
          0.0,
          1.0,
        ),
      )

    val body = program.vertBodyStr
    assert(
      body.contains("rotate(in.position, angle)"),
      s"Missing rotate call:\n$body",
    )

  // =========================================================================
  // VarExpr inside WgslFn.dsl — manual var declaration
  // =========================================================================

  test("VarVec2 inside WgslFn.dsl generates var decl and reassignment"):
    val fn =
      WgslFn.dsl[(v: Vec2, delta: Vec2), Vec2]("accumulate"): (p, ret) =>
        val acc = VarVec2("acc")
        Block(
          acc := p.v,
          acc := acc + p.delta,
          ret(acc),
        )

    val data = fn.asInstanceOf[WgslFnData]

    assert(data.src.contains("var acc = v;"), s"Missing var decl:\n${data.src}")
    assert(
      data.src.contains("acc = (acc + delta);"),
      s"Missing reassign:\n${data.src}",
    )
    assert(data.src.contains("return acc;"), s"Missing return:\n${data.src}")

  // =========================================================================
  // WgslFn.dsl with typed locals (ctx-style API)
  // =========================================================================

  test("WgslFn.dsl with typed locals generates var decl and reassignment"):
    val fn: WgslFn[(v: Vec2, delta: Vec2), Vec2] =
      WgslFn.dsl[(acc: Var[Vec2]), (v: Vec2, delta: Vec2), Vec2](
        "accumulate",
      ): ctx =>
        val acc = ctx.locals.acc
        Block(
          acc := ctx.params.v,
          acc := acc + ctx.params.delta,
          ctx.ret(acc),
        )

    val data = fn.asInstanceOf[WgslFnData]
    assert(data.src.contains("var acc = v;"), s"Missing var decl:\n${data.src}")
    assert(
      data.src.contains("acc = (acc + delta);"),
      s"Missing reassign:\n${data.src}",
    )
    assert(data.src.contains("return acc;"), s"Missing return:\n${data.src}")

  test("WgslFn.dsl with const local generates const decl"):
    val fn: WgslFn[(v: Vec2, scale: Float), Vec2] =
      WgslFn.dsl[(s: Const[Float]), (v: Vec2, scale: Float), Vec2](
        "scaleVec",
      ): ctx =>
        Block(
          ctx.locals.s := ctx.params.scale,
          ctx.ret(ctx.params.v * ctx.locals.s),
        )

    val data = fn.asInstanceOf[WgslFnData]
    assert(
      data.src.contains("const s = scale;"),
      s"Missing const decl:\n${data.src}",
    )

  test("WgslFn.dsl with mixed locals (var + let)"):
    val fn: WgslFn[(v: Vec2), Vec2] =
      WgslFn.dsl[(acc: Var[Vec2], tmp: Vec2), (v: Vec2), Vec2](
        "mixedLocals",
      ): ctx =>
        Block(
          ctx.locals.acc := ctx.params.v,
          ctx.locals.tmp := ctx.locals.acc,
          ctx.ret(ctx.locals.tmp),
        )

    val data = fn.asInstanceOf[WgslFnData]
    assert(
      data.src.contains("var acc = v;"),
      s"Missing var decl:\n${data.src}",
    )
    assert(
      data.src.contains("let tmp = acc;"),
      s"Missing let decl:\n${data.src}",
    )
