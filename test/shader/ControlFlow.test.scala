package trivalibs.graphics.shader.dsl

import munit.FunSuite
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.shader.given

class ControlFlowTest extends FunSuite:

  // ---------------------------------------------------------------------------
  // Boolean comparison & equality operators (return BoolExpr)
  // ---------------------------------------------------------------------------

  test("FloatExpr < returns BoolExpr"):
    val r: BoolExpr = FloatExpr("a") < FloatExpr("b")
    assertEquals(r.toString, "(a < b)")

  test("FloatExpr <= returns BoolExpr"):
    assertEquals((FloatExpr("a") <= FloatExpr("b")).toString, "(a <= b)")

  test("FloatExpr > returns BoolExpr"):
    assertEquals((FloatExpr("a") > FloatExpr("b")).toString, "(a > b)")

  test("FloatExpr >= returns BoolExpr"):
    assertEquals((FloatExpr("a") >= FloatExpr("b")).toString, "(a >= b)")

  test("FloatExpr === returns BoolExpr"):
    assertEquals((FloatExpr("a") === FloatExpr("b")).toString, "(a == b)")

  test("FloatExpr !== returns BoolExpr"):
    assertEquals((FloatExpr("a") !== FloatExpr("b")).toString, "(a != b)")

  test("FloatExpr < accepts Double literal via conversion"):
    assertEquals((FloatExpr("x") < 0.5).toString, "(x < 0.5)")

  test("IntExpr === returns BoolExpr"):
    assertEquals((IntExpr("a") === IntExpr("b")).toString, "(a == b)")

  test("UIntExpr === returns BoolExpr"):
    assertEquals((UIntExpr("a") === UIntExpr("b")).toString, "(a == b)")

  test("UIntExpr === accepts UInt literal via .u"):
    val r: BoolExpr = UIntExpr("qi") === 0.u
    assertEquals(r.toString, "(qi == 0u)")

  // ---------------------------------------------------------------------------
  // BoolExpr combinators
  // ---------------------------------------------------------------------------

  test("BoolExpr && BoolExpr"):
    val a = FloatExpr("x") < FloatExpr("y")
    val b = FloatExpr("u") > FloatExpr("v")
    assertEquals((a && b).toString, "((x < y) && (u > v))")

  test("BoolExpr || BoolExpr"):
    val a = BoolExpr("p")
    val b = BoolExpr("q")
    assertEquals((a || b).toString, "(p || q)")

  test("!BoolExpr"):
    assertEquals((!BoolExpr("flag")).toString, "!(flag)")

  // ---------------------------------------------------------------------------
  // select — function form (WGSL signature: false, true, cond) and extension
  // ---------------------------------------------------------------------------

  test("select function form preserves WGSL arg order"):
    val cond = FloatExpr("a") > FloatExpr("b")
    val r: FloatExpr = select(FloatExpr("zero"), FloatExpr("one"), cond)
    assertEquals(r.toString, "select(zero, one, (a > b))")

  test("BoolExpr.select extension flips args for natural reading"):
    val cond = BoolExpr("c")
    val r: Vec2Expr = cond.select(Vec2Expr("hot"), Vec2Expr("cold"))
    assertEquals(r.toString, "select(cold, hot, c)")

  // ---------------------------------------------------------------------------
  // Stmt.ifBlock / Stmt.whileBlock — direct form
  // ---------------------------------------------------------------------------

  test("Stmt.ifBlock generates if with re-indented body"):
    val s = Stmt.ifBlock(
      BoolExpr("flag"),
      Block(Stmt.let("x", FloatExpr("1.0"))),
    )
    val expected =
      """  if (flag) {
        |    let x = 1.0;
        |  }""".stripMargin
    assertEquals(s: String, expected)

  test("Stmt.whileBlock generates while with re-indented body"):
    val s = Stmt.whileBlock(
      BoolExpr("flag"),
      Block(Stmt.let("x", FloatExpr("1.0"))),
    )
    val expected =
      """  while (flag) {
        |    let x = 1.0;
        |  }""".stripMargin
    assertEquals(s: String, expected)

  test("nested ifBlock indents recursively"):
    val inner = Stmt.ifBlock(
      BoolExpr("inner"),
      Block(Stmt.let("v", FloatExpr("1.0"))),
    )
    val outer = Stmt.ifBlock(BoolExpr("outer"), Block(inner))
    val expected =
      """  if (outer) {
        |    if (inner) {
        |      let v = 1.0;
        |    }
        |  }""".stripMargin
    assertEquals(outer: String, expected)

  // ---------------------------------------------------------------------------
  // when — top-level helper
  // ---------------------------------------------------------------------------

  test("when delegates to ifBlock"):
    val s: Stmt = when(BoolExpr("c"))(Block(Stmt.let("x", FloatExpr("1.0"))))
    val expected =
      """  if (c) {
        |    let x = 1.0;
        |  }""".stripMargin
    assertEquals(s: String, expected)

  test("when + elseDo generates if / else"):
    val s = when(BoolExpr("c"))(Block(Stmt.let("x", FloatExpr("1.0"))))
      .elseDo(Block(Stmt.let("x", FloatExpr("2.0"))))
    val expected =
      """  if (c) {
        |    let x = 1.0;
        |  } else {
        |    let x = 2.0;
        |  }""".stripMargin
    assertEquals(s: String, expected)

  test("Stmt -> Block conversion works for single-statement bodies"):
    // No Block(...) wrapping needed — Stmt is implicitly a Block.
    val s: Stmt = when(BoolExpr("c"))(Stmt.let("x", FloatExpr("1.0")))
    val expected =
      """  if (c) {
        |    let x = 1.0;
        |  }""".stripMargin
    assertEquals(s: String, expected)

  // ---------------------------------------------------------------------------
  // BoolExpr extension twins
  // ---------------------------------------------------------------------------

  test("BoolExpr.thenDo matches when"):
    val s: Stmt = BoolExpr("c").thenDo(Stmt.let("x", FloatExpr("1.0")))
    assertEquals(
      s: String,
      """  if (c) {
        |    let x = 1.0;
        |  }""".stripMargin,
    )

  test("BoolExpr.thenDo + elseDo matches when + elseDo"):
    val s = BoolExpr("c")
      .thenDo(Stmt.let("x", FloatExpr("1.0")))
      .elseDo(Stmt.let("x", FloatExpr("2.0")))
    assertEquals(
      s: String,
      """  if (c) {
        |    let x = 1.0;
        |  } else {
        |    let x = 2.0;
        |  }""".stripMargin,
    )

  // ---------------------------------------------------------------------------
  // when / elseIf / elseDo — multi-branch chain
  // ---------------------------------------------------------------------------

  test("when + elseIf + elseDo generates if / else if / else"):
    val s: Stmt = when(BoolExpr("c1"))(Stmt.let("x", FloatExpr("1.0")))
      .elseIf(BoolExpr("c2"))(Stmt.let("x", FloatExpr("2.0")))
      .elseDo(Stmt.let("x", FloatExpr("3.0")))
    val expected =
      """  if (c1) {
        |    let x = 1.0;
        |  } else if (c2) {
        |    let x = 2.0;
        |  } else {
        |    let x = 3.0;
        |  }""".stripMargin
    assertEquals(s: String, expected)

  test("a chain without elseDo acts as Stmt"):
    val s: Stmt = when(BoolExpr("c1"))(Stmt.let("x", FloatExpr("1.0")))
      .elseIf(BoolExpr("c2"))(Stmt.let("x", FloatExpr("2.0")))
    val expected =
      """  if (c1) {
        |    let x = 1.0;
        |  } else if (c2) {
        |    let x = 2.0;
        |  }""".stripMargin
    assertEquals(s: String, expected)

  // ---------------------------------------------------------------------------
  // Integration — control flow inside WgslFn.dsl
  // ---------------------------------------------------------------------------

  test("if inside WgslFn.dsl with early return"):
    val fn: WgslFn[(x: Float), Float] =
      WgslFn.dsl("clip"): (p, ret) =>
        Block(
          when(p.x < 0.0)(ret(FloatExpr("0.0"))),
          ret(p.x),
        )
    val src = fn.asInstanceOf[WgslFnData].src
    assert(src.contains("if ((x < 0.0)) {"), src)
    assert(src.contains("    return 0.0;"), src)
    assert(src.contains("\n  return x;\n"), src)

  // ---------------------------------------------------------------------------
  // unroll — build-time repetition, no loop in the emitted WGSL
  // ---------------------------------------------------------------------------

  test("unroll emits one statement group per index"):
    val acc = VarFloat("acc")
    val s = Block(
      acc := FloatExpr("0.0"),
      unroll(1, 4)(i => acc := acc + FloatExpr(s"v[$i]")),
    )
    val expected =
      """|  var acc = 0.0;
         |  acc = (acc + v[1]);
         |  acc = (acc + v[2]);
         |  acc = (acc + v[3]);""".stripMargin
    assertEquals(Block.unwrap(s), expected)

  test("unroll(count) starts at zero"):
    val s = unroll(3)(i => Stmt.raw(s"  x$i;"))
    assertEquals(Block.unwrap(s), "  x0;\n  x1;\n  x2;")

  test("unroll emits nothing for an empty range"):
    assertEquals(Block.unwrap(unroll(2, 2)(i => Stmt.raw(s"  x$i;"))), "")

  test("unroll accepts a multi-statement body"):
    val s = unroll(2)(i =>
      Block(
        Stmt.raw(s"  let a$i = 1.0;"),
        Stmt.raw(s"  let b$i = 2.0;"),
      ),
    )
    val expected =
      """|  let a0 = 1.0;
         |  let b0 = 2.0;
         |  let a1 = 1.0;
         |  let b1 = 2.0;""".stripMargin
    assertEquals(Block.unwrap(s), expected)
