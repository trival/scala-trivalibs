package trivalibs.graphics.shader.dsl

import munit.FunSuite
import trivalibs.graphics.math.gpu.*
import trivalibs.graphics.math.gpu.given
import trivalibs.utils.js.Arr

class LoopsTest extends FunSuite:

  // ---------------------------------------------------------------------------
  // loop — counted, emits a WGSL `for`
  // ---------------------------------------------------------------------------

  test("loop over a runtime bound"):
    val s = loop(IntExpr("count"))(i => Stmt.let("x", i))
    assertEquals(
      s: String,
      """  for (var i: i32 = 0; i < count; i++) {
        |    let x = i;
        |  }""".stripMargin,
    )

  test("loop over a literal bound"):
    val s = loop(4)(i => Stmt.let("x", i))
    assertEquals(
      s: String,
      """  for (var i: i32 = 0; i < 4; i++) {
        |    let x = i;
        |  }""".stripMargin,
    )

  test("loop from/until, mixed literal and expression bounds"):
    assertEquals(
      loop(1, IntExpr("n"))(i => Stmt.let("x", i)): String,
      """  for (var i: i32 = 1; i < n; i++) {
        |    let x = i;
        |  }""".stripMargin,
    )
    assertEquals(
      loop(IntExpr("a"), IntExpr("b"))(i => Stmt.let("x", i)): String,
      """  for (var i: i32 = a; i < b; i++) {
        |    let x = i;
        |  }""".stripMargin,
    )
    assertEquals(
      loop(IntExpr("a"), 8)(i => Stmt.let("x", i)): String,
      """  for (var i: i32 = a; i < 8; i++) {
        |    let x = i;
        |  }""".stripMargin,
    )
    assertEquals(
      loop(2, 5)(i => Stmt.let("x", i)): String,
      """  for (var i: i32 = 2; i < 5; i++) {
        |    let x = i;
        |  }""".stripMargin,
    )

  test("the index is usable as an IntExpr"):
    val s = loop(4)(i => Stmt.let("x", i + IntExpr("1")))
    assert((s: String).contains("let x = (i + 1);"), s: String)

  // ---------------------------------------------------------------------------
  // Counter naming — derived from nesting depth, deterministic
  // ---------------------------------------------------------------------------

  test("nested loops get i / j / k"):
    val s = loop(2)(i =>
      loop(3)(j =>
        loop(4)(k => Stmt.let("x", i + j + k)),
      ),
    )
    val body = s: String
    assert(body.contains("for (var i: i32 = 0; i < 2; i++)"), body)
    assert(body.contains("for (var j: i32 = 0; j < 3; j++)"), body)
    assert(body.contains("for (var k: i32 = 0; k < 4; k++)"), body)
    assert(body.contains("let x = ((i + j) + k);"), body)

  test("sibling loops at the same depth both use i — each is its own scope"):
    val s = Block(
      loop(2)(i => Stmt.let("x", i)),
      loop(3)(i => Stmt.let("y", i)),
    )
    val body = Block.unwrap(s)
    assert(body.contains("for (var i: i32 = 0; i < 2; i++)"), body)
    assert(body.contains("for (var i: i32 = 0; i < 3; i++)"), body)

  test("building the same loop twice emits identical source"):
    def build() = loop(IntExpr("n"))(i => loop(2)(j => Stmt.let("x", i + j)))
    assertEquals(build(): String, build(): String)

  test("a loop nested in an if indents recursively"):
    val s: Stmt = when(BoolExpr("c"))(loop(2)(i => Stmt.let("x", i)))
    assertEquals(
      s: String,
      """  if (c) {
        |    for (var i: i32 = 0; i < 2; i++) {
        |      let x = i;
        |    }
        |  }""".stripMargin,
    )

  // ---------------------------------------------------------------------------
  // loopIf — condition-driven
  // ---------------------------------------------------------------------------

  test("loopIf emits a while"):
    val s = loopIf(BoolExpr("c"))(Stmt.let("x", FloatExpr("1.0")))
    assertEquals(
      s: String,
      """  while (c) {
        |    let x = 1.0;
        |  }""".stripMargin,
    )

  // ---------------------------------------------------------------------------
  // Jumps
  // ---------------------------------------------------------------------------

  test("break / continue"):
    assertEquals(break: String, "  break;")
    assertEquals(continue: String, "  continue;")

  test("breakIf / continueIf wrap the jump in an if"):
    assertEquals(
      breakIf(BoolExpr("c")): String,
      """  if (c) {
        |    break;
        |  }""".stripMargin,
    )
    assertEquals(
      continueIf(BoolExpr("c")): String,
      """  if (c) {
        |    continue;
        |  }""".stripMargin,
    )

  // ---------------------------------------------------------------------------
  // Extension twins — each must emit exactly what its function form emits
  // ---------------------------------------------------------------------------

  test("IntExpr.loop matches loop"):
    assertEquals(
      IntExpr("n").loop(i => Stmt.let("x", i)): String,
      loop(IntExpr("n"))(i => Stmt.let("x", i)): String,
    )

  test("Int.loop matches loop"):
    assertEquals(
      4.loop(i => Stmt.let("x", i)): String,
      loop(4)(i => Stmt.let("x", i)): String,
    )

  test("Int.unroll matches unroll"):
    assertEquals(
      3.unroll(i => Stmt.let(s"x$i", FloatExpr("1.0"))): String,
      unroll(3)(i => Stmt.let(s"x$i", FloatExpr("1.0"))): String,
    )

  test("Arr.unroll matches unroll over values"):
    val xs = Arr(2.0, 4.0)
    assertEquals(
      xs.unroll((v, i) => Stmt.let(s"x$i", FloatExpr(v.toString))): String,
      unroll(xs)((v, i) => Stmt.let(s"x$i", FloatExpr(v.toString))): String,
    )

  test("BoolExpr.thenLoop matches loopIf"):
    assertEquals(
      BoolExpr("c").thenLoop(Stmt.let("x", FloatExpr("1.0"))): String,
      loopIf(BoolExpr("c"))(Stmt.let("x", FloatExpr("1.0"))): String,
    )

  test("BoolExpr.thenBreak / thenContinue match breakIf / continueIf"):
    assertEquals(
      BoolExpr("c").thenBreak: String,
      breakIf(BoolExpr("c")): String,
    )
    assertEquals(
      BoolExpr("c").thenContinue: String,
      continueIf(BoolExpr("c")): String,
    )

  // ---------------------------------------------------------------------------
  // unroll over values — the build-time twin of `loop`
  // ---------------------------------------------------------------------------

  test("unroll over values gives each value and its index"):
    val s = unroll(Arr("a", "b", "c")): (v, i) =>
      Stmt.let(s"$v$i", FloatExpr("1.0"))
    assertEquals(
      s: String,
      """  let a0 = 1.0;
        |  let b1 = 1.0;
        |  let c2 = 1.0;""".stripMargin,
    )

  test("unroll over an empty Arr emits nothing"):
    assertEquals(unroll(Arr[String]())((v, i) => Stmt.raw(v)): String, "")

  // ---------------------------------------------------------------------------
  // Declaration placement — where a `var` declares, and what that means
  // ---------------------------------------------------------------------------

  test("a local first assigned inside a loop declares inside the braces"):
    val cur = LetVec4("cur")
    val s = loop(2)(i => Block(cur := Expr.raw("stops[0]")))
    assertEquals(
      s: String,
      """  for (var i: i32 = 0; i < 2; i++) {
        |    let cur = stops[0];
        |  }""".stripMargin,
    )

  test("a var assigned before the loop only assigns inside it"):
    val col = VarVec3("col")
    val s = Block(
      col := Expr.raw("vec3<f32>(0.0)"),
      loop(2)(i => col := Expr.raw("vec3<f32>(1.0)")),
    )
    assertEquals(
      Block.unwrap(s),
      """  var col = vec3<f32>(0.0);
        |  for (var i: i32 = 0; i < 2; i++) {
        |    col = vec3<f32>(1.0);
        |  }""".stripMargin,
    )

  test("an unrolled body shares one scope — one declaration, then assignments"):
    val acc = VarVec3("acc")
    val s = unroll(3)(i => acc := Expr.raw(s"vec3<f32>($i.0)"))
    assertEquals(
      s: String,
      """  var acc = vec3<f32>(0.0);
        |  acc = vec3<f32>(1.0);
        |  acc = vec3<f32>(2.0);""".stripMargin,
    )

  test("declaration follows build order, not placement order"):
    val a = VarVec2("a")
    val first = a := Expr.raw("p")
    val second = a := Expr.raw("q")
    assertEquals(
      Block.unwrap(Block(second, first)),
      """  a = q;
        |  var a = p;""".stripMargin,
    )
