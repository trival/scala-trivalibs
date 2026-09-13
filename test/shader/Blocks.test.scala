package trivalibs.graphics.shader.dsl

import munit.FunSuite
import trivalibs.graphics.math.gpu.*
import trivalibs.graphics.math.gpu.given
import trivalibs.utils.js.Arr

class BlocksTest extends FunSuite:

  // ---------------------------------------------------------------------------
  // Nesting — a Block is a legal part of a Block, at any depth
  // ---------------------------------------------------------------------------

  test("a nested Block flattens into the enclosing statement list"):
    val group: Block = Block(Stmt.raw("  b;"), Stmt.raw("  c;"))
    val s = Block(Stmt.raw("  a;"), group, Stmt.raw("  d;"))
    assertEquals(
      s: String,
      """  a;
        |  b;
        |  c;
        |  d;""".stripMargin,
    )

  test("nesting three deep still flattens to one list"):
    val inner: Block = Block(Stmt.raw("  c;"))
    val mid: Block = Block(Stmt.raw("  b;"), inner)
    val s = Block(Stmt.raw("  a;"), mid, Stmt.raw("  d;"))
    assertEquals(
      s: String,
      """  a;
        |  b;
        |  c;
        |  d;""".stripMargin,
    )

  test("a Stmt is a Block — no conversion needed"):
    val s: Block = Stmt.raw("  a;")
    assertEquals(s: String, "  a;")

  test("an IfChain is a Stmt and a Block"):
    val chain = when(BoolExpr("c"))(Stmt.raw("  a;"))
    val asStmt: Stmt = chain
    val asBlock: Block = chain
    assertEquals(asStmt: String, asBlock: String)

  // ---------------------------------------------------------------------------
  // Block.empty — the "emit nothing" part
  // ---------------------------------------------------------------------------

  test("Block.empty contributes no line"):
    val s = Block(Stmt.raw("  a;"), Block.empty, Stmt.raw("  b;"))
    assertEquals(
      s: String,
      """  a;
        |  b;""".stripMargin,
    )

  test("a Block of only empties is empty"):
    assertEquals(Block(Block.empty, Block.empty): String, "")

  test("Block.empty works as a build-time conditional branch"):
    val col = VarVec3("col")
    def tap(on: Boolean): Block =
      if on then col := Expr.raw("vec3<f32>(1.0)") else Block.empty
    val s = Block(tap(true), tap(false), tap(true))
    assertEquals(
      s: String,
      """  var col = vec3<f32>(1.0);
        |  col = vec3<f32>(1.0);""".stripMargin,
    )

  test("Block over an Arr skips empties too"):
    val parts = Arr[Block](Stmt.raw("  a;"), Block.empty, Stmt.raw("  b;"))
    assertEquals(
      Block(parts): String,
      """  a;
        |  b;""".stripMargin,
    )

  // ---------------------------------------------------------------------------
  // Nested groups keep the indentation scheme correct
  // ---------------------------------------------------------------------------

  test("a nested group inside `when` indents as one body"):
    val group: Block = Block(Stmt.raw("  b;"), Stmt.raw("  c;"))
    val s = when(BoolExpr("f"))(Block(Stmt.raw("  a;"), group))
    assertEquals(
      s: String,
      """  if (f) {
        |    a;
        |    b;
        |    c;
        |  }""".stripMargin,
    )

  test("unroll returns a Block that composes as an argument"):
    val x = VarFloat("x")
    val s = Block(
      x := 0.0,
      unroll(3)(i => Stmt.raw(s"  y$i;")),
      x := 1.0,
    )
    assertEquals(
      s: String,
      """  var x = 0.0;
        |  y0;
        |  y1;
        |  y2;
        |  x = 1.0;""".stripMargin,
    )

  test("Arr.unroll without the index"):
    val s = Arr("a", "b").unroll(v => Stmt.raw(s"  $v;"))
    assertEquals(
      s: String,
      """  a;
        |  b;""".stripMargin,
    )

  // ---------------------------------------------------------------------------
  // scope — a bare WGSL `{ ... }`
  // ---------------------------------------------------------------------------

  test("scope emits a braced compound statement"):
    val s = scope(Block(Stmt.let("x", FloatExpr("1.0"))))
    assertEquals(
      s: String,
      """  {
        |    let x = 1.0;
        |  }""".stripMargin,
    )

  test("sibling scopes may declare the same local name"):
    val s = unroll(2): i =>
      scope:
        val av = LetVec4("av")
        Block(av := Expr.raw(s"lines[$i]"))
    assertEquals(
      s: String,
      """  {
        |    let av = lines[0];
        |  }
        |  {
        |    let av = lines[1];
        |  }""".stripMargin,
    )

  test("a scope nested in a loop indents recursively"):
    val s = loop(2)(i => scope(Stmt.let("x", i)))
    assertEquals(
      s: String,
      """  for (var i: i32 = 0; i < 2; i++) {
        |    {
        |      let x = i;
        |    }
        |  }""".stripMargin,
    )

  test("an outer var is assignable from inside a scope"):
    val col = VarVec3("col")
    val s = Block(
      col := Expr.raw("vec3<f32>(0.0)"),
      scope(col := Expr.raw("vec3<f32>(1.0)")),
    )
    assertEquals(
      s: String,
      """  var col = vec3<f32>(0.0);
        |  {
        |    col = vec3<f32>(1.0);
        |  }""".stripMargin,
    )
