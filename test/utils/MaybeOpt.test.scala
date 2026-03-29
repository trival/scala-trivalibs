package trivalibs.utils.js

import munit.FunSuite
import scala.scalajs.js

class MaybeOptTest extends FunSuite:

  // =========================================================================
  // Maybe — treats only js.undefined as empty; null is a valid value
  // =========================================================================

  test("Maybe: real value orElse returns value"):
    val m: Maybe[String] = "hello"
    assertEquals(m.orElse("fallback"), "hello")

  test("Maybe: real value safe returns value"):
    val m: Maybe[String] = "hello"
    assertEquals(m.safe, "hello")

  test("Maybe: Maybe.Not (undefined) falls back"):
    val m: Maybe[String] = Maybe.Not
    assertEquals(m.orElse("fallback"), "fallback")

  test("Maybe: null is a valid value, not treated as empty"):
    val m: Maybe[String | Null] = null
    // null should NOT trigger fallback — Maybe only checks for undefined
    assertEquals(m.orElse("fallback"), null)

  test("Maybe: null passes through safe"):
    val m: Maybe[String | Null] = null
    assertEquals(m.safe, null)

  // =========================================================================
  // Opt — treats null as empty (strict === null check)
  // =========================================================================

  test("Opt: real value is non-null"):
    val o: Opt[String] = "hello"
    assertEquals(o.isNull, false)
    assertEquals(o.nonNull, true)
    assertEquals(o.getOr("fallback"), "hello")
    assertEquals(o.get, "hello")

  test("Opt: Opt.Null is null"):
    val o: Opt[String] = Opt.Null
    assertEquals(o.isNull, true)
    assertEquals(o.nonNull, false)
    assertEquals(o.getOr("fallback"), "fallback")

  test("Opt: literal null is null"):
    val o: Opt[String] = null
    assertEquals(o.isNull, true)
    assertEquals(o.getOr("fallback"), "fallback")

  // Opt uses strict === null, so undefined is NOT caught.
  // Opt fields should only ever hold A or null, never undefined.
  // Use Maybe for undefined-based optionality.
  test("Opt: undefined is NOT treated as null (strict check)"):
    val o: Opt[js.Any] = js.undefined.asInstanceOf[Opt[js.Any]]
    // undefined !== null in JS strict equality
    assertEquals(o.isNull, false)

  // =========================================================================
  // Maybe vs Opt: the key distinction
  // =========================================================================

  test("Maybe passes null through, Opt rejects null"):
    val asMaybe: Maybe[String | Null] = null
    val asOpt: Opt[String] = null

    // Maybe: null is a legitimate value → orElse NOT triggered
    assertEquals(asMaybe.orElse("fallback"), null)

    // Opt: null means absent → getOr IS triggered
    assertEquals(asOpt.getOr("fallback"), "fallback")

  test("Both treat real values the same"):
    val asMaybe: Maybe[Int] = 42
    val asOpt: Opt[Int] = 42

    assertEquals(asMaybe.orElse(0), 42)
    assertEquals(asOpt.getOr(0), 42)

  test("Maybe.Not (undefined) vs Opt.Null (null) are distinct JS values"):
    val m: Maybe[String] = Maybe.Not
    val o: Opt[String] = Opt.Null

    assertEquals(m.orElse("fallback"), "fallback")
    assertEquals(o.getOr("fallback"), "fallback")

    // Underlying JS values are different
    assertEquals(js.isUndefined(m.asInstanceOf[js.Any]), true)
    assertEquals(o == null, true)
