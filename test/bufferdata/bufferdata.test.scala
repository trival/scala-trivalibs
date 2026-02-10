package trivalibs.bufferdata

import munit.FunSuite
import scala.scalajs.js.typedarray.{ArrayBuffer, DataView}

class BufferDataTest extends FunSuite:

  // ==========================================================================
  // Phase 1: Primitive View Tests
  // ==========================================================================

  test("F32View get/set round-trip"):
    val buffer = new ArrayBuffer(4)
    val view = new DataView(buffer)
    val f32 = F32View(view, 0)

    f32.set(3.14159f)
    assertEqualsFloat(f32.get, 3.14159f, 0.00001f)

  test("F64View get/set round-trip"):
    val buffer = new ArrayBuffer(8)
    val view = new DataView(buffer)
    val f64 = F64View(view, 0)

    f64.set(2.718281828)
    assertEqualsDouble(f64.get, 2.718281828, 0.0000001)

  test("U8View get/set round-trip"):
    val buffer = new ArrayBuffer(1)
    val view = new DataView(buffer)
    val u8 = U8View(view, 0)

    u8.set(255)
    assertEquals(u8.get, 255.toShort)

  test("U16View get/set round-trip"):
    val buffer = new ArrayBuffer(2)
    val view = new DataView(buffer)
    val u16 = U16View(view, 0)

    u16.set(65535)
    assertEquals(u16.get, 65535)

  test("U32View get/set round-trip"):
    val buffer = new ArrayBuffer(4)
    val view = new DataView(buffer)
    val u32 = U32View(view, 0)

    u32.set(4294967295.0)
    assertEqualsDouble(u32.get, 4294967295.0, 0.1)

  test("I8View get/set round-trip"):
    val buffer = new ArrayBuffer(1)
    val view = new DataView(buffer)
    val i8 = I8View(view, 0)

    i8.set(-128)
    assertEquals(i8.get, (-128).toByte)

  test("I16View get/set round-trip"):
    val buffer = new ArrayBuffer(2)
    val view = new DataView(buffer)
    val i16 = I16View(view, 0)

    i16.set(-32768)
    assertEquals(i16.get, (-32768).toShort)

  test("I32View get/set round-trip"):
    val buffer = new ArrayBuffer(4)
    val view = new DataView(buffer)
    val i32 = I32View(view, 0)

    i32.set(-2147483648)
    assertEquals(i32.get, -2147483648)

  // ==========================================================================
  // Phase 2: Struct Layout Tests (New Typed API)
  // ==========================================================================

  test("Typed struct computes correct size for (F32, U8)"):
    val layout = struct[(F32, U8)]
    assertEquals(layout.sizeInBytes, 5)

  test("Typed struct computes correct size for (F32, F32, U8)"):
    val layout = struct[(F32, F32, U8)]
    assertEquals(layout.sizeInBytes, 9)

  test("Typed struct size for multiple fields"):
    val layout = struct[(F32, U8, F64)]
    // F32 (4) + U8 (1) + F64 (8) = 13 bytes
    assertEquals(layout.sizeInBytes, 13)

  test("Typed struct field access verifies correct offsets"):
    // Offsets are computed at compile time via match types
    // F32 at offset 0, U8 at offset 4, F64 at offset 5
    val array = struct[(F32, U8, F64)].allocate(1)
    array(0)(0).set(1.0f)
    array(0)(1).set(2: Short)
    array(0)(2).set(3.0)

    assertEqualsFloat(array(0)(0).get, 1.0f, 0.001f)
    assertEquals(array(0)(1).get, 2.toShort)
    assertEqualsDouble(array(0)(2).get, 3.0, 0.001)

  // ==========================================================================
  // Phase 3: StructArray and StructRef Tests (Typed API)
  // ==========================================================================

  test("Typed struct.allocate creates correct buffer size"):
    val layout = struct[(F32, U8)]
    val array = layout.allocate(10)

    assertEquals(array.length, 10)
    assertEquals(array.arrayBuffer.byteLength, 50) // 10 * 5 bytes

  test("Typed StructRef indexed access with inferred types"):
    val layout = struct[(F32, U8)]
    val array = layout.allocate(1)

    // Write using typed accessors - no .asF32 needed!
    array(0)(0).set(42.5f)
    array(0)(1).set(200: Short)

    // Read back - types are inferred!
    val f32Val: Float = array(0)(0).get
    val u8Val: Short = array(0)(1).get
    assertEqualsFloat(f32Val, 42.5f, 0.001f)
    assertEquals(u8Val, 200.toShort)

  test("Typed StructArray indexing with multiple elements"):
    val layout = struct[(F32, U8)]
    val array = layout.allocate(100)

    // Write to various indices
    for i <- 0 until 100 do
      array(i)(0).set(i.toFloat * 2.0f)
      array(i)(1).set((i % 256).toShort)

    // Verify all values
    for i <- 0 until 100 do
      assertEqualsFloat(array(i)(0).get, i.toFloat * 2.0f, 0.001f)
      assertEquals(array(i)(1).get, (i % 256).toShort)

  test("Typed StructRef with 3 fields"):
    val layout = struct[(F32, F32, U8)]
    val array = layout.allocate(1)

    array(0)(0).set(1.0f)
    array(0)(1).set(2.0f)
    array(0)(2).set(3: Short)

    assertEqualsFloat(array(0)(0).get, 1.0f, 0.001f)
    assertEqualsFloat(array(0)(1).get, 2.0f, 0.001f)
    assertEquals(array(0)(2).get, 3.toShort)

  test("Typed struct with multiple primitive types"):
    val layout = struct[(F32, F64, U8, I32)]
    val array = layout.allocate(1)

    array(0)(0).set(1.5f)
    array(0)(1).set(2.5)
    array(0)(2).set(128: Short)
    array(0)(3).set(-1000)

    assertEqualsFloat(array(0)(0).get, 1.5f, 0.001f)
    assertEqualsDouble(array(0)(1).get, 2.5, 0.001)
    assertEquals(array(0)(2).get, 128.toShort)
    assertEquals(array(0)(3).get, -1000)

  // ==========================================================================
  // Phase 4: Single Struct Shorthand
  // ==========================================================================

  test("Typed struct() creates single struct"):
    val layout = struct[(F32, U8)]
    val single = layout()

    single(0).set(123.456f)
    single(1).set(42: Short)

    assertEqualsFloat(single(0).get, 123.456f, 0.001f)
    assertEquals(single(1).get, 42.toShort)

  // ==========================================================================
  // Phase 5: Nested Struct Support (Typed)
  // ==========================================================================

  test("Typed nested struct layout computes correct size"):
    type Vec2 = (F32, F32)
    type Particle = (Vec2, U8)

    val vec2Layout = struct[Vec2]
    val particleLayout = struct[Particle]

    assertEquals(vec2Layout.sizeInBytes, 8)
    assertEquals(particleLayout.sizeInBytes, 9)

  test("Typed nested struct field access"):
    type Vec2 = (F32, F32)
    type Particle = (Vec2, U8)

    val array = struct[Particle].allocate(1)

    // Access nested Vec2 fields through position - types inferred!
    array(0)(0)(0).set(10.0f) // position.x
    array(0)(0)(1).set(20.0f) // position.y
    array(0)(1).set(100: Short) // life

    val x: Float = array(0)(0)(0).get
    val y: Float = array(0)(0)(1).get
    val life: Short = array(0)(1).get

    assertEqualsFloat(x, 10.0f, 0.001f)
    assertEqualsFloat(y, 20.0f, 0.001f)
    assertEquals(life, 100.toShort)

  test("Typed deeply nested structs"):
    type Vec2 = (F32, F32)
    type Transform = (Vec2, Vec2) // position, velocity
    type Entity = (Transform, U8) // transform, health

    val vec2Layout = struct[Vec2]
    val transformLayout = struct[Transform]
    val entityLayout = struct[Entity]

    assertEquals(vec2Layout.sizeInBytes, 8)
    assertEquals(transformLayout.sizeInBytes, 16)
    assertEquals(entityLayout.sizeInBytes, 17)

    val entities = entityLayout.allocate(1)
    entities(0)(0)(0)(0).set(1.0f) // transform.position.x
    entities(0)(0)(0)(1).set(2.0f) // transform.position.y
    entities(0)(0)(1)(0).set(3.0f) // transform.velocity.x
    entities(0)(0)(1)(1).set(4.0f) // transform.velocity.y
    entities(0)(1).set(255: Short) // health

    assertEqualsFloat(entities(0)(0)(0)(0).get, 1.0f, 0.001f)
    assertEqualsFloat(entities(0)(0)(0)(1).get, 2.0f, 0.001f)
    assertEqualsFloat(entities(0)(0)(1)(0).get, 3.0f, 0.001f)
    assertEqualsFloat(entities(0)(0)(1)(1).get, 4.0f, 0.001f)
    assertEquals(entities(0)(1).get, 255.toShort)

  // ==========================================================================
  // Phase 6: copyFrom and sliceBuffer
  // ==========================================================================

  test("Typed StructRef.copyFrom copies data correctly"):
    val layout = struct[(F32, U8)]
    val array = layout.allocate(2)

    // Set up source
    array(0)(0).set(99.9f)
    array(0)(1).set(77: Short)

    // Copy to destination
    array(1).copyFrom(array(0))

    // Verify copy
    assertEqualsFloat(array(1)(0).get, 99.9f, 0.001f)
    assertEquals(array(1)(1).get, 77.toShort)

  test("Typed StructRef.sliceBuffer creates independent buffer"):
    val layout = struct[(F32, U8)]
    val array = layout.allocate(1)

    array(0)(0).set(123.0f)
    array(0)(1).set(45: Short)

    // Extract slice
    val slice = array(0).sliceBuffer

    assertEquals(slice.byteLength, 5)

    // Verify data in slice
    val sliceView = new DataView(slice)
    assertEqualsFloat(sliceView.getFloat32(0, true), 123.0f, 0.001f)
    assertEquals(sliceView.getUint8(4), 45.toShort)

  test("Typed FieldRef.sliceBuffer for nested struct"):
    type Vec2 = (F32, F32)
    type Particle = (Vec2, U8)

    val particle = struct[Particle]()

    particle(0)(0).set(10.0f)
    particle(0)(1).set(20.0f)

    // Extract just the Vec2 portion
    val vec2Slice = particle(0).sliceBuffer

    assertEquals(vec2Slice.byteLength, 8)

    val sliceView = new DataView(vec2Slice)
    assertEqualsFloat(sliceView.getFloat32(0, true), 10.0f, 0.001f)
    assertEqualsFloat(sliceView.getFloat32(4, true), 20.0f, 0.001f)

  test("Typed get arrayBuffer from StructArray"):
    val layout = struct[(F32, U8)]
    val array = layout.allocate(10)

    val rawBuffer = array.arrayBuffer
    assertEquals(rawBuffer.byteLength, 50)

  test("sliceBuffer creates independent copy, copyFrom transfers back"):
    type Vec2 = (F32, F32)
    type Particle = (Vec2, U8)
    val layout = struct[Particle]

    // 1. Create a StructArray of Particles
    val particles = layout.allocate(3)
    particles(0)(0)(0).set(10.0f) // particle 0: pos.x
    particles(0)(0)(1).set(20.0f) // particle 0: pos.y
    particles(0)(1).set(100: Short) // particle 0: life
    particles(1)(0)(0).set(30.0f) // particle 1: pos.x
    particles(1)(0)(1).set(40.0f) // particle 1: pos.y
    particles(1)(1).set(200: Short) // particle 1: life
    particles(2)(0)(0).set(0.0f) // particle 2: zeroed
    particles(2)(0)(1).set(0.0f)
    particles(2)(1).set(0: Short)

    // 2. Create a single particle from a slice of particle 1
    val slicedBuffer = particles(1).sliceBuffer
    val detachedParticle = layout.fromBuffer(slicedBuffer)

    // Verify slice has correct initial values
    assertEqualsFloat(detachedParticle(0)(0).get, 30.0f, 0.001f)
    assertEqualsFloat(detachedParticle(0)(1).get, 40.0f, 0.001f)
    assertEquals(detachedParticle(1).get, 200.toShort)

    // 3. Modify the detached struct
    detachedParticle(0)(0).set(999.0f)
    detachedParticle(0)(1).set(888.0f)
    detachedParticle(1).set(77: Short)

    // 4. Verify original particle 1 was NOT modified (independent buffer)
    assertEqualsFloat(particles(1)(0)(0).get, 30.0f, 0.001f)
    assertEqualsFloat(particles(1)(0)(1).get, 40.0f, 0.001f)
    assertEquals(particles(1)(1).get, 200.toShort)

    // 5. Copy the modified detached particle to particle 2
    particles(2).copyFrom(detachedParticle)

    // 6. Verify particle 2 now has the modified values
    assertEqualsFloat(particles(2)(0)(0).get, 999.0f, 0.001f)
    assertEqualsFloat(particles(2)(0)(1).get, 888.0f, 0.001f)
    assertEquals(particles(2)(1).get, 77.toShort)

    // 7. Verify particle 0 and 1 are still unchanged
    assertEqualsFloat(particles(0)(0)(0).get, 10.0f, 0.001f)
    assertEqualsFloat(particles(0)(0)(1).get, 20.0f, 0.001f)
    assertEquals(particles(0)(1).get, 100.toShort)
    assertEqualsFloat(particles(1)(0)(0).get, 30.0f, 0.001f)
    assertEqualsFloat(particles(1)(0)(1).get, 40.0f, 0.001f)
    assertEquals(particles(1)(1).get, 200.toShort)

  // ==========================================================================
  // Phase 7: Named Field Access via Extensions
  // ==========================================================================
  //
  // Recommended approach: simple inline extensions on unnamed tuples.
  // This is zero-cost and more concise than any macro-based approach.

  // Define types
  type Vec2 = (F32, F32)
  type Particle = (Vec2, U8)
  type Transform = (Vec2, Vec2) // position, velocity
  type Entity = (Transform, U8) // transform, health

  // Extensions for Vec2 - only need StructRef since nested access returns StructRef!
  extension (v: StructRef[Vec2])
    inline def x = v(0)
    inline def y = v(1)

  // Extensions for Particle
  extension (p: StructRef[Particle])
    inline def pos = p(0)
    inline def life = p(1)

  // Extensions for Transform - only need StructRef
  extension (t: StructRef[Transform])
    inline def position = t(0)
    inline def velocity = t(1)

  // Extensions for Entity
  extension (e: StructRef[Entity])
    inline def transform = e(0)
    inline def health = e(1)

  test("Named extensions on single struct"):
    val v = struct[Vec2]()

    v.x.set(10.0f)
    v.y.set(20.0f)

    assertEqualsFloat(v.x.get, 10.0f, 0.001f)
    assertEqualsFloat(v.y.get, 20.0f, 0.001f)

  test("Named extensions with nested struct"):
    val p = struct[Particle]()

    p.pos.x.set(100.0f)
    p.pos.y.set(200.0f)
    p.life.set(255: Short)

    assertEqualsFloat(p.pos.x.get, 100.0f, 0.001f)
    assertEqualsFloat(p.pos.y.get, 200.0f, 0.001f)
    assertEquals(p.life.get, 255.toShort)

  test("Named extensions with deeply nested struct"):
    val e = struct[Entity]()

    e.transform.position.x.set(1.0f)
    e.transform.position.y.set(2.0f)
    e.transform.velocity.x.set(3.0f)
    e.transform.velocity.y.set(4.0f)
    e.health.set(100: Short)

    assertEqualsFloat(e.transform.position.x.get, 1.0f, 0.001f)
    assertEqualsFloat(e.transform.position.y.get, 2.0f, 0.001f)
    assertEqualsFloat(e.transform.velocity.x.get, 3.0f, 0.001f)
    assertEqualsFloat(e.transform.velocity.y.get, 4.0f, 0.001f)
    assertEquals(e.health.get, 100.toShort)

  test("Named extensions with array access"):
    val particles = struct[Particle].allocate(100)

    for i <- 0 until 100 do
      particles(i).pos.x.set(i.toFloat)
      particles(i).pos.y.set(i.toFloat * 2)
      particles(i).life.set((i % 256).toShort)

    // Verify values
    assertEqualsFloat(particles(0).pos.x.get, 0.0f, 0.001f)
    assertEqualsFloat(particles(50).pos.x.get, 50.0f, 0.001f)
    assertEqualsFloat(particles(50).pos.y.get, 100.0f, 0.001f)
    assertEquals(particles(99).life.get, 99.toShort)

  test("Named extensions types are inferred correctly"):
    val p = struct[Particle]()

    p.pos.x.set(42.0f)
    p.pos.y.set(84.0f)
    p.life.set(128: Short)

    // Types should be inferred without casts
    val xVal: Float = p.pos.x.get
    val yVal: Float = p.pos.y.get
    val lifeVal: Short = p.life.get

    assertEqualsFloat(xVal, 42.0f, 0.001f)
    assertEqualsFloat(yVal, 84.0f, 0.001f)
    assertEquals(lifeVal, 128.toShort)

  test("Named extensions mixed with index access"):
    val p = struct[Particle]()

    // Can mix named and index-based access
    p.pos.x.set(10.0f) // named
    p(0)(1).set(20.0f) // index-based for y
    p.life.set(50: Short) // named

    assertEqualsFloat(p.pos.x.get, 10.0f, 0.001f)
    assertEqualsFloat(p.pos.y.get, 20.0f, 0.001f) // read via named
    assertEqualsFloat(p(0)(1).get, 20.0f, 0.001f) // same value via index
    assertEquals(p.life.get, 50.toShort)

  test("Assignment operator := works as alias for set"):
    val p = struct[Particle]()

    // Use := instead of .set()
    p.pos.x := 100.0f
    p.pos.y := 200.0f
    p.life := (50: Short)

    assertEqualsFloat(p.pos.x.get, 100.0f, 0.001f)
    assertEqualsFloat(p.pos.y.get, 200.0f, 0.001f)
    assertEquals(p.life.get, 50.toShort)

  test("Apply method () works as alias for get and set"):
    val p = struct[Particle]()

    // Use apply(value) to set
    p.pos.x(100.0f)
    p.pos.y(200.0f)
    p.life(50: Short)

    // Use apply() to get
    val x: Float = p.pos.x()
    val y: Float = p.pos.y()
    val life: Short = p.life()

    assertEqualsFloat(x, 100.0f, 0.001f)
    assertEqualsFloat(y, 200.0f, 0.001f)
    assertEquals(life, 50.toShort)

  // ==========================================================================
  // Phase 8: Bulk Set with ValueTuple
  // ==========================================================================

  test("Bulk set flat struct with := operator"):
    val v = struct[Vec2]()

    v := (10.0f, 20.0f)

    assertEqualsFloat(v.x.get, 10.0f, 0.001f)
    assertEqualsFloat(v.y.get, 20.0f, 0.001f)

  test("Bulk set nested struct with := operator"):
    val p = struct[Particle]()

    p := ((100.0f, 200.0f), 255: Short)

    assertEqualsFloat(p.pos.x.get, 100.0f, 0.001f)
    assertEqualsFloat(p.pos.y.get, 200.0f, 0.001f)
    assertEquals(p.life.get, 255.toShort)

  test("Bulk set deeply nested struct with := operator"):
    val e = struct[Entity]()

    e := (((1.0f, 2.0f), (3.0f, 4.0f)), 100: Short)

    assertEqualsFloat(e.transform.position.x.get, 1.0f, 0.001f)
    assertEqualsFloat(e.transform.position.y.get, 2.0f, 0.001f)
    assertEqualsFloat(e.transform.velocity.x.get, 3.0f, 0.001f)
    assertEqualsFloat(e.transform.velocity.y.get, 4.0f, 0.001f)
    assertEquals(e.health.get, 100.toShort)

  test("Bulk set partial nested struct"):
    val e = struct[Entity]()

    // Set entire entity first
    e := (((10.0f, 20.0f), (30.0f, 40.0f)), 50: Short)

    // Now set just the position within transform
    e.transform.position := (100.0f, 200.0f)

    // Position should be updated
    assertEqualsFloat(e.transform.position.x.get, 100.0f, 0.001f)
    assertEqualsFloat(e.transform.position.y.get, 200.0f, 0.001f)

    // Velocity should remain unchanged
    assertEqualsFloat(e.transform.velocity.x.get, 30.0f, 0.001f)
    assertEqualsFloat(e.transform.velocity.y.get, 40.0f, 0.001f)

    // Health should remain unchanged
    assertEquals(e.health.get, 50.toShort)

  test("Bulk set using explicit .set() method"):
    val p = struct[Particle]()

    // Use explicit .set() instead of := operator
    p.set(((50.0f, 75.0f), 128: Short))

    assertEqualsFloat(p.pos.x.get, 50.0f, 0.001f)
    assertEqualsFloat(p.pos.y.get, 75.0f, 0.001f)
    assertEquals(p.life.get, 128.toShort)

  // ==========================================================================
  // Helper methods
  // ==========================================================================

  private def assertEqualsFloat(
      actual: Float,
      expected: Float,
      delta: Float
  ): Unit =
    assert(
      math.abs(actual - expected) <= delta,
      s"Expected ~$expected within $delta, got $actual"
    )

  private def assertEqualsDouble(
      actual: Double,
      expected: Double,
      delta: Double
  ): Unit =
    assert(
      math.abs(actual - expected) <= delta,
      s"Expected ~$expected within $delta, got $actual"
    )

  // ==========================================================================
  // Iterator and For-Comprehension Tests
  // ==========================================================================

  test("for-comprehension basic iteration"):
    type Particle = (F32, F32, U8)
    val particles = StructArray.allocate[Particle](10)

    // Initialize with direct indexing
    for i <- 0 until 10 do
      particles(i)(0) := i.toFloat * 2.0f
      particles(i)(1) := i.toFloat * 3.0f
      particles(i)(2) := (i * 10).toShort

    // Read back with for-comprehension
    var count = 0
    var sum = 0.0f
    for p <- particles do
      sum += p(0).get
      count += 1

    assertEquals(count, 10)
    assertEqualsFloat(sum, (0 until 10).map(_ * 2.0f).sum, 0.001f)

  test("for-comprehension with guard"):
    type Particle = (F32, U8)
    val particles = StructArray.allocate[Particle](10)

    // Initialize
    for i <- 0 until 10 do
      particles(i)(0) := i.toFloat
      particles(i)(1) := (if i % 2 == 0 then 100 else 0).toShort

    // Filter with guard
    var evenCount = 0
    for p <- particles if p(1).get > 0 do evenCount += 1

    assertEquals(evenCount, 5)

  test("for-comprehension yield"):
    type Vec2 = (F32, F32)
    val vectors = StructArray.allocate[Vec2](5)

    // Initialize
    for i <- 0 until 5 do
      vectors(i)(0) := i.toFloat
      vectors(i)(1) := i.toFloat * 2.0f

    // Collect with yield
    val xValues = (for v <- vectors yield v(0).get).toSeq

    assertEquals(xValues.length, 5)
    assertEqualsFloat(xValues(0), 0.0f, 0.001f)
    assertEqualsFloat(xValues(4), 4.0f, 0.001f)

  test("for-comprehension nested"):
    type Vec2 = (F32, F32)
    val vectors1 = StructArray.allocate[Vec2](3)
    val vectors2 = StructArray.allocate[Vec2](3)

    for i <- 0 until 3 do
      vectors1(i)(0) := i.toFloat
      vectors1(i)(1) := 0.0f
      vectors2(i)(0) := 0.0f
      vectors2(i)(1) := i.toFloat

    // Nested for-comprehension
    val pairs = (for
      v1 <- vectors1
      v2 <- vectors2
    yield (v1(0).get, v2(1).get)).toSeq

    assertEquals(pairs.length, 9)
    assertEqualsFloat(pairs(0)._1, 0.0f, 0.001f)
    assertEqualsFloat(pairs(8)._2, 2.0f, 0.001f)

  // ==========================================================================
  // Inline Foreach Tests
  // ==========================================================================

  test("inline foreach"):
    type Particle = (F32, F32, U8)
    val particles = StructArray.allocate[Particle](10)

    // Initialize
    for i <- 0 until 10 do
      particles(i)(0) := i.toFloat
      particles(i)(1) := i.toFloat * 2.0f
      particles(i)(2) := 100.toShort

    // Update with inline foreach
    particles.foreach { p =>
      p(0) := p(0).get * 0.5f
      p(1) := p(1).get * 0.5f
    }

    // Verify
    for i <- 0 until 10 do
      assertEqualsFloat(particles(i)(0).get, i.toFloat * 0.5f, 0.001f)
      assertEqualsFloat(particles(i)(1).get, i.toFloat, 0.001f)

  test("inline foreach side effects"):
    type Vec2 = (F32, F32)
    val vectors = StructArray.allocate[Vec2](5)

    for i <- 0 until 5 do
      vectors(i)(0) := i.toFloat
      vectors(i)(1) := i.toFloat

    var sum = 0.0f
    vectors.foreach { v =>
      sum += v(0).get + v(1).get
    }

    assertEqualsFloat(sum, (0 until 5).map(i => i * 2.0f).sum, 0.001f)

  test("inline foreach vs manual loop"):
    type Particle = (F32, U8)
    val particles1 = StructArray.allocate[Particle](100)
    val particles2 = StructArray.allocate[Particle](100)

    // Initialize both identically
    for i <- 0 until 100 do
      particles1(i)(0) := i.toFloat
      particles1(i)(1) := (i % 256).toShort
      particles2(i)(0) := i.toFloat
      particles2(i)(1) := (i % 256).toShort

    // Update with foreach
    particles1.foreach { p =>
      p(0) := p(0).get * 2.0f
    }

    // Update with manual loop
    var i = 0
    while i < particles2.length do
      particles2(i)(0) := particles2(i)(0).get * 2.0f
      i += 1

    // Results should be identical
    for i <- 0 until 100 do
      assertEqualsFloat(particles1(i)(0).get, particles2(i)(0).get, 0.001f)

  // ==========================================================================
  // Indices Tests
  // ==========================================================================

  test("indices"):
    type Vec2 = (F32, F32)
    val vectors = StructArray.allocate[Vec2](10)

    for i <- vectors.indices do
      vectors(i)(0) := i.toFloat
      vectors(i)(1) := i.toFloat * 2.0f

    for i <- vectors.indices do
      assertEqualsFloat(vectors(i)(0).get, i.toFloat, 0.001f)
      assertEqualsFloat(vectors(i)(1).get, i.toFloat * 2.0f, 0.001f)

  // ==========================================================================
  // Iterable Interop Tests
  // ==========================================================================

  test("pass to iterable function"):
    type Vec2 = (F32, F32)
    val vectors = StructArray.allocate[Vec2](5)

    for i <- 0 until 5 do
      vectors(i)(0) := i.toFloat
      vectors(i)(1) := i.toFloat * 2.0f

    // Function expecting Iterable
    def sumFirstComponents(items: Iterable[StructRef[(F32, F32)]]): Float =
      var sum = 0.0f
      for item <- items do sum += item(0).get
      sum

    val result = sumFirstComponents(vectors)
    assertEqualsFloat(result, (0 until 5).map(_.toFloat).sum, 0.001f)

  test("iterator map filter"):
    type Particle = (F32, U8)
    val particles = StructArray.allocate[Particle](10)

    for i <- 0 until 10 do
      particles(i)(0) := i.toFloat
      particles(i)(1) := (if i % 2 == 0 then 100 else 0).toShort

    // Use iterator methods
    val evenPositions = particles.iterator
      .filter(p => p(1).get > 0)
      .map(p => p(0).get)
      .toSeq

    assertEquals(evenPositions.length, 5)
    assertEqualsFloat(evenPositions(0), 0.0f, 0.001f)
    assertEqualsFloat(evenPositions(1), 2.0f, 0.001f)

  test("hittable list pattern"):
    // Simulate the HittableList pattern from the plan

    trait Hittable:
      def value: Float

    // Wrapper that makes StructRef implement Hittable
    class HittableWrapper(ref: StructRef[F32 *: EmptyTuple]) extends Hittable:
      def value: Float = ref(0).get

    class HittableList(items: Iterable[Hittable]):
      def sum: Float =
        var total = 0.0f
        for item <- items do total += item.value
        total

    // Create StructArray and wrap elements
    type ValueStruct = F32 *: EmptyTuple
    val values = StructArray.allocate[ValueStruct](5)

    for i <- 0 until 5 do values(i)(0) := i.toFloat * 10.0f

    // Pass wrapped iterator to HittableList
    val hittables = new HittableList(
      values.iterator.map(ref => new HittableWrapper(ref)).toSeq
    )

    assertEqualsFloat(hittables.sum, (0 until 5).map(_ * 10.0f).sum, 0.001f)
