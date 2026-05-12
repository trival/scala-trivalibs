package trivalibs.graphics.shader.lib.random

import trivalibs.graphics.shader.dsl.WgslFnData
import munit.FunSuite

class PsrdnoiseFnsTest extends FunSuite:

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  test("mod289v3f emits correct signature and body"):
    val data = Psrdnoise.mod289v3f.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn mod289v3f(x: vec3<f32>)"), data.src)
    assert(data.src.contains("-> vec3<f32>"), data.src)
    assert(data.src.contains("floor(x / 289.0)"), data.src)
    assertEquals(data.deps.length, 0)

  test("mod289v4f emits correct signature and body"):
    val data = Psrdnoise.mod289v4f.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn mod289v4f(x: vec4<f32>)"), data.src)
    assert(data.src.contains("-> vec4<f32>"), data.src)
    assertEquals(data.deps.length, 0)

  test("permute289v4f emits correct signature and depends on mod289v4f"):
    val data = Psrdnoise.permute289v4f.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn permute289v4f(i: vec4<f32>)"), data.src)
    assert(data.src.contains("mod289v4f(i)"), data.src)
    assert(data.src.contains("34.0 + 10.0"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("mod289v4f"), depNames.toString)

  // ---------------------------------------------------------------------------
  // 2D
  // ---------------------------------------------------------------------------

  test("tilingRotNoise2d emits correct signature"):
    val data = Psrdnoise.tilingRotNoise2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn tiling_rot_noise_2d(pos: vec2<f32>, period: vec2<f32>, normRot: f32)"), data.src)
    assert(data.src.contains("-> vec3<f32>"), data.src)

  test("tilingRotNoise2d converts normRot to alpha in radians"):
    val data = Psrdnoise.tilingRotNoise2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("let alpha = normRot * 6.28318530718"), data.src)

  test("tilingRotNoise2d contains period tiling branch"):
    val data = Psrdnoise.tilingRotNoise2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("any(period > vec2<f32>(0.0, 0.0))"), data.src)
    assert(data.src.contains("period.x > 0.0"), data.src)
    assert(data.src.contains("period.y > 0.0"), data.src)

  test("tilingRotNoise2d returns gradient packed as vec3"):
    val data = Psrdnoise.tilingRotNoise2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("return vec3<f32>(n, grad.x, grad.y)"), data.src)

  test("tilingRotNoise2d depends on mod289v3f"):
    val data = Psrdnoise.tilingRotNoise2d.asInstanceOf[WgslFnData]
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("mod289v3f"), depNames.toString)

  test("tilingNoise2d calls tiling_rot_noise_2d with zero rotation"):
    val data = Psrdnoise.tilingNoise2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn tiling_noise_2d(pos: vec2<f32>, period: vec2<f32>)"), data.src)
    assert(data.src.contains("tiling_rot_noise_2d(pos, period, 0.0)"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("tiling_rot_noise_2d"), depNames.toString)

  test("rotNoise2d calls tiling_rot_noise_2d with zero period"):
    val data = Psrdnoise.rotNoise2d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn rot_noise_2d(pos: vec2<f32>, normRot: f32)"), data.src)
    assert(data.src.contains("tiling_rot_noise_2d(pos, vec2<f32>(0.0, 0.0), normRot)"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("tiling_rot_noise_2d"), depNames.toString)

  // ---------------------------------------------------------------------------
  // 3D
  // ---------------------------------------------------------------------------

  test("tilingRotNoise3d emits correct signature"):
    val data = Psrdnoise.tilingRotNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn tiling_rot_noise_3d(pos: vec3<f32>, period: vec3<f32>, normRot: f32)"), data.src)
    assert(data.src.contains("-> vec4<f32>"), data.src)

  test("tilingRotNoise3d converts normRot to alpha in radians"):
    val data = Psrdnoise.tilingRotNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("let alpha = normRot * 6.28318530718"), data.src)

  test("tilingRotNoise3d uses FCC lattice matrices"):
    val data = Psrdnoise.tilingRotNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("let M = mat3x3<f32>"), data.src)
    assert(data.src.contains("let Mi = mat3x3<f32>"), data.src)

  test("tilingRotNoise3d contains period tiling branch"):
    val data = Psrdnoise.tilingRotNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("any(period > vec3<f32>(0.0))"), data.src)

  test("tilingRotNoise3d contains rotation branch"):
    val data = Psrdnoise.tilingRotNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("if(alpha != 0.0)"), data.src)

  test("tilingRotNoise3d returns gradient packed as vec4"):
    val data = Psrdnoise.tilingRotNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("return vec4<f32>(n, grad.x, grad.y, grad.z)"), data.src)

  test("tilingRotNoise3d depends on permute289v4f"):
    val data = Psrdnoise.tilingRotNoise3d.asInstanceOf[WgslFnData]
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("permute289v4f"), depNames.toString)

  test("tilingNoise3d calls tiling_rot_noise_3d with zero rotation"):
    val data = Psrdnoise.tilingNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn tiling_noise_3d(pos: vec3<f32>, period: vec3<f32>)"), data.src)
    assert(data.src.contains("tiling_rot_noise_3d(pos, period, 0.0)"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("tiling_rot_noise_3d"), depNames.toString)

  test("rotNoise3d calls tiling_rot_noise_3d with zero period"):
    val data = Psrdnoise.rotNoise3d.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn rot_noise_3d(pos: vec3<f32>, normRot: f32)"), data.src)
    assert(data.src.contains("tiling_rot_noise_3d(pos, vec3<f32>(0.0, 0.0, 0.0), normRot)"), data.src)
    val depNames = data.deps.map(_.name).toSeq
    assert(depNames.contains("tiling_rot_noise_3d"), depNames.toString)

  // ---------------------------------------------------------------------------
  // Dep chain: tilingNoise2d → tilingRotNoise2d → mod289v3f
  // ---------------------------------------------------------------------------

  test("tilingNoise2d dep chain reaches mod289v3f transitively"):
    import trivalibs.graphics.shader.dsl.{Program, WgslFn}
    import trivalibs.graphics.math.cpu.*
    import trivalibs.graphics.math.gpu.*
    import trivalibs.graphics.shader.{given}
    val prog = Program[EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple]()
    prog.fn(Psrdnoise.tilingNoise2d)
    val src = prog.helperFnsStr
    assert(src.contains("fn mod289v3f"), src)
    assert(src.contains("fn tiling_rot_noise_2d"), src)
    assert(src.contains("fn tiling_noise_2d"), src)
    assert(src.indexOf("fn mod289v3f") < src.indexOf("fn tiling_rot_noise_2d"), src)
    assert(src.indexOf("fn tiling_rot_noise_2d") < src.indexOf("fn tiling_noise_2d"), src)

  test("tilingNoise3d dep chain reaches mod289v4f and permute289v4f transitively"):
    import trivalibs.graphics.shader.dsl.{Program, WgslFn}
    import trivalibs.graphics.math.cpu.*
    import trivalibs.graphics.math.gpu.*
    import trivalibs.graphics.shader.{given}
    val prog = Program[EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple]()
    prog.fn(Psrdnoise.tilingNoise3d)
    val src = prog.helperFnsStr
    assert(src.contains("fn mod289v4f"), src)
    assert(src.contains("fn permute289v4f"), src)
    assert(src.contains("fn tiling_rot_noise_3d"), src)
    assert(src.contains("fn tiling_noise_3d"), src)
    assert(src.indexOf("fn mod289v4f") < src.indexOf("fn permute289v4f"), src)
    assert(src.indexOf("fn permute289v4f") < src.indexOf("fn tiling_rot_noise_3d"), src)
    assert(src.indexOf("fn tiling_rot_noise_3d") < src.indexOf("fn tiling_noise_3d"), src)
