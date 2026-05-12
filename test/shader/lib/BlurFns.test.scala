package trivalibs.graphics.shader.lib.blur

import trivalibs.graphics.shader.dsl.WgslFnData
import munit.FunSuite

class BlurFnsTest extends FunSuite:

  test("gaussianBlur emits diameter-driven loop"):
    val data = Blur.gaussianBlur.asInstanceOf[WgslFnData]
    assert(
      data.src.contains(
        "fn gaussian_blur(tex: texture_2d<f32>, s: sampler, diameter: f32",
      ),
      data.src,
    )
    assert(data.src.contains("-> vec4<f32>"), data.src)
    assert(data.src.contains("let sigma = diameter * 0.25"), data.src)
    assert(data.src.contains("ceil(sigma * 1.5)"), data.src)
    assert(data.src.contains("while (i <= support)"), data.src)

  test("gaussianBlur5 emits 5-tap precomputed weights"):
    val data = Blur.gaussianBlur5.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn gaussian_blur_5("), data.src)
    assert(data.src.contains("1.3333333333333333"), data.src)
    assert(data.src.contains("0.29411764705882354"), data.src)
    assert(data.src.contains("0.35294117647058826"), data.src)

  test("gaussianBlur9 emits 9-tap precomputed weights"):
    val data = Blur.gaussianBlur9.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn gaussian_blur_9("), data.src)
    assert(data.src.contains("1.3846153846"), data.src)
    assert(data.src.contains("3.2307692308"), data.src)
    assert(data.src.contains("0.2270270270"), data.src)
    assert(data.src.contains("0.0702702703"), data.src)

  test("gaussianBlur13 emits 13-tap precomputed weights"):
    val data = Blur.gaussianBlur13.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn gaussian_blur_13("), data.src)
    assert(data.src.contains("1.411764705882353"), data.src)
    assert(data.src.contains("3.2941176470588234"), data.src)
    assert(data.src.contains("5.176470588235294"), data.src)
    assert(data.src.contains("0.010381362401148057"), data.src)

  test("boxBlur emits diameter-driven uniform-weight loop"):
    val data = Blur.boxBlur.asInstanceOf[WgslFnData]
    assert(data.src.contains("fn box_blur("), data.src)
    assert(data.src.contains("floor(diameter * 0.5)"), data.src)
    assert(data.src.contains("while (i <= support)"), data.src)
    assert(data.src.contains("sum / (1.0 + f32(support) * 2.0)"), data.src)
