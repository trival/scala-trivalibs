package trivalibs.graphics.painter

import org.scalajs.dom.HTMLCanvasElement
import trivalibs.bufferdata.StructArray
import trivalibs.graphics.buffers.*
import trivalibs.graphics.geometry.BufferedGeometry
import trivalibs.graphics.math.*
import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.*
import trivalibs.graphics.shader.dsl.LayerProgram
import trivalibs.graphics.shader.dsl.Program
import trivalibs.utils.events.InputState
import trivalibs.utils.events.interactiveCanvas
import trivalibs.utils.js.*

import scala.compiletime.erasedValue
import scala.scalajs.js

private val LAYER_VERT_BODY =
  """  let x = f32((in.vertex_index << 1u) & 2u) * 2.0 - 1.0;
  let y = f32(in.vertex_index & 2u) * 2.0 - 1.0;
  out.uv = vec2f(x * 0.5 + 0.5, 0.5 - y * 0.5);
  out.position = vec4f(x, y, 0.0, 1.0);"""

private val BLIT_WGSL = """
struct VsOut {
  @builtin(position) pos: vec4f,
  @location(0) uv: vec2f,
}

@vertex
fn vs_main(@builtin(vertex_index) vi: u32) -> VsOut {
  let x = f32((vi << 1u) & 2u) * 2.0 - 1.0;
  let y = f32(vi & 2u) * 2.0 - 1.0;
  var out: VsOut;
  out.pos = vec4f(x, y, 0.0, 1.0);
  out.uv = vec2f(x * 0.5 + 0.5, 0.5 - y * 0.5);
  return out;
}

@group(0) @binding(0) var blit_texture: texture_2d<f32>;
@group(0) @binding(1) var blit_sampler: sampler;

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4f {
  return textureSample(blit_texture, blit_sampler, in.uv);
}
"""

class Painter(
    val device: GPUDevice,
    val queue: GPUQueue,
    val canvas: HTMLCanvasElement,
    val context: GPUCanvasContext,
    val preferredFormat: String,
):
  private val pipelineCache = Dict[GPURenderPipeline]()
  private var nextShadeId = 0
  private val resizeCallbacks = Arr[(Double, Double) => Unit]()

  def onResize(cb: (Double, Double) => Unit): Unit =
    resizeCallbacks.push(cb)
    cb(canvas.width, canvas.height)

  private[painter] def fireResize(w: Double, h: Double): Unit =
    var k = 0
    while k < resizeCallbacks.length do
      resizeCallbacks(k)(w, h)
      k += 1

  def width: Int = canvas.width
  def height: Int = canvas.height

  /** Convenience: set up an [[InputState]] for this painter's canvas with the
    * interactive-canvas defaults (focusable, cleared outline, focus on
    * pointer-down, initial focus). See [[interactiveCanvas]].
    */
  def input(
      initialFocus: Boolean = true,
      holdDelay: Double = 400.0,
      holdRadius: Double = 5.0,
      suppressContextMenu: Boolean = true,
      onActivity: Maybe[js.Function0[Unit]] = Maybe.Not,
  ): InputState =
    interactiveCanvas(
      canvas,
      initialFocus,
      holdDelay,
      holdRadius,
      suppressContextMenu,
      onActivity,
    )

  // =========================================================================
  // Shared samplers
  // =========================================================================

  lazy val samplerNearest: GPUSampler =
    device.createSampler(
      Obj.literal(magFilter = "nearest", minFilter = "nearest"),
    )

  lazy val samplerLinear: GPUSampler =
    device.createSampler(
      Obj.literal(magFilter = "linear", minFilter = "linear"),
    )

  def sampler(
      magFilter: FilterMode = FilterMode.Nearest,
      minFilter: FilterMode = FilterMode.Nearest,
      mipmapFilter: FilterMode = FilterMode.Nearest,
  ): GPUSampler =
    device.createSampler(
      Obj.literal(
        magFilter = magFilter.toJs,
        minFilter = minFilter.toJs,
        mipmapFilter = mipmapFilter.toJs,
      ),
    )

  // =========================================================================
  // Shade factory
  // =========================================================================

  // DSL overload — accepts a Program builder lambda (no panels)
  inline def shade[A, V, U](
      build: Program[A, V, U, EmptyTuple, FragOut] => Unit,
  ): Shade[U, EmptyTuple] =
    val program = new Program[A, V, U, EmptyTuple, FragOut]
    build(program)
    shadeFromWgsl[A, V, U, EmptyTuple](
      program.vertBodyStr,
      program.fragBodyStr,
      program.helperFnsStr,
    )

  // DSL overload — with panels P
  inline def shade[A, V, U, P](
      build: Program[A, V, U, P, FragOut] => Unit,
  ): Shade[U, P] =
    val program = new Program[A, V, U, P, FragOut]
    build(program)
    shadeFromWgsl[A, V, U, P](
      program.vertBodyStr,
      program.fragBodyStr,
      program.helperFnsStr,
    )

  // DSL overload — with panels P and custom fragment output FO
  inline def shade[A, V, U, P, FO](
      build: Program[A, V, U, P, FO] => Unit,
  ): Shade[U, P] =
    val program = new Program[A, V, U, P, FO]
    build(program)
    shadeFromWgslFO[A, V, U, P, FO](
      program.vertBodyStr,
      program.fragBodyStr,
      program.helperFnsStr,
    )

  // Raw WGSL string overload
  inline def shade[A, V, U](
      vertWgsl: String,
      fragWgsl: String,
  ): Shade[U, EmptyTuple] =
    shadeFromWgsl[A, V, U, EmptyTuple](vertWgsl, fragWgsl, "")

  private inline def buildIndices[T]: Dict[Int] =
    val names = derive.fieldNames[T]
    val dict = Dict[Int]()
    var i = 0
    while i < names.length do
      dict.asInstanceOf[js.Dynamic].updateDynamic(names(i))(i)
      i += 1
    dict

  private inline def shadeFromWgsl[A, V, U, P](
      vertWgsl: String,
      fragWgsl: String,
      helperFns: String,
  ): Shade[U, P] =
    val id = nextShadeId
    nextShadeId += 1
    val uniformIndices = buildIndices[U]
    val panelIndices = buildIndices[P]

    val panelDecls = derive.generatePanelDeclarations[P]

    inline erasedValue[U] match
      case _: EmptyTuple =>
        val sd = Shader[A, V, EmptyTuple](vertWgsl, fragWgsl, helperFns)
        val baseWgsl = sd.generateWGSL
        val wgsl =
          if panelDecls.isEmpty then baseWgsl else s"$baseWgsl\n\n$panelDecls"
        log(wgsl)
        val module = device.createShaderModule(Obj.literal(code = wgsl))
        val vbl = sd.vertexBufferLayout
        val panelBgl = layouts.createPanelBindGroupLayout[P](device)
        val bgls =
          if panelBgl.notNull then Arr[GPUBindGroupLayout](panelBgl)
          else Arr[GPUBindGroupLayout]()
        val pl = layouts.createPipelineLayout(device, bgls)
        Shade[U, P](
          id,
          module,
          vbl,
          null,
          panelBgl,
          pl,
          false,
          uniformIndices,
          panelIndices,
        )
      case _ =>
        type Wrapped = (values: U)
        val sd = Shader[A, V, Wrapped](vertWgsl, fragWgsl, helperFns)
        val baseWgsl = sd.generateWGSL
        val wgsl =
          if panelDecls.isEmpty then baseWgsl else s"$baseWgsl\n\n$panelDecls"
        log(wgsl)
        val module = device.createShaderModule(Obj.literal(code = wgsl))
        val vbl = sd.vertexBufferLayout
        val (bgls, _) = sd.createPipelineLayout(device)
        val panelBgl = layouts.createPanelBindGroupLayout[P](device)
        val allBgls =
          if panelBgl.notNull then bgls ++ Arr[GPUBindGroupLayout](panelBgl)
          else bgls
        val pl = layouts.createPipelineLayout(device, allBgls)
        Shade[U, P](
          id,
          module,
          vbl,
          bgls(0),
          panelBgl,
          pl,
          false,
          uniformIndices,
          panelIndices,
        )

  private inline def shadeFromWgslFO[A, V, U, P, FO](
      vertWgsl: String,
      fragWgsl: String,
      helperFns: String,
  ): Shade[U, P] =
    val id = nextShadeId
    nextShadeId += 1
    val uniformIndices = buildIndices[U]
    val panelIndices = buildIndices[P]

    val panelDecls = derive.generatePanelDeclarations[P]

    inline erasedValue[U] match
      case _: EmptyTuple =>
        val sd = Shader.full[A, V, EmptyTuple, None, VertOut, None, FO](
          vertWgsl,
          fragWgsl,
          helperFns,
        )
        val baseWgsl = sd.generateWGSL
        val wgsl =
          if panelDecls.isEmpty then baseWgsl else s"$baseWgsl\n\n$panelDecls"
        log(wgsl)
        val module = device.createShaderModule(Obj.literal(code = wgsl))
        val vbl = sd.vertexBufferLayout
        val panelBgl = layouts.createPanelBindGroupLayout[P](device)
        val bgls =
          if panelBgl.notNull then Arr[GPUBindGroupLayout](panelBgl)
          else Arr[GPUBindGroupLayout]()
        val pl = layouts.createPipelineLayout(device, bgls)
        Shade[U, P](
          id,
          module,
          vbl,
          null,
          panelBgl,
          pl,
          false,
          uniformIndices,
          panelIndices,
        )
      case _ =>
        type Wrapped = (values: U)
        val sd = Shader.full[A, V, Wrapped, None, VertOut, None, FO](
          vertWgsl,
          fragWgsl,
          helperFns,
        )
        val baseWgsl = sd.generateWGSL
        val wgsl =
          if panelDecls.isEmpty then baseWgsl else s"$baseWgsl\n\n$panelDecls"
        log(wgsl)
        val module = device.createShaderModule(Obj.literal(code = wgsl))
        val vbl = sd.vertexBufferLayout
        val (bgls, _) = sd.createPipelineLayout(device)
        val panelBgl = layouts.createPanelBindGroupLayout[P](device)
        val allBgls =
          if panelBgl.notNull then bgls ++ Arr[GPUBindGroupLayout](panelBgl)
          else bgls
        val pl = layouts.createPipelineLayout(device, allBgls)
        Shade[U, P](
          id,
          module,
          vbl,
          bgls(0),
          panelBgl,
          pl,
          false,
          uniformIndices,
          panelIndices,
        )

  // =========================================================================
  // LayerShade factory — fullscreen triangle with user fragment shader
  // =========================================================================

  inline def layerShade[U](
      build: LayerProgram[U, EmptyTuple, FragOut] => Unit,
  ): Shade[U, EmptyTuple] =
    val program = new LayerProgram[U, EmptyTuple, FragOut]
    build(program)
    layerShadeFromWgsl[U, EmptyTuple](program.fragBodyStr, program.helperFnsStr)

  inline def layerShade[U, P](
      build: LayerProgram[U, P, FragOut] => Unit,
  ): Shade[U, P] =
    val program = new LayerProgram[U, P, FragOut]
    build(program)
    layerShadeFromWgsl[U, P](program.fragBodyStr, program.helperFnsStr)

  inline def layerShade[U, P, FO](
      build: LayerProgram[U, P, FO] => Unit,
  ): Shade[U, P] =
    val program = new LayerProgram[U, P, FO]
    build(program)
    layerShadeFromWgslFO[U, P, FO](
      program.fragBodyStr,
      program.helperFnsStr,
    )

  private inline def layerShadeFromWgsl[U, P](
      fragWgsl: String,
      helperFns: String,
  ): Shade[U, P] =
    val id = nextShadeId
    nextShadeId += 1
    val uniformIndices = buildIndices[U]
    val panelIndices = buildIndices[P]
    type VBI = (vertex_index: BuiltinVertexIndex)
    val panelDecls = derive.generatePanelDeclarations[P]
    inline erasedValue[U] match
      case _: EmptyTuple =>
        val sd = Shader.full[
          EmptyTuple,
          (uv: Vec2),
          EmptyTuple,
          VBI,
          VertOut,
          EmptyTuple,
          FragOut,
        ](LAYER_VERT_BODY, fragWgsl, helperFns)
        val baseWgsl = sd.generateWGSL
        val wgsl =
          if panelDecls.isEmpty then baseWgsl else s"$baseWgsl\n\n$panelDecls"
        log(wgsl)
        val module = device.createShaderModule(Obj.literal(code = wgsl))
        val panelBgl = layouts.createPanelBindGroupLayout[P](device)
        val bgls =
          if panelBgl.notNull then Arr[GPUBindGroupLayout](panelBgl)
          else Arr[GPUBindGroupLayout]()
        val pl = layouts.createPipelineLayout(device, bgls)
        Shade[U, P](
          id,
          module,
          null,
          null,
          panelBgl,
          pl,
          false,
          uniformIndices,
          panelIndices,
        )
      case _ =>
        import scala.NamedTuple.AnyNamedTuple
        type FragU = NamedTuple.Map[U & AnyNamedTuple, derive.WrapFragment]
        type Wrapped = (values: FragU)
        val sd = Shader.full[
          EmptyTuple,
          (uv: Vec2),
          Wrapped,
          VBI,
          VertOut,
          EmptyTuple,
          FragOut,
        ](LAYER_VERT_BODY, fragWgsl, helperFns)
        val baseWgsl = sd.generateWGSL
        val wgsl =
          if panelDecls.isEmpty then baseWgsl else s"$baseWgsl\n\n$panelDecls"
        log(wgsl)
        val module = device.createShaderModule(Obj.literal(code = wgsl))
        val (bgls, _) = sd.createPipelineLayout(device)
        val panelBgl = layouts.createPanelBindGroupLayout[P](device)
        val allBgls =
          if panelBgl.notNull then bgls ++ Arr[GPUBindGroupLayout](panelBgl)
          else bgls
        val pl = layouts.createPipelineLayout(device, allBgls)
        Shade[U, P](
          id,
          module,
          null,
          bgls(0),
          panelBgl,
          pl,
          false,
          uniformIndices,
          panelIndices,
        )

  private inline def layerShadeFromWgslFO[U, P, FO](
      fragWgsl: String,
      helperFns: String,
  ): Shade[U, P] =
    val id = nextShadeId
    nextShadeId += 1
    val uniformIndices = buildIndices[U]
    val panelIndices = buildIndices[P]
    type VBI = (vertex_index: BuiltinVertexIndex)
    val panelDecls = derive.generatePanelDeclarations[P]
    inline erasedValue[U] match
      case _: EmptyTuple =>
        val sd = Shader.full[
          EmptyTuple,
          (uv: Vec2),
          EmptyTuple,
          VBI,
          VertOut,
          EmptyTuple,
          FO,
        ](LAYER_VERT_BODY, fragWgsl, helperFns)
        val baseWgsl = sd.generateWGSL
        val wgsl =
          if panelDecls.isEmpty then baseWgsl else s"$baseWgsl\n\n$panelDecls"
        log(wgsl)
        val module = device.createShaderModule(Obj.literal(code = wgsl))
        val panelBgl = layouts.createPanelBindGroupLayout[P](device)
        val bgls =
          if panelBgl.notNull then Arr[GPUBindGroupLayout](panelBgl)
          else Arr[GPUBindGroupLayout]()
        val pl = layouts.createPipelineLayout(device, bgls)
        Shade[U, P](
          id,
          module,
          null,
          null,
          panelBgl,
          pl,
          false,
          uniformIndices,
          panelIndices,
        )
      case _ =>
        import scala.NamedTuple.AnyNamedTuple
        type FragU = NamedTuple.Map[U & AnyNamedTuple, derive.WrapFragment]
        type Wrapped = (values: FragU)
        val sd = Shader.full[
          EmptyTuple,
          (uv: Vec2),
          Wrapped,
          VBI,
          VertOut,
          EmptyTuple,
          FO,
        ](LAYER_VERT_BODY, fragWgsl, helperFns)
        val baseWgsl = sd.generateWGSL
        val wgsl =
          if panelDecls.isEmpty then baseWgsl else s"$baseWgsl\n\n$panelDecls"
        log(wgsl)
        val module = device.createShaderModule(Obj.literal(code = wgsl))
        val (bgls, _) = sd.createPipelineLayout(device)
        val panelBgl = layouts.createPanelBindGroupLayout[P](device)
        val allBgls =
          if panelBgl.notNull then bgls ++ Arr[GPUBindGroupLayout](panelBgl)
          else bgls
        val pl = layouts.createPipelineLayout(device, allBgls)
        Shade[U, P](
          id,
          module,
          null,
          bgls(0),
          panelBgl,
          pl,
          false,
          uniformIndices,
          panelIndices,
        )

  // =========================================================================
  // Form factory
  // =========================================================================

  def form[F <: Tuple](
      geometry: Maybe[BufferedGeometry[F]] = Maybe.Not,
      vertices: Maybe[StructArray[F]] = Maybe.Not,
      topology: Maybe[PrimitiveTopology] = Maybe.Not,
      frontFace: Maybe[FrontFace] = Maybe.Not,
  ): Form = Form(this).set(geometry, vertices, topology, frontFace)

  // =========================================================================
  // Binding factory
  // =========================================================================

  inline def binding[T: UniformLayout as ul](
      value: T,
  ): BufferBinding[T, ul.Fields] =
    BufferBinding[T](device, value)

  inline def binding[T: UniformLayout as ul]: BufferBinding[T, ul.Fields] =
    BufferBinding[T](device)

  // =========================================================================
  // Shape factory
  // =========================================================================

  def shape[U, P](
      form: Form,
      shade: Shade[U, P],
      cullMode: Maybe[CullMode] = Maybe.Not,
      blendState: Maybe[Opt[BlendState]] = Maybe.Not,
  ): Shape[U, P] =
    Shape[U, P](this, form, shade).set(cullMode, blendState)

  // =========================================================================
  // Layer factory
  // =========================================================================

  def layer[U, P](
      shade: Shade[U, P],
      blendState: Maybe[Opt[BlendState]] = Maybe.Not,
      mipSource: Maybe[Int] = Maybe.Not,
      mipTarget: Maybe[Int] = Maybe.Not,
  ): Layer[U, P] =
    Layer[U, P](this, shade).set(blendState, mipSource, mipTarget)

  // =========================================================================
  // Panel factory
  // =========================================================================

  def panel[S <: AnyShape, L <: AnyLayer](
      width: Maybe[Int] = Maybe.Not,
      height: Maybe[Int] = Maybe.Not,
      clearColor: Maybe[Opt[ClearColor]] = Maybe.Not,
      depthTest: Maybe[Boolean] = Maybe.Not,
      multisample: Maybe[Boolean] = Maybe.Not,
      mipLevels: Maybe[Int] = Maybe.Not,
      mips: Maybe[Boolean] = Maybe.Not,
      format: Maybe[String] = Maybe.Not,
      formats: Maybe[Arr[String]] = Maybe.Not,
      shape: Maybe[S] = Maybe.Not,
      shapes: Maybe[Arr[S]] = Maybe.Not,
      layer: Maybe[L] = Maybe.Not,
      layers: Maybe[Arr[L]] = Maybe.Not,
  ): Panel = Panel(this).set(
    width = width,
    height = height,
    clearColor = clearColor,
    depthTest = depthTest,
    multisample = multisample,
    mipLevels = mipLevels,
    mips = mips,
    format = format,
    formats = formats,
    shape = shape,
    shapes = shapes,
    layer = layer,
    layers = layers,
  )

  // =========================================================================
  // draw() — direct-to-canvas rendering
  // =========================================================================

  def draw(
      shape: AnyShape,
      clearColor: Opt[(Double, Double, Double, Double)] = null,
  ): Unit =
    val encoder = device.createCommandEncoder()
    val textureView = context.getCurrentTexture().createView()

    val colorAttachment =
      if clearColor.notNull then
        val (r, g, b, a) = clearColor
        Obj.literal(
          view = textureView,
          loadOp = "clear",
          storeOp = "store",
          clearValue = Obj.literal(r = r, g = g, b = b, a = a),
        )
      else
        Obj.literal(
          view = textureView,
          loadOp = "load",
          storeOp = "store",
        )

    val pass = encoder.beginRenderPass(
      Obj.literal(colorAttachments = Arr(colorAttachment)),
    )

    renderShapeOnPass(pass, shape)
    pass.end()

    queue.submit(Arr(encoder.finish()))

  // =========================================================================
  // paint() / show() — Panel-based rendering
  // =========================================================================

  private def paintPanel(panel: Panel): Unit =
    val w = width
    val h = height
    panel.ensureSize(w, h)
    val msaa = panel.multisample

    // Step 1: render shapes into main texture(s)
    val encoder = device.createCommandEncoder()
    val panelFormats = panel.effectiveFormats

    val colorAttachments = Arr[js.Dynamic]()
    var t = 0
    while t < panel.targetCount do
      val attachment =
        if panel.clearColor.notNull then
          val (r, g, b, a) = panel.clearColor
          if msaa then
            Obj.literal(
              view = panel.msaaViewAt(t),
              resolveTarget = panel.renderViewAt(t),
              loadOp = "clear",
              storeOp = "discard",
              clearValue = Obj.literal(r = r, g = g, b = b, a = a),
            )
          else
            Obj.literal(
              view = panel.renderViewAt(t),
              loadOp = "clear",
              storeOp = "store",
              clearValue = Obj.literal(r = r, g = g, b = b, a = a),
            )
        else if msaa then
          Obj.literal(
            view = panel.msaaViewAt(t),
            resolveTarget = panel.renderViewAt(t),
            loadOp = "load",
            storeOp = "store",
          )
        else
          Obj.literal(
            view = panel.renderViewAt(t),
            loadOp = "load",
            storeOp = "store",
          )
      colorAttachments.push(attachment)
      t += 1

    val passDesc: js.Dynamic =
      Obj.literal(colorAttachments = colorAttachments)
    if panel.depthTest then
      passDesc.depthStencilAttachment = Obj.literal(
        view = panel.depthView,
        depthLoadOp = "clear",
        depthStoreOp = "store",
        depthClearValue = 1.0,
      )

    // Step 1: Render shapes (with depth/msaa from panel config)
    val shapePass = encoder.beginRenderPass(passDesc)

    var i = 0
    while i < panel.shapes.length do
      renderShapeOnPass(
        shapePass,
        panel.shapes(i),
        panel.depthTest,
        msaa,
        panelFormats,
        panel,
      )
      i += 1

    shapePass.end()
    queue.submit(Arr(encoder.finish()))

    // Step 2: Render layers in order — no depth, no msaa.
    // Layers are fullscreen quads that always render on top.
    // Consecutive non-ping-pong layers share a pass; ping-pong forces a new one.

    var srcView = panel.textureView
    var dstView = panel.pongView
    var hasPongLayers = false

    var curEncoder: Opt[GPUCommandEncoder] = null
    var curPass: Opt[GPURenderPassEncoder] = null

    var j = 0
    while j < panel.layers.length do
      val layer = panel.layers(j)
      val hasPanelLayout = layer.shade.panelBindGroupLayout.notNull
      val slot0Manual = hasPanelLayout &&
        layer.panelBindings.length > 0 && layer.panelBindings(0).notNull
      val needsPingPong = hasPanelLayout && !slot0Manual
      val hasMipTarget = layer.mipTarget >= 0

      if hasMipTarget then
        // Mip-targeted layer: render to specific mip level, no ping-pong
        if curPass.notNull then
          curPass.end()
          queue.submit(Arr(curEncoder.finish()))
          curPass = null

        val mipDstView = panel.textureViewAt(0, layer.mipTarget)
        val mipSrcView =
          if layer.mipSource >= 0 then panel.textureViewAt(0, layer.mipSource)
          else srcView

        val enc = device.createCommandEncoder()
        val mipPass = enc.beginRenderPass(
          Obj.literal(
            colorAttachments = Arr(
              Obj.literal(
                view = mipDstView,
                loadOp = "load",
                storeOp = "store",
              ),
            ),
          ),
        )
        renderLayerOnPass(
          mipPass,
          layer,
          formats = panelFormats,
          srcView = mipSrcView,
          panel = panel,
        )
        mipPass.end()
        queue.submit(Arr(enc.finish()))
      else if needsPingPong then
        hasPongLayers = true
        // End current pass if open, submit
        if curPass.notNull then
          curPass.end()
          queue.submit(Arr(curEncoder.finish()))
          curPass = null

        // Ping-pong pass: render to dstView, inject srcView at slot 0
        val enc = device.createCommandEncoder()
        val ppPass = enc.beginRenderPass(
          Obj.literal(
            colorAttachments = Arr(
              Obj.literal(
                view = dstView,
                loadOp = "load",
                storeOp = "store",
              ),
            ),
          ),
        )
        renderLayerOnPass(
          ppPass,
          layer,
          formats = panelFormats,
          srcView = srcView,
          panel = panel,
        )
        ppPass.end()
        queue.submit(Arr(enc.finish()))

        val tmp = srcView
        srcView = dstView
        dstView = tmp
      else
        // Lazily open a pass on srcView if none is open
        if curPass.isNull then
          curEncoder = device.createCommandEncoder()
          curPass = curEncoder.beginRenderPass(
            Obj.literal(
              colorAttachments = Arr(
                Obj.literal(
                  view = srcView,
                  loadOp = "load",
                  storeOp = "store",
                ),
              ),
            ),
          )
        renderLayerOnPass(curPass, layer, formats = panelFormats, panel = panel)

      j += 1

    if curPass.notNull then
      curPass.end()
      queue.submit(Arr(curEncoder.finish()))

    // Record the final output view for show()
    if hasPongLayers then panel.setOutputView(srcView)
    else panel.setOutputView(null)

    // Generate mipmaps if configured
    if panel.mipLevelCount > 1 then generateMipmaps(panel)

  def paint(panels: Panel*): Unit =
    var i = 0
    while i < panels.length do
      paintPanel(panels(i))
      i += 1

  // Concrete-arity overloads for the hot path — avoid the Scala varargs
  // pipeline (`ScalaRunTime.wrapRefArray` → `Seq` → `toJSVarArgsImpl`) so a
  // typical per-frame `paint(a, b, …)` call stays allocation-free.
  inline def paint(a: Panel): Unit =
    paintPanel(a)
  inline def paint(a: Panel, b: Panel): Unit =
    paintPanel(a)
    paintPanel(b)
  inline def paint(a: Panel, b: Panel, c: Panel): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
  inline def paint(a: Panel, b: Panel, c: Panel, d: Panel): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
  inline def paint(a: Panel, b: Panel, c: Panel, d: Panel, e: Panel): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
    paintPanel(e)
  inline def paint(
      a: Panel,
      b: Panel,
      c: Panel,
      d: Panel,
      e: Panel,
      f: Panel,
  ): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
    paintPanel(e)
    paintPanel(f)
  inline def paint(
      a: Panel,
      b: Panel,
      c: Panel,
      d: Panel,
      e: Panel,
      f: Panel,
      g: Panel,
  ): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
    paintPanel(e)
    paintPanel(f)
    paintPanel(g)
  inline def paint(
      a: Panel,
      b: Panel,
      c: Panel,
      d: Panel,
      e: Panel,
      f: Panel,
      g: Panel,
      h: Panel,
  ): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
    paintPanel(e)
    paintPanel(f)
    paintPanel(g)
    paintPanel(h)
  inline def paint(
      a: Panel,
      b: Panel,
      c: Panel,
      d: Panel,
      e: Panel,
      f: Panel,
      g: Panel,
      h: Panel,
      i: Panel,
  ): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
    paintPanel(e)
    paintPanel(f)
    paintPanel(g)
    paintPanel(h)
    paintPanel(i)
  inline def paint(
      a: Panel,
      b: Panel,
      c: Panel,
      d: Panel,
      e: Panel,
      f: Panel,
      g: Panel,
      h: Panel,
      i: Panel,
      j: Panel,
  ): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
    paintPanel(e)
    paintPanel(f)
    paintPanel(g)
    paintPanel(h)
    paintPanel(i)
    paintPanel(j)
  inline def paint(
      a: Panel,
      b: Panel,
      c: Panel,
      d: Panel,
      e: Panel,
      f: Panel,
      g: Panel,
      h: Panel,
      i: Panel,
      j: Panel,
      k: Panel,
  ): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
    paintPanel(e)
    paintPanel(f)
    paintPanel(g)
    paintPanel(h)
    paintPanel(i)
    paintPanel(j)
    paintPanel(k)
  inline def paint(
      a: Panel,
      b: Panel,
      c: Panel,
      d: Panel,
      e: Panel,
      f: Panel,
      g: Panel,
      h: Panel,
      i: Panel,
      j: Panel,
      k: Panel,
      l: Panel,
  ): Unit =
    paintPanel(a)
    paintPanel(b)
    paintPanel(c)
    paintPanel(d)
    paintPanel(e)
    paintPanel(f)
    paintPanel(g)
    paintPanel(h)
    paintPanel(i)
    paintPanel(j)
    paintPanel(k)
    paintPanel(l)

  def show(panel: Panel): Unit =
    val encoder = device.createCommandEncoder()
    val swapChainView = context.getCurrentTexture().createView()

    val pass = encoder.beginRenderPass(
      Obj.literal(
        colorAttachments = Arr(
          Obj.literal(
            view = swapChainView,
            loadOp = "load",
            storeOp = "store",
          ),
        ),
      ),
    )

    val bindGroup = device.createBindGroup(
      Obj.literal(
        layout = blitBindGroupLayout,
        entries = Arr(
          Obj.literal(binding = 0, resource = panel.outputView),
          Obj.literal(binding = 1, resource = blitSampler),
        ),
      ),
    )

    pass.setPipeline(blitPipeline)
    pass.setBindGroup(0, bindGroup)
    pass.draw(3)
    pass.end()

    queue.submit(Arr(encoder.finish()))

  def paintAndShow(p: Panel): Unit =
    paint(p)
    show(p)

  // =========================================================================
  // Blit pipeline — created lazily on first show()
  // =========================================================================

  private lazy val blitSampler: GPUSampler =
    device.createSampler(
      Obj.literal(magFilter = "nearest", minFilter = "nearest"),
    )

  private lazy val blitBindGroupLayout: GPUBindGroupLayout =
    device.createBindGroupLayout(
      Obj.literal(
        entries = Arr(
          Obj.literal(
            binding = 0,
            visibility = GPUShaderStage.FRAGMENT,
            texture = Obj.literal(),
          ),
          Obj.literal(
            binding = 1,
            visibility = GPUShaderStage.FRAGMENT,
            sampler = Obj.literal(),
          ),
        ),
      ),
    )

  private lazy val blitPipeline: GPURenderPipeline =
    val module = device.createShaderModule(Obj.literal(code = BLIT_WGSL))
    val pipelineLayout = device.createPipelineLayout(
      Obj.literal(bindGroupLayouts = Arr(blitBindGroupLayout)),
    )
    device.createRenderPipeline(
      Obj.literal(
        layout = pipelineLayout,
        vertex = Obj.literal(module = module, entryPoint = "vs_main"),
        fragment = Obj.literal(
          module = module,
          entryPoint = "fs_main",
          targets = Arr(Obj.literal(format = preferredFormat)),
        ),
        primitive = Obj.literal(topology = "triangle-list"),
      ),
    )

  // =========================================================================
  // Mipmap generation
  // =========================================================================

  private lazy val mipBlitSampler: GPUSampler =
    device.createSampler(
      Obj.literal(magFilter = "linear", minFilter = "linear"),
    )

  private val mipBlitPipelines: Dict[GPURenderPipeline] =
    Dict[GPURenderPipeline]()

  private def getMipBlitPipeline(format: String): GPURenderPipeline =
    if mipBlitPipelines.has(format) then mipBlitPipelines.at(format)
    else
      val module = device.createShaderModule(Obj.literal(code = BLIT_WGSL))
      val pl = device.createPipelineLayout(
        Obj.literal(bindGroupLayouts = Arr(blitBindGroupLayout)),
      )
      val p = device.createRenderPipeline(
        Obj.literal(
          layout = pl,
          vertex = Obj.literal(module = module, entryPoint = "vs_main"),
          fragment = Obj.literal(
            module = module,
            entryPoint = "fs_main",
            targets = Arr(Obj.literal(format = format)),
          ),
          primitive = Obj.literal(topology = "triangle-list"),
        ),
      )
      mipBlitPipelines.set(format, p)
      p

  private def generateMipmaps(panel: Panel): Unit =
    val mipCount = panel.mipLevelCount
    if mipCount <= 1 then return
    val pipeline = getMipBlitPipeline(preferredFormat)

    var i = 1
    while i < mipCount do
      val srcView = panel.textureViewAt(0, i - 1)
      val dstView = panel.textureViewAt(0, i)

      val encoder = device.createCommandEncoder()
      val pass = encoder.beginRenderPass(
        Obj.literal(
          colorAttachments = Arr(
            Obj.literal(
              view = dstView,
              loadOp = "clear",
              storeOp = "store",
              clearValue = Obj.literal(r = 0, g = 0, b = 0, a = 0),
            ),
          ),
        ),
      )

      val bindGroup = device.createBindGroup(
        Obj.literal(
          layout = blitBindGroupLayout,
          entries = Arr(
            Obj.literal(binding = 0, resource = srcView),
            Obj.literal(binding = 1, resource = mipBlitSampler),
          ),
        ),
      )

      pass.setPipeline(pipeline)
      pass.setBindGroup(0, bindGroup)
      pass.draw(3)
      pass.end()
      queue.submit(Arr(encoder.finish()))
      i += 1

  // =========================================================================
  // Working buffers for binding merge (reusable, single-threaded JS)
  // =========================================================================

  private val _workBindings: BindingSlots = Arr()
  private val _workPanelBindings: Arr[Opt[PanelBinding]] = Arr()

  private def copyToWork(
      bindings: BindingSlots,
      panelBindings: Arr[Opt[PanelBinding]],
  ): Unit =
    _workBindings.length = bindings.length
    var i = 0
    while i < bindings.length do
      _workBindings(i) = bindings(i)
      i += 1
    _workPanelBindings.length = panelBindings.length
    var j = 0
    while j < panelBindings.length do
      _workPanelBindings(j) = panelBindings(j)
      j += 1

  private def applyPanelRuntimeBindings(
      panel: Panel,
      shade: Shade[?, ?],
      workBindings: BindingSlots,
      workPanelBindings: Arr[Opt[PanelBinding]],
  ): Unit =
    val dict = panel.runtimeBindings
    val keys =
      js.Object.keys(dict.asInstanceOf[js.Object]).asInstanceOf[Arr[String]]
    var i = 0
    while i < keys.length do
      val name = keys(i)
      val value = dict.at(name)
      if shade.uniformIndices.has(name) then
        val idx = shade.uniformIndices.at(name)
        if idx >= workBindings.length || workBindings(idx) == null then
          while workBindings.length <= idx do workBindings.push(null)
          workBindings(idx) =
            value.asInstanceOf[BufferBinding[?, ?] | GPUSampler]
      else if shade.panelIndices.has(name) then
        val idx = shade.panelIndices.at(name)
        if idx >= workPanelBindings.length || workPanelBindings(idx).isNull then
          while workPanelBindings.length <= idx do workPanelBindings.push(null)
          val pb =
            if value.isInstanceOf[PanelBinding] then
              value.asInstanceOf[PanelBinding]
            else PanelBinding(value.asInstanceOf[Panel])
          workPanelBindings(idx) = pb
      i += 1

  private def applyInstanceBindings(
      inst: Instance[?, ?],
      workBindings: BindingSlots,
      workPanelBindings: Arr[Opt[PanelBinding]],
  ): Unit =
    var i = 0
    while i < inst.bindings.length do
      if inst.bindings(i) != null then
        while workBindings.length <= i do workBindings.push(null)
        workBindings(i) = inst.bindings(i)
      i += 1
    var j = 0
    while j < inst.panelBindings.length do
      if inst.panelBindings(j).notNull then
        while workPanelBindings.length <= j do workPanelBindings.push(null)
        workPanelBindings(j) = inst.panelBindings(j)
      j += 1

  private def hasPanelRuntimeBindings(panel: Opt[Panel]): Boolean =
    panel.notNull && js.Object
      .keys(panel.runtimeBindings.asInstanceOf[js.Object])
      .asInstanceOf[Arr[String]]
      .length > 0

  // =========================================================================
  // Bind group helpers
  // =========================================================================

  private def setValueBindGroup(
      pass: GPURenderPassEncoder,
      shade: Shade[?, ?],
      bindings: BindingSlots,
  ): Unit =
    if bindings.length > 0 && shade.valueBindGroupLayout.notNull then
      val entries = Arr[js.Dynamic]()
      var i = 0
      while i < bindings.length do
        val b = bindings(i)
        if b != null then entries.push(bindingEntry(i, b))
        i += 1
      val bg = device.createBindGroup(
        Obj.literal(layout = shade.valueBindGroupLayout, entries = entries),
      )
      pass.setBindGroup(0, bg)

  private def setPanelBindGroup(
      pass: GPURenderPassEncoder,
      shade: Shade[?, ?],
      panelBindings: Arr[Opt[PanelBinding]],
      srcView: Opt[GPUTextureView] = null,
  ): Unit =
    if shade.panelBindGroupLayout.notNull then
      val entries = Arr[js.Dynamic]()
      if srcView.notNull then
        entries.push(Obj.literal(binding = 0, resource = srcView))
      val startIdx = if srcView.notNull then 1 else 0
      var k = startIdx
      while k < panelBindings.length do
        val pb = panelBindings(k)
        if pb.notNull then
          val view =
            if pb.depth then pb.panel.depthSamplingView
            else pb.panel.textureViewAt(pb.index, pb.mipLevel)
          entries.push(Obj.literal(binding = k, resource = view))
        k += 1
      if entries.length > 0 then
        val pg = device.createBindGroup(
          Obj.literal(
            layout = shade.panelBindGroupLayout,
            entries = entries,
          ),
        )
        pass.setBindGroup(1, pg)

  // =========================================================================
  // Per-shape render pass helper (shared by draw() and paint())
  // =========================================================================

  private def renderShapeOnPass(
      pass: GPURenderPassEncoder,
      shape: AnyShape,
      depthTest: Boolean = false,
      multisample: Boolean = false,
      formats: Arr[String] = null,
      panel: Opt[Panel] = null,
  ): Unit =
    val fmts = if formats != null then formats else Arr(preferredFormat)
    val pipeline = getPipeline(
      shape.shade,
      shape.blendState,
      fmts,
      depthTest,
      multisample,
      shape.form.topology,
      shape.cullMode,
      shape.form.frontFace,
    )

    pass.setPipeline(pipeline)
    pass.setVertexBuffer(0, shape.form.vertexBuffer)
    val hasIndex = shape.form.indexBuffer.notNull
    if hasIndex then
      pass.setIndexBuffer(shape.form.indexBuffer.get, shape.form.indexFormat)

    val instanceCount = shape.instances.length
    val hasPanelBinds = hasPanelRuntimeBindings(panel)

    inline def drawCall(): Unit =
      if hasIndex then pass.drawIndexed(shape.form.indexCount)
      else pass.draw(shape.form.vertexCount)

    if instanceCount == 0 then
      if hasPanelBinds then
        copyToWork(shape.bindings, shape.panelBindings)
        applyPanelRuntimeBindings(
          panel,
          shape.shade,
          _workBindings,
          _workPanelBindings,
        )
        setValueBindGroup(pass, shape.shade, _workBindings)
        setPanelBindGroup(pass, shape.shade, _workPanelBindings)
      else
        setValueBindGroup(pass, shape.shade, shape.bindings)
        setPanelBindGroup(pass, shape.shade, shape.panelBindings)
      drawCall()
    else
      var i = 0
      while i < instanceCount do
        val inst = shape.instances.items(i)
        copyToWork(shape.bindings, shape.panelBindings)
        if hasPanelBinds then
          applyPanelRuntimeBindings(
            panel,
            shape.shade,
            _workBindings,
            _workPanelBindings,
          )
        applyInstanceBindings(inst, _workBindings, _workPanelBindings)
        setValueBindGroup(pass, shape.shade, _workBindings)
        setPanelBindGroup(pass, shape.shade, _workPanelBindings)
        drawCall()
        i += 1

  // =========================================================================
  // Per-layer render pass helper
  // =========================================================================

  private def renderLayerOnPass(
      pass: GPURenderPassEncoder,
      layer: AnyLayer,
      depthTest: Boolean = false,
      multisample: Boolean = false,
      formats: Arr[String] = null,
      srcView: Opt[GPUTextureView] = null,
      panel: Opt[Panel] = null,
  ): Unit =
    val fmts = if formats != null then formats else Arr(preferredFormat)
    val pipeline = getPipeline(
      layer.shade,
      layer.blendState,
      fmts,
      depthTest,
      multisample,
    )

    pass.setPipeline(pipeline)

    val instanceCount = layer.instances.length
    val hasPanelBinds = hasPanelRuntimeBindings(panel)

    if instanceCount == 0 then
      if hasPanelBinds then
        copyToWork(layer.bindings, layer.panelBindings)
        applyPanelRuntimeBindings(
          panel,
          layer.shade,
          _workBindings,
          _workPanelBindings,
        )
        setValueBindGroup(pass, layer.shade, _workBindings)
        val effectiveSrcView =
          if _workPanelBindings.length > 0 && _workPanelBindings(0).notNull
          then null
          else srcView
        setPanelBindGroup(
          pass,
          layer.shade,
          _workPanelBindings,
          effectiveSrcView,
        )
      else
        setValueBindGroup(pass, layer.shade, layer.bindings)
        setPanelBindGroup(pass, layer.shade, layer.panelBindings, srcView)
      pass.draw(3)
    else
      var i = 0
      while i < instanceCount do
        val inst = layer.instances.items(i)
        copyToWork(layer.bindings, layer.panelBindings)
        if hasPanelBinds then
          applyPanelRuntimeBindings(
            panel,
            layer.shade,
            _workBindings,
            _workPanelBindings,
          )
        applyInstanceBindings(inst, _workBindings, _workPanelBindings)
        setValueBindGroup(pass, layer.shade, _workBindings)
        val effectiveSrcView =
          if _workPanelBindings.length > 0 && _workPanelBindings(0).notNull
          then null
          else srcView
        setPanelBindGroup(
          pass,
          layer.shade,
          _workPanelBindings,
          effectiveSrcView,
        )
        pass.draw(3)
        i += 1

  // =========================================================================
  // Pipeline creation + caching
  // =========================================================================

  private def blendKeyStr(bs: Opt[BlendState]): String =
    if bs.isNull then "n"
    else
      val c = bs.color
      val a = bs.alpha
      s"${c.srcFactor}.${c.dstFactor}.${c.operation}|${a.srcFactor}.${a.dstFactor}.${a.operation}"

  private def getPipeline(
      shade: Shade[?, ?],
      blendState: Opt[BlendState],
      formats: Arr[String],
      depthTest: Boolean,
      multisample: Boolean,
      topology: PrimitiveTopology = PrimitiveTopology.TriangleList,
      cullMode: CullMode = CullMode.None,
      frontFace: FrontFace = FrontFace.CCW,
  ): GPURenderPipeline =
    val key =
      s"${shade.id}|${blendKeyStr(blendState)}|${formats.join(",")}|$depthTest|$multisample|${topology}|${cullMode}|${frontFace}"
    if pipelineCache.has(key) then pipelineCache.at(key)
    else
      val targets = Arr[js.Dynamic]()
      var ti = 0
      while ti < formats.length do
        val target =
          if blendState.isNull then Obj.literal(format = formats(ti))
          else Obj.literal(format = formats(ti), blend = blendState)
        targets.push(target)
        ti += 1

      val vertexDescriptor =
        if shade.vertexBufferLayout.notNull then
          Obj.literal(
            module = shade.shaderModule,
            entryPoint = "vs_main",
            buffers = Arr(shade.vertexBufferLayout),
          )
        else
          Obj.literal(
            module = shade.shaderModule,
            entryPoint = "vs_main",
          )

      val desc = Obj.literal(
        layout = shade.pipelineLayout,
        vertex = vertexDescriptor,
        fragment = Obj.literal(
          module = shade.shaderModule,
          entryPoint = "fs_main",
          targets = targets,
        ),
        primitive = Obj.literal(
          topology = topology.toJs,
          cullMode = cullMode.toJs,
          frontFace = frontFace.toJs,
        ),
      )
      if depthTest then
        desc.depthStencil = Obj.literal(
          format = "depth24plus",
          depthWriteEnabled = true,
          depthCompare = "less",
        )
      if multisample then desc.multisample = Obj.literal(count = 4)
      val p = device.createRenderPipeline(desc)
      pipelineCache.set(key, p)
      p

  // =========================================================================
  // Bind group creation
  // =========================================================================

  private def bindingEntry(
      i: Int,
      b: BufferBinding[?, ?] | GPUSampler,
  ): js.Dynamic =
    if b.isInstanceOf[BufferBinding[?, ?]] then
      val buffer = b.asInstanceOf[BufferBinding[?, ?]]
      Obj.literal(
        binding = i,
        resource = Obj.literal(buffer = buffer.gpuBuffer),
      )
    else Obj.literal(binding = i, resource = b.asInstanceOf[GPUSampler])

object Painter:
  def init(canvas: HTMLCanvasElement): js.Promise[Painter] =
    val maybeGpu = WebGPU.getGPU()
    if maybeGpu.isEmpty then
      js.Promise.reject(
        js.Error("WebGPU is not supported"),
      )
    else
      val gpu = maybeGpu.safe
      for
        adapter <- gpu.requestAdapter().orError("Failed to get WebGPU adapter")
        device <- adapter.requestDevice()
      yield
        val queue = device.queue
        val context = WebGPU.getContext(canvas)
        val format = gpu.getPreferredCanvasFormat()

        context.configure(
          Obj.literal(
            device = device,
            format = format,
          ),
        )

        val painter = Painter(device, queue, canvas, context, format)

        // Set initial canvas size
        val w = canvas.clientWidth
        val h = canvas.clientHeight
        canvas.width = w
        canvas.height = h

        // Set up ResizeObserver — updates canvas dimensions and fires painter callbacks
        val observer = js.Dynamic
          .newInstance(js.Dynamic.global.ResizeObserver)(
            ((entries: js.Array[js.Dynamic]) =>
              val entry = entries(0)
              val rw: Double = entry.contentRect.width.asInstanceOf[Double]
              val rh: Double = entry.contentRect.height.asInstanceOf[Double]
              if rw > 0 && rh > 0 then
                canvas.width = rw.toInt
                canvas.height = rh.toInt
                painter.fireResize(rw, rh),
            ): js.Function1[js.Array[js.Dynamic], Unit],
          )
        observer.observe(canvas)

        painter

  def init(canvas: HTMLCanvasElement)(
      setup: Painter => Unit,
  ): js.Promise[Unit] =
    init(canvas).map(setup)
