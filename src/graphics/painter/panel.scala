package trivalibs.graphics.painter

import trivalibs.graphics.buffers.BufferBinding
import trivalibs.graphics.buffers.UniformValue
import trivalibs.graphics.painter.*
import trivalibs.utils.js.*

import scala.compiletime.summonFrom
import scala.scalajs.js

type ClearColor = (Double, Double, Double, Double)

class PanelBinding(
    val panel: Panel,
    val index: Int = 0,
    val mipLevel: Int = -1,
    val depth: Boolean = false,
) extends js.Object

type PanelBindingValue = BufferBinding[?, ?] | GPUSampler | Panel | PanelBinding

class Panel(val painter: Painter):
  var specWidth: Int = 0
  var specHeight: Int = 0
  var clearColor: Opt[ClearColor] = null
  var depthTest: Boolean = false
  var multisample: Boolean = false
  var mipLevels: Int = 1
  var formats: Arr[String] = Arr()
  var shapes: Arr[AnyShape] = Arr()
  var layers: Arr[AnyLayer] = Arr()
  var runtimeBindings: Dict[PanelBindingValue] = Dict[PanelBindingValue]()

  private var _textures: Arr[GPUTexture] = Arr()
  private var _textureViews: Arr[GPUTextureView] = Arr()
  private var _samplingViews: Arr[Opt[GPUTextureView]] = Arr()
  private var _pongTextures: Arr[GPUTexture] = Arr()
  private var _pongViews: Arr[GPUTextureView] = Arr()
  private var _depthTexture: Opt[GPUTexture] = null
  private var _depthView: Opt[GPUTextureView] = null
  private var _depthSamplable: Boolean = false
  private var _msaaTextures: Arr[GPUTexture] = Arr()
  private var _msaaViews: Arr[GPUTextureView] = Arr()
  private var _outputView: Opt[GPUTextureView] = null
  private var _width: Int = 0
  private var _height: Int = 0
  private val _mipViews: Dict[GPUTextureView] = Dict[GPUTextureView]()

  def panelWidth: Int = _width
  def panelHeight: Int = _height

  def mipLevelCount: Int =
    if mipLevels == 0 then
      val maxDim = Math.max(_width, _height)
      if maxDim <= 0 then 1
      else (Math.log(maxDim.toDouble) / Math.log(2.0)).toInt + 1
    else mipLevels

  def effectiveFormats: Arr[String] =
    if formats.length == 0 then Arr(painter.preferredFormat) else formats

  def targetCount: Int = effectiveFormats.length

  def textureView: GPUTextureView = _textureViews(0)
  def pongView: GPUTextureView = _pongViews(0)
  def depthView: GPUTextureView = _depthView.get
  def msaaView: GPUTextureView = _msaaViews(0)
  def outputView: GPUTextureView =
    if _outputView.notNull then _outputView.get else _textureViews(0)

  def textureViewAt(index: Int = 0, mipLevel: Int = -1): GPUTextureView =
    if mipLevel < 0 then
      val sv = _samplingViews(index)
      if sv.notNull then sv.get else _textureViews(index)
    else
      val key = s"$index|$mipLevel"
      if _mipViews.has(key) then _mipViews.at(key)
      else
        val view = _textures(index).createView(
          Obj.literal(baseMipLevel = mipLevel, mipLevelCount = 1),
        )
        _mipViews.set(key, view)
        view

  def renderViewAt(index: Int): GPUTextureView = _textureViews(index)
  def pongViewAt(index: Int): GPUTextureView = _pongViews(index)
  def msaaViewAt(index: Int): GPUTextureView = _msaaViews(index)

  def depthSamplingView: GPUTextureView =
    if !_depthSamplable && _depthTexture.notNull then
      _depthTexture.get.destroy()
      val depthTex = painter.device.createTexture(
        Obj.literal(
          size = Obj.literal(width = _width, height = _height),
          format = "depth24plus",
          usage =
            GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING,
          sampleCount = if multisample then 4 else 1,
        ),
      )
      _depthTexture = depthTex
      _depthView = depthTex.createView()
      _depthSamplable = true
    _depthView

  def binding(
      index: Int = 0,
      mipLevel: Int = -1,
      depth: Boolean = false,
  ): PanelBinding =
    new PanelBinding(this, index, mipLevel, depth)

  private[painter] def setOutputView(view: GPUTextureView): Unit =
    _outputView = view

  def set[S <: AnyShape, L <: AnyLayer](
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
  ): this.type =
    width.foreach(v => this.specWidth = v)
    height.foreach(v => this.specHeight = v)
    clearColor.foreach(v => this.clearColor = v)
    depthTest.foreach(v => this.depthTest = v)
    multisample.foreach(v => this.multisample = v)
    mips.foreach(v => if v then this.mipLevels = 0)
    mipLevels.foreach(v => if v > 0 then this.mipLevels = v)
    formats.orMaybe(format.map(f => Arr(f))).foreach(v => this.formats = v)
    shapes
      .orMaybe(shape.map(s => Arr(s.asInstanceOf[AnyShape])))
      .foreach(v => this.shapes = v.asInstanceOf[Arr[AnyShape]])
    layers
      .orMaybe(layer.map(l => Arr(l)))
      .foreach(v => this.layers = v.asInstanceOf[Arr[AnyLayer]])
    this

  private inline def processPanelEntry[N <: String & Singleton, V](
      pair: BindPair[N, V],
  ): Unit =
    inline pair.value match
      case sampler: GPUSampler =>
        runtimeBindings.set(pair.name, sampler)
      case bb: BufferBinding[?, ?] =>
        runtimeBindings.set(pair.name, bb)
      case pb: PanelBinding =>
        runtimeBindings.set(pair.name, pb)
      case p: Panel =>
        runtimeBindings.set(pair.name, p)
      case rawValue =>
        if runtimeBindings.has(pair.name)
          && runtimeBindings.at(pair.name).isInstanceOf[BufferBinding[?, ?]]
        then
          runtimeBindings
            .at(pair.name)
            .asInstanceOf[BufferBinding[V, ?]]
            .set(rawValue)
        else
          summonFrom:
            case uv: UniformValue[V, f] =>
              val bb =
                BufferBinding[V, f](painter.device, rawValue)(using uv)
              runtimeBindings.set(pair.name, bb)

  inline def bind[N1 <: String & Singleton, V1](
      e1: BindPair[N1, V1],
  ): this.type =
    processPanelEntry(e1)
    this

  inline def bind[N1 <: String & Singleton, V1, N2 <: String & Singleton, V2](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
  ): this.type =
    processPanelEntry(e1)
    processPanelEntry(e2)
    this

  inline def bind[
      N1 <: String & Singleton,
      V1,
      N2 <: String & Singleton,
      V2,
      N3 <: String & Singleton,
      V3,
  ](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
      e3: BindPair[N3, V3],
  ): this.type =
    processPanelEntry(e1)
    processPanelEntry(e2)
    processPanelEntry(e3)
    this

  inline def bind[
      N1 <: String & Singleton,
      V1,
      N2 <: String & Singleton,
      V2,
      N3 <: String & Singleton,
      V3,
      N4 <: String & Singleton,
      V4,
  ](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
      e3: BindPair[N3, V3],
      e4: BindPair[N4, V4],
  ): this.type =
    processPanelEntry(e1)
    processPanelEntry(e2)
    processPanelEntry(e3)
    processPanelEntry(e4)
    this

  inline def bind[
      N1 <: String & Singleton,
      V1,
      N2 <: String & Singleton,
      V2,
      N3 <: String & Singleton,
      V3,
      N4 <: String & Singleton,
      V4,
      N5 <: String & Singleton,
      V5,
  ](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
      e3: BindPair[N3, V3],
      e4: BindPair[N4, V4],
      e5: BindPair[N5, V5],
  ): this.type =
    processPanelEntry(e1)
    processPanelEntry(e2)
    processPanelEntry(e3)
    processPanelEntry(e4)
    processPanelEntry(e5)
    this

  inline def bind[
      N1 <: String & Singleton,
      V1,
      N2 <: String & Singleton,
      V2,
      N3 <: String & Singleton,
      V3,
      N4 <: String & Singleton,
      V4,
      N5 <: String & Singleton,
      V5,
      N6 <: String & Singleton,
      V6,
  ](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
      e3: BindPair[N3, V3],
      e4: BindPair[N4, V4],
      e5: BindPair[N5, V5],
      e6: BindPair[N6, V6],
  ): this.type =
    processPanelEntry(e1)
    processPanelEntry(e2)
    processPanelEntry(e3)
    processPanelEntry(e4)
    processPanelEntry(e5)
    processPanelEntry(e6)
    this

  inline def bind[
      N1 <: String & Singleton,
      V1,
      N2 <: String & Singleton,
      V2,
      N3 <: String & Singleton,
      V3,
      N4 <: String & Singleton,
      V4,
      N5 <: String & Singleton,
      V5,
      N6 <: String & Singleton,
      V6,
      N7 <: String & Singleton,
      V7,
  ](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
      e3: BindPair[N3, V3],
      e4: BindPair[N4, V4],
      e5: BindPair[N5, V5],
      e6: BindPair[N6, V6],
      e7: BindPair[N7, V7],
  ): this.type =
    processPanelEntry(e1)
    processPanelEntry(e2)
    processPanelEntry(e3)
    processPanelEntry(e4)
    processPanelEntry(e5)
    processPanelEntry(e6)
    processPanelEntry(e7)
    this

  inline def bind[
      N1 <: String & Singleton,
      V1,
      N2 <: String & Singleton,
      V2,
      N3 <: String & Singleton,
      V3,
      N4 <: String & Singleton,
      V4,
      N5 <: String & Singleton,
      V5,
      N6 <: String & Singleton,
      V6,
      N7 <: String & Singleton,
      V7,
      N8 <: String & Singleton,
      V8,
  ](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
      e3: BindPair[N3, V3],
      e4: BindPair[N4, V4],
      e5: BindPair[N5, V5],
      e6: BindPair[N6, V6],
      e7: BindPair[N7, V7],
      e8: BindPair[N8, V8],
  ): this.type =
    processPanelEntry(e1)
    processPanelEntry(e2)
    processPanelEntry(e3)
    processPanelEntry(e4)
    processPanelEntry(e5)
    processPanelEntry(e6)
    processPanelEntry(e7)
    processPanelEntry(e8)
    this

  private def needsPong: Boolean =
    var i = 0
    while i < layers.length do
      if layers(i).shade.panelBindGroupLayout != null then return true
      i += 1
    false

  private[painter] def ensureSize(canvasW: Int, canvasH: Int): Unit =
    val targetW = if specWidth == 0 then canvasW else specWidth
    val targetH = if specHeight == 0 then canvasH else specHeight
    if targetW != _width || targetH != _height then
      // Destroy old textures
      var d = 0
      while d < _textures.length do
        _textures(d).destroy()
        d += 1
      d = 0
      while d < _pongTextures.length do
        _pongTextures(d).destroy()
        d += 1
      d = 0
      while d < _msaaTextures.length do
        _msaaTextures(d).destroy()
        d += 1
      if _depthTexture.notNull then _depthTexture.get.destroy()

      _width = targetW
      _height = targetH

      // Clear mip view cache
      val mipKeys = js.Object
        .keys(_mipViews.asInstanceOf[js.Object])
        .asInstanceOf[Arr[String]]
      var mk = 0
      while mk < mipKeys.length do
        js.special.delete(_mipViews, mipKeys(mk))
        mk += 1

      val mipCount = mipLevelCount
      val fmts = effectiveFormats
      val hasPong = needsPong
      val colorUsage =
        GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING

      _textures = Arr()
      _textureViews = Arr()
      _samplingViews = Arr()
      _pongTextures = Arr()
      _pongViews = Arr()
      _msaaTextures = Arr()
      _msaaViews = Arr()

      var i = 0
      while i < fmts.length do
        val fmt = fmts(i)
        val tex = painter.device.createTexture(
          Obj.literal(
            size = Obj.literal(width = targetW, height = targetH),
            format = fmt,
            usage = colorUsage,
            mipLevelCount = mipCount,
          ),
        )
        _textures.push(tex)
        _textureViews.push(
          tex.createView(Obj.literal(baseMipLevel = 0, mipLevelCount = 1)),
        )
        _samplingViews.push(
          if mipCount > 1 then tex.createView() else null,
        )

        if hasPong then
          val pongTex = painter.device.createTexture(
            Obj.literal(
              size = Obj.literal(width = targetW, height = targetH),
              format = fmt,
              usage = colorUsage,
              mipLevelCount = mipCount,
            ),
          )
          _pongTextures.push(pongTex)
          _pongViews.push(
            pongTex.createView(
              Obj.literal(baseMipLevel = 0, mipLevelCount = 1),
            ),
          )

        if multisample then
          val msaaTex = painter.device.createTexture(
            Obj.literal(
              size = Obj.literal(width = targetW, height = targetH),
              format = fmt,
              sampleCount = 4,
              usage = GPUTextureUsage.RENDER_ATTACHMENT,
            ),
          )
          _msaaTextures.push(msaaTex)
          _msaaViews.push(msaaTex.createView())

        i += 1

      if depthTest then
        val depthUsage =
          if _depthSamplable then
            GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING
          else GPUTextureUsage.RENDER_ATTACHMENT
        val depthTex = painter.device.createTexture(
          Obj.literal(
            size = Obj.literal(width = targetW, height = targetH),
            format = "depth24plus",
            usage = depthUsage,
            sampleCount = if multisample then 4 else 1,
          ),
        )
        _depthTexture = depthTex
        _depthView = depthTex.createView()
