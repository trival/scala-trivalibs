package trivalibs.graphics.painter

import trivalibs.graphics.buffers.BufferBinding
import trivalibs.graphics.buffers.UniformValue
import trivalibs.graphics.painter.*
import trivalibs.utils.js.*

import scala.compiletime.summonFrom
import scala.scalajs.js

type ClearColor = (Double, Double, Double, Double)

/** The eager per-slot view bundle for one panel texture. Collapses the three
  * former lineages (`_textureViews`, `_samplingViews`, `_mipViews`) into one:
  *   - `perMip` — one single-level view per mip level (length = `mipCount`).
  *     `perMip(0)` (aliased [[attach]]) is the render-attachment view; higher
  *     entries feed hand-built mip chains and `generateMipmaps`.
  *   - `sampling` — a full-chain view for shader sampling (`textureSampleLevel`).
  * Built once alongside the texture; travels with it through swaps.
  */
private[painter] final class SlotViews(
    val perMip: Arr[GPUTextureView],
    val sampling: GPUTextureView,
):
  inline def attach: GPUTextureView = perMip(0)

/** A reference to one texture view of a [[Panel]], used as a shader input. Pick
  * which view via `panel.binding(index = …, mipLevel = …, depth = …)`:
  *   - `index` — render-target index (for MRT panels with multiple `formats`).
  *   - `mipLevel` — a specific mip (`-1` = the full sampling view / base).
  *   - `depth = true` — bind the panel's depth texture instead of color.
  *
  * A bare `Panel` passed as a binding value is shorthand for `panel.binding()`.
  */
class PanelBinding(
    val panel: Panel,
    val index: Int = 0,
    val mipLevel: Int = -1,
    val depth: Boolean = false,
) extends js.Object

type PanelBindingValue = BufferBinding[?, ?] | GPUSampler | Panel | PanelBinding

/** A render target: owns one or more GPU textures and renders its ordered
  * [[shapes]] then ordered [[layers]] into them when passed to
  * [[Painter.paint]]. Create via [[Painter.panel]]; mutate later with [[set]].
  * Its output texture can be sampled by other passes (bind the panel itself, or
  * [[binding]] for a specific view). Present it with [[Painter.show]].
  */
class Panel private[painter] (val painter: Painter):
  private[painter] var specWidth: Int = 0
  private[painter] var specHeight: Int = 0
  private[painter] var clearColor: Opt[ClearColor] = null
  private[painter] var depthTest: Boolean = false
  private[painter] var multisample: Boolean = false
  private[painter] var mipLevels: Int = 1
  private[painter] var formats: Arr[TextureFormat] = Arr()
  private[painter] var shapes: Arr[AnyShape] = Arr()
  private[painter] var layers: Arr[AnyLayer] = Arr()
  // Public (not private[painter]) because the inline `bind` expands into
  // sketch code and references this field directly.
  var runtimeBindings: Dict[PanelBindingValue] = Dict[PanelBindingValue]()

  private var _textures: Arr[GPUTexture] = Arr()
  // One view bundle per main slot, lockstep with `_textures`.
  private[painter] var slotViews: Arr[SlotViews] = Arr()
  private var _pongTextures: Arr[GPUTexture] = Arr()
  // Slot-0 pong view bundle — held so that after a ping-pong layer pass we can
  // swap main ↔ pong in slot 0 (`swapPongMain`) and keep slot-0 sampling valid.
  // (Merges into slot 1 of `slotViews` in Stage 4.)
  private var _pongSlotViews: Arr[SlotViews] = Arr()
  private var _depthTexture: Opt[GPUTexture] = null
  private var _depthView: Opt[GPUTextureView] = null
  private var _depthSamplable: Boolean = false
  // MSAA depth can't be sampled as `texture_depth_2d`; when a multisample panel's
  // depth is sampled, an internal pass resolves it into this single-sample
  // texture (see `Painter.resolvePanelDepth`).
  private var _resolvedDepthTexture: Opt[GPUTexture] = null
  private var _resolvedDepthView: Opt[GPUTextureView] = null
  private var _needsDepthResolve: Boolean = false
  private var _msaaTextures: Arr[GPUTexture] = Arr()
  private var _msaaViews: Arr[GPUTextureView] = Arr()
  private var _outputView: Opt[GPUTextureView] = null
  private var _width: Int = 0
  private var _height: Int = 0

  private[painter] def panelWidth: Int = _width
  private[painter] def panelHeight: Int = _height

  /** Effective number of mip levels: an explicit `mipLevels`, or — when
    * `mipLevels == 0` (set via `mips = true`) — the full chain computed from
    * the panel size.
    */
  private[painter] def mipLevelCount: Int =
    if mipLevels == 0 then
      val maxDim = Math.max(_width, _height)
      if maxDim <= 0 then 1
      else (Math.log(maxDim.toDouble) / Math.log(2.0)).toInt + 1
    else mipLevels

  private[painter] def effectiveFormats: Arr[TextureFormat] =
    if formats.length == 0 then Arr(painter.preferredFormat) else formats

  private[painter] def targetCount: Int = effectiveFormats.length

  private[painter] def textureView: GPUTextureView = slotViews(0).attach
  // Nullable by design: read eagerly in `paintPanel` before the layer loop even
  // for non-pong panels (where `_pongSlotViews` is empty). Returning `null` here
  // — rather than dereferencing an absent bundle — keeps that harmless, matching
  // the pre-bundle behaviour. Only ever *used* inside the ping-pong branch.
  private[painter] def pongView: GPUTextureView =
    if _pongSlotViews.length > 0 then _pongSlotViews(0).attach else null
  private[painter] def depthView: GPUTextureView = _depthView.get
  // True when this panel's (multisample) depth must be resolved to a sampleable
  // single-sample texture after the shape pass. The render-target depth view to
  // read is [[depthView]]; the resolve target is [[resolvedDepthTarget]].
  private[painter] def needsDepthResolve: Boolean = _needsDepthResolve
  private[painter] def resolvedDepthTarget: GPUTextureView = _resolvedDepthView.get
  private[painter] def msaaView: GPUTextureView = _msaaViews(0)
  private[painter] def outputView: GPUTextureView =
    if _outputView.notNull then _outputView.get else slotViews(0).attach

  /** After a ping-pong layer pass the final result lives in the pong texture,
    * but external samplers (and `outputView` for `show()`) read from main.
    * Swap slot-0 main ↔ pong so the post-pong result becomes the new main —
    * uniforms can stay bound, only panel bind groups (which view the panel's
    * sampling texture) need refreshing on the next read. The view bundles travel
    * with their textures, so no mip-view cache to invalidate.
    *
    * Slot 0 only: ping-pong is single-target by design (`renderLayerOnPass`
    * for a pong pass uses one color attachment), even on MRT panels.
    */
  private[painter] def swapPongMain(): Unit =
    val t = _textures(0)
    _textures(0) = _pongTextures(0)
    _pongTextures(0) = t
    val sv = slotViews(0)
    slotViews(0) = _pongSlotViews(0)
    _pongSlotViews(0) = sv

  private[painter] def msaaViewAt(index: Int): GPUTextureView = _msaaViews(
    index,
  )

  // (Re)allocate the depth render texture (and, for a sampled MSAA panel, the
  // single-sample resolve texture). Shared by `ensureSize` and the lazy upgrade
  // in `depthSamplingView`.
  private def allocDepth(): Unit =
    if _depthTexture.notNull then _depthTexture.get.destroy()
    if _resolvedDepthTexture.notNull then _resolvedDepthTexture.get.destroy()
    val depthUsage =
      if _depthSamplable then
        GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING
      else GPUTextureUsage.RENDER_ATTACHMENT
    val depthTex = painter.device.createTexture(
      Obj.literal(
        size = Obj.literal(width = _width, height = _height),
        format = "depth24plus",
        usage = depthUsage,
        sampleCount = if multisample then 4 else 1,
      ),
    )
    _depthTexture = depthTex
    _depthView = depthTex.createView()
    if _depthSamplable && multisample then
      val resTex = painter.device.createTexture(
        Obj.literal(
          size = Obj.literal(width = _width, height = _height),
          format = "depth24plus",
          usage =
            GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING,
          sampleCount = 1,
        ),
      )
      _resolvedDepthTexture = resTex
      _resolvedDepthView = resTex.createView()
      _needsDepthResolve = true
    else
      _resolvedDepthTexture = null
      _resolvedDepthView = null
      _needsDepthResolve = false

  // Return a depth view usable as a `texture_depth_2d` shader input. The first
  // time a panel's depth is sampled it's re-allocated with TEXTURE_BINDING; for
  // a multisample panel that also allocates the single-sample resolve texture
  // (filled by `Painter.resolvePanelDepth` each frame) and we hand that back.
  private[painter] def depthSamplingView: GPUTextureView =
    if !_depthSamplable && _depthTexture.notNull then
      _depthSamplable = true
      allocDepth()
    if _needsDepthResolve then _resolvedDepthView.get else _depthView.get

  /** Make a [[PanelBinding]] selecting one view of this panel to feed another
    * pass as a texture: a render-target `index`, a specific `mipLevel` (`-1` =
    * sampling view), or the `depth` texture. A bare panel binds `binding()`.
    */
  def binding(
      index: Int = 0,
      mipLevel: Int = -1,
      depth: Boolean = false,
  ): PanelBinding =
    new PanelBinding(this, index, mipLevel, depth)

  private[painter] def setOutputView(view: GPUTextureView): Unit =
    _outputView = view

  /** Reconfigure this panel (same options as [[Painter.panel]]); returns `this`
    * for chaining. Only provided args change. `mips = true` sets
    * `mipLevels = 0` (full auto chain); the singular `shape`/`layer`/`format`
    * are sugar and the plural forms take precedence.
    */
  def set[S <: AnyShape, L <: AnyLayer](
      width: Maybe[Int] = Maybe.Not,
      height: Maybe[Int] = Maybe.Not,
      clearColor: Maybe[Opt[ClearColor]] = Maybe.Not,
      depthTest: Maybe[Boolean] = Maybe.Not,
      multisample: Maybe[Boolean] = Maybe.Not,
      mipLevels: Maybe[Int] = Maybe.Not,
      mips: Maybe[Boolean] = Maybe.Not,
      format: Maybe[TextureFormat] = Maybe.Not,
      formats: Maybe[Arr[TextureFormat]] = Maybe.Not,
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
    // MRT panels can't host auto-pong layers: ping-pong is single-target by
    // design (one color attachment), so it can't post-process multiple render
    // targets. Compose a chain of single-format panels instead. Checked here —
    // the choke point for both initial construction and later reconfiguration.
    if this.formats.length > 1 && needsPong then
      throw jsError(
        "Panel: MRT (multiple formats) cannot host auto-pong layers. " +
          "Chain a single-format panel for post-processing instead.",
      )
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

  /** Bind values shared by all of this panel's shapes/layers, by uniform/panel
    * field name: `panel.bind("name" := value, …)`. Values may be a
    * `BufferBinding`, a raw uniform value (auto-boxed), a `GPUSampler`, a
    * `Panel`, or a [[PanelBinding]]. Overloads take 1–8 pairs; chain `.bind`
    * for more. (Per-shape bindings still go on the shape; these are defaults.)
    */
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

  // Allocate a pong scratch iff some layer auto-pongs. Uses the shared
  // `Layer.autoPongsSlot0` predicate — the stricter form, so a layer that
  // manually binds slot 0 no longer over-allocates an unused pong texture.
  private def needsPong: Boolean =
    var i = 0
    while i < layers.length do
      if layers(i).autoPongsSlot0 then return true
      i += 1
    false

  // Build the eager per-slot view bundle for a freshly-allocated texture: one
  // single-level view per mip level plus one full-chain sampling view.
  private def buildSlotViews(tex: GPUTexture, mipCount: Int): SlotViews =
    val perMip = Arr[GPUTextureView]()
    var m = 0
    while m < mipCount do
      perMip.push(
        tex.createView(Obj.literal(baseMipLevel = m, mipLevelCount = 1)),
      )
      m += 1
    new SlotViews(perMip, tex.createView())

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
      // Depth textures are (re)allocated by `allocDepth()` below.

      _width = targetW
      _height = targetH

      val mipCount = mipLevelCount
      val fmts = effectiveFormats
      val hasPong = needsPong
      val colorUsage =
        GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING

      _textures = Arr()
      slotViews = Arr()
      _pongTextures = Arr()
      _pongSlotViews = Arr()
      _msaaTextures = Arr()
      _msaaViews = Arr()

      var i = 0
      while i < fmts.length do
        val fmt = fmts(i)
        val tex = painter.device.createTexture(
          Obj.literal(
            size = Obj.literal(width = targetW, height = targetH),
            format = fmt.toJs,
            usage = colorUsage,
            mipLevelCount = mipCount,
          ),
        )
        _textures.push(tex)
        slotViews.push(buildSlotViews(tex, mipCount))

        if hasPong then
          val pongTex = painter.device.createTexture(
            Obj.literal(
              size = Obj.literal(width = targetW, height = targetH),
              format = fmt.toJs,
              usage = colorUsage,
              mipLevelCount = mipCount,
            ),
          )
          _pongTextures.push(pongTex)
          _pongSlotViews.push(buildSlotViews(pongTex, mipCount))

        if multisample then
          val msaaTex = painter.device.createTexture(
            Obj.literal(
              size = Obj.literal(width = targetW, height = targetH),
              format = fmt.toJs,
              sampleCount = 4,
              usage = GPUTextureUsage.RENDER_ATTACHMENT,
            ),
          )
          _msaaTextures.push(msaaTex)
          _msaaViews.push(msaaTex.createView())

        i += 1

      if depthTest then allocDepth()
