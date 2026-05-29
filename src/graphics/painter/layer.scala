package trivalibs.graphics.painter

import trivalibs.utils.js.*

type AnyLayer = Layer[?, ?]

/** A full-screen post-processing pass attached to a [[Panel]], built from a
  * [[Painter.layerShade]]. Layers run in order after the panel's shapes, each
  * reading the previous pass's output (the panel auto-injects it as the first
  * panel-texture slot unless you bind that slot to an external panel). Bind
  * uniforms/panels with `.bind(...)`; add per-draw overrides via
  * `instances.add(...)`. Create through [[Painter.layer]]. See [[mipSource]] /
  * [[mipTarget]] / [[blendState]] for mip-chain and accumulation passes.
  */
class Layer[U, P] private[painter] (
    val painter: Painter,
    val shade: Shade[U, P],
) extends Bindable[U, P]:
  /** Internal: blend mode, set via [[set]] / [[Painter.layer]]. */
  private[painter] var blendState: Opt[BlendState] = null

  /** Internal: source mip level, set via [[set]] / [[Painter.layer]]. */
  private[painter] var mipSource: Int = -1

  /** Internal: target mip level, set via [[set]] / [[Painter.layer]]. */
  private[painter] var mipTarget: Int = -1
  var bindings: BindingSlots = Arr()
  var panelBindings: Arr[Opt[PanelBinding]] = Arr()

  /** Per-draw-call binding overrides; one rendered draw per added instance
    * (e.g. one additive draw per light). See [[InstanceList]].
    */
  val instances: InstanceList[U, P] = InstanceList[U, P](shade, painter)

  /** Set [[blendState]] / [[mipSource]] / [[mipTarget]]; returns `this`. */
  def set(
      blendState: Maybe[Opt[BlendState]] = Maybe.Not,
      mipSource: Maybe[Int] = Maybe.Not,
      mipTarget: Maybe[Int] = Maybe.Not,
  ): this.type =
    blendState.foreach(v => this.blendState = v)
    mipSource.foreach(v => this.mipSource = v)
    mipTarget.foreach(v => this.mipTarget = v)
    this
