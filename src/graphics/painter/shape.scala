package trivalibs.graphics.painter

import trivalibs.graphics.buffers.BufferBinding
import trivalibs.graphics.buffers.UniformValue
import trivalibs.graphics.painter.GPUDevice
import trivalibs.graphics.painter.GPUSampler
import trivalibs.graphics.shader.derive
import trivalibs.utils.js.*

import scala.compiletime.summonFrom
import scala.scalajs.js

type AnyShape = Shape[?, ?]

type BindingSlots =
  Arr[BufferBinding[?, ?] | GPUSampler | Null] // Null = empty slot

/** A name→value binding pair produced by the `"name" := value` syntax and
  * consumed by [[Bindable.bind]] / [[Panel.bind]] / instance `add`. `name` must
  * match a uniform or panel field of the shade's schema.
  */
class BindPair[N <: String & Singleton, V](val name: N, val value: V)

/** Sugar to build a [[BindPair]]: `"mvp" := mat4Binding`. */
extension [N <: String & Singleton](name: N)
  inline def :=[V](value: V): BindPair[N, V] = BindPair(name, value)

/** Mixin for things that carry shader bindings — [[Shape]], [[Layer]],
  * [[Instance]]. Provides the `bind(...)` overloads (1–8 [[BindPair]]s; chain
  * for more) that assign uniform/panel values by field name. Accepted values: a
  * `BufferBinding`, a raw uniform value (auto-boxed into a `BufferBinding`), a
  * `GPUSampler`, a `Panel`, or a `PanelBinding`.
  */
trait Bindable[U, P]:
  val shade: Shade[U, P]
  val painter: Painter
  inline def device: GPUDevice = painter.device
  var bindings: BindingSlots
  var panelBindings: Arr[Opt[PanelBinding]]

  // Hook fired after any `.bind(...)` mutation. Default no-op; [[Layer]] overrides
  // it to drop its static bind-group cache. (Fires on in-place uniform re-sets
  // too — harmless: it just means a per-frame-rebinding layer forgoes caching,
  // exactly today's rebuild-every-draw behaviour.)
  protected def onBindingsChanged(): Unit = ()

  inline def bind[N1 <: String & Singleton, V1](
      e1: BindPair[N1, V1],
  ): this.type =
    processEntry(e1)
    this

  inline def bind[
      N1 <: String & Singleton,
      V1,
      N2 <: String & Singleton,
      V2,
  ](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
  ): this.type =
    processEntry(e1)
    processEntry(e2)
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
    processEntry(e1)
    processEntry(e2)
    processEntry(e3)
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
    processEntry(e1)
    processEntry(e2)
    processEntry(e3)
    processEntry(e4)
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
    processEntry(e1)
    processEntry(e2)
    processEntry(e3)
    processEntry(e4)
    processEntry(e5)
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
    processEntry(e1)
    processEntry(e2)
    processEntry(e3)
    processEntry(e4)
    processEntry(e5)
    processEntry(e6)
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
    processEntry(e1)
    processEntry(e2)
    processEntry(e3)
    processEntry(e4)
    processEntry(e5)
    processEntry(e6)
    processEntry(e7)
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
    processEntry(e1)
    processEntry(e2)
    processEntry(e3)
    processEntry(e4)
    processEntry(e5)
    processEntry(e6)
    processEntry(e7)
    processEntry(e8)
    this

  protected inline def processEntry[N <: String & Singleton, V](
      pair: BindPair[N, V],
  ): Unit =
    inline if derive.containsName[N, U] then
      inline pair.value match
        case sampler: GPUSampler =>
          derive.checkSamplerFieldType[N, U]
          val idx = shade.uniformIndices.at(pair.name)
          while bindings.length <= idx do bindings.push(null)
          bindings(idx) = sampler
        case bb: BufferBinding[?, ?] =>
          derive.checkUniformFieldType[N, V, U]
          val idx = shade.uniformIndices.at(pair.name)
          while bindings.length <= idx do bindings.push(null)
          bindings(idx) = bb
        case rawValue =>
          derive.checkUniformFieldType[N, V, U]
          val idx = shade.uniformIndices.at(pair.name)
          if idx < bindings.length && bindings(idx) != null then
            bindings(idx).asInstanceOf[BufferBinding[V, ?]].set(rawValue)
          else
            summonFrom:
              case uv: UniformValue[V, f] =>
                val bb = BufferBinding[V, f](device, rawValue)(using uv)
                while bindings.length <= idx do bindings.push(null)
                bindings(idx) = bb
    else inline if derive.containsName[N, P] then
      inline pair.value match
        case pb: PanelBinding =>
          val idx = shade.panelIndices.at(pair.name)
          while panelBindings.length <= idx do panelBindings.push(null)
          panelBindings(idx) = pb
        case p: Panel =>
          val idx = shade.panelIndices.at(pair.name)
          while panelBindings.length <= idx do panelBindings.push(null)
          panelBindings(idx) = PanelBinding(p)
        case _ =>
          scala.compiletime.error(
            "Panel binding value must be a Panel or PanelBinding instance",
          )
    else
      scala.compiletime.error(
        "Name not found in Uniforms or Panel bindings",
      )
    onBindingsChanged()

/** A drawable: a [[Form]] (geometry) rendered with a [[Shade]], plus its bound
  * uniforms/panels and optional per-instance draws. Create via
  * [[Painter.shape]], bind with `.bind("name" := value, …)`, and add it to a
  * [[Panel]]. Use `instances.add(...)` for instanced draws (e.g. many
  * transforms sharing one form).
  */
class Shape[U, P] private[painter] (
    val painter: Painter,
    private[painter] val form: Form,
    val shade: Shade[U, P],
) extends Bindable[U, P]:
  /** Internal: cull mode, set via [[set]] / [[Painter.shape]]. */
  private[painter] var cullMode: CullMode = CullMode.None

  /** Internal: blend mode, set via [[set]] / [[Painter.shape]]. */
  private[painter] var blendState: Opt[BlendState] = null
  var bindings: BindingSlots = Arr()
  var panelBindings: Arr[Opt[PanelBinding]] = Arr()

  /** Per-draw-call binding overrides — one rendered instance per added entry,
    * sharing this shape's form/shade. See [[InstanceList]].
    */
  val instances: InstanceList[U, P] = InstanceList[U, P](shade, painter)

  /** Set [[cullMode]] (default [[CullMode.None]]) / [[blendState]] (default
    * `null` — opaque, no blending); returns `this`.
    */
  def set(
      cullMode: Maybe[CullMode] = Maybe.Not,
      blendState: Maybe[Opt[BlendState]] = Maybe.Not,
  ): this.type =
    cullMode.foreach(v => this.cullMode = v)
    blendState.foreach(v => this.blendState = v)
    this
