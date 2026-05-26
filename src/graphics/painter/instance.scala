package trivalibs.graphics.painter

import trivalibs.utils.js.*

/** Per-draw-call binding overrides for one instance of a [[Shape]] or [[Layer]].
  * Fields not overridden fall back to the shape/layer bindings. Mutate later via
  * `shape.instances(i).bind(...)`.
  */
class Instance[U, P](
    val shade: Shade[U, P],
    val painter: Painter,
) extends Bindable[U, P]:
  var bindings: BindingSlots = Arr()
  var panelBindings: Arr[Opt[PanelBinding]] = Arr()

/** The ordered list of [[Instance]]s on a [[Shape]] or [[Layer]] — one rendered
  * draw per instance. Build it with `add("name" := value, …)` (1–8 pairs per
  * call), e.g. one draw per light or per transform:
  * {{{
  * for i <- 0 until n do
  *   shape.instances.add("model" := models(i), "tint" := tints(i))
  * }}}
  */
class InstanceList[U, P](val shade: Shade[U, P], val painter: Painter):
  val items: Arr[Instance[U, P]] = Arr()

  /** The instance at index `i` (e.g. to re-`bind` it per frame). */
  def apply(i: Int): Instance[U, P] = items(i)

  /** Number of instances. */
  def length: Int = items.length

  /** Remove all instances. */
  def clear(): Unit = items.length = 0

  /** Add an empty instance; returns its index. Overloads take 1–8 [[BindPair]]s
    * to bind in one call.
    */
  def add(): Int =
    val inst = Instance(shade, painter)
    items.push(inst)
    items.length - 1

  inline def add[N1 <: String & Singleton, V1](
      e1: BindPair[N1, V1],
  ): Int =
    val i = add()
    items(i).bind(e1)
    i

  inline def add[N1 <: String & Singleton, V1, N2 <: String & Singleton, V2](
      e1: BindPair[N1, V1],
      e2: BindPair[N2, V2],
  ): Int =
    val i = add()
    items(i).bind(e1, e2)
    i

  inline def add[
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
  ): Int =
    val i = add()
    items(i).bind(e1, e2, e3)
    i

  inline def add[
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
  ): Int =
    val i = add()
    items(i).bind(e1, e2, e3, e4)
    i

  inline def add[
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
  ): Int =
    val i = add()
    items(i).bind(e1, e2, e3, e4, e5)
    i

  inline def add[
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
  ): Int =
    val i = add()
    items(i).bind(e1, e2, e3, e4, e5, e6)
    i

  inline def add[
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
  ): Int =
    val i = add()
    items(i).bind(e1, e2, e3, e4, e5, e6, e7)
    i

  inline def add[
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
  ): Int =
    val i = add()
    items(i).bind(e1, e2, e3, e4, e5, e6, e7, e8)
    i
