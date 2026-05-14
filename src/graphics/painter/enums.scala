package trivalibs.graphics.painter

import scala.scalajs.js

opaque type FilterMode = String
object FilterMode:
  val Nearest: FilterMode = "nearest"
  val Linear: FilterMode = "linear"
  extension (f: FilterMode) inline def toJs: js.Any = f.asInstanceOf[js.Any]

opaque type PrimitiveTopology = String
object PrimitiveTopology:
  val TriangleList: PrimitiveTopology = "triangle-list"
  val TriangleStrip: PrimitiveTopology = "triangle-strip"
  val LineList: PrimitiveTopology = "line-list"
  val LineStrip: PrimitiveTopology = "line-strip"
  val PointList: PrimitiveTopology = "point-list"
  extension (t: PrimitiveTopology)
    inline def toJs: js.Any = t.asInstanceOf[js.Any]

opaque type CullMode = String
object CullMode:
  val None: CullMode = "none"
  val Front: CullMode = "front"
  val Back: CullMode = "back"
  extension (c: CullMode) inline def toJs: js.Any = c.asInstanceOf[js.Any]

opaque type FrontFace = String
object FrontFace:
  val CCW: FrontFace = "ccw"
  val CW: FrontFace = "cw"
  extension (f: FrontFace) inline def toJs: js.Any = f.asInstanceOf[js.Any]

opaque type BlendFactor = String
object BlendFactor:
  val Zero: BlendFactor = "zero"
  val One: BlendFactor = "one"
  val Src: BlendFactor = "src"
  val OneMinusSrc: BlendFactor = "one-minus-src"
  val SrcAlpha: BlendFactor = "src-alpha"
  val OneMinusSrcAlpha: BlendFactor = "one-minus-src-alpha"
  val Dst: BlendFactor = "dst"
  val OneMinusDst: BlendFactor = "one-minus-dst"
  val DstAlpha: BlendFactor = "dst-alpha"
  val OneMinusDstAlpha: BlendFactor = "one-minus-dst-alpha"

opaque type BlendOp = String
object BlendOp:
  val Add: BlendOp = "add"
  val Subtract: BlendOp = "subtract"
  val ReverseSubtract: BlendOp = "reverse-subtract"
  val Min: BlendOp = "min"
  val Max: BlendOp = "max"

class BlendFn(
    val srcFactor: BlendFactor,
    val dstFactor: BlendFactor,
    val operation: BlendOp = BlendOp.Add,
) extends js.Object

class BlendState(
    val color: BlendFn,
    val alpha: BlendFn,
) extends js.Object

object BlendState:
  val Alpha = BlendState(
    color = BlendFn(BlendFactor.SrcAlpha, BlendFactor.OneMinusSrcAlpha),
    alpha = BlendFn(BlendFactor.One, BlendFactor.OneMinusSrcAlpha),
  )
  val Additive = BlendState(
    color = BlendFn(BlendFactor.SrcAlpha, BlendFactor.One),
    alpha = BlendFn(BlendFactor.One, BlendFactor.One),
  )
  val Multiply = BlendState(
    color = BlendFn(BlendFactor.Dst, BlendFactor.Zero),
    alpha = BlendFn(BlendFactor.DstAlpha, BlendFactor.Zero),
  )
