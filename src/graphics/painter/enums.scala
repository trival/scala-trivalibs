package trivalibs.graphics.painter

import scala.scalajs.js

/** GPU texture format for a [[Panel]]'s render target(s) — the typed
  * replacement for raw WGSL format strings. Use the constants
  * (`TextureFormat.Rgba16Float` for HDR, etc.) with `panel(format = …)` /
  * `formats = Arr(…)`; a bare string won't type-check. Add constants here as
  * needed — this is a curated subset of the WebGPU formats.
  */
opaque type TextureFormat = String
object TextureFormat:
  val Rgba8Unorm: TextureFormat = "rgba8unorm"
  val Rgba8UnormSrgb: TextureFormat = "rgba8unorm-srgb"
  val Bgra8Unorm: TextureFormat = "bgra8unorm"
  val Bgra8UnormSrgb: TextureFormat = "bgra8unorm-srgb"
  val R8Unorm: TextureFormat = "r8unorm"
  val Rg8Unorm: TextureFormat = "rg8unorm"
  val R16Float: TextureFormat = "r16float"
  val Rg16Float: TextureFormat = "rg16float"
  val Rgba16Float: TextureFormat = "rgba16float"
  val R32Float: TextureFormat = "r32float"
  val Rg32Float: TextureFormat = "rg32float"
  val Rgba32Float: TextureFormat = "rgba32float"

  /** Wrap a raw WebGPU format string (e.g. the preferred canvas format).
    * Internal — sketches use the named constants. */
  private[graphics] inline def of(s: String): TextureFormat = s
  extension (f: TextureFormat)
    inline def toJs: js.Any = f.asInstanceOf[js.Any]
    /** The underlying WGSL format string. Internal. */
    private[graphics] inline def asString: String = f

/** Sampler filtering mode (`Nearest` / `Linear`), used for mag/min/mipmap
  * filters in [[Painter.sampler]].
  */
opaque type FilterMode = String
object FilterMode:
  val Nearest: FilterMode = "nearest"
  val Linear: FilterMode = "linear"
  extension (f: FilterMode) inline def toJs: js.Any = f.asInstanceOf[js.Any]

/** Sampler texture-coordinate address mode, used for U/V/W wrapping in
  * [[Painter.sampler]]. `ClampToEdge` (the default) clamps to the border texel;
  * `Repeat` tiles the texture (use it for seamless repeat-sampled tiles);
  * `MirrorRepeat` tiles with alternating mirroring.
  */
opaque type AddressMode = String
object AddressMode:
  val ClampToEdge: AddressMode = "clamp-to-edge"
  val Repeat: AddressMode = "repeat"
  val MirrorRepeat: AddressMode = "mirror-repeat"
  extension (a: AddressMode) inline def toJs: js.Any = a.asInstanceOf[js.Any]

/** Vertex primitive topology for a [[Form]] (default `TriangleList`). */
opaque type PrimitiveTopology = String
object PrimitiveTopology:
  val TriangleList: PrimitiveTopology = "triangle-list"
  val TriangleStrip: PrimitiveTopology = "triangle-strip"
  val LineList: PrimitiveTopology = "line-list"
  val LineStrip: PrimitiveTopology = "line-strip"
  val PointList: PrimitiveTopology = "point-list"
  extension (t: PrimitiveTopology)
    inline def toJs: js.Any = t.asInstanceOf[js.Any]

/** Face-culling mode for a [[Shape]] (default `None`). */
opaque type CullMode = String
object CullMode:
  val None: CullMode = "none"
  val Front: CullMode = "front"
  val Back: CullMode = "back"
  extension (c: CullMode) inline def toJs: js.Any = c.asInstanceOf[js.Any]

/** Winding order that counts as front-facing for a [[Form]] (default `CCW`). */
opaque type FrontFace = String
object FrontFace:
  val CCW: FrontFace = "ccw"
  val CW: FrontFace = "cw"
  extension (f: FrontFace) inline def toJs: js.Any = f.asInstanceOf[js.Any]

/** A WebGPU blend factor (source/destination coefficient) — building block of a
  * [[BlendFn]]. Prefer the [[BlendState]] presets for common cases.
  */
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

/** The combine operation for a [[BlendFn]] (default `Add`). */
opaque type BlendOp = String
object BlendOp:
  val Add: BlendOp = "add"
  val Subtract: BlendOp = "subtract"
  val ReverseSubtract: BlendOp = "reverse-subtract"
  val Min: BlendOp = "min"
  val Max: BlendOp = "max"

/** One blend equation (`src * srcFactor` ⊕ `dst * dstFactor`) for either the
  * color or the alpha channel of a [[BlendState]].
  */
class BlendFn(
    val srcFactor: BlendFactor,
    val dstFactor: BlendFactor,
    val operation: BlendOp = BlendOp.Add,
) extends js.Object

/** How a [[Shape]] or [[Layer]] blends with the existing render target — a color
  * [[BlendFn]] and an alpha [[BlendFn]]. Use the presets [[BlendState.Alpha]],
  * [[BlendState.Additive]], [[BlendState.Multiply]] for the common cases.
  */
class BlendState(
    val color: BlendFn,
    val alpha: BlendFn,
) extends js.Object

object BlendState:
  /** Standard src-over alpha blending. */
  val Alpha = BlendState(
    color = BlendFn(BlendFactor.SrcAlpha, BlendFactor.OneMinusSrcAlpha),
    alpha = BlendFn(BlendFactor.One, BlendFactor.OneMinusSrcAlpha),
  )
  /** Additive accumulation (bloom upsample, additive lights). */
  val Additive = BlendState(
    color = BlendFn(BlendFactor.SrcAlpha, BlendFactor.One),
    alpha = BlendFn(BlendFactor.One, BlendFactor.One),
  )
  /** Modulate the target by the source color. */
  val Multiply = BlendState(
    color = BlendFn(BlendFactor.Dst, BlendFactor.Zero),
    alpha = BlendFn(BlendFactor.DstAlpha, BlendFactor.Zero),
  )
