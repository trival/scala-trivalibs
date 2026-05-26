package trivalibs.graphics.painter

import trivalibs.graphics.geometry.BufferedGeometry
import trivalibs.bufferdata.StructArray
import trivalibs.utils.js.*
import trivalibs.graphics.painter.*

import scala.scalajs.js.typedarray.Uint16Array
import scala.scalajs.js.typedarray.Uint32Array

/** GPU geometry for a [[Shape]]: a vertex buffer (and optional index buffer)
  * plus topology and front-face winding. Create via [[Painter.form]]; reassign
  * geometry later with [[set]] (the old GPU buffer is destroyed and a new one
  * allocated — no in-place resize).
  */
class Form private[painter] (private[painter] val painter: Painter):
  private[painter] var vertexBuffer: Opt[GPUBuffer] = null
  private[painter] var vertexCount: Int = 0
  private[painter] var indexBuffer: Opt[GPUBuffer] = null
  private[painter] var indexCount: Int = 0
  private[painter] var indexFormat: String = "uint16"
  private[painter] var topology: PrimitiveTopology = PrimitiveTopology.TriangleList
  private[painter] var frontFace: FrontFace = FrontFace.CCW

  /** (Re)upload geometry and set topology/front-face; returns `this`. Provide
    * either `geometry` (a `BufferedGeometry`, may include indices) or raw
    * `vertices` (a `StructArray` from `allocateAttribs`). Only provided args
    * change.
    */
  def set[F <: Tuple](
      geometry: Maybe[BufferedGeometry[F]] = Maybe.Not,
      vertices: Maybe[StructArray[F]] = Maybe.Not,
      topology: Maybe[PrimitiveTopology] = Maybe.Not,
      frontFace: Maybe[FrontFace] = Maybe.Not,
  ): this.type =
    topology.foreach(v => this.topology = v)
    frontFace.foreach(v => this.frontFace = v)
    geometry.foreach: geo =>
      uploadVertices(geo.vertices)
      if geo.indices.notNull then uploadIndices(geo.indices)
    vertices.foreach(uploadVertices)
    this

  private def uploadIndices(raw: Uint16Array | Uint32Array): Unit =
    val (ab, count, fmt) =
      if raw.isInstanceOf[Uint16Array] then
        val u16 = raw.asInstanceOf[Uint16Array]
        (u16.buffer, u16.length, "uint16")
      else
        val u32 = raw.asInstanceOf[Uint32Array]
        (u32.buffer, u32.length, "uint32")
    val buf = painter.device.createBuffer(
      Obj.literal(
        size = ab.byteLength,
        usage = GPUBufferUsage.INDEX | GPUBufferUsage.COPY_DST,
      ),
    )
    painter.queue.writeBuffer(buf, 0.0, ab)
    if indexBuffer.notNull then indexBuffer.get.destroy()
    indexBuffer = buf
    indexCount = count
    indexFormat = fmt

  private def uploadVertices[F <: Tuple](verts: StructArray[F]): Unit =
    val buf = painter.device.createBuffer(
      Obj.literal(
        size = verts.arrayBuffer.byteLength,
        usage = GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
      ),
    )
    painter.queue.writeBuffer(buf, 0.0, verts.arrayBuffer)
    if vertexBuffer.notNull then vertexBuffer.get.destroy()
    vertexBuffer = buf
    vertexCount = verts.length
