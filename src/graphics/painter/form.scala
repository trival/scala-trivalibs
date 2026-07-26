package trivalibs.graphics.painter

import trivalibs.graphics.geometry.BufferedGeometry
import trivalibs.bufferdata.StructArray
import trivalibs.utils.js.*
import trivalibs.graphics.painter.*

import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.typedarray.Uint16Array
import scala.scalajs.js.typedarray.Uint32Array

/** WebGPU buffer sizes and writes must be a non-zero multiple of 4 bytes. */
private inline def paddedBufferSize(size: Int): Int =
  val p = (size + 3) & ~3
  if p < 4 then 4 else p

/** One vertex buffer (plus optional index buffer) of a [[Form]].
  *
  * Allocated capacity (`maxSize`) is tracked separately from the bytes
  * currently in use (`currentSize`): an upload reuses the existing `GPUBuffer`
  * whenever the new data still fits, and the draw binds only the live slice —
  * so a smaller upload can never leak vertices from a previous, larger one.
  */
private[painter] class FormBuffers:
  var vertexBuffer: Opt[GPUBuffer] = null
  var vertexBufferMaxSize: Int = 0
  var vertexBufferCurrentSize: Int = 0
  var vertexCount: Int = 0
  var indexBuffer: Opt[GPUBuffer] = null
  var indexBufferMaxSize: Int = 0
  var indexBufferCurrentSize: Int = 0
  var indexCount: Int = 0
  var indexFormat: String = "uint16"

/** GPU geometry for a [[Shape]]: one or more vertex buffers (each with an
  * optional index buffer) plus topology and front-face winding. Create via
  * [[Painter.form]]; reassign geometry later with [[set]].
  *
  * A form holding several buffers draws them in sequence with the same
  * pipeline, bind groups, topology and front face — one shape, N geometry
  * chunks. That is what a split polyline needs: `line2d` cuts a stroke into
  * fragments at sharp corners, and all fragments stay one drawable.
  *
  * Uploads are grow-only: the GPU buffer is reused while the new data fits and
  * only reallocated when it exceeds the current capacity, so per-frame geometry
  * updates don't thrash the allocator.
  */
class Form private[painter] (private[painter] val painter: Painter):
  private[painter] val buffers: Arr[FormBuffers] = Arr()
  private[painter] var activeBuffers: Int = 0
  private[painter] var topology: PrimitiveTopology = PrimitiveTopology.TriangleList
  private[painter] var frontFace: FrontFace = FrontFace.CCW

  /** (Re)upload geometry and set topology/front-face; returns `this`. Only
    * provided args change.
    *
    * Geometry comes as either a `BufferedGeometry` (from the geometry/mesh
    * helpers, may include indices) or a raw `StructArray` of vertices (from
    * `allocateAttribs`), singly or as an `Arr` of buffers drawn in sequence.
    * The plural forms win over the singular ones, and buffers left over from a
    * previous, longer upload become inactive.
    *
    * A vertex-only upload (`vertices` / `verticesAll`) clears the index buffer
    * of the buffers it writes — vertices and indices are always set together.
    */
  def set[F <: Tuple](
      geometry: Maybe[BufferedGeometry[F]] = Maybe.Not,
      vertices: Maybe[StructArray[F]] = Maybe.Not,
      geometries: Maybe[Arr[BufferedGeometry[F]]] = Maybe.Not,
      verticesAll: Maybe[Arr[StructArray[F]]] = Maybe.Not,
      topology: Maybe[PrimitiveTopology] = Maybe.Not,
      frontFace: Maybe[FrontFace] = Maybe.Not,
  ): this.type =
    topology.foreach(v => this.topology = v)
    frontFace.foreach(v => this.frontFace = v)
    geometry.foreach: geo =>
      upload(0, geo.vertices, geo.indices)
      activeBuffers = 1
    vertices.foreach: verts =>
      upload(0, verts, null)
      activeBuffers = 1
    geometries.foreach: geos =>
      var i = 0
      while i < geos.length do
        val geo = geos(i)
        upload(i, geo.vertices, geo.indices)
        i += 1
      activeBuffers = geos.length
    verticesAll.foreach: all =>
      var i = 0
      while i < all.length do
        upload(i, all(i), null)
        i += 1
      activeBuffers = all.length
    this

  private def upload[F <: Tuple](
      index: Int,
      verts: StructArray[F],
      indices: Opt[Uint16Array | Uint32Array],
  ): Unit =
    while buffers.length <= index do buffers.push(FormBuffers())
    val b = buffers(index)
    uploadVertices(b, verts)
    if indices.notNull then uploadIndices(b, indices)
    else
      b.indexCount = 0
      b.indexBufferCurrentSize = 0

  private def uploadVertices[F <: Tuple](
      b: FormBuffers,
      verts: StructArray[F],
  ): Unit =
    val data = verts.arrayBuffer
    val size = data.byteLength
    val padded = paddedBufferSize(size)
    if b.vertexBuffer.isNull || b.vertexBufferMaxSize < padded then
      if b.vertexBuffer.notNull then b.vertexBuffer.get.destroy()
      b.vertexBuffer = painter.device.createBuffer(
        Obj.literal(
          size = padded,
          usage = GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
        ),
      )
      b.vertexBufferMaxSize = padded
    painter.queue.writeBuffer(b.vertexBuffer.get, 0.0, alignedData(data))
    b.vertexBufferCurrentSize = size
    b.vertexCount = verts.length

  private def uploadIndices(
      b: FormBuffers,
      raw: Uint16Array | Uint32Array,
  ): Unit =
    var data: ArrayBuffer = null
    var count = 0
    if raw.isInstanceOf[Uint16Array] then
      val u16 = raw.asInstanceOf[Uint16Array]
      data = u16.buffer
      count = u16.length
      b.indexFormat = "uint16"
    else
      val u32 = raw.asInstanceOf[Uint32Array]
      data = u32.buffer
      count = u32.length
      b.indexFormat = "uint32"
    val size = data.byteLength
    val padded = paddedBufferSize(size)
    if b.indexBuffer.isNull || b.indexBufferMaxSize < padded then
      if b.indexBuffer.notNull then b.indexBuffer.get.destroy()
      b.indexBuffer = painter.device.createBuffer(
        Obj.literal(
          size = padded,
          usage = GPUBufferUsage.INDEX | GPUBufferUsage.COPY_DST,
        ),
      )
      b.indexBufferMaxSize = padded
    painter.queue.writeBuffer(b.indexBuffer.get, 0.0, alignedData(data))
    b.indexBufferCurrentSize = size
    b.indexCount = count

  /** A write must cover a whole multiple of 4 bytes — an odd `uint16` index
    * count is padded into an aligned copy.
    */
  private def alignedData(data: ArrayBuffer): ArrayBuffer =
    val size = data.byteLength
    if (size & 3) == 0 then data
    else
      val out = new ArrayBuffer(paddedBufferSize(size))
      new Uint8Array(out).set(new Uint8Array(data))
      out
