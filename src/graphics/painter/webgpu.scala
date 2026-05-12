package trivalibs.graphics.painter

import org.scalajs.dom
import org.scalajs.dom.HTMLCanvasElement
import trivalibs.utils.js.Arr

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.*

// =============================================================================
// Core WebGPU Types
// =============================================================================

@js.native
trait GPU extends js.Object:
  def requestAdapter(): js.Promise[GPUAdapter | Null] = js.native
  def requestAdapter(
      options: js.Dynamic,
  ): js.Promise[GPUAdapter | Null] = js.native
  def getPreferredCanvasFormat(): String = js.native

@js.native
trait GPUAdapter extends js.Object:
  def requestDevice(): js.Promise[GPUDevice] = js.native
  def requestDevice(descriptor: js.Dynamic): js.Promise[GPUDevice] = js.native

@js.native
trait GPUDevice extends js.Object:
  val queue: GPUQueue = js.native
  def createShaderModule(descriptor: js.Dynamic): GPUShaderModule = js.native
  def createRenderPipeline(descriptor: js.Dynamic): GPURenderPipeline =
    js.native
  def createCommandEncoder(): GPUCommandEncoder = js.native
  def createCommandEncoder(descriptor: js.Dynamic): GPUCommandEncoder =
    js.native
  def createBuffer(descriptor: js.Dynamic): GPUBuffer = js.native
  def createBindGroup(descriptor: js.Dynamic): GPUBindGroup = js.native
  def createBindGroupLayout(descriptor: js.Dynamic): GPUBindGroupLayout =
    js.native
  def createPipelineLayout(descriptor: js.Dynamic): GPUPipelineLayout =
    js.native
  def createTexture(descriptor: js.Dynamic): GPUTexture = js.native
  def createSampler(): GPUSampler = js.native
  def createSampler(descriptor: js.Dynamic): GPUSampler = js.native

// =============================================================================
// Queue
// =============================================================================

@js.native
trait GPUQueue extends js.Object:
  def submit(commandBuffers: Arr[GPUCommandBuffer]): Unit = js.native
  def writeBuffer(
      buffer: GPUBuffer,
      bufferOffset: Double,
      data: Float32Array | Uint8Array | ArrayBuffer,
  ): Unit = js.native
  def writeBuffer(
      buffer: GPUBuffer,
      bufferOffset: Double,
      data: Float32Array | Uint8Array | ArrayBuffer,
      dataOffset: Double,
  ): Unit = js.native
  def writeBuffer(
      buffer: GPUBuffer,
      bufferOffset: Double,
      data: Float32Array | Uint8Array | ArrayBuffer,
      dataOffset: Double,
      size: Double,
  ): Unit = js.native

// =============================================================================
// Shader and Pipeline
// =============================================================================

@js.native
trait GPUShaderModule extends js.Object

@js.native
trait GPURenderPipeline extends js.Object:
  def getBindGroupLayout(index: Int): GPUBindGroupLayout = js.native

@js.native
trait GPUPipelineLayout extends js.Object

// =============================================================================
// Command Encoding
// =============================================================================

@js.native
trait GPUCommandEncoder extends js.Object:
  def beginRenderPass(descriptor: js.Dynamic): GPURenderPassEncoder = js.native
  def finish(): GPUCommandBuffer = js.native
  def finish(descriptor: js.Dynamic): GPUCommandBuffer = js.native

@js.native
trait GPURenderPassEncoder extends js.Object:
  def setPipeline(pipeline: GPURenderPipeline): Unit = js.native
  def setVertexBuffer(slot: Int, buffer: GPUBuffer): Unit = js.native
  def setVertexBuffer(slot: Int, buffer: GPUBuffer, offset: Double): Unit =
    js.native
  def setVertexBuffer(
      slot: Int,
      buffer: GPUBuffer,
      offset: Double,
      size: Double,
  ): Unit = js.native
  def setBindGroup(index: Int, bindGroup: GPUBindGroup): Unit = js.native
  def setBindGroup(
      index: Int,
      bindGroup: GPUBindGroup,
      dynamicOffsets: js.Array[Double],
  ): Unit = js.native
  def draw(vertexCount: Int): Unit = js.native
  def draw(vertexCount: Int, instanceCount: Int): Unit = js.native
  def draw(vertexCount: Int, instanceCount: Int, firstVertex: Int): Unit =
    js.native
  def draw(
      vertexCount: Int,
      instanceCount: Int,
      firstVertex: Int,
      firstInstance: Int,
  ): Unit = js.native
  def setIndexBuffer(buffer: GPUBuffer, indexFormat: String): Unit = js.native
  def drawIndexed(indexCount: Int): Unit = js.native
  def drawIndexed(indexCount: Int, instanceCount: Int): Unit = js.native
  def end(): Unit = js.native

@js.native
trait GPUCommandBuffer extends js.Object

// =============================================================================
// Resources
// =============================================================================

@js.native
trait GPUBuffer extends js.Object:
  def destroy(): Unit = js.native

@js.native
trait GPUBindGroup extends js.Object

@js.native
trait GPUBindGroupLayout extends js.Object

// =============================================================================
// Canvas and Textures
// =============================================================================

@js.native
trait GPUCanvasContext extends js.Object:
  def configure(config: js.Dynamic): Unit = js.native
  def getCurrentTexture(): GPUTexture = js.native

@js.native
trait GPUTexture extends js.Object:
  def createView(): GPUTextureView = js.native
  def createView(descriptor: js.Dynamic): GPUTextureView = js.native
  def destroy(): Unit = js.native

@js.native
trait GPUTextureView extends js.Object

@js.native
trait GPUSampler extends js.Object

// =============================================================================
// Constants
// =============================================================================

object GPUTextureUsage:
  val COPY_SRC = 0x01
  val COPY_DST = 0x02
  val TEXTURE_BINDING = 0x04
  val STORAGE_BINDING = 0x08
  val RENDER_ATTACHMENT = 0x10

object GPUBufferUsage:
  val MAP_READ = 0x0001
  val MAP_WRITE = 0x0002
  val COPY_SRC = 0x0004
  val COPY_DST = 0x0008
  val INDEX = 0x0010
  val VERTEX = 0x0020
  val UNIFORM = 0x0040
  val STORAGE = 0x0080
  val INDIRECT = 0x0100
  val QUERY_RESOLVE = 0x0200

object GPUShaderStage:
  val VERTEX = 0x1
  val FRAGMENT = 0x2
  val COMPUTE = 0x4

// =============================================================================
// Helpers
// =============================================================================

object WebGPU:
  /** Get the GPU object from navigator, if WebGPU is supported */
  def getGPU(): js.UndefOr[GPU] =
    dom.window.navigator
      .asInstanceOf[js.Dynamic]
      .gpu
      .asInstanceOf[js.UndefOr[GPU]]

  /** Get WebGPU context from a canvas element */
  def getContext(canvas: HTMLCanvasElement): GPUCanvasContext =
    canvas.getContext("webgpu").asInstanceOf[GPUCanvasContext]
