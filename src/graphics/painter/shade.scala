package trivalibs.graphics.painter

import trivalibs.utils.js.*
import trivalibs.graphics.painter.*

import scala.scalajs.js

class Shade[U, P](
    val id: Int,
    val shaderModule: GPUShaderModule,
    val vertexBufferLayout: Opt[js.Dynamic],
    val valueBindGroupLayout: Opt[GPUBindGroupLayout],
    val panelBindGroupLayout: Opt[GPUBindGroupLayout],
    val pipelineLayout: GPUPipelineLayout,
    val isLayer: Boolean,
    val uniformIndices: js.Dictionary[Int],
    val panelIndices: js.Dictionary[Int],
)
