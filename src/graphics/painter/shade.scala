package trivalibs.graphics.painter

import trivalibs.utils.js.*
import trivalibs.graphics.painter.*

import scala.annotation.publicInBinary
import scala.scalajs.js

/** A compiled shader — the WGSL module plus its vertex/bind-group/pipeline
  * layouts. Opaque handle in sketch code: create via [[Painter.shade]] /
  * [[Painter.layerShade]] and pass to [[Painter.shape]] / [[Painter.layer]];
  * its fields drive the render pipeline internally and aren't part of the
  * sketch API. (`uniformIndices`/`panelIndices` are public only because the
  * inline `bind` machinery reads them.)
  */
class Shade[U, P] @publicInBinary private[painter] (
    private[painter] val id: Int,
    private[painter] val shaderModule: GPUShaderModule,
    private[painter] val vertexBufferLayout: Opt[js.Dynamic],
    private[painter] val valueBindGroupLayout: Opt[GPUBindGroupLayout],
    private[painter] val panelBindGroupLayout: Opt[GPUBindGroupLayout],
    private[painter] val pipelineLayout: GPUPipelineLayout,
    private[painter] val isLayer: Boolean,
    val uniformIndices: Dict[Int],
    val panelIndices: Dict[Int],
)
