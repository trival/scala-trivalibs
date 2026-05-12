package trivalibs.graphics.shader

import scala.compiletime.erasedValue
import scala.scalajs.js
import trivalibs.bufferdata.StructArray
import trivalibs.utils.js.Arr
import trivalibs.graphics.painter.{GPUBindGroupLayout, GPUPipelineLayout, GPUDevice}

/** Complete shader definition with all type parameters
  *
  * Type parameters are unconstrained to allow EmptyTuple (None) for optional
  * fields. The derive functions handle type checking at compile time via inline
  * matches.
  */
case class ShaderDef[
    Attribs, // Custom vertex attributes (@location)
    Varyings, // Vertex out = Fragment in (custom @location)
    Uniforms, // Tuple of NamedTuples with visibility wrappers
    VertBuiltinIn, // @builtin inputs (default: None)
    VertBuiltinOut, // @builtin outputs (default: VertOut)
    FragBuiltinIn, // @builtin inputs (default: None)
    FragmentOut // Custom outputs (default: FragOut)
](
    vertexBody: String,
    fragmentBody: String,
    helperFns: String = "",
):

  /** Derive WebGPU vertex buffer layout descriptor from the Attribs type */
  inline def vertexBufferLayout: js.Dynamic =
    layouts.vertexBufferLayout[Attribs]

  /** Create bind group layouts from the Uniforms type */
  inline def createBindGroupLayouts(
      device: GPUDevice
  ): Arr[GPUBindGroupLayout] =
    layouts.createBindGroupLayouts[Uniforms](device)

  /** Create both bind group layouts and pipeline layout from the Uniforms type
    */
  inline def createPipelineLayout(
      device: GPUDevice
  ): (Arr[GPUBindGroupLayout], GPUPipelineLayout) =
    layouts.createPipelineLayoutFromUniforms[Uniforms](device)

  /** Generate complete WGSL shader code */
  inline def generateWGSL: String =
    val vertexInputStruct =
      derive.generateCombinedStruct[Attribs, VertBuiltinIn]("VertexInput")
    val vertexOutputStruct =
      derive.generateCombinedStruct[Varyings, VertBuiltinOut]("VertexOutput")
    val fragmentOutputStruct =
      derive.generateCombinedStruct[FragmentOut, EmptyTuple]("FragmentOutput")
    val uniformDecls = derive.generateUniforms[Uniforms]

    buildWGSL(
      vertexInputStruct,
      vertexOutputStruct,
      fragmentOutputStruct,
      uniformDecls,
      vertexBody,
      fragmentBody
    )

  private def buildWGSL(
      vertexInputStruct: String,
      vertexOutputStruct: String,
      fragmentOutputStruct: String,
      uniformDecls: String,
      vertexBody: String,
      fragmentBody: String
  ): String =
    val parts = Arr(
      vertexInputStruct,
      vertexOutputStruct,
      fragmentOutputStruct,
      uniformDecls,
      helperFns,
      buildVertexMain(vertexBody),
      buildFragmentMain(fragmentBody)
    ).filter(_.nonEmpty)

    parts.mkString("\n\n")

  private def buildVertexMain(body: String): String =
    s"""@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
  var out: VertexOutput;
$body
  return out;
}"""

  private def buildFragmentMain(body: String): String =
    s"""@fragment
fn fs_main(in: VertexOutput) -> FragmentOutput {
  var out: FragmentOutput;
$body
  return out;
}"""

/** Factory object with convenient constructors */
object Shader:
  /** Simple API with sensible defaults
    *
    * Defaults:
    *   - VertBuiltinIn = None
    *   - VertBuiltinOut = VertOut (position: BuiltinPosition)
    *   - FragBuiltinIn = None
    *   - FragmentOut = FragOut (color: Vec4)
    */
  def apply[A, V, U](
      vertexBody: String,
      fragmentBody: String,
      helperFns: String = "",
  ): ShaderDef[A, V, U, None, VertOut, None, FragOut] =
    new ShaderDef[A, V, U, None, VertOut, None, FragOut](
      vertexBody,
      fragmentBody,
      helperFns,
    )

  /** Full API for custom builtins or fragment outputs */
  def full[A, V, U, VBI, VBO, FBI, FO](
      vertexBody: String,
      fragmentBody: String,
      helperFns: String = "",
  ): ShaderDef[A, V, U, VBI, VBO, FBI, FO] =
    new ShaderDef[A, V, U, VBI, VBO, FBI, FO](vertexBody, fragmentBody, helperFns)
