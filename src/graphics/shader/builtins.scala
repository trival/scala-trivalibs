package trivalibs.graphics.shader

/** Type class for WGSL builtin types */
trait BuiltinType[T]:
  def wgslBuiltin: String // e.g., "position", "vertex_index"
  def wgslType: String // e.g., "vec4<f32>", "u32"

object BuiltinType:
  inline def apply[T: BuiltinType as bt]: BuiltinType[T] = bt

// =============================================================================
// Vertex Shader Builtins
// =============================================================================

/** @builtin(vertex_index) - vertex index input */
opaque type BuiltinVertexIndex = Unit

object BuiltinVertexIndex:
  given BuiltinType[BuiltinVertexIndex]:
    def wgslBuiltin = "vertex_index"
    def wgslType = "u32"

/** @builtin(instance_index) - instance index input */
opaque type BuiltinInstanceIndex = Unit

object BuiltinInstanceIndex:
  given BuiltinType[BuiltinInstanceIndex]:
    def wgslBuiltin = "instance_index"
    def wgslType = "u32"

/** @builtin(position) - clip space position output from vertex shader */
opaque type BuiltinPosition = Unit

object BuiltinPosition:
  given BuiltinType[BuiltinPosition]:
    def wgslBuiltin = "position"
    def wgslType = "vec4<f32>"

// =============================================================================
// Fragment Shader Builtins
// =============================================================================

/** @builtin(position) - fragment position input (same as BuiltinPosition but as input)
  */
opaque type BuiltinFragCoord = Unit

object BuiltinFragCoord:
  given BuiltinType[BuiltinFragCoord]:
    def wgslBuiltin = "position"
    def wgslType = "vec4<f32>"

/** @builtin(front_facing) - whether fragment is front-facing */
opaque type BuiltinFrontFacing = Unit

object BuiltinFrontFacing:
  given BuiltinType[BuiltinFrontFacing]:
    def wgslBuiltin = "front_facing"
    def wgslType = "bool"

/** @builtin(sample_index) - sample index for multisampling */
opaque type BuiltinSampleIndex = Unit

object BuiltinSampleIndex:
  given BuiltinType[BuiltinSampleIndex]:
    def wgslBuiltin = "sample_index"
    def wgslType = "u32"
