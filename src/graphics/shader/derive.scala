package trivalibs.graphics.shader

import scala.compiletime.*
import scala.NamedTuple.AnyNamedTuple
import trivalibs.graphics.buffers.BufferBinding
import trivalibs.graphics.math.gpu.Expr.Sampler
import trivalibs.utils.js.Arr

/** Utilities for deriving WGSL code from Scala types at compile time */
object derive:

  // ===========================================================================
  // Named Tuple Introspection (handles both empty and non-empty tFreitaguples)
  // ===========================================================================

  /** Get field names from a named tuple type as a tuple of strings */
  inline def fieldNames[T]: Arr[String] =
    inline erasedValue[T] match
      case _: EmptyTuple    => Arr()
      case _: AnyNamedTuple =>
        fieldNamesImpl[NamedTuple.Names[T & AnyNamedTuple]]

  private inline def fieldNamesImpl[T <: Tuple]: Arr[String] =
    inline erasedValue[T] match
      case _: EmptyTuple     => Arr()
      case _: (name *: rest) =>
        Arr(constValue[name].asInstanceOf[String]) ++ fieldNamesImpl[rest]

  /** Get WGSL type names for each field in a named tuple */
  inline def fieldWgslTypes[T]: Arr[String] =
    inline erasedValue[T] match
      case _: EmptyTuple    => Arr()
      case _: AnyNamedTuple =>
        fieldWgslTypesImpl[NamedTuple.DropNames[T & AnyNamedTuple]]

  private inline def fieldWgslTypesImpl[T <: Tuple]: Arr[String] =
    inline erasedValue[T] match
      case _: EmptyTuple     => Arr()
      case _: (head *: rest) =>
        Arr(summonInline[WGSLType[head]].wgslName) ++ fieldWgslTypesImpl[rest]

  /** Get builtin info for each field in a named tuple of builtins */
  inline def fieldBuiltins[T]: Arr[(String, String, String)] =
    inline erasedValue[T] match
      case _: EmptyTuple    => Arr()
      case _: AnyNamedTuple =>
        fieldBuiltinsImpl[
          NamedTuple.Names[T & AnyNamedTuple],
          NamedTuple.DropNames[T & AnyNamedTuple]
        ]

  private inline def fieldBuiltinsImpl[N <: Tuple, T <: Tuple]
      : Arr[(String, String, String)] =
    inline (erasedValue[N], erasedValue[T]) match
      case (_: EmptyTuple, _: EmptyTuple)                   => Arr()
      case (_: (name *: namesRest), _: (head *: typesRest)) =>
        val bt = summonInline[BuiltinType[head]]
        Arr(
          (
            constValue[name].asInstanceOf[String],
            bt.wgslBuiltin,
            bt.wgslType
          )
        ) ++ fieldBuiltinsImpl[namesRest, typesRest]

  // ===========================================================================
  // WGSL Struct Generation
  // ===========================================================================

  /** Generate a WGSL struct with @location annotations */
  inline def generateLocationStruct[T](structName: String): String =
    val names = fieldNames[T]
    val types = fieldWgslTypes[T]
    generateLocationStructFromLists(structName, names, types)

  private def generateLocationStructFromLists(
      structName: String,
      names: Arr[String],
      types: Arr[String]
  ): String =
    if names.isEmpty then ""
    else
      val fields =
        names.zip(types).zipWithIndex.map { case ((name, typ), idx) =>
          s"  @location($idx) $name: $typ,"
        }
      s"struct $structName {\n${fields.join("\n")}\n}"

  /** Generate a WGSL struct with @builtin annotations */
  inline def generateBuiltinStruct[T](structName: String): String =
    val builtins = fieldBuiltins[T]
    generateBuiltinStructFromList(structName, builtins)

  private def generateBuiltinStructFromList(
      structName: String,
      builtins: Arr[(String, String, String)]
  ): String =
    if builtins.isEmpty then ""
    else
      val fields = builtins.map { case (name, builtin, typ) =>
        s"  @builtin($builtin) $name: $typ,"
      }
      s"struct $structName {\n${fields.join("\n")}\n}"

  /** Generate a combined struct with both @location and @builtin fields */
  inline def generateCombinedStruct[Locations, Builtins](
      structName: String
  ): String =
    val locNames = fieldNames[Locations]
    val locTypes = fieldWgslTypes[Locations]
    val builtins = fieldBuiltins[Builtins]
    generateCombinedStructFromLists(structName, locNames, locTypes, builtins)

  private def generateCombinedStructFromLists(
      structName: String,
      locNames: Arr[String],
      locTypes: Arr[String],
      builtins: Arr[(String, String, String)]
  ): String =
    val locationFields =
      locNames.zip(locTypes).zipWithIndex.map { case ((name, typ), idx) =>
        s"  @location($idx) $name: $typ,"
      }
    val builtinFields = builtins.map { case (name, builtin, typ) =>
      s"  @builtin($builtin) $name: $typ,"
    }
    val allFields = locationFields ++ builtinFields
    if allFields.isEmpty then ""
    else s"struct $structName {\n${allFields.join("\n")}\n}"

  // ===========================================================================
  // Uniform Generation
  // ===========================================================================

  /** Generate uniform declarations from nested named tuples */
  inline def generateUniforms[T]: String =
    inline erasedValue[T] match
      case _: EmptyTuple    => ""
      case _: AnyNamedTuple =>
        // T is a named tuple like (group1: (...), group2: (...))
        // We need to iterate over the values (the inner named tuples)
        generateUniformsFromNamedTuple[T & AnyNamedTuple](0)
      case _ => ""

  private inline def generateUniformsFromNamedTuple[T <: AnyNamedTuple](
      groupIdx: Int
  ): String =
    generateUniformsImpl[NamedTuple.DropNames[T]](groupIdx)

  private inline def generateUniformsImpl[T <: Tuple](groupIdx: Int): String =
    inline erasedValue[T] match
      case _: EmptyTuple     => ""
      case _: (head *: rest) =>
        val groupDecls = generateUniformGroup[head](groupIdx)
        val restDecls = generateUniformsImpl[rest](groupIdx + 1)
        if restDecls.isEmpty then groupDecls
        else s"$groupDecls\n$restDecls"

  private inline def generateUniformGroup[T](groupIdx: Int): String =
    inline erasedValue[T] match
      case _: EmptyTuple    => ""
      case _: AnyNamedTuple =>
        val names = fieldNames[T]
        val types = uniformFieldTypes[NamedTuple.DropNames[T & AnyNamedTuple]]
        generateUniformGroupFromLists(groupIdx, names, types)
      case _ => ""

  /** Extract WGSL type names, unwrapping
    * VertexUniform/FragmentUniform/SharedUniform
    */
  private inline def uniformFieldTypes[T <: Tuple]: Arr[String] =
    inline erasedValue[T] match
      case _: EmptyTuple     => Arr()
      case _: (head *: rest) =>
        Arr(unwrapUniformType[head]) ++ uniformFieldTypes[rest]

  /** Unwrap uniform wrapper types to get the inner WGSL type name */
  private inline def unwrapUniformType[T]: String =
    // For now, all wrapper types are just type aliases, so T is the actual type
    summonInline[WGSLType[T]].wgslName

  // ===========================================================================
  // Uniform Field Index Resolution (compile-time name → binding index)
  // ===========================================================================

  /** Resolve a field name to its binding index within a named tuple.
    * Produces a compile-time error if the name doesn't match any field.
    */
  inline def uniformFieldIndex[Name <: String, U]: Int =
    inline erasedValue[U] match
      case _: AnyNamedTuple =>
        uniformFieldIndexImpl[
          Name,
          NamedTuple.Names[U & AnyNamedTuple],
          0,
        ]

  private inline def uniformFieldIndexImpl[
      Name <: String,
      Names <: Tuple,
      Idx <: Int,
  ]: Int =
    inline erasedValue[Names] match
      case _: EmptyTuple =>
        error("Binding name not found in Uniforms type")
      case _: (name *: rest) =>
        inline constValue[name] match
          case _: Name => constValue[Idx]
          case _       => uniformFieldIndexImpl[Name, rest, compiletime.ops.int.S[Idx]]

  /** Resolve the value type for a named field, unwrapping visibility wrappers.
    * Returns the inner type (e.g. Vec3 from FragmentUniform[Vec3]).
    */
  inline def uniformFieldType[Name <: String, U]: Any =
    inline erasedValue[U] match
      case _: AnyNamedTuple =>
        uniformFieldTypeImpl[
          Name,
          NamedTuple.Names[U & AnyNamedTuple],
          NamedTuple.DropNames[U & AnyNamedTuple],
        ]

  private inline def uniformFieldTypeImpl[
      Name <: String,
      Names <: Tuple,
      Types <: Tuple,
  ]: Any =
    inline (erasedValue[Names], erasedValue[Types]) match
      case (_: EmptyTuple, _) =>
        error("Binding name not found in Uniforms type")
      case (_: (name *: namesRest), _: (head *: typesRest)) =>
        inline constValue[name] match
          case _: Name => erasedValue[UnwrapUniform[head]]
          case _ =>
            uniformFieldTypeImpl[Name, namesRest, typesRest]

  /** Match type to unwrap visibility wrappers */
  type UnwrapUniform[T] = T match
    case VertexUniform[t]   => t
    case FragmentUniform[t] => t
    case SharedUniform[t]   => t
    case _                  => T

  /** Auto-wraps bare types with FragmentUniform; already-wrapped types pass
    * through. Used by layerShade to infer fragment visibility for all uniforms.
    */
  type WrapFragment[T] = T match
    case VertexUniform[?]   => T
    case FragmentUniform[?] => T
    case SharedUniform[?]   => T
    case _                  => FragmentUniform[T]

  /** Compile-time check that V matches the expected type for field Name in U.
    * V can be the raw type T or a BufferBinding[T, ?] — both are accepted.
    */
  inline def checkUniformFieldType[Name <: String, V, U]: Unit =
    inline erasedValue[U] match
      case _: AnyNamedTuple =>
        checkFieldTypeImpl[
          Name,
          V,
          NamedTuple.Names[U & AnyNamedTuple],
          NamedTuple.DropNames[U & AnyNamedTuple],
        ]

  private inline def checkFieldTypeImpl[
      Name <: String,
      V,
      Names <: Tuple,
      Types <: Tuple,
  ]: Unit =
    inline (erasedValue[Names], erasedValue[Types]) match
      case (_: EmptyTuple, _) =>
        error("Binding name not found in Uniforms type")
      case (_: (name *: namesRest), _: (head *: typesRest)) =>
        inline constValue[name] match
          case _: Name =>
            type Expected = UnwrapUniform[head]
            summonFrom:
              case _: (V =:= Expected) => ()
              case _: (V <:< BufferBinding[Expected, ?]) => ()
              // Float ↔ Double interchange — both map to WGSL f32.
              case _ =>
                summonFrom:
                  case _: (Expected =:= Float) =>
                    summonFrom:
                      case _: (V =:= Double) => ()
                      case _: (V <:< BufferBinding[Double, ?]) => ()
                      case _ => error("Binding type mismatch: value type does not match uniform field type")
                  case _: (Expected =:= Double) =>
                    summonFrom:
                      case _: (V =:= Float) => ()
                      case _: (V <:< BufferBinding[Float, ?]) => ()
                      case _ => error("Binding type mismatch: value type does not match uniform field type")
                  case _ => error("Binding type mismatch: value type does not match uniform field type")
          case _ =>
            checkFieldTypeImpl[Name, V, namesRest, typesRest]

  // ===========================================================================
  // Uniform Group List Generation
  // ===========================================================================

  private def generateUniformGroupFromLists(
      groupIdx: Int,
      names: Arr[String],
      types: Arr[String]
  ): String =
    names
      .zip(types)
      .zipWithIndex
      .map { case ((name, typ), bindingIdx) =>
        if typ == "sampler" then
          s"@group($groupIdx) @binding($bindingIdx) var $name: sampler;"
        else
          s"@group($groupIdx) @binding($bindingIdx) var<uniform> $name: $typ;"
      }
      .join("\n")

  // ===========================================================================
  // Panel Type Helpers
  // ===========================================================================

  /** Check at compile time if name N exists as a field in named tuple T. */
  inline def containsName[N <: String & Singleton, T]: Boolean =
    inline erasedValue[T] match
      case _: EmptyTuple    => false
      case _: AnyNamedTuple =>
        containsNameImpl[N, NamedTuple.Names[T & AnyNamedTuple]]
      case _ => false

  private inline def containsNameImpl[N <: String & Singleton, Names <: Tuple]
      : Boolean =
    inline erasedValue[Names] match
      case _: EmptyTuple     => false
      case _: (name *: rest) =>
        inline constValue[name] match
          case _: N => true
          case _    => containsNameImpl[N, rest]

  /** Resolve a panel field name to its binding index. */
  inline def panelFieldIndex[Name <: String, P]: Int =
    inline erasedValue[P] match
      case _: AnyNamedTuple =>
        uniformFieldIndexImpl[
          Name,
          NamedTuple.Names[P & AnyNamedTuple],
          0,
        ]

  /** Generate @group(1) WGSL declarations for all panel texture fields. */
  inline def generatePanelDeclarations[P]: String =
    inline erasedValue[P] match
      case _: EmptyTuple    => ""
      case _: AnyNamedTuple =>
        generatePanelDeclsImpl[
          NamedTuple.Names[P & AnyNamedTuple],
          NamedTuple.DropNames[P & AnyNamedTuple],
        ](0)
      case _ => ""

  private inline def generatePanelDeclsImpl[
      Names <: Tuple,
      Types <: Tuple,
  ](bindingIdx: Int): String =
    inline (erasedValue[Names], erasedValue[Types]) match
      case (_: EmptyTuple, _: EmptyTuple) => ""
      case (_: (name *: namesRest), _: (head *: typesRest)) =>
        val fieldName = constValue[name].asInstanceOf[String]
        val decl =
          s"@group(1) @binding($bindingIdx) var $fieldName: texture_2d<f32>;"
        val rest = generatePanelDeclsImpl[namesRest, typesRest](bindingIdx + 1)
        if rest.isEmpty then decl else s"$decl\n$rest"

  /** Compile-time check that field Name in U is a Sampler uniform.
    * Used in processEntry when binding a GPUSampler.
    */
  inline def checkSamplerFieldType[Name <: String, U]: Unit =
    inline erasedValue[U] match
      case _: AnyNamedTuple =>
        checkSamplerFieldImpl[
          Name,
          NamedTuple.Names[U & AnyNamedTuple],
          NamedTuple.DropNames[U & AnyNamedTuple],
        ]

  private inline def checkSamplerFieldImpl[
      Name <: String,
      Names <: Tuple,
      Types <: Tuple,
  ]: Unit =
    inline (erasedValue[Names], erasedValue[Types]) match
      case (_: EmptyTuple, _) =>
        error("Field not found in Uniforms type")
      case (_: (name *: namesRest), _: (head *: typesRest)) =>
        inline constValue[name] match
          case _: Name =>
            inline erasedValue[head] match
              case _: VertexUniform[Sampler]   => ()
              case _: FragmentUniform[Sampler] => ()
              case _: SharedUniform[Sampler]   => ()
              case _: trivalibs.graphics.math.gpu.Expr.Sampler => ()
              case _ =>
                error(
                  "This uniform field is not a Sampler — bind GPUSampler only to Sampler fields",
                )
          case _ =>
            checkSamplerFieldImpl[Name, namesRest, typesRest]
