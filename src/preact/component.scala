package trivalibs.preact.component

import trivalibs.preact.bindings.*
import trivalibs.preact.signals.ComponentVarContext
import trivalibs.preact.signals.VarContext
import trivalibs.utils.js.*

import scala.quoted.*
import scala.scalajs.js

class AttributeModifier[Key <: String, Value](val key: Key, val value: Value):
  inline def apply(jsAttribs: js.Dynamic): Unit =
    if key == "cls" then
      // Special handling for "cls" to concatenate multiple class values
      if value != js.undefined && value != "" then
        val existing = jsAttribs.selectDynamic(key)
        val newValue =
          if existing != js.undefined then s"$existing ${value}"
          else value
        jsAttribs.updateDynamic(key)(newValue.asInstanceOf[js.Any])
    else jsAttribs.updateDynamic(key)(value.asInstanceOf[js.Any])

extension [Key <: String, Value](key: Key)
  inline def :=(value: Value) = AttributeModifier(key, value)

class ChildModifier(val child: Child):
  inline def apply(childrenArray: Arr[Child]): Unit =
    childrenArray.push(child.asInstanceOf[js.Any])

inline def NullChild: ChildModifier = ChildModifier(())

// Given conversions for children
given Conversion[String, ChildModifier] = ChildModifier(_)
given Conversion[Int, ChildModifier] = ChildModifier(_)
given Conversion[Double, ChildModifier] = ChildModifier(_)
given Conversion[VNode, ChildModifier] = ChildModifier(_)

// === Component Factory ===

trait Props extends JS:
  val key: Opt[String] // Dummy prop to satisfy component macro

/** Base class for components with auto-derived Modifier type. The Modifier type
  * is refined by the `component` macro based on the Props type.
  */
abstract class ComponentBase[P <: Props](renderFn: js.Function1[P, VNode]):
  type Modifier

  // Wrap render function ONCE to inject ComponentVarContext for signals
  // This must be a stable reference so Preact doesn't remount the component
  private val wrappedRenderFn: js.Function1[P, VNode] = (props: P) =>
    // println(s"[ComponentBase] Rendering component with ComponentVarContext")
    // Set dynamic context for this render - this affects Var() calls at runtime
    trivalibs.preact.signals.withContext(ComponentVarContext):
      renderFn(props)

  def apply(ms: Modifier*): VNode =
    val attrs = Obj.literal()
    val children = Arr[Child]()
    ms.foreach:
      case am: AttributeModifier[?, ?] =>
        am(attrs)
      case cm: ChildModifier =>
        cm(children)

    h(wrappedRenderFn, attrs, children)

/** Create a component with auto-derived Modifier type from the Props type P.
  *
  * Usage:
  * {{{
  * trait MyProps extends js.Object:
  *   val title: String
  *   val count: Int
  *
  * val MyComponent = component[MyProps]: props =>
  *   div(props.title, props.count.toString)
  *
  * // Usage: MyComponent("title" := "Hello", "count" := 42)
  * }}}
  */
transparent inline def component[P <: Props](inline renderFn: P => VNode): Any =
  ${ componentImpl[P]('renderFn) }

private def componentImpl[P: Type](
    renderFn: Expr[P => VNode]
)(using Quotes): Expr[Any] =
  import quotes.reflect.*

  val tpe = TypeRepr.of[P]
  val typeSymbol = tpe.typeSymbol
  val fields = typeSymbol.memberFields

  if fields.isEmpty then
    report.errorAndAbort(s"Type ${typeSymbol.name} has no fields")

  val childrenType = TypeRepr.of[Children]
  val attrModSymbol =
    Symbol.requiredClass("trivalibs.preact.component.AttributeModifier")

  // Separate children field from other fields
  val (childrenFields, otherFields) = fields.partition: fieldSym =>
    fieldSym.name == "children" && tpe.memberType(fieldSym) =:= childrenType

  // Build AttributeModifier[fieldName, fieldType] for non-children fields
  val fieldTypes: List[TypeRepr] = otherFields.map: fieldSym =>
    val fieldName = fieldSym.name
    val memberType = tpe.memberType(fieldSym)
    val fieldNameType = ConstantType(StringConstant(fieldName))
    AppliedType(attrModSymbol.typeRef, List(fieldNameType, memberType))

  // Only add ChildModifier if there's a children: Children field
  val allTypes =
    if childrenFields.nonEmpty then fieldTypes :+ TypeRepr.of[ChildModifier]
    else fieldTypes

  if allTypes.isEmpty then
    report.errorAndAbort(s"Type ${typeSymbol.name} has no usable fields")

  val unionType = allTypes.reduce((left, right) => OrType(left, right))

  // report.info(s"Component Modifier type: ${unionType.show}")

  // Generate ComponentBase subclass with concrete Modifier type
  (tpe.asType, unionType.asType) match
    case ('[p], '[modifierType]) =>
      '{
        new ComponentBase[p & Props]($renderFn.asInstanceOf[p & JS => VNode]):
          type Modifier = modifierType
      }
    case _ =>
      report.errorAndAbort("Failed to construct component types")
