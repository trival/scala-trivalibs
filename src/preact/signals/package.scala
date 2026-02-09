package trivalibs.preact

import scala.util.DynamicVariable

/** Preact signals integration with automatic context-based dispatch.
  *
  * Uses DynamicVariable to switch between global context (signal()) and
  * component context (useSignal()) at runtime.
  */
package object signals:
  /** Runtime context that switches between global and component contexts.
    * Defaults to GlobalVarContext, components set it to ComponentVarContext.
    */
  private val currentContext = new DynamicVariable[VarContext](GlobalVarContext)

  /** Run code with a specific VarContext.
    * Used internally by components to inject ComponentVarContext.
    */
  def withContext[A](ctx: VarContext)(body: => A): A =
    currentContext.withValue(ctx)(body)
