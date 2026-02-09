package trivalibs.preact.signals

import trivalibs.preact.bindings.*

/** Context that provides methods for creating reactive Vars. Different
  * implementations provide global (signal) or component-local (useSignal)
  * variants.
  */
trait VarContext:
  def createVar[A](initialValue: A): Var[A]
  def createMemo[A](computation: => A): ReadVar[A]
  def createEffect(body: => Unit): PreactEffectHandle

/** Global context - uses signal() from @preact/signals */
object GlobalVarContext extends VarContext:
  inline def createVar[A](initialValue: A): Var[A] =
    // println(
    //   s"[GlobalVarContext] Creating signal with initial value: $initialValue"
    // )
    new Var(signal(initialValue))

  inline def createMemo[A](computation: => A): ReadVar[A] =
    // println("[GlobalVarContext] Creating computed signal")
    new Memo(computed(() => computation))
  inline def createEffect(body: => Unit): PreactEffectHandle =
    // println("[GlobalVarContext] Creating effect")
    effect(() => body)

/** Component-local context - uses useSignal() hook from @preact/signals
  * IMPORTANT: This must only be used during component render, not after
  */
object ComponentVarContext extends VarContext:
  inline def createVar[A](initialValue: A): Var[A] =
    // println(
    //   s"[ComponentVarContext] Creating useSignal with initial value: $initialValue"
    // )
    new Var(useSignal(initialValue))

  inline def createMemo[A](computation: => A): ReadVar[A] =
    // println("[ComponentVarContext] Creating useComputed")
    new Memo(useComputed(() => computation))

  inline def createEffect(body: => Unit): PreactEffectHandle =
    // println("[ComponentVarContext] Creating useSignalEffect")
    useSignalEffect(() => body)
