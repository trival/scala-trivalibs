package trivalibs.preact.signals

import trivalibs.preact.bindings.*
import trivalibs.preact.component.ChildModifier

import scala.scalajs.js

/** Public API for creating reactive variables. Automatically dispatches to
  * signal() or useSignal() based on runtime context.
  */
object Var:
  /** Create a reactive variable. Uses signal() in global context, useSignal()
    * in component context. Context is determined dynamically at runtime.
    */
  inline def apply[A](initialValue: A): Var[A] =
    currentContext.value.createVar(initialValue)

  /** Create a memo (derived) reactive value. Uses computed() in global context,
    * useComputed() in component context. Context is determined dynamically at
    * runtime.
    */
  inline def memo[A](computation: => A): ReadVar[A] =
    currentContext.value.createMemo(computation)

  /** Create an effect that runs when dependencies change. Uses effect() in
    * global context, useSignalEffect() in component context. Context is
    * determined dynamically at runtime.
    */
  inline def effect(body: => Unit): PreactEffectHandle =
    currentContext.value.createEffect(body)

  /** Batch multiple updates into one commit */
  inline def batch(updates: => Unit): Unit =
    batch$js(() => updates)

/** Read-only reactive variable (covariant) */
sealed trait ReadVar[+A]:
  /** Read the current value */
  def apply(): A

  /** Expose underlying signal for direct JS interop */
  def underlying: js.Any

  /** Map to create derived value */
  def map[B](f: A => B): ReadVar[B] =
    currentContext.value.createMemo(f(apply()))

/** Writable reactive variable */
final class Var[A](val underlying: PreactSignal[A]) extends ReadVar[A]:
  /** Read the current value */
  inline def apply(): A = underlying.value

  /** Write a new value */
  inline def apply(value: A): Unit = underlying.value = value

  /** Update via function */
  inline def apply(f: A => A): Unit = underlying.value = f(underlying.value)

/** Computed (derived) reactive value - read-only */
final class Memo[A](val underlying: PreactComputed[A]) extends ReadVar[A]:
  /** Read the current value */
  inline def apply(): A = underlying.value

/** Integration with the HTML DSL modifier system.
  *
  * Allows using signals directly in HTML templates. Preact will automatically
  * track signal access and update the DOM when signals change.
  */

// Convert signals to child modifiers - allows using signals directly as children
given stringVarToChild: Conversion[ReadVar[String], ChildModifier] =
  signal => ChildModifier(signal.underlying.asInstanceOf[Child])

given intVarToChild: Conversion[ReadVar[Int], ChildModifier] =
  signal => ChildModifier(signal.underlying.asInstanceOf[Child])

given doubleVarToChild: Conversion[ReadVar[Double], ChildModifier] =
  signal => ChildModifier(signal.underlying.asInstanceOf[Child])

given vnodeVarToChild: Conversion[ReadVar[VNode], ChildModifier] =
  signal => ChildModifier(signal.underlying.asInstanceOf[Child])
