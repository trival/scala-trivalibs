package trivalibs.utils.js

import scala.annotation.targetName
import scala.scalajs.js

type JS = js.Object

// === Maybe[T] — js.undefined-based optional (for "not provided" semantics) ===

type Maybe[T] = js.UndefOr[T]
object Maybe:
  inline def Not = js.undefined

inline def maybe[A](condition: Boolean, value: A): Maybe[A] =
  if condition then value else Maybe.Not

extension [A](m: Maybe[A])
  inline def orElse(default: => A): A =
    if js.isUndefined(m.asInstanceOf[js.Any]) then default
    else m.asInstanceOf[A]
  inline def safe: A =
    m.asInstanceOf[A]

type Opt[+A] = A | Null
object Opt:
  inline def Null: Opt[Nothing] = null

extension [A](opt: Opt[A])
  @targetName("opt_isNull")
  inline def isNull: Boolean = opt == null
  @targetName("opt_nonNull")
  inline def nonNull: Boolean = opt != null
  @targetName("opt_getOr")
  inline def getOr(default: => A): A =
    if opt != null then opt.asInstanceOf[A] else default
  @targetName("opt_get")
  inline def get: A = opt.asInstanceOf[A]

object Obj:
  val literal = js.Dynamic.literal

type Dict[A] = js.Dictionary[A]
object Dict:
  inline def apply[A](): Dict[A] = js.Dictionary[A]()

type Arr[A] = js.Array[A]

object Arr:
  /** Returns true if the given value is an array. */
  inline def isArray(arg: scala.Any): Boolean = js.Array.isArray(arg)

  /** <span class="badge badge-ecma2015" style="float: right;">ECMAScript
    * 2015</span>
    *
    * Creates a new array from js.Iterable.
    */
  inline def from[A](iterable: js.Iterable[A]): js.Array[A] =
    js.Array.from(iterable)

  /** Creates a new array with the given items. */
  inline def apply[A](items: A*): js.Array[A] = js.Array(items*)

// Core extensions for for-comprehension support
extension [A](promise: js.Promise[A])
  inline def map[B](f: A => B): js.Promise[B] =
    promise.`then`[B](f)

  inline def flatMap[B](f: A => js.Promise[B]): js.Promise[B] =
    promise.`then`[B](f)

  inline def withFilter(p: A => Boolean): js.Promise[A] =
    promise.`then`[A]: a =>
      if p(a) then a
      else
        throw new NoSuchElementException("Promise.withFilter predicate failed")

  inline def recover[B >: A](pf: PartialFunction[Any, B]): js.Promise[B] =
    promise.`catch`[B]: (err: Any) =>
      if pf.isDefinedAt(err) then pf(err)
      else throw err.asInstanceOf[Throwable]

  inline def foreach(f: A => Unit): Unit =
    promise.`then`[Unit](f)
    ()

  inline def tap(f: A => Unit): js.Promise[A] =
    promise.`then`[A]: a =>
      f(a)
      a

// Nullable promise helpers — delegate to Opt extensions
extension [A](promise: js.Promise[A | Null])

  inline def orError(message: String): js.Promise[A] =
    promise.`then`[A]: value =>
      if value == null then throw new NoSuchElementException(message)
      else value.asInstanceOf[A]

  inline def orElse(default: => A): js.Promise[A] =
    promise.`then`[A]: value =>
      (value: Opt[A]).getOr(default)

inline def log(args: js.Any*) = js.Dynamic.global.console.log(args*)
