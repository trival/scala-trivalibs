package trivalibs.utils.js

import scala.scalajs.js

type JS = js.Object

type Opt[T] = js.UndefOr[T]
object Opt:
  inline def Null = js.undefined

inline def maybe[A](condition: Boolean, value: A): Opt[A] =
  if condition then value else Opt.Null

extension [A](opt: Opt[A])
  inline def getOr(default: => A): A =
    opt.getOrElse(default)
  inline def safe: A =
    opt.asInstanceOf[A]

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

  inline def tap(f: A => Unit): js.Promise[A] =
    promise.`then`[A]: a =>
      f(a)
      a

// Nullable value handling (for `T | Null` results)
extension [A](promise: js.Promise[A | Null])
  inline def toOption: js.Promise[Option[A]] =
    promise.`then`[Option[A]]: value =>
      if value == null then None
      else Some(value.asInstanceOf[A])

  inline def orError(message: String): js.Promise[A] =
    promise.`then`[A]: value =>
      if value == null then throw new NoSuchElementException(message)
      else value.asInstanceOf[A]

  inline def orElse(default: => A): js.Promise[A] =
    promise.`then`[A]: value =>
      if value == null then default
      else value.asInstanceOf[A]

inline def log(args: js.Any*) = js.Dynamic.global.console.log(args*)
