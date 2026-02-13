package trivalibs.utils.js

import scala.scalajs.js

type Opt[T] = js.UndefOr[T]
type JS = js.Object

inline def maybe[A](condition: Boolean, value: A): Opt[A] =
  if condition then value else js.undefined

object Obj:
  val literal = js.Dynamic.literal

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

def test(): Unit =
  val x = Obj.literal(a = 1, b = "hello")

/** Helper to make tailwind recognize class names */
inline def cls(str: String): String = str
