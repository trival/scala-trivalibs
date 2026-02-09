package trivalibs.utils.js

import scala.scalajs.js

type Opt[T] = js.UndefOr[T]
type JS = js.Object

inline def maybe[A](condition: Boolean, value: A): Opt[A] =
  if condition then value else js.undefined

/** Helper to make tailwind recognize class names */
inline def cls(str: String): String = str
