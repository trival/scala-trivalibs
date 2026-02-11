package trivalibs.utils.promise

import scala.scalajs.js

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
