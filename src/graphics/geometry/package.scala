package trivalibs.graphics.geometry

import trivalibs.graphics.math.*
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.given
import trivalibs.utils.js.*

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Elem
import scala.NamedTuple.Names
import scala.compiletime.constValue
import scala.compiletime.ops.int.S
import scala.scalajs.js

// position type class and givens

/** Extracts a `Vec3` position from a vertex `T`. Given automatically for any
  * `Vec3`-like type and for any named tuple / case class with a `position: Vec3`
  * field (so `(position: Vec3, uv: Vec2)` just works). Required by [[Mesh]],
  * [[toBufferedGeometry]], and the polygon ops. */
trait Position[T]:
  extension (t: T) def pos: Vec3

given [V] => Vec3Base[V] => Position[V]:
  extension (v: V) def pos: Vec3 = Vec3(v.x, v.y, v.z)

// Index of the literal "position" field within a named tuple's names. The
// match stays stuck (a compile error) for tuples lacking such a field, so the
// derived given below only applies to vertex types that actually carry one.
type PositionIndex[N <: Tuple] <: Int = N match
  case "position" *: _ => 0
  case _ *: rest       => S[PositionIndex[rest]]

// Bakes the resolved field index into a runtime instance so the abstract `.pos`
// used inside `Mesh[T]` reads the right slot without per-call inlining.
private def positionAtIndex[T](idx: Int): Position[T] =
  new Position[T]:
    extension (t: T)
      def pos: Vec3 =
        t.asInstanceOf[Product].productElement(idx).asInstanceOf[Vec3]

// Generic Position for any named tuple / case class with a `position: Vec3`
// field — derives the index at the (concrete) summon site via `constValue`.
inline given [T <: AnyNamedTuple]
  => (Elem[T, PositionIndex[Names[T]]] =:= Vec3) => Position[T] =
  positionAtIndex[T](constValue[PositionIndex[Names[T]]])

// lerp type class and givens

/** Linear interpolation for `T` — given for `Double` and the `Vec2-4` types.
  * Enables vertex interpolation in subdivision / plane clipping
  * (`Quad.subdivide*`, `Grid.subdivide`, `splitByPlane`). */
trait Lerp[T]:
  extension (a: T) def lerp(b: T, t: Double): T

given doubleLerp: Lerp[Double]:
  extension (a: Double) def lerp(b: Double, t: Double): Double = a + (b - a) * t

given vec3Lerp: [V] => Vec3Base[V] => Vec3ImmutableOps[V] => Lerp[V]:
  extension (a: V) def lerp(b: V, t: Double): V = a.mix(b, t)

given vec2Lerp: [V] => Vec2Base[V] => Vec2ImmutableOps[V] => Lerp[V]:
  extension (a: V) def lerp(b: V, t: Double): V = a.mix(b, t)

given vec4Lerp: [V] => Vec4Base[V] => Vec4ImmutableOps[V] => Lerp[V]:
  extension (a: V) def lerp(b: V, t: Double): V = a.mix(b, t)

// geometry types

/** An infinite plane `normal · p = d`, used to clip/split polygons
  * ([[Triangle.splitByPlane]] / [[Quad.splitByPlane]]). `signedDist` is positive
  * on the normal's side; `flip` reverses the facing. */
class Plane(val normal: Vec3, val d: Double):
  def signedDist(p: Vec3): Double = normal.dot(p) - d
  def flip: Plane = Plane(-normal, -d)

// float equality helpers

private inline val ROUNDING = 10000.0
private inline val DELTA = 1.0 / ROUNDING

def posKey(v: Vec3): String =
  s"${(v.x * ROUNDING).toInt},${(v.y * ROUNDING).toInt},${(v.z * ROUNDING).toInt}"

// math extensions

extension (v: Vec3)
  def approxEq(w: Vec3): Boolean =
    (v.x * ROUNDING).toInt == (w.x * ROUNDING).toInt &&
      (v.y * ROUNDING).toInt == (w.y * ROUNDING).toInt &&
      (v.z * ROUNDING).toInt == (w.z * ROUNDING).toInt
  def onPlane(p: Plane): Boolean = math.abs(p.signedDist(v)) < DELTA
  def inFront(p: Plane): Boolean = p.signedDist(v) > DELTA
  def behind(p: Plane): Boolean = p.signedDist(v) < -DELTA
