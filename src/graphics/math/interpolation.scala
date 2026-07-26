package trivalibs.graphics.math

// Interpolation primitives shared across the library.
//
// `Lerp` is the *abstracted-over* form: a constraint for algorithms that are
// generic in the value they interpolate (`Line[T]`, `Quad.subdivide`,
// `Grid.subdivide`, `splitByPlane`). Concrete interpolation on a known vector
// type goes through the ops traits instead — `a.mix(b, t)`, and the Bézier
// statics `Vec2.cubicBezier(t, a, c1, c2, b)` — which compile to plain
// arithmetic with no instance to summon.
//
// If a generic curve consumer ever appears (a spline / catmull-rom builder, or
// `Line[T]` growing curved segments in T-space), extend this module with an
// `Interpolate[T] extends Lerp[T]` carrying the Bézier ops, with the vector
// instances delegating to those statics — so the math keeps exactly one
// implementation and the type class stays a thin dispatch layer.

/** Linear interpolation for `T` — given for `Double`, `Unit` and the `Vec2-4`
  * types. Enables vertex interpolation in subdivision / plane clipping
  * (`Quad.subdivide*`, `Grid.subdivide`, `splitByPlane`) and in the `Line`
  * transformations.
  */
trait Lerp[T]:
  extension (a: T) def lerp(b: T, t: Double): T

// The givens live in the companion so implicit search finds them without an
// `import trivalibs.graphics.math.given` at every call site.
object Lerp:
  given doubleLerp: Lerp[Double]:
    extension (a: Double)
      def lerp(b: Double, t: Double): Double = a + (b - a) * t

  /** For data-less generic containers — `Line[Unit]` needs a `Lerp` even though
    * there is nothing to interpolate.
    */
  given unitLerp: Lerp[Unit]:
    extension (a: Unit) def lerp(b: Unit, t: Double): Unit = ()

  given vec3Lerp: [V] => Vec3Base[V] => Vec3ImmutableOps[V] => Lerp[V]:
    extension (a: V) def lerp(b: V, t: Double): V = a.mix(b, t)

  given vec2Lerp: [V] => Vec2Base[V] => Vec2ImmutableOps[V] => Lerp[V]:
    extension (a: V) def lerp(b: V, t: Double): V = a.mix(b, t)

  given vec4Lerp: [V] => Vec4Base[V] => Vec4ImmutableOps[V] => Lerp[V]:
    extension (a: V) def lerp(b: V, t: Double): V = a.mix(b, t)
