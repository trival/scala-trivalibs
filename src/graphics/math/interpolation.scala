package trivalibs.graphics.math

import trivalibs.graphics.math.cpu.{Vec2Tuple, Vec3Tuple, Vec4Tuple, given}
import trivalibs.utils.numbers.NumExt.given

// Interpolation primitives shared across the library.
//
// `LerpBy` is the *abstracted-over* form: a constraint for algorithms that are
// generic in the value they interpolate (`Line[T]`, `Quad.subdivide`,
// `Grid.subdivide`, `splitByPlane`). Concrete interpolation on a known vector
// type goes through the ops traits instead — `a.mix(b, t)`, and the Bézier
// statics `Vec2.cubicBezier(t, a, c1, c2, b)` — which compile to plain
// arithmetic with no instance to summon.
//
// The givens here are deliberately reachable through implicit scope only, never
// exported into identifier scope by the preludes: an extension method resolves
// by name from a single source, so an imported `LerpBy` given would *hide* the
// ops traits' own `lerp` — collapsing their overload sets (component-wise `t`,
// CPU-value bounds, literal bounds) to this single signature. See
// `documents/lerp-typeclass-refactor.md`.
//
// If a generic curve consumer ever appears (a spline / catmull-rom builder, or
// `Line[T]` growing curved segments in T-space), extend this module with an
// `Interpolate[T] extends Lerp[T]` carrying the Bézier ops, with the vector
// instances delegating to those statics — so the math keeps exactly one
// implementation and the type class stays a thin dispatch layer.

/** Linear interpolation of a `T` by a parameter of type `P`.
  *
  * The second parameter exists for exactly one reason: the shader side's scalar
  * is `FloatExpr`, and a shader interpolates by a `FloatExpr` that varies per
  * fragment. It is **not** there to accommodate `Float` — on Scala.js every
  * number is a double, `Float` only adds conversion boilerplate on the
  * Scala↔JS boundary, and CPU code here does not use it. That is why the
  * original `Lerp` hardcoded `Double`, and why [[Lerp]] — the `Double` alias —
  * remains the only spelling any CPU algorithm needs.
  */
trait LerpBy[T, P]:
  extension (a: T) def lerp(b: T, t: P): T

/** [[LerpBy]] with a plain `Double` parameter — the CPU case, and what
  * `[T: Lerp]` means at every subdivision / plane-clipping / `Line` call site.
  *
  * Use it in *constraints* (`[T: Lerp]`, `using Lerp[T]`). Do **not** use it as
  * the parent of a `given …:` body — an alias is not a class type there, and
  * whether it dealiases depends on compilation order, so it compiles in one
  * build and fails in another. The instances spell `LerpBy[T, Double]` out.
  */
type Lerp[T] = LerpBy[T, Double]

// The givens live in the companion so implicit search finds them without an
// import at every call site — and so that they stay *out* of identifier scope,
// where they would hide the vector ops' own `lerp` (see the note above).
//
// Every instance is concrete and non-parameterized — a module, which costs one
// singleton and no allocation at any call site. A parameterized given would
// instead materialise an anonymous class per call site and `new` it on every
// call; that is measured in `documents/lerp-typeclass-refactor.md`, and it is
// why there is no generic `[V] => Vec3ImmutableOps[V] => Lerp[V]` here. The
// price is that a new representation needs its instance added by hand.
object LerpBy:

  /** `Double` interpolates by itself. Two deliberate choices here, both
    * performance rather than style, both measured (see
    * `documents/lerp-typeclass-refactor.md`):
    *
    *   - **concrete instances, not one `[P: NumExt] => LerpBy[P, P]`.** A
    *     parameterized given compiles to a class, and at an `inline` use site
    *     the class is duplicated per call site and `new`-ed on every call. A
    *     non-parameterized given is a module — no allocation, ever.
    *   - **`inline def lerp`.** With a module given the receiver is statically
    *     known at an inlined `lerpIn`, so an `inline` body folds the whole call
    *     into arithmetic: `t.lerpIn(a, b)` emits `a * (1.0 - t) + b * t`,
    *     matching what `NumExt`'s own `inline lerpIn` used to emit. Without it
    *     the call survives as a module call. (An `inline` member is only legal
    *     because these are given *bodies*; inside an `inline given` alias it
    *     would be a nested inline method, which the compiler rejects.)
    *
    * `FloatExpr` gets its own instance in `gpu/expr.scala`, in the companion
    * that puts it in implicit scope.
    */
  given doubleLerp: LerpBy[Double, Double]:
    extension (a: Double)
      inline def lerp(b: Double, t: Double): Double = a.mix(b, t)

  /** For data-less generic containers — `Line[Unit]` needs a `Lerp` even though
    * there is nothing to interpolate.
    */
  given unitLerp: LerpBy[Unit, Double]:
    extension (a: Unit) def lerp(b: Unit, t: Double): Unit = ()

  /** The tuple representations. Concrete like every other instance — but they
    * have to live *here* rather than in `object Vec3Tuple`, because
    * `Vec3Tuple` is an alias for `(Double, Double, Double)`: the implicit scope
    * of `Lerp[Vec3Tuple]` is the companions of `Tuple3` and `Double`, never
    * `object Vec3Tuple`. `LerpBy`'s own companion is always in scope, so that
    * is where they go.
    */
  given vec2TupleLerp: LerpBy[Vec2Tuple, Double]:
    extension (a: Vec2Tuple)
      inline def lerp(b: Vec2Tuple, t: Double): Vec2Tuple = a.mix(b, t)

  given vec3TupleLerp: LerpBy[Vec3Tuple, Double]:
    extension (a: Vec3Tuple)
      inline def lerp(b: Vec3Tuple, t: Double): Vec3Tuple = a.mix(b, t)

  given vec4TupleLerp: LerpBy[Vec4Tuple, Double]:
    extension (a: Vec4Tuple)
      inline def lerp(b: Vec4Tuple, t: Double): Vec4Tuple = a.mix(b, t)

/** Reverse-argument `lerp` on `Double` — the receiver is the interpolation
  * parameter `t`, the bounds are the arguments: `t.lerpIn(lo, hi)`. Any
  * [[Lerp]] type works as the bounds, scalars and every `Vec2`/`Vec3`/`Vec4`
  * representation included — `0.5.lerpIn(0.0, 1.0)`,
  * `t.lerpIn(FloorTint, CeilTint)`.
  *
  * `inline` with an `inline` context parameter so the summoned instance folds
  * away and the call compiles to the same arithmetic a direct `lo.mix(hi, t)`
  * does.
  *
  * The GPU side defines its own `lerpIn` on `FloatExpr`
  * (`math/gpu/float_expr.scala`) rather than sharing this one: `Double`
  * converts to `FloatExpr`, so a single mixed-domain definition would let a CPU
  * call resolve into the GPU set and hand back an expression where the author
  * wrote CPU math.
  */
// `lerpIn` is defined once per *receiver domain* — here for `Double`, and for
// `FloatExpr` in `gpu/float_expr.scala` — rather than once on `LerpBy` itself.
// That is not duplication kept for speed; the unified form does not work:
//
//   trait LerpBy[T, P]:
//     extension (t: P) def lerpIn(lo: T, hi: T): T = lo.lerp(hi, t)
//
// fails twice over. The receiver `P` is not enough to find the instance —
// `Double`'s implicit scope does not contain `LerpBy`'s companion, so nothing
// resolves until the givens are imported into identifier scope, which is the
// one thing that must not happen (it hides the ops traits' `lerp`). And with
// them imported it is *ambiguous*: the receiver fixes only `P`, leaving `T`
// free, so every instance sharing a parameter type offers the same extension —
// "both object doubleLerp and object unitLerp provide an extension method
// lerpIn on (0.5d : Double)". Taking the receiver as the *parameter* and
// inferring `T` from the arguments is what makes the call site resolvable.
extension (t: Double)
  inline def lerpIn[T](lo: T, hi: T)(using inline l: Lerp[T]): T =
    l.lerp(lo)(hi, t)
