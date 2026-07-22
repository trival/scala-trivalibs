package trivalibs.graphics.math.cpu

// Conversions between the mutable `Vec*` classes and their immutable
// `Vec*Tuple` siblings.
//
// Both directions are also available implicitly — the `given Conversion`s live
// in each class companion, so they are in implicit scope with no import at all.
// That is what lets a `(r, g, b, a)` literal still be passed wherever a `Vec4`
// is expected (notably `ClearColor`, which is a `Vec4`).
//
// The named forms below are top-level rather than companion members, for the
// same reason the swizzles in `swizzles.scala` are: it makes them available
// from `import trivalibs.graphics.math.cpu.*`. They must also live in a single
// file — Scala 3 requires all overloads of a top-level name to be defined in
// one group of top-level definitions, and `toTuple` is overloaded per arity.
//
// All `inline`: each compiles to a bare tuple or constructor literal, with no
// wrapper function in the emitted JS.

/** The components of this vector as an immutable [[Vec2Tuple]]. */
extension (v: Vec2) inline def toTuple: Vec2Tuple = (v.x, v.y)

/** The components of this vector as an immutable [[Vec3Tuple]]. */
extension (v: Vec3) inline def toTuple: Vec3Tuple = (v.x, v.y, v.z)

/** The components of this vector as an immutable [[Vec4Tuple]]. */
extension (v: Vec4) inline def toTuple: Vec4Tuple = (v.x, v.y, v.z, v.w)

/** This tuple as a fresh mutable [[Vec2]]. */
extension (t: Vec2Tuple) inline def toVec2: Vec2 = new Vec2(t._1, t._2)

/** This tuple as a fresh mutable [[Vec3]]. */
extension (t: Vec3Tuple) inline def toVec3: Vec3 = new Vec3(t._1, t._2, t._3)

/** This tuple as a fresh mutable [[Vec4]]. */
extension (t: Vec4Tuple)
  inline def toVec4: Vec4 = new Vec4(t._1, t._2, t._3, t._4)
