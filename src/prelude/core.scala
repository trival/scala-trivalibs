package trivalibs.prelude

/** The always-on vocabulary of trivalibs: JS interop containers and numeric
  * extensions. Every sketch and every library file wants these, so they get one
  * import instead of three:
  *
  * ```scala
  * import trivalibs.prelude.core.{*, given}
  * ```
  *
  * covers `trivalibs.utils.js.*`, `trivalibs.utils.numbers.*` (`Pi`, `Tau`, the
  * `Float`/`Double` conversions) and the `NumExt` / `IntExt` givens that carry
  * `x.sin`, `x.sqrt`, `i.timesRepeat` and friends.
  *
  * The original packages keep working unchanged — a file that wants only `Arr`
  * still writes `import trivalibs.utils.js.Arr`. The prelude is a convenience
  * bundle, not a replacement.
  *
  * ==Why the `…$package` paths==
  *
  * Scala 3 refuses a wildcard `export` whose prefix is a package ("not a valid
  * prefix for a wildcard export"). Top-level definitions of a package do live
  * in a synthetic object named after their source file (`js.scala` →
  * `js$package`), and that object *is* a valid prefix — so the wildcard goes
  * through it. One line per source file that carries top-level definitions.
  *
  * Classes, traits and objects are direct package members, not members of the
  * synthetic object, so they are exported by name from the package itself. A
  * name that exists in both namespaces (`type Arr` next to `object Arr`) would
  * then be exported twice and clash, so it is hidden from the wildcard (`Arr as
  * _`) and taken from the named export, which carries both the type and the
  * object.
  *
  * ==Maintenance==
  *
  * Adding a public class/object/trait to one of the bundled packages means
  * adding its name here; adding a source file means adding its `$package` line.
  * Renaming a source file breaks the export path. All three fail loudly at
  * compile time, never silently. See [[trivalibs.prelude.painter]] for the
  * rendering bundle.
  */
object core:

  // trivalibs.utils.js — Arr / Dict / Maybe / Opt / Obj and the JS helpers.
  export trivalibs.utils.js.`js$package`.{
    Maybe as _,
    Dict as _,
    Arr as _,
    *,
    given,
  }
  export trivalibs.utils.js.{Maybe, Dict, Arr, Obj}

  // trivalibs.utils.numbers — Pi, Tau, the Float/Double conversions, NumOps.
  export trivalibs.utils.numbers.`numbers$package`.{*, given}
  export trivalibs.utils.numbers.{NumOps, NumExt, IntExt}

  // The numeric extension methods themselves (`x.sin`, `i.toDouble` …), which
  // live as givens inside the NumExt / IntExt companions.
  export trivalibs.utils.numbers.NumExt.given
  export trivalibs.utils.numbers.IntExt.given
