# WGSL Scala DSL — Implementation Design

## 1. Overview

### What This Replaces

Currently, shader bodies are raw WGSL strings:

```scala
val shade = painter.shade[Attribs, Varyings, Uniforms](
  vertWgsl = """
    let rotated = rotation * in.position;
    out.position = vec4<f32>(rotated + translation, 0.0, 1.0);
    """,
  fragWgsl = """
    out.color = vec4<f32>(color, 1.0);
    """,
)
```

The existing `ShaderDef` already derives struct declarations, uniform bindings,
and function signatures from type parameters. Only the function _bodies_ are
untyped strings. This DSL replaces those strings with typed Scala expressions
that compile to WGSL.

### Why

1. **Type safety** — catch WGSL errors at Scala compile time (wrong component
   access, type mismatches in operators)
2. **Unified CPU/GPU vocabulary** — the same `.sin`, `.normalized`, `+`, `*`
   syntax works on CPU math types and GPU expression types
3. **Refactoring safety** — renaming a uniform field propagates through the DSL;
   currently it silently breaks the raw WGSL string
4. **Path to shared functions** — once both sides share the same trait
   vocabulary, a single function definition can emit either CPU code or WGSL

### Target API

```scala
val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
  program.vert[(rotated: Vec2)]: ctx =>
    Block(
      ctx.locals.rotated := ctx.bindings.rotation * ctx.in.position,
      ctx.out.position := vec4(
        ctx.locals.rotated + ctx.bindings.translation, 0.0, 1.0,
      ),
    )

  program.frag: ctx =>
    Block(
      ctx.out.color := vec4(ctx.bindings.color, 1.0),
    )
```

Key distinction: `val rotated = ctx.bindings.rotation * ctx.in.position` creates
a Scala-level alias that **inlines** the expression everywhere it appears in the
generated WGSL. By contrast, `ctx.locals.rotated := expr` generates an actual
WGSL `let rotated = expr;` statement — a named intermediate value in the shader.

---

## 2. Math API Consistency & Evolution Guidelines

### 2.1 The Principle: CPU/GPU Naming Parity

Every math operation must use the **same name and syntax** on CPU and GPU. When
adding new operations to `gpu.math`, they must also be expressible in WGSL. When
exposing WGSL built-ins in the DSL, they must follow the naming conventions
established in `src/gpu/math/api_and_naming_conventions.md`.

**Extension method syntax is preferred over free function syntax:**

```scala
// ✅ Preferred — matches CPU API
x.sin
x.clamp(lo, hi)
v.normalized
v.dot(w)

// ❌ Avoid — function call syntax
sin(x)
clamp(x, lo, hi)
normalize(v)
dot(v, w)
```

Free functions are used only for WGSL built-ins that have no natural receiver
object (e.g., `select(f, t, cond)` — ternary-like, no obvious receiver).
Multi-argument WGSL built-ins where the first argument is the primary operand
are still implemented as extension methods: `a.mix(b, t)`, `a.min(b)`,
`x.smoothstep(lo, hi)`, etc.

### 2.2 Math Library Evolution Rules

These guidelines govern all future changes to `src/gpu/math/`:

1. **WGSL parity required** — only add operations that have a WGSL equivalent.
   If an operation cannot be expressed in WGSL, it does not belong in the shared
   math vocabulary. CPU-only utilities go in a separate package.

2. **Extension method syntax** — follow the existing `x.sin`, `x.clamp(lo, hi)`
   pattern from `NumExt`. New operations are added as extension methods, not
   free functions.

3. **Immutable/mutable naming** — per `api_and_naming_conventions.md`:
   - Past tense / adjective for immutable ops: `v.normalized`, `m.transposed`
   - Present tense + `(out)` for mutable ops: `v.normalize(out)`,
     `m.transpose(out)`

4. **Trait type parameter order** — always `[Num, Container]` (e.g.,
   `[Double, Vec3]`, `[FloatExpr, Vec3Expr]`).

5. **Consistent naming across types** — if `Vec3` has `.length`, `Vec2` and
   `Vec4` must too. If `Mat4` has `.determinant`, so must `Mat2` and `Mat3`.

6. **No CPU-only math in the shared API** — operations like `fit0111` /
   `fit1101` that map to inline arithmetic (`x * 2 - 1`) are fine (they expand
   to valid WGSL expressions). But operations requiring CPU-specific runtime
   support (e.g., serialization, string formatting) stay in CPU-only packages.

### 2.3 CPU ↔ WGSL Operation Mapping

This table is the canonical reference for name parity. When adding a new
operation to either side, add it here first.

#### Scalar Operations (NumExt → FloatExpr)

| CPU (`NumExt`)  | GPU (DSL)       | WGSL Output       | Notes                    |
| --------------- | --------------- | ----------------- | ------------------------ |
| `x.sin`         | `x.sin`         | `sin(x)`          |                          |
| `x.cos`         | `x.cos`         | `cos(x)`          |                          |
| `x.tan`         | `x.tan`         | `tan(x)`          |                          |
| `x.asin`        | `x.asin`        | `asin(x)`         |                          |
| `x.acos`        | `x.acos`        | `acos(x)`         |                          |
| `x.atan`        | `x.atan`        | `atan(x)`         |                          |
| `x.atan2(y)`    | `x.atan2(y)`    | `atan2(x, y)`     |                          |
| `x.sqrt`        | `x.sqrt`        | `sqrt(x)`         |                          |
| `x.pow(y)`      | `x.pow(y)`      | `pow(x, y)`       |                          |
| `x.abs`         | `x.abs`         | `abs(x)`          |                          |
| `x.floor`       | `x.floor`       | `floor(x)`        |                          |
| `x.ceil`        | `x.ceil`        | `ceil(x)`         |                          |
| `x.clamp(a, b)` | `x.clamp(a, b)` | `clamp(x, a, b)`  |                          |
| `x.clamp01`     | `x.clamp01`     | `saturate(x)`     | WGSL `saturate` = [0, 1] |
| `x.fit0111`     | `x.fit0111`     | `(x * 2.0 - 1.0)` | Inline arithmetic        |
| `x.fit1101`     | `x.fit1101`     | `(x * 0.5 + 0.5)` | Inline arithmetic        |
| —               | `x.sign`        | `sign(x)`         | Add to NumExt            |
| —               | `x.round`       | `round(x)`        | Add to NumExt            |
| —               | `x.trunc`       | `trunc(x)`        | Add to NumExt            |
| —               | `x.fract`       | `fract(x)`        | Add to NumExt            |
| —               | `x.exp`         | `exp(x)`          | Add to NumExt            |
| —               | `x.exp2`        | `exp2(x)`         | Add to NumExt            |
| —               | `x.log`         | `log(x)`          | Add to NumExt            |
| —               | `x.log2`        | `log2(x)`         | Add to NumExt            |
| —               | `x.inverseSqrt` | `inverseSqrt(x)`  | Add to NumExt            |
| —               | `x.degrees`     | `degrees(x)`      | Add to NumExt            |
| —               | `x.radians`     | `radians(x)`      | Add to NumExt            |
| —               | `x.sinh`        | `sinh(x)`         | Add to NumExt            |
| —               | `x.cosh`        | `cosh(x)`         | Add to NumExt            |
| —               | `x.tanh`        | `tanh(x)`         | Add to NumExt            |

Operations marked "Add to NumExt" should be added to
`trivalibs/src/utils/numbers.scala` to maintain CPU/GPU parity. They all have
trivial `Math.*` implementations.

#### Vector Operations (Vec*Base/ImmutableOps → Vec*Expr)

| CPU                | GPU (DSL)          | WGSL Output        | Notes                  |
| ------------------ | ------------------ | ------------------ | ---------------------- |
| `v + w`            | `v + w`            | `(v + w)`          |                        |
| `v - w`            | `v - w`            | `(v - w)`          |                        |
| `v * w`            | `v * w`            | `(v * w)`          | Componentwise          |
| `v * s`            | `v * s`            | `(v * s)`          | Scalar multiply        |
| `v / w`            | `v / w`            | `(v / w)`          | Componentwise          |
| `v / s`            | `v / s`            | `(v / s)`          | Scalar divide          |
| `v.dot(w)`         | `v.dot(w)`         | `dot(v, w)`        |                        |
| `v.cross(w)`       | `v.cross(w)`       | `cross(v, w)`      | Vec3 only              |
| `v.length`         | `v.length`         | `length(v)`        |                        |
| `v.length_squared` | `v.length_squared` | `dot(v, v)`        | No WGSL built-in       |
| `v.normalize`      | `v.normalize`      | `normalize(v)`     | Immutable convention   |
| `v.x` / `.y` / …   | `v.x` / `.y` / …   | `v.x` / `.y` / …   | Component access       |
| `v.r` / `.g` / …   | `v.r` / `.g` / …   | `v.x` / `.y` / …   | Color aliases          |
| —                  | `v.distance(w)`    | `distance(v, w)`   | Add to VecBase         |
| —                  | `v.reflect(n)`     | `reflect(v, n)`    | Add to VecImmutableOps |
| —                  | `v.refract(n, η)`  | `refract(v, n, η)` | Add to VecImmutableOps |

Note: MutableOps (`v.normalizeSelf()`, `v.addTo(out, w)`) have no GPU equivalent
— WGSL is a functional language with no in-place mutation of vectors. MutableOps
remain CPU-only.

#### Matrix Operations (Mat*ImmutableOps → Mat*Expr)

| CPU             | GPU (DSL)       | WGSL Output      | Notes            |
| --------------- | --------------- | ---------------- | ---------------- |
| `m * n`         | `m * n`         | `(m * n)`        | Matrix multiply  |
| `m * v`         | `m * v`         | `(m * v)`        | Matrix × vector  |
| `m * s`         | `m * s`         | `(m * s)`        | Scalar multiply  |
| `m.transpose`   | `m.transpose`   | `transpose(m)`   |                  |
| `m.determinant` | `m.determinant` | `determinant(m)` |                  |
| `m.inversed`    | —               | —                | No WGSL built-in |

#### Multi-Argument Operations (Extension Methods)

These WGSL built-ins take multiple arguments but have a clear primary operand
(the first argument). They are implemented as extension methods, not free
functions:

| CPU                    | GPU (DSL)              | WGSL Output             | Notes         |
| ---------------------- | ---------------------- | ----------------------- | ------------- |
| `a.min(b)`             | `a.min(b)`             | `min(a, b)`             | Add to NumExt |
| `a.max(b)`             | `a.max(b)`             | `max(a, b)`             | Add to NumExt |
| `a.mix(b, t)`          | `a.mix(b, t)`          | `mix(a, b, t)`          | Add to NumExt |
| `x.step(edge)`         | `x.step(edge)`         | `step(edge, x)`         | Add to NumExt |
| `x.smoothstep(lo, hi)` | `x.smoothstep(lo, hi)` | `smoothstep(lo, hi, x)` | Add to NumExt |
| `a.fma(b, c)`          | `a.fma(b, c)`          | `fma(a, b, c)`          | Add to NumExt |

These have overloads for `FloatExpr` and `Vec*Expr` arguments as appropriate.

Note: `step` and `smoothstep` have reversed argument order between the extension
method (receiver = value being tested) and the WGSL built-in (edge/bounds come
first). The extension syntax reads more naturally: `x.smoothstep(lo, hi)` =
"smooth-step x between lo and hi".

#### Free Functions (No Natural Receiver)

Only `select` remains as a free function — it is a ternary-like operation with
no obvious primary operand:

```scala
select(f, t, cond)    // -> select(f, t, cond)  Ternary: if cond then t else f
```

---

## 3. Expression Types

### 3.1 Representation: Opaque Types over String

Following the project's bundle-size rules (no `enum`, no `case class` for data
nodes), expressions are **opaque types wrapping `String`** at runtime. The
string contains a valid WGSL expression fragment. Type safety is enforced at
compile time; at runtime, an expression is just a string.

```scala
package gpu.shader.dsl

// Typed expression wrappers — phantom type information, zero runtime cost
opaque type Expr = String
opaque type FloatExpr <: Expr = String
opaque type Vec2Expr  <: Expr = String
opaque type Vec3Expr  <: Expr = String
opaque type Vec4Expr  <: Expr = String
opaque type Mat2Expr  <: Expr = String
opaque type Mat3Expr  <: Expr = String
opaque type Mat4Expr  <: Expr = String
opaque type BoolExpr  <: Expr = String
opaque type IntExpr   <: Expr = String
opaque type UIntExpr  <: Expr = String
```

All `*Expr` types are subtypes of `Expr`, so any function accepting `Expr` works
on all of them. But operators are defined only on specific types, preventing
e.g. `Vec2Expr + Mat4Expr`.

### 3.2 Why Opaque Types over AST Nodes

An AST approach (e.g., `sealed trait Expr; case class Add(l: Expr, r: Expr)`) is
viable — pattern matching is a language feature (not stdlib), case classes
compile efficiently, and shader compilation is a one-time operation (not in the
render loop). However, the opaque-over-String approach is preferred because:

- **Minimal JS bundle** — no class hierarchy, no vtable dispatch; just strings
- **Same type safety** — the Scala compiler enforces which operations are valid
  on which expression types via the opaque type wrappers
- **Readable WGSL output** — expressions are built as the exact strings that
  appear in the final shader; easy to debug
- **Simpler implementation** — no traversal/serialization pass needed; the
  expression _is_ the output

If we later need expression analysis (optimization, validation, dead code
elimination), an AST layer could be introduced behind the same API. The opaque
type surface would remain unchanged.

### 3.3 Companion Factories

```scala
object FloatExpr:
  inline def apply(s: String): FloatExpr = s
  inline def lit(v: Double): FloatExpr = v.toString
  inline def lit(v: Float): FloatExpr = v.toString

object Vec2Expr:
  inline def apply(s: String): Vec2Expr = s

object Vec3Expr:
  inline def apply(s: String): Vec3Expr = s

object Vec4Expr:
  inline def apply(s: String): Vec4Expr = s
// ... same pattern for Mat*Expr, BoolExpr, IntExpr, UIntExpr
```

### 3.4 Implicit Literal Conversions

To allow writing `0.0` and `1.0` directly in DSL expressions:

```scala
given Conversion[Double, FloatExpr] = v => FloatExpr(v.toString)
given Conversion[Float, FloatExpr]  = v => FloatExpr(v.toString)
given Conversion[Int, FloatExpr]    = v => FloatExpr(s"f32($v)")
given Conversion[Int, IntExpr]      = v => IntExpr(v.toString)
```

This enables:

```scala
ctx.out.position := vec4(ctx.locals.rotated + ctx.bindings.translation, 0.0, 1.0)
//                                                                     ^^^  ^^^
//                                    Double literals auto-convert to FloatExpr
```

---

## 4. Operators and Math Extensions

### 4.1 FloatExpr Operations

```scala
extension (a: FloatExpr)
  // Arithmetic
  inline def +(b: FloatExpr): FloatExpr = s"($a + $b)"
  inline def -(b: FloatExpr): FloatExpr = s"($a - $b)"
  inline def *(b: FloatExpr): FloatExpr = s"($a * $b)"
  inline def /(b: FloatExpr): FloatExpr = s"($a / $b)"
  inline def unary_- : FloatExpr = s"(-$a)"

  // NumExt parity — extension method syntax
  inline def sin: FloatExpr         = s"sin($a)"
  inline def cos: FloatExpr         = s"cos($a)"
  inline def tan: FloatExpr         = s"tan($a)"
  inline def asin: FloatExpr        = s"asin($a)"
  inline def acos: FloatExpr        = s"acos($a)"
  inline def atan: FloatExpr        = s"atan($a)"
  inline def atan2(b: FloatExpr): FloatExpr = s"atan2($a, $b)"
  inline def sqrt: FloatExpr        = s"sqrt($a)"
  inline def pow(e: FloatExpr): FloatExpr   = s"pow($a, $e)"
  inline def abs: FloatExpr         = s"abs($a)"
  inline def floor: FloatExpr       = s"floor($a)"
  inline def ceil: FloatExpr        = s"ceil($a)"
  inline def clamp(lo: FloatExpr, hi: FloatExpr): FloatExpr =
    s"clamp($a, $lo, $hi)"
  inline def clamp01: FloatExpr     = s"saturate($a)"
  inline def fit0111: FloatExpr     = s"($a * 2.0 - 1.0)"
  inline def fit1101: FloatExpr     = s"($a * 0.5 + 0.5)"

  // Additional WGSL built-ins
  inline def sign: FloatExpr        = s"sign($a)"
  inline def round: FloatExpr       = s"round($a)"
  inline def trunc: FloatExpr       = s"trunc($a)"
  inline def fract: FloatExpr       = s"fract($a)"
  inline def exp: FloatExpr         = s"exp($a)"
  inline def exp2: FloatExpr        = s"exp2($a)"
  inline def log: FloatExpr         = s"log($a)"
  inline def log2: FloatExpr        = s"log2($a)"
  inline def inverseSqrt: FloatExpr = s"inverseSqrt($a)"
  inline def degrees: FloatExpr     = s"degrees($a)"
  inline def radians: FloatExpr     = s"radians($a)"
  inline def sinh: FloatExpr        = s"sinh($a)"
  inline def cosh: FloatExpr        = s"cosh($a)"
  inline def tanh: FloatExpr        = s"tanh($a)"

  // Multi-argument extension methods (§2.3 table)
  inline def min(b: FloatExpr): FloatExpr  = s"min($a, $b)"
  inline def max(b: FloatExpr): FloatExpr  = s"max($a, $b)"
  inline def mix(b: FloatExpr, t: FloatExpr): FloatExpr = s"mix($a, $b, $t)"
  inline def step(edge: FloatExpr): FloatExpr = s"step($edge, $a)"
  inline def smoothstep(lo: FloatExpr, hi: FloatExpr): FloatExpr =
    s"smoothstep($lo, $hi, $a)"
  inline def fma(b: FloatExpr, c: FloatExpr): FloatExpr = s"fma($a, $b, $c)"

  // Comparison → BoolExpr
  inline def <(b: FloatExpr): BoolExpr  = s"($a < $b)"
  inline def >(b: FloatExpr): BoolExpr  = s"($a > $b)"
  inline def <=(b: FloatExpr): BoolExpr = s"($a <= $b)"
  inline def >=(b: FloatExpr): BoolExpr = s"($a >= $b)"
  inline def ===(b: FloatExpr): BoolExpr = s"($a == $b)"
  inline def !==(b: FloatExpr): BoolExpr = s"($a != $b)"
```

Note: `==` and `!=` cannot be overridden in Scala (they are `final` on `Any`),
so we use `===` and `!==`. This is standard in Scala DSLs.

### 4.2 Vector Operations (Vec3Expr as Example)

```scala
extension (v: Vec3Expr)
  // Arithmetic — mirrors Vec3ImmutableOps
  @targetName("vec3AddVec")
  inline def +(w: Vec3Expr): Vec3Expr   = s"($v + $w)"
  @targetName("vec3AddScalar")
  inline def +(s: FloatExpr): Vec3Expr  = s"($v + vec3<f32>($s))"
  @targetName("vec3SubVec")
  inline def -(w: Vec3Expr): Vec3Expr   = s"($v - $w)"
  @targetName("vec3SubScalar")
  inline def -(s: FloatExpr): Vec3Expr  = s"($v - vec3<f32>($s))"
  @targetName("vec3MulVec")
  inline def *(w: Vec3Expr): Vec3Expr   = s"($v * $w)"
  @targetName("vec3MulScalar")
  inline def *(s: FloatExpr): Vec3Expr  = s"($v * $s)"
  @targetName("vec3DivVec")
  inline def /(w: Vec3Expr): Vec3Expr   = s"($v / $w)"
  @targetName("vec3DivScalar")
  inline def /(s: FloatExpr): Vec3Expr  = s"($v / $s)"
  inline def unary_- : Vec3Expr         = s"(-$v)"

  // Properties — mirrors Vec3Base
  inline def dot(w: Vec3Expr): FloatExpr     = s"dot($v, $w)"
  inline def cross(w: Vec3Expr): Vec3Expr    = s"cross($v, $w)"
  inline def length: FloatExpr               = s"length($v)"
  inline def length_squared: FloatExpr       = s"dot($v, $v)"
  inline def normalized: Vec3Expr            = s"normalize($v)"
  inline def distance(w: Vec3Expr): FloatExpr = s"distance($v, $w)"
  inline def reflected(n: Vec3Expr): Vec3Expr  = s"reflect($v, $n)"
  inline def refracted(n: Vec3Expr, eta: FloatExpr): Vec3Expr =
    s"refract($v, $n, $eta)"

  // Component access — mirrors .x/.y/.z and .r/.g/.b
  inline def x: FloatExpr = s"$v.x"
  inline def y: FloatExpr = s"$v.y"
  inline def z: FloatExpr = s"$v.z"
  inline def r: FloatExpr = s"$v.x"
  inline def g: FloatExpr = s"$v.y"
  inline def b: FloatExpr = s"$v.z"

  // Componentwise NumExt operations
  inline def sin: Vec3Expr   = s"sin($v)"
  inline def cos: Vec3Expr   = s"cos($v)"
  inline def abs: Vec3Expr   = s"abs($v)"
  inline def floor: Vec3Expr = s"floor($v)"
  inline def ceil: Vec3Expr  = s"ceil($v)"
  inline def fract: Vec3Expr = s"fract($v)"
  inline def clamp(lo: Vec3Expr, hi: Vec3Expr): Vec3Expr =
    s"clamp($v, $lo, $hi)"
  inline def clamp01: Vec3Expr = s"saturate($v)"

  // Multi-argument extension methods
  inline def min(w: Vec3Expr): Vec3Expr  = s"min($v, $w)"
  inline def max(w: Vec3Expr): Vec3Expr  = s"max($v, $w)"
  inline def mix(w: Vec3Expr, t: FloatExpr): Vec3Expr = s"mix($v, $w, $t)"
  inline def step(edge: Vec3Expr): Vec3Expr = s"step($edge, $v)"
  inline def smoothstep(lo: Vec3Expr, hi: Vec3Expr): Vec3Expr =
    s"smoothstep($lo, $hi, $v)"
  inline def fma(w: Vec3Expr, c: Vec3Expr): Vec3Expr = s"fma($v, $w, $c)"

  // Swizzles (most common; add more as needed)
  inline def xy: Vec2Expr = s"$v.xy"
  inline def xz: Vec2Expr = s"$v.xz"
  inline def yz: Vec2Expr = s"$v.yz"
```

Vec2Expr and Vec4Expr follow the same pattern. Vec2Expr omits `.z`, `.w`,
`cross`. Vec4Expr adds `.w`, `.a`, `.xyz`, `.rgb`, and additional swizzles.

### 4.3 Matrix Operations

```scala
extension (m: Mat4Expr)
  @targetName("mat4MulMat")
  inline def *(n: Mat4Expr): Mat4Expr   = s"($m * $n)"
  @targetName("mat4MulVec")
  inline def *(v: Vec4Expr): Vec4Expr   = s"($m * $v)"
  @targetName("mat4MulScalar")
  inline def *(s: FloatExpr): Mat4Expr  = s"($m * $s)"
  inline def transposed: Mat4Expr       = s"transpose($m)"
  inline def determinant: FloatExpr     = s"determinant($m)"
// Same pattern for Mat2Expr (× Vec2Expr) and Mat3Expr (× Vec3Expr)
```

### 4.4 BoolExpr Operations

```scala
extension (a: BoolExpr)
  inline def &&(b: BoolExpr): BoolExpr = s"($a && $b)"
  inline def ||(b: BoolExpr): BoolExpr = s"($a || $b)"
  inline def unary_! : BoolExpr        = s"(!$a)"
```

---

## 5. Constructors and Swizzles

### 5.1 Vector Constructors

The DSL provides `vec2`, `vec3`, `vec4` constructor objects in lowercase. This
avoids collision with the existing CPU types `Vec2`, `Vec3`, `Vec4` in
`gpu.math`, and matches WGSL syntax (`vec4<f32>(...)`).

```scala
package gpu.shader.dsl

object vec2:
  inline def apply(x: FloatExpr, y: FloatExpr): Vec2Expr =
    s"vec2<f32>($x, $y)"

object vec3:
  inline def apply(x: FloatExpr, y: FloatExpr, z: FloatExpr): Vec3Expr =
    s"vec3<f32>($x, $y, $z)"
  inline def apply(xy: Vec2Expr, z: FloatExpr): Vec3Expr =
    s"vec3<f32>($xy, $z)"
  inline def apply(x: FloatExpr, yz: Vec2Expr): Vec3Expr =
    s"vec3<f32>($x, $yz)"

object vec4:
  inline def apply(x: FloatExpr, y: FloatExpr, z: FloatExpr, w: FloatExpr): Vec4Expr =
    s"vec4<f32>($x, $y, $z, $w)"
  inline def apply(xyz: Vec3Expr, w: FloatExpr): Vec4Expr =
    s"vec4<f32>($xyz, $w)"
  inline def apply(xy: Vec2Expr, z: FloatExpr, w: FloatExpr): Vec4Expr =
    s"vec4<f32>($xy, $z, $w)"
  inline def apply(xy: Vec2Expr, zw: Vec2Expr): Vec4Expr =
    s"vec4<f32>($xy, $zw)"
  inline def apply(x: FloatExpr, yzw: Vec3Expr): Vec4Expr =
    s"vec4<f32>($x, $yzw)"
```

User writes: `vec4(ctx.locals.rotated + ctx.bindings.translation, 0.0, 1.0)`
WGSL output: `vec4<f32>((rotated + translation), 0.0, 1.0)`

### 5.2 Swizzle Patterns

Common swizzles are provided as extension methods. Additional swizzles are added
incrementally as needed — full combinatorial coverage (hundreds of methods) is
not needed up front.

```scala
// Vec4Expr swizzles
extension (v: Vec4Expr)
  inline def xy: Vec2Expr   = s"$v.xy"
  inline def xz: Vec2Expr   = s"$v.xz"
  inline def yz: Vec2Expr   = s"$v.yz"
  inline def xw: Vec2Expr   = s"$v.xw"
  inline def zw: Vec2Expr   = s"$v.zw"
  inline def xyz: Vec3Expr  = s"$v.xyz"
  inline def xyw: Vec3Expr  = s"$v.xyw"
  inline def xzw: Vec3Expr  = s"$v.xzw"
  inline def yzw: Vec3Expr  = s"$v.yzw"
  inline def rgb: Vec3Expr  = s"$v.rgb"
```

---

## 6. Context Object

### 6.1 Structure

The context provides typed access to shader inputs, outputs, bindings, and local
variables:

```
ctx.in       — vertex attributes (vert) or varyings (frag)
ctx.out      — varyings (vert) or fragment output (frag)
ctx.bindings — uniforms (no prefix — uniforms are top-level in WGSL)
ctx.locals   — local variables declared via the Locals type parameter
```

### 6.2 Accessor Implementation via Selectable

```scala
// Read-only access — returns Expr for use in expressions
class ExprAccessor(prefix: String) extends Selectable:
  def selectDynamic(name: String): Expr =
    Expr.raw(if prefix.isEmpty then name else s"$prefix.$name")

// Write access — returns AssignTarget that supports :=
class AssignAccessor(prefix: String) extends Selectable:
  def selectDynamic(name: String): AssignTarget =
    AssignTarget(if prefix.isEmpty then name else s"$prefix.$name")

class AssignTarget(val target: String):
  inline def :=(value: Expr): Stmt = Stmt.assign(target, value)

// Local variable access — returns LocalRef that supports := and use as Expr
class LocalAccessor extends Selectable:
  def selectDynamic(name: String): LocalRef = LocalRef(name)

class LocalRef(val name: String):
  inline def :=(value: Expr): Stmt = Stmt.let(name, value)

// LocalRef can be used as an expression via implicit conversion
given Conversion[LocalRef, Expr] = ref => Expr.raw(ref.name)
```

### 6.3 Uniform Accessor — DSL Prefix, Bare WGSL Names

In the DSL, uniforms are accessed via `ctx.bindings.rotation` — the `bindings.`
prefix provides a consistent, discoverable API namespace. In the generated WGSL,
however, uniforms are top-level `var<uniform>` declarations (not struct
members), so the accessor strips the prefix and emits just `rotation`:

```scala
val bindings = ExprAccessor("")  // empty prefix → bare names in WGSL output
// ctx.bindings.rotation  →  emits "rotation" in WGSL
```

This is a DSL-side naming convention only — it does not affect WGSL generation.
The `ExprAccessor("")` with empty prefix ensures the generated expression
contains just the bare variable name.

### 6.4 Context Class

Both vertex and fragment stages can define local variables, so there is a single
context class with all accessors:

```scala
class ShaderCtx(
  val in: ExprAccessor,
  val out: AssignAccessor,
  val bindings: ExprAccessor,
  val locals: LocalAccessor,
)
```

When the stage has no locals (no `Locals` type parameter), `locals` is still
present but unused — calling any field on it generates a valid WGSL name, and
the absence of a corresponding `let` declaration will be caught by the WGSL
compiler.

### 6.5 Type Safety: Untyped First, Then Typed

**Step 1c (MVP)**: Accessors return untyped `Expr`. Type errors surface when the
generated WGSL fails to compile on the GPU — the same failure mode as raw WGSL
strings, but with operator-level type checking (you can't `+` a `Vec2Expr` and a
`Mat4Expr`). This gets a working draft running first.

**Step 1d (Typed Accessors)**: Use Scala 3 `Selectable` with refined structural
types so that `ctx.in.position` returns `Vec2Expr` (not generic `Expr`) based on
the named tuple type parameter. This requires deriving a refinement type from
the named tuple, which is achievable via `transparent inline` + `summonFrom` —
consistent with the existing typed bindings pattern.

---

## 7. Statements and Blocks

### 7.1 Statement Representation

```scala
opaque type Stmt = String

object Stmt:
  inline def assign(target: String, value: Expr): Stmt =
    s"  $target = ${value: String};"
  inline def let(name: String, value: Expr): Stmt =
    s"  let $name = ${value: String};"
  inline def varDecl(name: String, value: Expr): Stmt =
    s"  var $name = ${value: String};"
  inline def varDeclTyped(name: String, wgslType: String, value: Expr): Stmt =
    s"  var $name: $wgslType = ${value: String};"
  inline def varAssign(name: String, value: Expr): Stmt =
    s"  $name = ${value: String};"
  inline def raw(s: String): Stmt = s
```

### 7.2 Block

A `Block` is a sequence of statements that produces a WGSL code string. Varargs
(`Stmt*`) compile to `Seq` in Scala.js, which is lightweight — Scala.js
optimizes `Seq` well and it doesn't pull in heavy collection infrastructure.
Complex shaders need many statements, so fixed-arity limits are too restrictive:

```scala
opaque type Block = String

object Block:
  def apply(stmts: Stmt*): Block =
    stmts.mkString("\n")
```

### 7.3 Variable Declaration: `let` vs `var`

WGSL distinguishes immutable (`let`) and mutable (`var`) local variables:

- `ctx.locals.rotated := expr` → generates `let rotated = expr;`
  - The local value cannot be reassigned
  - Used for named intermediates

For mutable variables (accumulators, loop counters), provide `MutableLocalRef`:

```scala
class MutableLocalRef(val name: String):
  inline def init(value: Expr): Stmt = Stmt.varDecl(name, value)
  inline def :=(value: Expr): Stmt = Stmt.varAssign(name, value)

given Conversion[MutableLocalRef, Expr] = ref => Expr.raw(ref.name)
```

How to distinguish `let` vs `var` locals in the type parameter:

```scala
// Marker type for mutable locals
sealed trait Var[T]

// type Locals = (rotated: Vec2, acc: Var[Vec3])
//                ^^^^^^^ let    ^^^^^^^^ var
```

The `LocalAccessor` uses `inline erasedValue` to check if the field type is
`Var[T]` and returns the appropriate ref type.

### 7.4 Control Flow

```scala
// If / else
inline def ifThen(cond: BoolExpr)(body: Block): Stmt =
  Stmt.raw(s"  if (${cond: String}) {\n$body\n  }")

inline def ifThenElse(cond: BoolExpr)(thenBody: Block)(elseBody: Block): Stmt =
  Stmt.raw(s"  if (${cond: String}) {\n$thenBody\n  } else {\n$elseBody\n  }")

// While loop
inline def whileLoop(cond: BoolExpr)(body: Block): Stmt =
  Stmt.raw(s"  while (${cond: String}) {\n$body\n  }")
```

Example usage:

```scala
program.frag[(brightness: Float)]: ctx =>
  Block(
    ctx.locals.brightness := ctx.in.color.length,
    ifThenElse(ctx.locals.brightness > 0.5)(
      Block(ctx.out.color := vec4(1.0, 1.0, 1.0, 1.0))
    )(
      Block(ctx.out.color := vec4(0.0, 0.0, 0.0, 1.0))
    ),
  )
```

Generated WGSL:

```wgsl
  let brightness = length(in.color);
  if (brightness > 0.5) {
    out.color = vec4<f32>(1.0, 1.0, 1.0, 1.0);
  } else {
    out.color = vec4<f32>(0.0, 0.0, 0.0, 1.0);
  }
```

---

## 8. Hygienic Naming

### 8.1 Name Sources in Generated WGSL

| Source              | Example      | Comes From                  |
| ------------------- | ------------ | --------------------------- |
| Struct fields       | `position`   | `Attribs` named tuple       |
| Uniform variables   | `rotation`   | `Uniforms` named tuple      |
| Local variables     | `rotated`    | `Locals` type parameter     |
| Built-in parameters | `in`, `out`  | Generated function skeleton |
| WGSL keywords       | `let`, `var` | Language reserved           |

### 8.2 Collision Scenarios

1. **Local vs uniform**: `Locals = (color: Vec3)` with
   `Uniforms = (color: Vec4)` — the `let color = ...;` would shadow the uniform
   `color`. This is valid WGSL (inner scope shadows outer), but likely a user
   mistake.

2. **Local is WGSL keyword**: `Locals = (var: Vec3)` — generates
   `let var = ...;` which is a syntax error.

3. **Local vs struct field**: `Locals = (position: Vec2)` with
   `Attribs = (position: Vec2)` — safe, because struct fields are qualified as
   `in.position`.

### 8.3 Strategy

**Compile-time**: Validate local names against WGSL reserved words using
`inline` + `constValue` + `compiletime.error()`:

```scala
private inline def checkNotReserved[Names <: Tuple]: Unit =
  inline erasedValue[Names] match
    case _: EmptyTuple => ()
    case _: (name *: rest) =>
      inline constValue[name].asInstanceOf[String] match
        case "let" | "var" | "fn" | "struct" | "return" | "if" | "else"
           | "while" | "for" | "break" | "continue" | "switch" | "case"
           | "default" | "true" | "false" | "loop" | "continuing"
           | "discard" | "enable" | "const" | "override" | "type"
           | "alias" | "diagnostic" | "in" | "out" =>
          error("Local variable name is a WGSL reserved word")
        case _ => checkNotReserved[rest]
```

**Runtime**: Check for local vs uniform name collisions when generating the WGSL
body. Throw an error if a local name matches a uniform name. This could be moved
to compile-time in a future milestone with more inline machinery.

**No name mangling**: Local names appear as-is in the generated WGSL. This keeps
the output readable and debuggable. Collision prevention is done through
validation, not transformation.

---

## 9. Code Generation & Integration

### 9.1 Generation Flow

```
User DSL code
  → Block of Stmts (each Stmt is a String)
  → Block.toString = newline-joined string
  → Passed as vertexBody / fragmentBody to ShaderDef
  → ShaderDef.generateWGSL wraps it in struct declarations + function signatures
```

The DSL produces only the function body. All surrounding infrastructure
(structs, uniform declarations, function signatures, `var out`, `return out`) is
handled by the existing `ShaderDef.generateWGSL`.

### 9.2 Program Builder

```scala
class Program[A, V, U]:
  private var vertBody: String = ""
  private var fragBody: String = ""

  private def makeCtx: ShaderCtx = ShaderCtx(
    in = ExprAccessor("in"),
    out = AssignAccessor("out"),
    bindings = ExprAccessor(""),
    locals = LocalAccessor(),
  )

  inline def vert[Locals](body: ShaderCtx => Block): Unit =
    // TODO: compile-time reserved word check on Locals names
    vertBody = body(makeCtx): String

  inline def vert(body: ShaderCtx => Block): Unit =
    vertBody = body(makeCtx): String

  inline def frag[Locals](body: ShaderCtx => Block): Unit =
    fragBody = body(makeCtx): String

  inline def frag(body: ShaderCtx => Block): Unit =
    fragBody = body(makeCtx): String
```

### 9.3 Painter Integration

Add a new overload of `painter.shade` that accepts a DSL program builder:

```scala
// In Painter (painter.scala)
inline def shade[A, V, U](
  build: Program[A, V, U] => Unit,
): Shade[U] =
  val program = Program[A, V, U]()
  build(program)
  shade[A, V, U](
    vertWgsl = program.vertBody,
    fragWgsl = program.fragBody,
  )
```

The existing string-based `shade[A, V, U](vertWgsl, fragWgsl)` remains
unchanged. Both APIs coexist — the DSL is purely additive.

### 9.4 Standalone Usage (Testing)

```scala
val program = Program[Attribs, Varyings, Uniforms]()
program.vert[(rotated: Vec2)]: ctx =>
  Block(
    ctx.locals.rotated := ctx.bindings.rotation * ctx.in.position,
    ctx.out.position := vec4(ctx.locals.rotated + ctx.bindings.translation, 0.0, 1.0),
  )
program.frag: ctx =>
  Block(
    ctx.out.color := vec4(ctx.bindings.color, 1.0),
  )

// Inspect generated WGSL body strings
println(program.vertBody)
println(program.fragBody)
```

### 9.5 Full Example: From DSL to Generated WGSL

**DSL Input:**

```scala
type Attribs = (position: Vec2)
type Varyings = EmptyTuple
type Uniforms = (
  color: FragmentUniform[Vec3],
  rotation: VertexUniform[Mat2],
  translation: VertexUniform[Vec2],
)

val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
  program.vert[(rotated: Vec2)]: ctx =>
    Block(
      ctx.locals.rotated := ctx.bindings.rotation * ctx.in.position,
      ctx.out.position := vec4(
        ctx.locals.rotated + ctx.bindings.translation, 0.0, 1.0,
      ),
    )
  program.frag: ctx =>
    Block(
      ctx.out.color := vec4(ctx.bindings.color, 1.0),
    )
```

**Generated vertex body string:**

```
  let rotated = (rotation * in.position);
  out.position = vec4<f32>((rotated + translation), 0.0, 1.0);
```

**Generated fragment body string:**

```
  out.color = vec4<f32>(color, 1.0);
```

**Full generated WGSL (after ShaderDef wrapping):**

```wgsl
struct VertexInput {
  @location(0) position: vec2<f32>,
}

struct VertexOutput {
  @builtin(position) position: vec4<f32>,
}

struct FragmentOutput {
  @location(0) color: vec4<f32>,
}

@group(0) @binding(0) var<uniform> color: vec3<f32>;
@group(0) @binding(1) var<uniform> rotation: mat2x2<f32>;
@group(0) @binding(2) var<uniform> translation: vec2<f32>;

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
  var out: VertexOutput;
  let rotated = (rotation * in.position);
  out.position = vec4<f32>((rotated + translation), 0.0, 1.0);
  return out;
}

@fragment
fn fs_main(in: VertexOutput) -> FragmentOutput {
  var out: FragmentOutput;
  out.color = vec4<f32>(color, 1.0);
  return out;
}
```

---

## 10. Implementation Milestones

The implementation evolves CPU and GPU math together from day one, adding
operations incrementally as needed by real shader examples rather than
front-loading a complete math library.

### Milestone 1: Working Typed DSL (MVP)

This milestone delivers a minimal but complete end-to-end DSL: expression types,
enough math for basic shaders, program builder, painter integration, typed
accessors, and mutable variables.

#### Step 1a: CPU Math Gaps — Matrix Multiply

**Modify**: `src/gpu/math/mat2.scala`, `mat3.scala`, `mat4.scala`

The CPU math library is missing matrix-matrix and matrix-vector multiplication.
Without these, most basic shaders (MVP transforms, rotations) cannot work.

- Add `m * n` (matrix × matrix) to `Mat2ImmutableOps`, `Mat3ImmutableOps`,
  `Mat4ImmutableOps`
- Add `m * v` (matrix × vector) to each — `Mat2 * Vec2`, `Mat3 * Vec3`,
  `Mat4 * Vec4`
- Corresponding `MutableOps` variants: `m.multiply(n, out)`,
  `m.multiply(v, out)`
- Unit tests

#### Step 1b: Expression Types + Trait Instances

**Create**: `src/gpu/shader/dsl/expr.scala`

- Opaque types: `Expr`, `FloatExpr`, `Vec2Expr`, `Vec3Expr`, `Vec4Expr`,
  `Mat2Expr`, `Mat3Expr`, `Mat4Expr`, `BoolExpr`
- `given NumOps[FloatExpr]` — `+`, `-`, `*`, `/`, `unary_-`, `zero`, `one`
- `given NumExt[FloatExpr]` — `sin`, `cos`, `sqrt`, `clamp`, etc. (emit WGSL
  built-in strings)
- `given Vec*Base[FloatExpr, Vec*Expr]` — component access, `dot`,
  `length_squared`; override `dot` and `length` to emit WGSL built-ins
- `given Vec*ImmutableOps[FloatExpr, Vec*Expr]` — arithmetic, `normalized`,
  `cross`; override `normalized`, `cross`, `length` to emit WGSL built-ins
  instead of trait-derived expansions
- `given Mat*ImmutableOps[FloatExpr, Mat*Expr]` — arithmetic, `m * m`, `m * v`,
  `transposed`, `determinant`
- Implicit conversions from `Double`/`Float` literals
- `vec2(...)`, `vec3(...)`, `vec4(...)` constructor objects
- `Stmt`, `Block` opaque types
- Unit tests verifying generated WGSL expression strings

#### Step 1c: Program Builder + Painter Integration

**Create**: `src/gpu/shader/dsl/context.scala`,
`src/gpu/shader/dsl/program.scala` **Modify**: `src/gpu/painter/painter.scala`
**Create**: `drafts/painter_dsl/` (PainterDsl.scala, index.html, main.js)

- `ExprAccessor`, `AssignAccessor`, `LocalAccessor` with `Selectable`
- `ShaderCtx` class (with locals for all stages)
- `Program[A, V, U]` builder
- New `shade` overload on `Painter` accepting `Program` builder
- Port `painter_typed_bindings` draft to use DSL
- Visual verification: renders identical result

#### Step 1d: Typed Accessors

**Extend**: `src/gpu/shader/dsl/context.scala`

- Refined `Selectable` types so `ctx.in.position` returns `Vec2Expr` (not
  generic `Expr`) based on the named tuple type parameter
- Uses `transparent inline` + `summonFrom` to derive refinement from the named
  tuple — consistent with existing typed bindings pattern
- Compile-time type checking of all field access

#### Step 1e: Mutable Variables

**Extend**: `src/gpu/shader/dsl/stmt.scala`, `context.scala`

- `Var[T]` marker type for mutable locals
- `MutableLocalRef` with `init` and `:=`
- Cross-namespace collision detection (local vs uniform)
- Reserved word validation

### Ongoing: Incremental Math and Features

After Milestone 1, new math operations, swizzles, control flow (`ifThen`,
`ifThenElse`, `whileLoop`), and additional expression types (`IntExpr`,
`UIntExpr`) are added incrementally as needed by real shader examples. Each new
operation is added to both CPU (`gpu.math` / `NumExt`) and GPU (`*Expr`
extensions) simultaneously to maintain parity.

### Future: Shared CPU/GPU Functions

With trait instances implemented in Step 1b, shared generic functions become
possible:

- A function `def lighting[N: NumOps: NumExt, V: Vec3Base[N, _]](...)` works on
  both `(Double, Vec3)` and `(FloatExpr, Vec3Expr)`
- Validate with a shared lighting function that compiles to both native code and
  WGSL
- Identify and resolve any remaining trait gaps that prevent full generic usage

---

## 11. File Organization

```
src/gpu/math/
├── mat2.scala          — Add m * m, m * v (Step 1a)
├── mat3.scala          — Add m * m, m * v (Step 1a)
└── mat4.scala          — Add m * m, m * v (Step 1a)

src/gpu/shader/dsl/
├── expr.scala          — Opaque expression types + operator extensions
├── constructors.scala  — vec2/vec3/vec4 constructor objects
├── stmt.scala          — Stmt, Block opaque types + control flow (incremental)
├── context.scala       — ExprAccessor, AssignAccessor, LocalAccessor,
│                         ShaderCtx, typed accessors
├── program.scala       — Program[A, V, U] builder class
└── package.scala       — Re-exports, implicit conversions, free functions

src/gpu/painter/
└── painter.scala       — Add new shade overload (Step 1c)

drafts/painter_dsl/     — Draft example using DSL (Step 1c)
├── PainterDsl.scala
├── index.html
└── main.js
```

---

## 12. Open Questions

1. **Color aliases in WGSL output**: CPU `.r`/`.g`/`.b` map to `.x`/`.y`/`.z` in
   WGSL. Should the DSL output `.r`/`.g`/`.b` (valid WGSL) or normalize to
   `.x`/`.y`/`.z`? WGSL accepts both, so outputting `.r`/`.g`/`.b` preserves
   intent and readability.

2. **Integer types**: WGSL has `i32` and `u32`. The current DSL includes
   `IntExpr` and `UIntExpr` but doesn't define operations on them. These are
   needed for texture coordinates, vertex indices, etc. Add in a later milestone
   when needed.

---

## 13. Decisions Made

- **Opaque types over String** — not AST nodes. Zero allocation, follows bundle
  size rules. AST is viable (case classes are efficient, pattern matching is a
  language feature) but opaque types are simpler and produce smaller bundles.
- **Extension method syntax** (`x.sin`, `v.normalized`, `a.min(b)`) — matches
  CPU math API per `api_and_naming_conventions.md`. Multi-argument WGSL
  built-ins use extension methods too (`a.mix(b, t)`, `x.smoothstep(lo, hi)`).
  Only `select(f, t, cond)` remains a free function.
- **Past tense for immutable GPU ops** — `v.reflected(n)`, `v.refracted(n, η)`,
  `v.normalized`, `m.transposed` — consistent with
  `api_and_naming_conventions.md`.
- **Lowercase constructors** (`vec4(...)`) — avoids collision with `gpu.math`
  types, matches WGSL syntax.
- **`Selectable` for context accessors** — enables `ctx.in.position` syntax with
  runtime-dynamic field lookup.
- **`bindings.` prefix in DSL, bare names in WGSL** — `ctx.bindings.rotation`
  provides a consistent DSL namespace; `ExprAccessor("")` strips the prefix so
  the generated WGSL emits just `rotation`.
- **Single ShaderCtx with locals** — both vertex and fragment stages can define
  local variables. No separate `VertCtx` needed.
- **Varargs for Block** — `Block(stmts: Stmt*)` using `Seq`. Scala.js optimizes
  `Seq` well; fixed-arity limits are too restrictive for complex shaders.
- **Parenthesization handled by Scala precedence** — Scala's built-in operator
  precedence (based on first character: `*`/`/` bind tighter than `+`/`-`) means
  `a + b * c` evaluates as `a + (b * c)` at the Scala level, producing correct,
  safely parenthesized WGSL output. No custom precedence tracking needed.
- **No name mangling** — local variable names appear as-is in WGSL for
  readability. Collisions prevented by validation.
- **`===` / `!==` for equality** — Scala `==`/`!=` are final on `Any`.
- **DSL is additive** — string-based shader bodies remain supported. Both APIs
  coexist.
- **CPU/GPU math evolved together** — `FloatExpr` and `Vec*Expr` implement the
  same `NumOps`, `NumExt`, `Vec*Base`, `Vec*ImmutableOps` traits as CPU types.
  Override trait defaults with WGSL built-ins where available (`normalize`,
  `length`, `dot`, `cross`). New math is added incrementally as needed.
- **Typed accessors early** — `Selectable` refinement types are part of
  Milestone 1, not deferred. Aligns with the project's existing typed bindings
  pattern.
- **Mutable variables early** — `Var[T]` marker type and `MutableLocalRef` are
  part of Milestone 1, needed for non-trivial shaders.

---

## 14. Implementation Status

This section tracks what has been implemented, what deviates from the original
plan, and what remains to be done.

### Completed Steps

#### Step 1a: CPU Math Gaps — Matrix Multiply ✅

Matrix multiplication was already present in the math library
(`Mat2ImmutableOps`, `Mat3ImmutableOps`, `Mat4ImmutableOps`) before this work
began. No changes needed.

#### Step 1b: Expression Types + Trait Instances ✅

**File**: `src/gpu/shader/dsl/expr.scala`

Implemented as planned:

- All opaque types: `Expr`, `FloatExpr`, `Vec2Expr`–`Vec4Expr`,
  `Mat2Expr`–`Mat4Expr`, `BoolExpr`
- `given NumOps[FloatExpr]`, `given NumExt[FloatExpr]`
- `given Vec*Base[FloatExpr, Vec*Expr]` and
  `given Vec*ImmutableOps[FloatExpr, Vec*Expr]`
- `given Mat*Base[FloatExpr, Mat*Expr]` and
  `given Mat*ImmutableOps[FloatExpr, Mat*Expr]`
- Implicit conversions: `Double → FloatExpr`, `Float → FloatExpr`,
  `Int → FloatExpr`
- `vec2(...)`, `vec3(...)`, `vec4(...)` constructors
- `Stmt` and `Block` opaque types

**Deviation from plan**: `IntExpr` and `UIntExpr` not yet implemented (not
needed for current shaders). Constructors and statements live in `expr.scala`
rather than separate `constructors.scala` and `stmt.scala` files — simpler for
the current code size.

#### Step 1c: Program Builder + Painter Integration ✅

**Files**: `src/gpu/shader/dsl/context.scala`,
`src/gpu/shader/dsl/program.scala`, `src/gpu/painter/painter.scala`,
`drafts/painter_dsl/`

Implemented as planned. The `painter_dsl` draft renders two animated rotating
triangles using the DSL, visually identical to the string-based
`painter_typed_bindings` draft.

#### Step 1d: Typed Accessors ✅

**Files**: `src/gpu/shader/dsl/context.scala`, `src/gpu/shader/dsl/expr.scala`,
`src/gpu/shader/dsl/types.scala`, `src/gpu/shader/dsl/local_ops.scala`

Implemented with significant deviations from the original plan due to Scala 3
type system constraints. All `ctx.in`, `ctx.out`, `ctx.bindings`, and
`ctx.locals` are fully typed Selectables.

**Major deviations from plan:**

1. **Separate context types instead of single `ShaderCtx`**: The plan called for
   a single `ShaderCtx` class. Implementation uses `VertexCtx[A, V, U, L]` and
   `FragmentCtx[V, U, L]` — separate types are needed because vertex and
   fragment stages have different input types (attributes vs varyings) and
   different fixed outputs (`position: Vec4` vs `color: Vec4`).

2. **Three accessor classes instead of two**: The plan described `ExprAccessor`
   and `AssignAccessor` plus `LocalAccessor` using `LocalRef` with implicit
   conversion. Implementation uses `TypedExprAccessor[F]`,
   `TypedAssignAccessor[F]`, and `TypedLocalAccessor[F]` — all parameterized
   with a `Fields` named tuple type for compile-time field checking.

3. **Local\* opaque types instead of `LocalRef` + conversion**: The plan used
   `class LocalRef(name: String)` with `given Conversion[LocalRef, Expr]` for
   math operations. This doesn't work because `given Conversion` does NOT
   trigger extension method resolution from type class instances in Scala 3.
   Instead, individual opaque types are used:

   ```scala
   opaque type LocalExpr  <: Expr                   = String
   opaque type LocalFloat <: FloatExpr & LocalExpr  = String
   opaque type LocalVec2  <: Vec2Expr & LocalExpr   = String
   // ... etc for Vec3, Vec4, Mat2–Mat4, Bool
   ```

   The `:=` operator is an extension on `LocalExpr` (inherited by all subtypes).
   Match types `ToLocal[T]` and `ToExpr[T]` in `types.scala` map GPU math types
   to their corresponding Local/Expr types.

4. **Separate `local_ops.scala` file for WGSL-native operations**: Opaque type
   transparency is per-file in Scala 3. Inside `expr.scala` where the opaques
   are defined, all `*Expr` types are `String`, so `+` resolves to `String.+`
   (concatenation) instead of the intended `Vec2ImmutableOps` extension. The
   file `local_ops.scala` defines vector arithmetic extensions (`+`, `-`, `*`,
   `/`) in a separate file where the opaque boundary is intact. These shadow the
   trait-based extensions and generate compact WGSL (`(a + b)`) instead of
   component-wise expansion (`vec2<f32>(a.x + b.x, a.y + b.y)`).

5. **Native WGSL matrix-vector multiplication**: The plan assumed `m * v` from
   `Mat2ImmutableOps` would generate correct WGSL. In practice, the trait's
   `inline def *` expands component-wise (correct for CPU but verbose for WGSL).
   Direct extensions in `local_ops.scala` override this with native `(m * v)`
   syntax via `Vec*Expr.matMul` helpers defined in `expr.scala`.

6. **`& AnyNamedTuple` intersections instead of bounds**: The plan used
   `Program[A <: AnyNamedTuple, V <: Tuple, U <: AnyNamedTuple]`. This breaks
   because `EmptyTuple` is NOT `<: AnyNamedTuple` and `painter.scala`'s
   `shade[A, V, U]` has unconstrained type params. The fix: remove all bounds
   from `Program`, `VertexCtx`, `FragmentCtx`, and use `& AnyNamedTuple`
   intersections at `NamedTuple.Map` call sites only.

7. **`program.frag[EmptyTuple]` instead of `program.frag`**: The plan had both
   `frag(body: ...)` and `frag[Locals](body: ...)` overloads. These caused
   ambiguous overload errors. Only the parameterized version exists; callers use
   `EmptyTuple` for no-locals stages.

**Key Scala 3 learnings:**

- Named tuple aliases don't reduce in match types (documented in CLAUDE.md).
  Using `NamedTuple.Map` directly works; `MapTuple` match type wrapper does not.
- Opaque type `<:` bounds (e.g., `LocalVec2 <: Vec2Expr & LocalExpr`) are
  visible outside the defining file, but extension methods from given instances
  (e.g., `Vec2ImmutableOps[FloatExpr, Vec2Expr]`) are NOT found on subtypes
  because the type parameter inference infers the subtype, not the supertype.
- Inside the opaque type defining file, `String.+` takes priority over any
  extension `+` because all `*Expr` types are transparent `= String` there. This
  is why `local_ops.scala` must be a separate file.

#### Step 1e: External Reusable Functions (`WgslFn`) ✅

**Files**: `src/gpu/shader/dsl/fn.scala`, `src/gpu/shader/dsl/program.scala`,
`src/gpu/shader/shader.scala`, `src/gpu/painter/painter.scala`,
`src/test/WgslFnTest.scala`, `drafts/painter_dsl/PainterDsl.scala`

Implemented the `WgslFn[P, R]` typed WGSL helper function feature. Key API:

```scala
// Define with raw WGSL body
val applyTransform: WgslFn[(pos: Vec2, mat: Mat2, offset: Vec2), Vec2] =
  WgslFn.raw("apply_transform"):
    "  return mat * pos + offset;"

// Define with Scala DSL body
val add: WgslFn[(a: Float, b: Float), Float] =
  WgslFn.dsl("add"): (p, ret) =>
    ret(p.a + p.b)

// Register + call in a shader program
program.fn(applyTransform)   // idempotent
applyTransform(ctx.in.position, ctx.bindings.rotation, ctx.bindings.translation)
// → Vec2Expr("apply_transform(in.position, rotation, translation)")
```

Generated WGSL inserts helper functions between uniform declarations and
`vs_main`/`fs_main`. Verified via `painter_dsl` draft: triangle rotates
correctly with the helper function visible in the logged WGSL.

**Key implementation details:**

- `WgslFnData(name, src) extends js.Object` — runtime carrier, zero boxing.
- `opaque type WgslFn[P, R] = WgslFnData` — compile-time typed wrapper.
- `buildParamList[P]` — compile-time iteration over `NamedTuple.Names[P]` +
  `NamedTuple.DropNames[P]` + `WGSLType` summon (same pattern as
  `derive.scala`).
- `callExpr[R]` — `inline erasedValue[R] match` + `.asInstanceOf[ToExpr[R]]`
  cast to the correct opaque `*Expr` type, same as `ToExpr` in `types.scala`.
- `ReturnEmitter[R]` — wraps a `ToExpr[R]` in `return ...;` using
  `v.asInstanceOf[String]` to cross the opaque boundary from outside
  `expr.scala`.
- `private[dsl] def nameOf/srcOf` in the companion — exposes opaque internals to
  per-arity `apply` extensions defined in the same file.
- `program.fn(f)` — deduplicates by name using `hasOwnProperty` (same idiom as
  `Painter.pipelineCache`). `helperFnsStr` joins all registered srcs with
  `"\n\n"`.
- `helperFns: String = ""` added to `ShaderDef` and `Shader.apply` with
  backwards-compatible default. Inserted between `uniformDecls` and
  `buildVertexMain` in the WGSL parts array.
- Per-arity `apply` extensions — arities 1–6, both unnamed
  (`N1 *: N2 *: EmptyTuple`) and named-tuple
  (`NamedTuple.NamedTuple[K1 *: K2 *: EmptyTuple, N1 *: N2 *: EmptyTuple]`)
  variants, because Scala 3 does NOT match named tuples against unnamed tuple
  patterns.

**Key Scala 3 learnings:**

- Named tuples `(v: Vec2, angle: Float)` do NOT match extension parameters typed
  as `N1 *: N2 *: EmptyTuple`. Must provide parallel named-tuple extension
  variants using the explicit
  `NamedTuple.NamedTuple[K *: EmptyTuple, N *: EmptyTuple]` form.
- `private[dsl]` package-scoped visibility on methods in a companion object
  allows same-package extensions to access opaque internals without exposing
  them outside the package.

#### Step 1g: Mutable Variables and Constants ✅

**Files**: `src/graphics/math/gpu/expr.scala`,
`src/graphics/shader/dsl/types.scala`, `src/graphics/shader/dsl/context.scala`,
`src/graphics/shader/dsl/program.scala`

Implemented `Var[T]` and `Const[T]` marker types for full WGSL local variable
support. All three WGSL declaration forms are now available:

```scala
type Locals = (acc: Var[Vec2], scale: Const[Float], tmp: Vec2)

program.vert[Locals]: ctx =>
  Block(
    ctx.locals.scale := 2.0,                       // const scale = 2.0;
    ctx.locals.acc := ctx.in.position,              // var acc = in.position;
    ctx.locals.tmp := ctx.locals.acc + bindings.delta, // let tmp = (acc + delta);
    ctx.locals.acc := ctx.locals.tmp,               // acc = tmp;
    ctx.out.position := vec4(ctx.locals.acc, 0.0, 1.0),
  )
```

Reusable Scala helper functions with `Var*` parameters inline directly — no
`WgslFn` needed:

```scala
def accumulate(acc: VarVec2, value: Vec2Expr): Stmt =
  acc := acc + value

// Inlines to: acc = (acc + delta);
accumulate(ctx.locals.acc, ctx.bindings.delta)
```

**Key implementation details:**

- `VarExpr extends LocalExpr` — stateful: first `:=` emits `var name = expr;`,
  subsequent `:=` emit `name = expr;`
- `ConstExpr extends LocalExpr` — `:=` always emits `const name = expr;`
- Opaque types `VarFloat`, `VarVec2`, `VarVec3`, `VarVec4` (bounds
  `<: Vec*Expr & VarExpr`) and `ConstFloat`, `ConstVec2`, `ConstVec3`,
  `ConstVec4` (bounds `<: Vec*Expr & ConstExpr`) inside `object Expr`
- `ToLocal[T]` match type extended to handle `Var[T]` and `Const[T]` wrappers
- `buildLocalKinds[L]` — inline compile-time iteration over
  `NamedTuple.Names[L]` + `NamedTuple.DropNames[L]`, builds a `Dict[String]`
  mapping field names to `"v"` or `"c"`. Guards against `EmptyTuple` before
  reducing the named tuple intersection.
- `TypedLocalAccessor` updated to accept `kinds: Dict[String]` and dispatch to
  `VarExpr`, `ConstExpr`, or `LocalExpr` based on the dict.
- `Program.vert[L]` and `Program.frag[L]` call `buildLocalKinds[L]` and pass the
  result to `TypedLocalAccessor`.

### Remaining Steps

#### Additional Math Operations — Not Started

Only the arithmetic operators used by the current `painter_dsl` draft are
implemented. Missing from the plan: comparison operators (`<`, `>`, `===`),
boolean operators (`&&`, `||`, `!`), WGSL built-ins (`sign`, `round`, `fract`,
`mix`, `smoothstep`, etc.), swizzles, `dot`, `cross`, `length`, `normalized`,
color aliases (`.r`, `.g`, `.b`). These will be added incrementally as needed.

#### Control Flow — Not Started

`ifThen`, `ifThenElse`, `whileLoop` are not implemented. Add when needed.

#### Hygienic Naming — Not Started

No compile-time reserved word validation or local-vs-uniform collision
detection.

### Current File Layout

```
src/gpu/shader/dsl/
├── expr.scala       — Opaque expression types, trait instances, constructors,
│                      Stmt/Block, Local* opaque types, Vec*Expr.matMul/binop helpers
├── types.scala      — Match types: ToExpr, UniformToExpr, ToLocal, ToAssignTarget
├── context.scala    — TypedExprAccessor, TypedAssignAccessor, TypedLocalAccessor,
│                      AssignTarget, VertexCtx, FragmentCtx
├── program.scala    — Program[A, V, U] builder class (+ fn registration, helperFnsStr)
├── fn.scala         — WgslFnData, WgslFn[P,R], WgslFn.raw/dsl, ReturnEmitter,
│                      per-arity apply extensions (arities 1–6, named + unnamed variants)
└── local_ops.scala  — WGSL-native arithmetic extensions for Vec*Expr and Local*
                       types (must be separate file for opaque type boundary)
```

Compared to plan: no separate `constructors.scala`, `stmt.scala`, or
`package.scala`. Everything fits in fewer files. `local_ops.scala`,
`types.scala`, and `fn.scala` are additions not in the original plan.
