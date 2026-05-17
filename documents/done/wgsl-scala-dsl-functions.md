# WGSL Scala DSL — External Reusable Functions

## 1. Overview

### The Problem

Shader programs often share common computations: lighting models, rotation
helpers, noise functions, color space conversions. In the current DSL, there is
no way to factor out reusable logic — every vertex and fragment body is written
inline. Raw WGSL found in external resources also cannot be integrated in a
typed way.

### The Goal

Introduce `WgslFn[P, R]` — a first-class typed WGSL function that:

- Has a **typed parameter list** (named tuple `P`) and a **typed return value**
  (`R`), using the same DSL expression types as the rest of the system
- Can be **written in two ways**: as a raw WGSL string body, or using the Scala
  DSL (`Block` of `Stmt`)
- **Generates a named WGSL function** at the top level of the shader output,
  before `vs_main` and `fs_main`
- Is **registered to a `Program`** — only registered functions are emitted
- Is **called from DSL code** using function-call syntax via an `apply`
  extension method: `myFn(arg1, arg2)`

---

## 2. User API

### 2.1 Defining a Function — Raw WGSL Body

```scala
import gpu.shader.dsl.*
import gpu.math.*

// Parameters: named tuple (v: Vec2, angle: Float)
// Return type: Vec2
// Body: raw WGSL string (e.g. from an external resource)
val rotateVec: WgslFn[(v: Vec2, angle: Float), Vec2] =
  WgslFn.raw("rotate_vec"):
    """
    let c = cos(angle);
    let s = sin(angle);
    return vec2<f32>(v.x * c - v.y * s, v.x * s + v.y * c);
    """
```

Generated WGSL:

```wgsl
fn rotate_vec(v: vec2<f32>, angle: f32) -> vec2<f32> {
    let c = cos(angle);
    let s = sin(angle);
    return vec2<f32>(v.x * c - v.y * s, v.x * s + v.y * c);
}
```

The named tuple drives both the parameter names (`v`, `angle`) and their WGSL
types (`vec2<f32>`, `f32`) — derived from the existing `WGSLType` type class.

### 2.2 Defining a Function — Scala DSL Body

```scala
val rotateVec: WgslFn[(v: Vec2, angle: Float), Vec2] =
  WgslFn.dsl("rotate_vec"): (p, ret) =>
    ret(vec2(
      p.v.x * p.angle.cos - p.v.y * p.angle.sin,
      p.v.x * p.angle.sin + p.v.y * p.angle.cos,
    ))
```

`p` is a `TypedExprAccessor` whose `Fields` are derived from `P` via
`NamedTuple.Map[P, ToExpr]` — the same pattern used for `ctx.in` and
`ctx.bindings`. `ret(expr)` emits `return expr;` as the last statement.

For `Unit` return type (void function), `ret` is omitted:

```scala
val noOp: WgslFn[(x: Float), Unit] =
  WgslFn.raw("no_op")("// void")
```

### 2.3 Registering Functions to a Program

```scala
val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
  program.fn(rotateVec)   // register — emitted before vs_main

  program.vert[(rotated: Vec2)]: ctx =>
    Block(
      ctx.locals.rotated := rotateVec(ctx.in.position, ctx.bindings.angle),
      ctx.out.position   := vec4(ctx.locals.rotated, 0.0, 1.0),
    )

  program.frag[EmptyTuple]: ctx =>
    Block(
      ctx.out.color := vec4(ctx.bindings.color, 1.0),
    )
```

`program.fn(f)` is idempotent — registering the same function name twice has no
effect (deduplicated by name using `Dict[Boolean]`).

### 2.4 Calling a Function from DSL Code

`WgslFn` supports function-call syntax via an `apply` extension method:

```scala
// Type-checked: rotateVec takes (Vec2Expr, FloatExpr), returns Vec2Expr
rotateVec(ctx.in.position, ctx.bindings.angle)   // : Vec2Expr

// Implicit conversions apply as usual
brighten(ctx.bindings.color, 0.8)                // Float literal → FloatExpr
```

The `apply` extension method:
1. Accepts arguments typed via `ToExpr[Ni]` for each parameter type `Ni`
2. Returns `ToExpr[R]` — the correct expression type
3. Emits the WGSL call expression: `fn_name(arg0, arg1, ...)`

### 2.5 A Function Calling Another Function

```scala
val oscillate: WgslFn[(t: Float), Vec2] =
  WgslFn.dsl("oscillate"): (p, ret) =>
    ret(rotateVec(vec2(0.5, 0.0), p.t))

// Register dependency before caller:
program.fn(rotateVec)
program.fn(oscillate)
```

Functions are emitted in registration order. WGSL requires definitions before
use, so dependencies must be registered first. Registration is explicit by
design — the same mental model as WGSL top-level order.

### 2.6 Full Example: DSL to Generated WGSL

**Scala input:**

```scala
type Attribs  = (position: Vec2)
type Varyings = EmptyTuple
type Uniforms = (
  color: FragmentUniform[Vec3],
  rotation: VertexUniform[Mat2],
  translation: VertexUniform[Vec2],
)

val applyTransform: WgslFn[(pos: Vec2, mat: Mat2, offset: Vec2), Vec2] =
  WgslFn.raw("apply_transform"):
    "return mat * pos + offset;"

val shade = painter.shade[Attribs, Varyings, Uniforms]: program =>
  program.fn(applyTransform)

  program.vert[EmptyTuple]: ctx =>
    Block(
      ctx.out.position := vec4(
        applyTransform(ctx.in.position, ctx.bindings.rotation, ctx.bindings.translation),
        0.0, 1.0,
      ),
    )

  program.frag[EmptyTuple]: ctx =>
    Block(
      ctx.out.color := vec4(ctx.bindings.color, 1.0),
    )
```

**Generated WGSL:**

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

fn apply_transform(pos: vec2<f32>, mat: mat2x2<f32>, offset: vec2<f32>) -> vec2<f32> {
  return mat * pos + offset;
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
  var out: VertexOutput;
  out.position = vec4<f32>(apply_transform(in.position, rotation, translation), 0.0, 1.0);
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

## 3. Implementation Strategy

### 3.1 New Type: `WgslFn[P, R]`

```scala
// Runtime carrier — JS-native class, zero Scala boxing
class WgslFnData(val name: String, val src: String) extends js.Object

// Compile-time typed wrapper — opaque, no extra allocation
opaque type WgslFn[P, R] = WgslFnData
```

At runtime, `WgslFn[P, R]` is just a `WgslFnData` with the function name and
complete WGSL source. All type information is erased. This is identical to how
`FloatExpr` etc. are `String` at runtime.

### 3.2 `WgslFn.raw` Constructor

```scala
object WgslFn:
  inline def raw[P, R](name: String)(body: String): WgslFn[P, R] =
    val paramList = buildParamList[P]    // compile-time inline
    val retType   = wgslReturnType[R]    // compile-time inline
    val src = s"fn $name($paramList) -> $retType {\n$body\n}"
    WgslFnData(name, src)
```

`buildParamList[P]` is an `inline` function that iterates `NamedTuple.Names[P]`
and `NamedTuple.DropNames[P]` to produce `"pos: vec2<f32>, angle: f32"`. This
mirrors the existing `derive.scala` field iteration that generates struct
declarations. `wgslReturnType[R]` uses `inline erasedValue[R]` to produce
`"void"` for `Unit` or summon `WGSLType[R].wgslName` otherwise.

### 3.3 `WgslFn.dsl` Constructor

```scala
  inline def dsl[P, R](name: String)(
    body: (TypedExprAccessor[NamedTuple.Map[P & AnyNamedTuple, ToExpr]], ReturnEmitter[R]) => Block
  ): WgslFn[P, R] =
    val p   = TypedExprAccessor[NamedTuple.Map[P & AnyNamedTuple, ToExpr]]("")
    val ret = ReturnEmitter[R]()
    val block = body(p, ret)
    val paramList = buildParamList[P]
    val retType   = wgslReturnType[R]
    val src = s"fn $name($paramList) -> $retType {\n${Block.unwrap(block)}\n}"
    WgslFnData(name, src)
```

`TypedExprAccessor` is reused as-is from `context.scala` — no changes needed.
The `Fields` type is `NamedTuple.Map[P & AnyNamedTuple, ToExpr]`, the same
pattern used for `ctx.in`. Parameters are accessed as bare names (`p.v`,
`p.angle`) because `TypedExprAccessor("")` uses an empty prefix.

`ReturnEmitter[R]` is a tiny typed wrapper:

```scala
class ReturnEmitter[R]:
  def apply(v: ToExpr[R]): Block =
    Block(Stmt.raw(s"  return ${v: String};"))
```

### 3.4 `apply` Extension Methods (Per-Arity)

The arity is fixed by the concrete `P` at each call site. We provide explicit
per-arity `apply` extensions (not a generic match on `P`) because named tuple
alias-reduction limitations make variadic approaches unreliable:

```scala
// Arity 1
extension [N1, R](fn: WgslFn[Tuple1[N1], R])
  inline def apply(a1: ToExpr[N1]): ToExpr[R] =
    callExpr[R](s"${fn.name}($a1)")

// Arity 2
extension [N1, N2, R](fn: WgslFn[(N1, N2), R])
  inline def apply(a1: ToExpr[N1], a2: ToExpr[N2]): ToExpr[R] =
    callExpr[R](s"${fn.name}($a1, $a2)")

// ... through arity 8
```

The `P` type parameter is destructured to concrete `Ni` types at the call site
(where `WgslFn[(v: Vec2, angle: Float), Vec2]` is a concrete type), so
`ToExpr[N1]` reduces correctly — no alias-reduction problem.

`callExpr[R]` casts the generated string to the right opaque type:

```scala
private inline def callExpr[R](s: String): ToExpr[R] =
  inline erasedValue[R] match
    case _: Float   => FloatExpr(s)
    case _: Double  => FloatExpr(s)
    case _: Vec2    => Vec2Expr(s)
    case _: Vec3    => Vec3Expr(s)
    case _: Vec4    => Vec4Expr(s)
    case _: Mat2    => Mat2Expr(s)
    case _: Mat3    => Mat3Expr(s)
    case _: Mat4    => Mat4Expr(s)
    case _: Boolean => BoolExpr(s)
    case _: Unit    => Expr.raw(s)
```

**Note on named tuples in arity extensions**: The extensions need to match
`WgslFn[(v: Vec2, angle: Float), Vec2]` where `P` is a `NamedTuple`. The arity
extensions use unlabeled type parameters (`[N1, N2, R]`) which should match the
underlying value types regardless of labels. This may require a type alias
`type FnArgs[P] = NamedTuple.DropNames[P & AnyNamedTuple]` to extract the value
types — to be resolved during implementation.

### 3.5 `Program` Changes

```scala
class Program[A, V, U]:
  var vertBody: Block = Block.empty
  var fragBody: Block = Block.empty
  private val fnSrcs  = Arr[String]()
  private val fnNames = Dict[Boolean]()

  def fn[P, R](f: WgslFn[P, R]): Unit =
    val data = f.asInstanceOf[WgslFnData]
    if !js.DynamicImplicits.truthValue(
        fnNames.asInstanceOf[js.Dynamic].hasOwnProperty(data.name)
    ) then
      fnNames(data.name) = true
      fnSrcs.push(data.src)

  def helperFnsStr: String = fnSrcs.mkString("\n\n")
  // vertBodyStr / fragBodyStr unchanged
```

The `hasOwnProperty` deduplication pattern is already established in
`Painter.pipelineCache` — same idiom.

### 3.6 `ShaderDef` Changes

Add `helperFns: String = ""` to `ShaderDef` and thread it into `buildWGSL`:

```scala
case class ShaderDef[A, V, U, VBI, VBO, FBI, FO](
    vertexBody: String,
    fragmentBody: String,
    helperFns: String = "",    // new, backwards-compatible default
):
  private def buildWGSL(...): String =
    val parts = Arr(
      vertexInputStruct,
      vertexOutputStruct,
      fragmentOutputStruct,
      uniformDecls,
      helperFns,               // inserted before vs_main/fs_main
      buildVertexMain(vertexBody),
      buildFragmentMain(fragmentBody)
    ).filter(_.nonEmpty)
    parts.mkString("\n\n")
```

`Shader.apply` gains the same default:

```scala
def apply[A, V, U](
    vertexBody: String,
    fragmentBody: String,
    helperFns: String = "",
): ShaderDef[A, V, U, None, VertOut, None, FragOut] = ...
```

### 3.7 `Painter.shade` DSL Overload Change

```scala
inline def shade[A, V, U](build: Program[A, V, U] => Unit): Shade[U] =
  val program = new Program[A, V, U]
  build(program)
  shade[A, V, U](program.vertBodyStr, program.fragBodyStr, program.helperFnsStr)
```

The raw-string overload gains `helperFns: String = ""` and passes it to
`Shader.apply`. All existing call sites remain valid.

---

## 4. Key Design Decisions

| Decision | Rationale |
|---|---|
| Named tuple `P` for parameters | Names drive both WGSL parameter names and DSL accessor field names |
| `apply` extension → `fn(args)` syntax | Extension `apply` enables natural function-call syntax; more idiomatic than `.call(...)` |
| Per-arity `apply` extensions (1–8) | Named tuple alias reduction is unreliable in variadic match types; arity overloads work reliably |
| Manual registration order | Auto-dependency would add runtime allocation per function; WGSL users already understand declaration order |
| `helperFns: String = ""` in ShaderDef | Backwards-compatible; helper functions must be top-level, not inside a function body |
| User-supplied WGSL name | No magic — functions defined at top level are reused across programs; explicit names are readable in WGSL output |
| `WgslFnData extends js.Object` | Zero boxing; field names preserved in JS output; established pattern in this codebase |
| `TypedExprAccessor("")` for DSL body | Empty prefix → bare parameter names in WGSL, reuses existing accessor machinery |

---

## 5. File Layout

### New File

```
src/gpu/shader/dsl/fn.scala
```

Contains:
- `class WgslFnData(val name: String, val src: String) extends js.Object`
- `opaque type WgslFn[P, R] = WgslFnData`
- `object WgslFn` with `raw`, `dsl`, private helpers
- `class ReturnEmitter[R]`
- Per-arity `apply` extensions (arity 1–8)
- `type FnParamToExpr[P] = NamedTuple.Map[P & AnyNamedTuple, ToExpr]`

### Modified Files

| File | Change |
|---|---|
| `src/gpu/shader/dsl/program.scala` | Add `fnSrcs: Arr[String]`, `fnNames: Dict[Boolean]`, `fn[P,R]` method, `helperFnsStr` accessor |
| `src/gpu/shader/shader.scala` | Add `helperFns: String = ""` to `ShaderDef` and `Shader.apply`; thread into `buildWGSL` |
| `src/gpu/painter/painter.scala` | Update DSL `shade` overload to forward `program.helperFnsStr`; add `helperFns` param to raw-string overload |

---

## 6. Implementation Sequencing

1. Add `WgslFnData`, `WgslFn` opaque type, and `WgslFn.raw` — verify param list
   and return type generation with unit tests
2. Add arity-1 and arity-2 `apply` extensions — verify `fn(args)` syntax works
   and type inference generates the correct WGSL expression string
3. Add `program.fn`, `fnSrcs`/`fnNames`, and `helperFnsStr` to `Program`
4. Add `helperFns` parameter to `ShaderDef.buildWGSL` and `Shader.apply`; update
   `Painter.shade` DSL overload
5. Add `WgslFn.dsl` with `ReturnEmitter` — verify DSL body can access named
   parameters
6. Extend `apply` to arity 3–8
7. Write a new draft `drafts/shader_fn/` demonstrating raw and DSL functions,
   including a function calling another function — visual verification

---

## 7. Open Questions

1. **Named tuple destructuring in arity extensions**: The `apply` extensions
   need to match `WgslFn[(v: Vec2, angle: Float), Vec2]` where `P` is a
   `NamedTuple`. Concretely: does
   `extension [N1, N2, R](fn: WgslFn[(N1, N2), R])` match a named tuple or only
   an unnamed one? If not, a `type FnArgs[P]` helper to extract `DropNames` may
   be needed. Resolve during Step 2.

2. **`buildParamList` implementation**: This needs to iterate
   `NamedTuple.Names[P]` (field names as string literals) alongside
   `NamedTuple.DropNames[P]` (value types) to produce the WGSL parameter string.
   The exact `inline` macro technique mirrors `derive.scala` — but
   `derive.scala` uses `constValue` on tuple element types. The implementation
   should follow the same pattern or reuse shared helpers from `derive.scala`.

3. **`wgslReturnType[R]` for void**: When `R = Unit`, the function should emit
   `-> void`. WGSL requires explicit `-> void` for void functions — confirm
   this is correct WGSL syntax.
