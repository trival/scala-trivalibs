Instead of writing raw WGSL, we envision a scala DSL that compiles down to WGSL.
It should use our Math and NumExt apis. The following raw example. Next we will
write peudocode, for how we want to write this in scala.

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

here comes the scala DSL version of the above WGSL code. We want to be able to
write this in a more scala-y way, and have it compile down to the above WGSL. We
need to design and implementation strategy for that

```scala
type Stmt = ???
type Block = ???
object Block:
  def apply(stmts: Stmt*): Block = ???

type Ctx[A, V, U, Locals] = ???

val shade = painter.shade[Attribs, Varyings, Uniforms]: programm =>
  // locals as named tuple type parameter on the vert and frag functions
  // appear as "locals" on ctx.
  program.vert[(rotated: Vec2)]: ctx =>
    Block(
      // rotated typed as Vec2Term
      ctx.locals.rotated := ctx.bindings.rotation * ctx.in.position,
      ctx.out.position := Vec4(
        ctx.locals.rotated + ctx.bindings.translation,
        0.0,
        1.0,
      ),
    )

  // local variables optional, only typed when needed
  program.frag: ctx =>
    Block(
      ctx.out.color := Vec4(ctx.bindings.color, 1.0),
    )

```

Above we could create a local scala variable "rotated" like this:

```scala
  program.vert[(rotated: Vec2)]: ctx =>
    val rotated = ctx.bindings.rotation * ctx.in.position
    ctx.out.position := Vec4(
      rotated + ctx.bindings.translation,
      0.0,
      1.0,
    )
```

this would inline the expression for "rotated" everywhere it is used and not
actually assigning a new wgsl variable.

We could also declare shorthand bindings for locals/inputs/outputs

```scala
type Stmt = ???
type Block = ???
object Block:
  def apply(stmts: Stmt*): Block = ???

type Ctx[A, V, U, Locals] = ???

val shade = painter.shade[Attribs, Varyings, Uniforms]: programm =>
  program.vert[(rotated: Vec2)]: ctx =>
    val rotated = ctx.locals.rotated
    val outPos = ctx.out.position
    val inPos = ctx.in.position
    val rot = ctx.bindings.rotation
    val t = ctx.bindings.translation

    Block(
      // rotated typed as Vec2Term
      rotated := rot * inPos,
      outPos := Vec4(rotated + t, 0.0, 1.0),
    )
```
