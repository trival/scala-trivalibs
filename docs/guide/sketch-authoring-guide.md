# Sketch Authoring Guide

How to build a sketch with the trivalibs painter — the end-to-end flow, the core
types, and the per-frame render model. For the exhaustive per-symbol reference,
hover in the IDE / query Metals MCP (`get-docs`, `inspect`) or browse the
generated Scaladoc (`bun run docs` → `docs/api/html/`). For the shader DSL op
catalog see [shader-dsl-guide.md](shader-dsl-guide.md); for traps see
[gotchas.md](gotchas.md).

> Naming note (vs the Rust painter): Rust `Layer` → Scala **`Panel`**; Rust
> `Effect` → Scala **`Layer`**; Rust `shade_effect` → **`layerShade`**.

## 1. Entry point

A sketch is a `@main` def that grabs the canvas and runs everything inside
`Painter.init(canvas) { p => … }`. Resource creation, resize handling, and the
animation loop all live in that closure.

```scala
package sketches.<category>.<name>

import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.graphics.buffers.*
import trivalibs.graphics.math.cpu.{*, given}
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.{*, given}
import trivalibs.utils.animation.animate

@main def myShape(): Unit =
  val canvas = document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]
  Painter.init(canvas): p =>
    // 1. shade  2. form  3. shape/layer  4. panel  5. animate
    ...
```

Copy `sketches/base-triangle/` as a starter (it has `index.html` + the import
block). Build with `bun run sketch <category>/<name>` from the repo root.

## 2. The core pipeline

```
Shade   = compiled shader (vertex+fragment, or fragment-only for a Layer)
Form    = geometry (vertex buffer + topology)
Shape   = Form + Shade + bound uniforms   → drawn into a Panel
Layer   = full-screen fragment pass (post-processing) → attached to a Panel
Panel   = render target: clears, draws its shapes, then runs its layers
paint   = render panels off-screen;  show = present one to the canvas
```

### 2a. 3D geometry — Mesh + `toBufferedGeometry` (the default path)

For anything beyond a single primitive, build a **`Mesh`** of `Quad`/`Triangle`
faces and convert it with **`toBufferedGeometry`** — this is the idiomatic way
to make 3D geometry. It gives you indexed buffers and optional generated
normals, and works with the `geometry` builders (`Box`, `sphereMesh`, `Grid`).

```scala
import trivalibs.graphics.geometry.{*, given}

type Attribs  = (position: Vec3, normal: Vec3)   // normal is generated (see below)
type Varyings = (normal: Vec3)
type Uniforms = (mvp: Mat4)

// Box → Mesh: vertices are Vec3 positions.
val box  = Box(Vec3.zero, 1.0, 1.0, 1.0)
val mesh = new Mesh[Vec3]()
box.faces.foreach((face, normal) => mesh.addFace(face, normal))  // normal optional
// (mesh.addFace(face) is fine too — normals are computed below when requested.)

// FaceVerticesWithFaceNormal generates a per-face normal → matches Attribs:
val form = p.form(geometry =
  toBufferedGeometry(mesh, MeshBufferType.FaceVerticesWithFaceNormal))

val shade = p.shade[Attribs, Varyings, Uniforms]: program =>
  program.vert: ctx =>
    Block(
      ctx.out.position := ctx.bindings.mvp * vec4(ctx.in.position, 1.0),
      ctx.out.normal   := ctx.in.normal,
    )
  program.frag: ctx =>
    ctx.out.color := vec4(ctx.in.normal.normalize * 0.5 + 0.5, 1.0)

val mvp   = p.binding[Mat4]
val shape = p.shape(form, shade, cullMode = CullMode.Back).bind("mvp" := mvp)
val panel = p.panel(shape = shape, clearColor = (0.05, 0.06, 0.1, 1.0), depthTest = true)
```

- **`MeshBufferType`** picks the vertex strategy: `FaceVertices` (default, no
  normals), `CompactVertices` (shared/de-duplicated vertices), or the
  `…WithFaceNormal` / `…WithVertexNormal` variants that **append a generated
  normal** to each vertex — so a trailing `normal: Vec3` appears in your
  `Attribs` (if the mesh vertex is itself a named tuple like
  `(position: Vec3, uv: Vec2)`, use `WithNormal[V]` for the attribs schema).
- **Builders**: `Box` — `.faces` (each as `(quad, normal)`), or per-face
  `.frontFace`/`.topFace`/… with a `(corner, uvw) => vertex` fn for custom
  per-vertex attributes (UVs etc., as in `rooms/base`); `sphereMesh(vSeg, hSeg)(f)`;
  `Grid` → `Mesh(grid.ccwQuads)`.
- Transform meshes with `mesh.map` / `flatMap`. See the `geometry3d_scene`
  example (box + sphere + terrain grid) and the `rooms/base` sketch.

### 2a′. Simple primitives — `allocateAttribs` (raw vertices)

For a one-off primitive (a single triangle / quad, a handful of vertices) where
a mesh is overkill, write raw vertices directly:

```scala
type Attribs = (position: Vec3, color: Vec3)
val verts = allocateAttribs[Attribs](3)
verts(0).set0(0.0, 0.5, 0.0)   // field 0 = position
verts(0).set1(1.0, 0.2, 0.2)   // field 1 = color
// … fill the other vertices …
val form = p.form(vertices = verts)
```

Reach for the `Mesh` path (2a) as soon as the geometry is non-trivial.

### 2b. Full-screen layer (post-processing / procedural)

A `Layer` needs no `Form` — it's a fragment shader over a full-screen triangle,
with `ctx.in.uv` in `[0,1]`:

```scala
type U = (time: Float, res: Vec2)
val shade = p.layerShade[U]: program =>
  program.frag: ctx =>
    ctx.out.color := vec4(ctx.in.uv, 0.0, 1.0)

val time = p.binding(0.0f)
val res  = p.binding[Vec2]
val panel = p.panel(layer = p.layer(shade).bind("time" := time, "res" := res))
```

## 3. Bindings

`p.binding[T]` (or `p.binding(initialValue)`) makes a uniform buffer; update it
with `b.set(v)` / `b := v` / `b.update(ref => …)`. Attach values by **field
name** with `"name" := value`:

```scala
shape.bind("mvp" := mvp, "tint" := Vec3(1, 0.5, 0.2))   // raw value auto-boxes
```

Values may be a `BufferBinding`, a raw uniform value (auto-boxed), a
`GPUSampler` (`p.samplerLinear`), a `Panel`, or a `panel.binding(...)`. `bind`
matches only on the **field name** and a compatible **value type** — it is
stage-agnostic and never mentions visibility (which is convenient: the same
`bind` call works regardless of stage).

Shader-stage **visibility** is instead declared in the shade's uniform schema
(the `U` type parameter), by wrapping field types: `VertexUniform[T]` /
`FragmentUniform[T]` (bare `T` = both stages; in `layerShade` everything is
fragment, so no wrapper needed). See the [shader DSL guide](shader-dsl-guide.md).

**Panel textures** are declared as a separate `P` schema of `FragmentPanel`
markers and read via `ctx.textures.<name>`:

```scala
type P = (tex: FragmentPanel)
val shade = p.layerShade[U, P]: program =>
  program.frag: ctx =>
    ctx.out.color := ctx.textures.tex.sample(ctx.in.uv, ctx.bindings.samp)
// bind a source panel:  layer.bind("tex" := sourcePanel, "samp" := p.samplerLinear)
```

**Instances** (one draw per entry, sharing the form/shade) via
`shape.instances.add("model" := m, …)`.

## 4. The render model (important)

`p.paint(panel)` does, per panel:

1. **Shape pass** — clears to `clearColor` (or loads if none), draws all
   `shapes` into the panel's base texture (with depth/MSAA if enabled).
2. **Layers** — runs each layer in order. A layer reads the previous pass's
   output; the painter auto-injects it as the layer's first panel-texture slot
   **unless** you bind that slot to an external panel (then it reads that
   instead — how a threshold/composite pass reads a separate scene panel).

`p.show(panel)` blits a painted panel to the canvas. Typical frame:

```scala
animate: tpf =>            // tpf is milliseconds (Rust passes seconds — rescale)
  time += tpf
  mvp.set(cam.viewProjMat * model)
  p.paint(panel)
  p.show(panel)
```

Multi-pass: pass panels to `paint` in dependency order, then `show` the last:

```scala
p.paint(scenePanel, bloomPanel, canvasPanel)   // each may sample the earlier ones
p.show(canvasPanel)
```

`p.paintAndShow(panel)` is the one-panel shortcut; `p.draw(shape, clearColor)`
renders a single shape straight to the canvas with no panel (simple demos).

### Mip chains

`p.panel(mips = true)` (or `mipLevels = N`) allocates a mip chain. By default
the painter auto-generates the chain from mip 0 after rendering. If you build
the chain by hand with mip-targeted layers
(`p.layer(shade, mipSource = i, mipTarget = i+1)`), the painter detects that and
**skips** auto-generation so your pyramid survives — this is how bloom
downsample/upsample works (see the `post/bloom` sketch and `examples/mipmaps`).

## 5. Camera, transforms, resize

```scala
val cam = PerspectiveCamera(fov = 0.9, aspect = w/h, near = 0.1, far = 100.0, pos = Vec3(0, 1.7, 0))
p.onResize: (w, h) => cam(aspect = w.toDouble / h)   // runs now + on every resize
// per frame:  mvp.set(cam.viewProjMat * obj.modelMat)
```

`Transform` is a mutable TRS (`translation`/`rotation`/`scale`); `SceneObject`
gives `.modelMat` / `.modelViewProjMat(cam)`. For first-person controls use
`p.input()` + `BasicFirstPersonCameraController` (see `rooms/base`).

## 6. Events

Only `onResize` is wired. For pointer/keyboard, either use `p.input()`
([InputState]) or attach DOM listeners to `p.canvas`. Keep event handling out of
the `animate` body.

## Where to look next

- Op catalog & shader patterns → [shader-dsl-guide.md](shader-dsl-guide.md)
- Pitfalls (Double-on-left, mip clobber, F32 vs Double) →
  [gotchas.md](gotchas.md)
- Per-symbol API → Metals `get-docs`/`inspect`, or the Scaladoc site
- Canonical code → `trivalibs/examples/*`, `sketches/*`
