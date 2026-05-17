# Transform, Camera & Scene — Design & Implementation Plan

Minimal 3D scene management helpers for the Scala WebGPU library. Ported and
adapted from `trivalibs_core` (Rust). Lives in a new `graphics.scene` namespace,
parallel to `graphics.painter` and `graphics.math`.

**Prerequisite**: `Quat` and `Mat4` constructors (`fromTranslation`,
`fromScale`, `fromAxisAngle`, `perspective`, `lookAt`) from the math library
plan must be implemented first. See `documents/math-library-design.md`.

---

## Table of Contents

1. [Design Goals](#1-design-goals)
2. [Transform](#2-transform)
3. [PerspectiveCamera](#3-perspectivecamera)
4. [Scene Helpers](#4-scene-helpers)
5. [File Organization](#5-file-organization)
6. [Implementation Plan](#6-implementation-plan)

---

## 1. Design Goals

Keep the same minimal philosophy as the Rust original:

- **Transform stores TRS separately** — translation, rotation (Quat), scale as
  independent fields. A matrix is only computed on demand via `toMatrix()`.
  This keeps rotation interpolation and composition cheap.
- **No scene graph / ECS** — no retained tree of scene nodes. `SceneObject` is a
  trait that anything with a `Transform` can implement. Hierarchies are computed
  lazily by walking the `parent` chain.
- **Mutable, zero-allocation in hot paths** — same pattern as `Vec3`, `Mat4`:
  mutable class with setters, `*Self` / `*To` variants for render-loop use.
- **No `Option`** — use `Opt[T]` (`js.UndefOr[T]`) per project conventions.
- **No `scala.collection.*`** — `Arr` for any sequences.
- **Camera caches projection matrix** — recalculated only when
  fov/aspect/near/far changes, not every frame.

---

## 2. Transform

### 2.1 Data Model

```scala
package graphics.scene

import graphics.math.cpu.{Vec3, Quat, Mat4}

class Transform(
  var translation: Vec3 = Vec3(0.0, 0.0, 0.0),
  var rotation:    Quat = Quat.identity,
  var scale:       Vec3 = Vec3(1.0, 1.0, 1.0),
)
```

Stored separately so that:

- Rotation can be composed via quaternion multiplication (cheap, no drift)
- Translation and scale are trivial to read and set independently
- `toMatrix()` is computed once per frame when needed, not speculatively

### 2.2 Constructors (companion object)

```scala
object Transform:
  def identity: Transform = Transform()

  def fromTranslation(t: Vec3): Transform =
    Transform(translation = t)

  def fromRotation(r: Quat): Transform =
    Transform(rotation = r)

  def fromScale(s: Vec3): Transform =
    Transform(scale = s)

  def fromScale(s: Double): Transform =
    Transform(scale = Vec3(s, s, s))

  def fromXyz(x: Double, y: Double, z: Double): Transform =
    Transform(translation = Vec3(x, y, z))
```

### 2.3 Matrix Computation

```scala
extension (t: Transform)
  // Compute the full TRS model matrix in one step — no intermediate allocations
  def toMatrix(): Mat4 =
    Mat4.fromTranslationRotationScale(t.translation, t.rotation, t.scale)
```

Column-major, right-handed. Translation in column 3. `fromTranslationRotationScale`
computes the TRS matrix directly from components without intermediate allocations.

### 2.4 Directional Vectors

These derive the local axes from the rotation quaternion:

```scala
  def localX: Vec3 = t.rotation * Vec3(1.0, 0.0, 0.0)
  def localY: Vec3 = t.rotation * Vec3(0.0, 1.0, 0.0)
  def localZ: Vec3 = t.rotation * Vec3(0.0, 0.0, 1.0)

  def right:   Vec3 =  t.localX
  def left:    Vec3 = -t.localX
  def up:      Vec3 =  t.localY
  def down:    Vec3 = -t.localY
  // Right-handed: forward is -Z (same as WGSL/WebGPU NDC)
  def forward: Vec3 = -t.localZ
  def back:    Vec3 =  t.localZ
```

### 2.5 Rotation Operations

```scala
  // Apply additional rotation on top of current rotation: q applied first
  def rotate(q: Quat): Transform =
    Transform(t.translation, q * t.rotation, t.scale)

  // Mutable: t.rotation *= q is defined as q * t.rotation (pre-multiply convention)
  def rotateSelf(q: Quat): Unit =
    t.rotation *= q

  def rotateX(angle: Double): Transform = t.rotate(Quat.fromRotationX(angle))
  def rotateY(angle: Double): Transform = t.rotate(Quat.fromRotationY(angle))
  def rotateZ(angle: Double): Transform = t.rotate(Quat.fromRotationZ(angle))
  def rotateXSelf(angle: Double): Unit  = t.rotateSelf(Quat.fromRotationX(angle))
  def rotateYSelf(angle: Double): Unit  = t.rotateSelf(Quat.fromRotationY(angle))
  def rotateZSelf(angle: Double): Unit  = t.rotateSelf(Quat.fromRotationZ(angle))
```

> **Note on Quat `*=` order**: `t.rotation *= q` is defined as `t.rotation = q * t.rotation`
> (pre-multiply — new rotation applied first). This is the reverse of the standard scalar
> `*=` convention but matches the intuitive "apply q on top of current rotation" semantics
> used by `rotateSelf`. See `math-library-design.md §6.3`.

### 2.6 Look-At

```scala
  // Orient so that -Z points toward target, Y toward up
  def lookAt(target: Vec3, up: Vec3 = Vec3.Y): Transform =
    val dir = (t.translation - target).normalize
    Transform(t.translation, Quat.fromLookRotation(dir, up), t.scale)

  def lookAtSelf(target: Vec3, up: Vec3 = Vec3.Y): Unit =
    val dir = (t.translation - target).normalize
    t.rotation = Quat.fromLookRotation(dir, up)

  // Orient so that -Z points along dir (dir should be normalized)
  def lookTo(dir: Vec3, up: Vec3 = Vec3.Y): Transform =
    Transform(t.translation, Quat.fromLookRotation(dir, up), t.scale)
```

### 2.7 Transform Composition

```scala
  // Combine transforms: parent * child
  // Equivalent to: child applied first, then parent
  def *(other: Transform): Transform =
    Transform(
      translation = t.translation + t.rotation * (t.scale * other.translation),
      rotation    = t.rotation * other.rotation,
      scale       = t.scale * other.scale,
    )

  // Apply transform to a point (same as toMatrix() * Vec4(v, 1))
  def transformPoint(v: Vec3): Vec3 =
    t.rotation * (t.scale * v) + t.translation
```

### 2.8 Translate and Orbit Operations

```scala
  // Translate by delta, keeping rotation and scale
  def translate(delta: Vec3): Transform =
    Transform(t.translation + delta, t.rotation, t.scale)

  def translateSelf(delta: Vec3): Unit =
    t.translation += delta

  // Rotate around an arbitrary pivot point in world space
  def rotateAround(pivot: Vec3, q: Quat): Transform =
    val newTranslation = pivot + q * (t.translation - pivot)
    Transform(newTranslation, q * t.rotation, t.scale)
```

---

## 3. PerspectiveCamera

### 3.1 Data Model

Stores projection parameters and a simplified FPS-style orientation
(horizontal - vertical angles) rather than a full quaternion. This makes
FPS-style camera control trivial and avoids gimbal lock for standard flythrough
cameras.

```scala
package graphics.scene

import graphics.math.cpu.{Vec3, Mat4, Quat}

class PerspectiveCamera(
  var fov:         Double = math.Pi / 4.0,  // radians, vertical
  var aspect:      Double = 1.0,
  var near:        Double = 0.1,
  var far:         Double = 1000.0,
  var rotHoriz:    Double = 0.0,            // yaw (around Y)
  var rotVert:     Double = 0.0,            // pitch (around X), clamped ±π/2
  var translation: Vec3   = Vec3.zero,
):
  private var _proj: Mat4 = Mat4.identity

  // Called after construction and whenever projection params change
  recalculateProjection()
```

### 3.2 Projection Management

```scala
  def recalculateProjection(): Unit =
    _proj = Mat4.perspective(fov, aspect, near, far)

  def setAspect(newAspect: Double): Unit =
    if aspect != newAspect then
      aspect = newAspect
      recalculateProjection()

  def setFov(newFov: Double): Unit =
    if fov != newFov then
      fov = newFov
      recalculateProjection()

  def set(
    fov:    Double = this.fov,
    aspect: Double = this.aspect,
    near:   Double = this.near,
    far:    Double = this.far,
  ): Unit =
    val needsProj = fov != this.fov || aspect != this.aspect ||
                    near != this.near || far != this.far
    this.fov = fov; this.aspect = aspect
    this.near = near; this.far = far
    if needsProj then recalculateProjection()
```

### 3.3 Rotation Constraints

```scala
  def setRotHoriz(a: Double): Unit =
    rotHoriz = a % (2 * math.Pi)
    if rotHoriz < 0 then rotHoriz += 2 * math.Pi

  def setRotVert(a: Double): Unit =
    rotVert = a.clamp(-math.Pi / 2.0, math.Pi / 2.0)
```

### 3.4 Transform Representation

```scala
  def transform(): Transform =
    val t = Transform.fromTranslation(translation)
    t.rotation = Quat.fromRotationY(rotHoriz) * Quat.fromRotationX(rotVert)
    t
```

### 3.5 Matrices

```scala
  def projectionMat(): Mat4 = _proj

  def viewMat(): Mat4 = transform().toMatrix().inverse

  def viewProjMat(): Mat4 = projectionMat() * viewMat()
```

### 3.6 FPS-Style Movement

```scala
  def resetTransform(pos: Vec3, rotH: Double, rotV: Double): Unit =
    translation = pos
    setRotHoriz(rotH)
    setRotVert(rotV)

  def updateTransform(
    forward: Double = 0.0,
    left:    Double = 0.0,
    up:      Double = 0.0,
    deltaH:  Double = 0.0,
    deltaV:  Double = 0.0,
  ): Unit =
    if deltaH != 0.0 then setRotHoriz(rotHoriz + deltaH)
    if deltaV != 0.0 then setRotVert(rotVert + deltaV)
    if up != 0.0 then
      translation = Vec3(translation.x, translation.y + up, translation.z)
    if forward != 0.0 then
      translation = translation + Vec3(-math.sin(rotHoriz), 0.0, -math.cos(rotHoriz)) * forward
    if left != 0.0 then
      translation = translation + Vec3(-math.cos(rotHoriz), 0.0, math.sin(rotHoriz)) * left
```

### 3.7 Coordinate Conversion

```scala
  def worldToNdc(worldPos: Vec3): Vec3 =
    viewProjMat().projectPoint3(worldPos)

  // Returns None (Opt.Null) if point is outside frustum (z not in [0,1])
  def worldToScreen(worldPos: Vec3, frameW: Double, frameH: Double): Opt[Vec2] =
    val ndc = worldToNdc(worldPos)
    if ndc.z < 0.0 || ndc.z > 1.0 then Opt.Null
    else Opt((ndc.x + 1.0) / 2.0 * frameW, (ndc.y + 1.0) / 2.0 * frameH) // → Vec2
```

> **Note**: `projectPoint3` is a Mat4 utility — equivalent to
> `(m * Vec4(v, 1.0)).xyz / w`. Add to `Mat4ImmutableOps` (not in math lib yet).

### 3.8 Ground Reflection

```scala
  def reflectedGroundCam(): PerspectiveCamera =
    val c = new PerspectiveCamera(fov, aspect, near, far)
    c.rotHoriz = rotHoriz
    c.setRotVert(-rotVert)
    c.translation = Vec3(translation.x, -translation.y, translation.z)
    c._proj = _proj
    c
```

---

## 4. Scene Helpers

### 4.1 Normal Matrix Helper

```scala
package graphics.scene

import graphics.math.cpu.{Mat3, Mat4}

// Upper 3×3 of the model matrix, inverted and transposed.
// Used for transforming surface normals correctly under non-uniform scale.
def normalMat(m: Mat4): Mat3 =
  Mat3.fromUpperLeft(m).inverse.transpose
```

> **Note**: `Mat3.fromUpperLeft(m: Mat4)` extracts the top-left 3×3 of a Mat4.
> Not yet in the math library — add to `Mat3ImmutableOps`.

### 4.2 SceneObject Trait

Rather than a rigid scene graph, `SceneObject` is a structural trait — anything
with a `Transform` and an optional parent reference can implement it.
Hierarchies are computed lazily by walking the parent chain.

```scala
trait SceneObject:
  def transform: Transform
  def parent: Opt[SceneObject]

  def modelMat(): Mat4 =
    // Compose transforms cheaply in TRS space; call toMatrix() only once at the end
    var t = transform
    var p = parent
    while p.nonEmpty do
      t = p.get.transform * t
      p = p.get.parent
    t.toMatrix()

  def modelViewMat(cam: PerspectiveCamera): Mat4 =
    cam.viewMat() * modelMat()

  def modelViewProjMat(cam: PerspectiveCamera): Mat4 =
    cam.viewProjMat() * modelMat()

  def modelNormalMat(): Mat3 =
    normalMat(modelMat())

  def viewNormalMat(cam: PerspectiveCamera): Mat3 =
    normalMat(modelViewMat(cam))
```

`Transform` itself implements `SceneObject` (no parent — acts as a standalone
scene object):

```scala
extension (t: Transform) // or: given SceneObject from Transform via implicit
  // Transform is itself a SceneObject with no parent
  def asSceneObject: SceneObject = new SceneObject:
    def transform = t
    def parent    = Opt.Null
```

> **Design note**: In Scala we avoid making `Transform` directly extend
> `SceneObject` since `SceneObject` refers to itself recursively
> (`parent: Opt[SceneObject]`). Instead, use the extension / wrapper approach
> above, or make `SceneObject` a class parameterized separately.

### 4.3 Usage Example

```scala
// A simple scene object: transform + optional parent
class Mesh(
  val transform: Transform,
  val parent: Opt[SceneObject] = Opt.Null,
) extends SceneObject

// Usage in a render loop:
val mesh  = Mesh(Transform.fromTranslation(Vec3(1.0, 0.0, 0.0)))
val child = Mesh(Transform.fromRotationY(0.5), Opt(mesh))

val mvp = child.modelViewProjMat(camera)
mvpBinding.set(mvp)
```

---

## 5. File Organization

```
src/graphics/scene/
  transform.scala         — Transform class + companion + extensions
  camera.scala            — PerspectiveCamera class
  scene_object.scala      — SceneObject trait + normalMat + helpers
  package.scala           — package-level exports
```

Parallel to `src/graphics/painter/` and `src/graphics/math/`. Imported via:

```scala
import graphics.scene.{*, given}
```

---

## 6. Implementation Plan

**Prerequisites (from math-library-design.md):**

1. `Mat4.fromTranslation(v: Vec3)` — needed by misc helpers
2. `Mat4.fromScale(v: Vec3)` — needed by misc helpers
3. `Mat4.fromTranslationRotationScale(t, r, s)` — primary constructor for
   `toMatrix()`
4. `Mat4.perspective(fov, aspect, near, far)` — needed by `PerspectiveCamera`
5. `Quat` class with: `identity`, `fromRotationX/Y/Z`, `fromAxisAngle`,
   `fromLookRotation`, `*` (compose), `* Vec3` (rotate vector), `toMat4`,
   `normalize`
6. `Mat3.fromUpperLeft(m: Mat4)` — needed by `normalMat()`
7. `Mat4.projectPoint3(v: Vec3): Vec3` — needed by `worldToNdc()`

**Step 1 — Math prerequisites (P1 from math-library-design.md)**

- Add `Mat4.fromTranslation`, `Mat4.fromScale`,
  `Mat4.fromTranslationRotationScale`, `Mat4.perspective`, `Mat4.lookAt` to
  `src/graphics/math/mat4.scala`
- Add mutable variants to `Mat4MutableOps`

**Step 2 — Quat (P2 from math-library-design.md)**

- Add `class Quat` + `object Quat` to `src/graphics/math/cpu/quat.scala`
- Implement: `identity`, `fromRotationX/Y/Z`, `fromAxisAngle`,
  `fromLookRotation` (from forward + up vectors), `*` (Hamilton product),
  `* Vec3`, `toMat4`, `normalize`, `conjugate`, `slerp`

**Step 3 — Mat3 + Mat4 utilities**

- Add `Mat3.fromUpperLeft(m: Mat4)` to `src/graphics/math/cpu/mat3.scala`
- Add `Mat4.projectPoint3(v: Vec3)` to `src/graphics/math/mat4.scala`

**Step 4 — Transform**

- Create `src/graphics/scene/transform.scala`
- Implement constructor, `toMatrix()`, directional vectors, rotation ops,
  look-at, `*` composition, `transformPoint`, orbit helpers

**Step 5 — PerspectiveCamera**

- Create `src/graphics/scene/camera.scala`
- Implement projection management, FPS movement, matrix getters, coordinate
  conversion, ground reflection

**Step 6 — Scene helpers**

- Create `src/graphics/scene/scene_object.scala`
- Implement `normalMat`, `SceneObject` trait, `Transform.asSceneObject`

**Step 7 — Update panel_tex draft to use 3D scene**

- Replace the manual `makeMvp` with `PerspectiveCamera` + `Transform`
- Two intermediate panels: shapes with `Transform` + Y-axis rotation
- Canvas panel: textured shapes sampled from the intermediate panels
- Camera: fixed perspective position, canvas-sized aspect ratio

---

## 7. Open Questions

1. **`SceneObject` parent type**: The Rust version uses `Option<&Self>` (same
   type parent). In Scala we use `Opt[SceneObject]` (trait object). This is
   slightly more flexible but erases the concrete type. Fine for our use case
   since `modelMat()` only needs `transform`.

2. **`Vec3.zero` / `Vec3.Y` constants**: The design uses `Vec3.zero` and
   `Vec3.Y` as named constants. These do not exist yet — add to `object Vec3` as
   `val zero = Vec3(0, 0, 0)`, `val X = Vec3(1,0,0)`, `val Y = Vec3(0,1,0)`,
   `val Z = Vec3(0,0,1)`.

3. **Depth buffer**: 3D scenes with overlapping geometry need depth testing.
   This is tracked in `panel-texture-and-effects-plan.md §8.3`. The `panel_tex`
   draft with 3D shapes will need `depthTest = true` on panels that render the
   intermediate geometry.

4. **Column vs row convention in `*` for Transform**: The composition
   `parent * child` matches the Rust implementation (child applied first). The
   `toMatrix()` produces T×R×S, so parent.toMatrix() ×
   child.toMatrix() gives the correct world transform. Verify this against
   WebGPU's column-major, right-handed coordinate system.
