# Line2D — UV distortion on width changes, and fold-back overlap on tight turns

Status: **analysis / options**. Nothing decided, nothing implemented.

Two artifacts became visible in `sketches/experiments/strokes/study1` that the
opaque uv-debug shade in `examples/bevel_lines_2d` cannot show:

- **A — zig-zag.** Patterns that should read as straight lines across the
  stroke kink once per quad wherever the width changes along the line.
- **B — needle fans.** At tight turns the ribbon throws off fans of long thin
  dark slivers, each spanning a large fraction of the canvas and converging on a
  point. A render (lower right of the study image especially) shows several.

The two have nothing in common mechanically. A is an interpolation problem and
is fixable exactly, mostly for free. B is a geometric self-intersection, and the
cheapest real fix is not in the geometry at all. **Subdivision helps A a little
and B not at all** — details below.

**Priority — and it is about blocking, not severity.** Both are worth solving.
They differ in what they cost while unsolved.

**A blocks a capability outright.** Varying width and a pattern texture on the
line are mutually exclusive today: pick one. Every uv-keyed pattern — bristles,
weave, dashes, anything with structure — breaks on exactly the strokes this
study shows are the interesting ones, and no choice of constants recovers it.
That is a capability we want and cannot have, which is what makes it first.

**B does not block, but it ambushes.** It is avoidable today by keeping the
width below what the turn can carry, decided by eye or by an approximate test —
this study generates its points randomly precisely to stress the builder, so it
hits it far harder than a real sketch would. But the failure is unmistakable
when it lands, and it lands without warning: one seed, one tuning pass, and a
finished-looking render has needles across it. That is a different cost from
A's, not a smaller one — A forecloses a capability, B is a trap that stays armed
under every stroke. Both get solved.

Note that B is **not** "the stroke overlaps itself" — that is wanted and stays.
It is only the hard-edged spiky triangle a tight curve throws off.

---

## 0. The pipeline under discussion

`Line.toBufferedGeometry` (`src/graphics/geometry/line2d.scala:382`) walks the
centerline and emits, per input vertex, one **rib**: a `top` and a `bottom`
outline vertex offset along the mitre bisector by `width` (capped at `5 ×
width`). Both outlines are then bevelled `smoothDepth` times, and the two are
zig-zagged into one triangle strip by the `balance` loop
(`line2d.scala:495`). Attributes written per vertex
(`writeLineVert`, `line2d.scala:317`):

| attribute  | value                                                     |
| ---------- | --------------------------------------------------------- |
| `position` | outline vertex position                                   |
| `width`    | centerline width at that rib (post-bevel: lerped)         |
| `length`   | accumulated centerline length at that rib                 |
| `uv.x`     | `length / totalLength` — arc length over the whole stroke |
| `uv.y`     | `0` top / `1` bottom, `0.5` at the two cap vertices       |
| `localUv`  | same, `x` normalized against this fragment only           |

So each pair of consecutive ribs spans a **quad**, and the strip splits that
quad into two triangles across one diagonal. That diagonal is the whole of
issue A.

---

## 1. Issue A — why a width change shears the uv

### The mechanism

Take one straight segment, ribs perpendicular, half-widths `w0` and `w1`, length
`L`, centerline on `y = 0`:

```
T0 = (0, -w0)   uv = (0, 0)          T1 = (L, -w1)   uv = (1, 0)
B0 = (0,  w0)   uv = (0, 1)          B1 = (L,  w1)   uv = (1, 1)
```

The strip emits triangles `A = (T0, B0, T1)` and `B = (B0, T1, B1)`. A
rasterizer interpolates a varying **affinely over each triangle
independently** — the unique affine function matching its three corner values.

- **`uv.x` is fine.** In `A` the two `x = 0` corners both carry `u = 0`, so the
  affine fit is `u = x/L`; in `B` the two `x = L` corners both carry `u = 1`,
  same fit. Both triangles agree, gradient included.
- **`uv.y` is not.** In `A`, `v = 0` at `T0` and `T1` — so iso-`v` lines run
  **parallel to the top edge**. In `B`, `v = 1` at `B0` and `B1` — iso-`v` lines
  run **parallel to the bottom edge**. When `w0 ≠ w1` those two edges are not
  parallel, so every iso-`v` line breaks at the diagonal.

That break is the zig-zag. The kink angle grows with `|w1 − w0| / L`, which in
this study is extreme by design (`WidthMin = 1/50`, `WidthMax = 1/4`, fresh
random width every `SubdivPerSegment` vertex). The kinks alternate along the
strip, which is what turns a per-quad kink into a visible saw pattern.

The value it _should_ have is the bilinear one:

```
v(x, y) = (y + h(x)) / (2 h(x))        h(x) = lerp(w0, w1, x/L)
```

which is **not** an affine function — hence no per-triangle affine
interpolation can reproduce it. It is, however, a **ratio of two affine
functions**, and that is the lever (A2 below).

### Secondary contributors, same root

- **Mitre joins.** At a corner the two ribs are not parallel, so even `uv.x`'s
  affine fits disagree between the two triangles. Smaller effect, same cause.
- **The `balance` interleave** (`line2d.scala:495`). After independent bevel
  passes the two outlines have different vertex counts, so a "rib" is often
  between a top and a bottom vertex at slightly different `length` — quads
  become general skewed quadrilaterals, not trapezoids, and repeated indices
  emit degenerate triangles. Skew amplifies A.
- **Cap ribs.** The first and last outline vertex sit on the centerline with
  `uv.y = 0.5` (`line2d.scala:500`). The full `0..1` cross-range is compressed
  into a degenerate first quad, so any uv-keyed pattern is strongly distorted
  right at the caps — and `splitAtAngle` puts a cap at every sharp corner.

### Strategies

#### A1 — Lengthwise subdivision (the obvious one)

Insert extra ribs along each segment so each quad spans a smaller `Δwidth`.

- Error shrinks roughly **linearly** with the number of subdivisions — it is
  never eliminated, only pushed below visibility.
- Costs vertices on every stroke, including the straight uniform-width ones that
  had no problem.
- Interacts badly with `cleanup`, whose entire job is to _remove_ vertices that
  carry no shape information; a subdivision pass would have to run after it.
- Does nothing for the cap distortion or for the mitre wedges.
- Where: `line2d.scala`, either a new `Line.subdivide(maxSegmentLength)`
  transformation or a parameter on `toBufferedGeometry`.

#### A2 — Interpolate `v` projectively (recommended)

Because the correct `v` is a **ratio of two affine functions**, we can
interpolate the numerator and the denominator separately — both are exactly
representable — and divide per fragment. This is hand-rolled
perspective-correct interpolation.

In the vertex stage, from attributes that **already exist**:

```
out.vNum = in.uv.y * in.width
out.vDen = in.width
```

fragment: `v = vNum / vDen`.

Check against the trapezoid above: numerator `uv.y · w` takes `0, w0, 0, w1` at
`T0, B0, T1, B1`, which is exactly `(y + h(x))/2` — affine. Denominator `w`
takes `w0, w0, w1, w1` — exactly `h(x)`, affine. Their ratio is exactly the
bilinear `v`. **The kink disappears completely** on straight tapered segments,
and adjacent triangles stay C⁰-continuous everywhere else because both varyings
agree along the shared diagonal by construction.

Notes:

- **No geometry change, no new attributes, no library change** — this can be
  done inside the sketch's shade today. That makes it cheap to validate.
- **Only `v` gets the divide. `u` must stay affine.** Dividing `u` by the same
  denominator would make it hyperbolic along a taper — i.e. arc length would
  stop being arc length, and everything keyed on stroke progress (`uv.x`
  fades, dash patterns) would stretch toward the wide end. The correction is
  right for the cross direction and wrong for the along direction; that
  asymmetry is not an oversight.
- **Approximate at mitres**, where the true half-extent is
  `width / dot(normal, prevNormal)` capped at `5 × width`, not `width`. Exact
  there needs the real offset as an attribute — see A3.
- If it proves out, the natural library form is a small shader-lib helper
  (`shader/lib/` — a `lineUv` block that takes the varyings and returns the
  corrected uv) plus a note in the `LineAttribs` scaladoc, rather than a
  changed attribute schema.

#### A3 — World-unit stroke coordinates instead of a normalized `v`

Write the **signed perpendicular offset in the line's own units** as an
attribute (top rib `+offset`, bottom `−offset`, cap `0`, using the actual mitred
offset the geometry already computes at `line2d.scala:415`). It interpolates
affinely and exactly — it _is_ an affine function of position on a trapezoid —
so the fragment gets:

- exact `v = 0.5 + d / (2 · width)` including at mitres (A2 done properly), and
- `d` itself: a cross-stroke coordinate in canvas units, which does **not**
  stretch when the stroke widens. For a bristle/weave look that is arguably the
  coordinate you actually want — bristles stay a constant physical size instead
  of fanning out with the width, the same reasoning that already puts the weave
  in `canvasPos` rather than uv in this study.

Cost: one more float per vertex, and an additive change to `LineAttribs`
(existing shaders keep compiling; they simply don't read the new field).

#### A4 — Analytic uv per fragment from flat per-segment varyings

Pass the segment's two rib endpoints as `@interpolate(flat)` varyings and invert
the bilinear map in the fragment shader. Exact everywhere, including mitres and
caps. But: the provoking-vertex convention under a triangle strip makes "which
segment am I in" fragile, it is a lot of varyings, and it needs flat
interpolation support plumbed through the shader DSL. Listed for completeness;
not proportionate to the problem.

#### A5 — Don't key the pattern on uv at all

The study's weave already reads `canvasPos`, and it shows no zig-zag for exactly
that reason. Any pattern moved from uv-space into canvas-space stops caring
about the tessellation. This is not a fix for the library — a stroke _needs_ a
usable cross-stroke coordinate — but it is worth stating that
`edgeFade`/`bristle` could be reformulated, and that a corrected `v` (A2/A3) is
mainly needed for the parts that genuinely must follow the ribbon: the edge
falloff, the end fade, any along-stroke texture.

### A — trade-off summary

| Strategy               | Exact?        | Cost                    | Where            | Fixes mitres | Fixes caps |
| ---------------------- | ------------- | ----------------------- | ---------------- | ------------ | ---------- |
| A1 subdivision         | no (∝ 1/N)    | vertices, a new pass    | library          | no           | no         |
| **A2 projective `v`**  | yes on tapers | 2 varyings + 1 divide   | **sketch first** | approx       | improves   |
| A3 offset attribute    | yes           | +1 float/vertex, schema | library          | yes          | improves   |
| A4 flat per-segment    | yes           | high, fragile           | library + DSL    | yes          | yes        |
| A5 world-space pattern | n/a           | none                    | sketch           | n/a          | n/a        |

---

## 2. Issue B — needle fans and fold-back at tight turns

### The mechanism

The occurrence characteristic is the obvious one: **a large width meeting a turn
too sharp to carry it**. What a render adds is how far the damage travels — the
visible artifact is a **fan of long thin needles converging on a point**,
several of them, each spanning a large fraction of the canvas.

It is **one formula with two symptoms**, not two mechanisms. The mitre offset is
applied symmetrically (`line2d.scala:417`):

```scala
offset = (v.width / normal.dot(prevNormal)).min(v.width * 5.0)
top    = normal *  offset + v.pos
bottom = normal * -offset + v.pos
```

`width / cos(θ/2)` is the right distance for the **outer** side — it is where the
two outer edges actually intersect — and applying the same magnitude to the
**inner** side is what makes the inner outline overshoot: the inner intersection
only lies inside both segments when they are long enough for it. When they are
not, the inner outline runs forward, jumps back past the corner, and runs
forward again. **That backwards run is real**, and it is the same number driving
the outward spike, not a separate effect.

So: outward, an excursion far off the centerline; inward, a fold that reverses.
Both from one `offset` that the turn cannot carry.

Run the arithmetic for this study:

- `v.width` is the **half**-extent (`top = normal·offset`, `bottom =
normal·−offset`), so `WidthMax = 1/4` means a stroke half a canvas wide.
- `splitAtAngle(3π/4)` leaves turns of up to 135° inside a fragment, so the
  mitre factor reaches `1/cos(67.5°) ≈ 2.6`.
- `2.6 × 0.25 ≈ 0.65` canvas units. One outline vertex is thrown two thirds of
  the canvas off the centerline — which is the length of the needles.

Then `smoothDepth = 4` bevels each outline corner into up to 16 vertices spread
along those excursions, and the `balance` walk pairs them against the far fewer
vertices the opposite outline has in the same `length` span. **Every pair becomes
a long thin triangle radiating from a near-common apex** — the fan, converging
exactly where the image shows it converging. Overdraw between the slivers is what
darkens them. The tessellation does not cause the artifact, but it is what turns
one bad vertex into fifty visible needles.

Note the existing clamp does not prevent any of this, by design: `.min(width * 5)`
shortens the excursion while keeping its **direction**, so it caps how long the
spike is rather than stopping it from being a spike. And a cap expressed as a
multiple of `width` scales with the very quantity that is causing the trouble.

Two more overlap sources stack onto the same corners:

- `splitAtAngle` splits at 3π/4 and then `toBufferedGeometries` _deliberately_
  extends each fragment's cap along the neighbour's direction
  (`line2d.scala:425` / `:438`) so the fragments meet without a gap. That
  overlap is by design and permanent.
- A long stroke through 20 random points over the canvas crosses itself
  constantly, by construction.

**Those two overlaps are wanted.** A stroke passing over itself, and fragments
meeting with a little overlap, read as a brush laying pigment over pigment —
that is realism, and the density it accumulates is part of the look. **The only
thing to fix is the needle fan and the fold under it**: a sharp, spiky artifact at a tight
curve, unrelated to anything the stroke is doing, arriving as a hard-edged
geometric shape rather than as a pass of the brush.

That distinction sets the whole shape of the fix. It has to be **local to the
fold** — it must not touch how the stroke composites with itself anywhere else.
Any global measure that makes overlap idempotent (B0, B4, B5) suppresses the
artifact by suppressing the effect we want along with it, and any measure that
removes self-intersections wholesale (a true offset-outline clip) removes the
wanted overlaps too. So the field narrows to B1 and B2, which act on the corner
and nowhere else.

### B0 — Make the blend idempotent under overlap — rejected, kept for the record

The study currently blends alpha with
`BlendFn(One, OneMinusSrcAlpha, Add)` — src-over, i.e. `src + dst·(1 − src)`,
which **compounds** across overlapping draws: `0.5 → 0.75 → 0.875`. Three-fold
coverage at a fold-back is the case it compounds hardest on, and it is the
direct cause of the dark spikes.

`BlendOp.Max` exists (`src/graphics/painter/enums.scala:104`). With
`BlendFn(One, One, BlendOp.Max)` the alpha channel keeps the highest coverage
any fragment produced, so drawing the same area twice or three times looks
identical to drawing it once. That removes the spikes at fold-backs, at the
split-fragment caps, and at every self-crossing — one line, no geometry work.

Caveats to check when trying it: the color function is `(One, Zero)`, so the
color still comes from whichever fragment drew last (fine while the stroke color
is near-constant, worth re-checking if the color ever varies along the stroke);
and MSAA resolve is unaffected since blending is per-sample.

**Why this is rejected here:** it is a global switch, and it cannot tell the
wanted overlap from the unwanted one. Compounding alpha is exactly the effect
this stroke wants — a second pass over the same pigment reading denser — so
trading it away to suppress one corner artifact costs more than the artifact
does. `BlendOp.Max` remains the right tool for a stroke whose look calls for
flat coverage; it is not the tool for this one.

### B1 and B2 share their detection math

B1 and B2 are **the same measurement with two different responses**, and neither
is a matter of tuning a constant by eye — both need the fold predicate computed.

The predicate is not a per-vertex curvature test. On a polyline that has been
through `smoothEdges` and `splitAtAngle`, a tight corner is a _run_ of small
turns, so a fold can be spread over several vertices none of which turns much on
its own. The honest form walks a window: accumulate turn angle `Δθ` and arc
length `s` outward from each vertex, and require

```
halfWidth ≤ s / Δθ
```

over every window — i.e. the half-width must fit inside the discrete radius the
line actually achieves over the distance it takes to turn. A single-vertex
`halfWidth ≤ len · tan(θ/2)` test is the degenerate one-segment case of the same
inequality and misses the spread-out corners.

Then the strategies diverge on what to do when it fails. **B6 replaces the
offending offset itself** and is the front-runner; B1 and B2 leave the offset
formula alone and work around it from the inside or from upstream. (The
numbering follows discovery order, not preference — B6 arrived once the render
showed how far the excursion travels.)

### B6 — A real miter limit with bevel fallback (front-runner)

The standard join treatment, and the one the current code stops just short of:
when the miter factor exceeds a limit, **do not place a vertex on the bisector
at all**. Emit a **bevel join** instead — two vertices at exactly `width`,
perpendicular to the incoming and the outgoing segment respectively, with the
corner cut straight across between them. SVG and Canvas both do exactly this,
`miterLimit` with a bevel fallback.

Why this fits better than anything else in the section:

- **It removes the fan at its source.** No long excursion exists, so
  `smoothEdges` has nothing to bevel into 16 vertices, and the `balance` walk has
  no far-flung vertices to fan against the opposite outline.
- **It fixes both symptoms at once**, because the excursion it replaces was the
  same number on both sides. The inner vertex stops overshooting to 2.6 × width
  and lands at `width` perpendicular to its own segment, so the backwards run
  goes away with the spike. Whatever residual overlap remains at a tight corner
  is local and pigment-like — the wanted kind.
- **Bounded by construction, in absolute terms.** Every outline vertex sits at
  `width` from the centerline. There is no multiple-of-width cap that grows with
  the width, and no configuration of turn and width that can produce an
  excursion again.
- **Precise and opt-in**, which is what was asked of a B fix: a `miterLimit`
  parameter on the geometry build. Below the limit nothing changes at all, so
  existing strokes keep their exact look, and the treatment is chosen.
- **No interaction with A.** Bevel vertices are at exactly `width`, so `width`
  stays the true half-extent and the A2/A3 uv correction keeps working — better
  than today, where a mitred vertex at 2.6 × width already breaks that
  assumption.
- **It is a small, local change** to the rib placement loop, unlike B1's
  intersection tests or B3–B5's rewrites.

Open question it raises: a bevel join emits two outline vertices where the
mitre emitted one, so the two outlines get different vertex counts more often
and the `balance` interleave carries more of the load. Worth watching, not
obviously a problem.

For any fold that survives B6, the two remaining options are not peers: B2 is
what we can do **today, without any library work**; B1 is the geometric version.

### B2 — Keep the situation from arising (available now, a workaround)

Fix the **centerline data** before any offsetting happens: keep the width below
what the turn can carry. Both offsets then follow symmetrically, there is no
special case in the outline builder, and the ribbon stays a valid ribbon.

This is available at every level of rigour, which is its real virtue:

- **By eye.** Pick a `WidthMax` and a point distribution that do not produce
  turns tight enough to fold. This is what the study can do this afternoon, with
  no code at all.
- **Approximately.** A cheap per-vertex `halfWidth ≤ len · tan(θ/2)` test in the
  sketch's own point generator, clamping width where it fails. Misses corners
  spread over several small turns, but catches the obvious ones.
- **Properly**, as a `Line` transformation using the windowed predicate,
  alongside `cleanup` / `smoothEdges`.

But it is a workaround, not a solution, and the cost is the part that matters:
**it forbids a region of the design space** — a wide stroke may no longer make a
sharp turn. That region is exactly what this study exists to explore. Narrowing
through a tight turn can be defended as brush behaviour, but it is a constraint
accepted under duress, not an effect anyone asked for.

One genuine advantage over B1: the reduced width is a real property of the
vertex, so it flows into the `width` attribute and the A2/A3 uv correction
derives from it with no extra work.

### B1 — Remove the fold in the geometry (preferred, if it can be exact and opt-in)

Keep the width the caller asked for; fix the **outline**. When the inner offset
would run past the neighbouring segments, collapse the inner vertex onto the
true intersection of the two inner edges (or onto the centerline) instead of
letting it overshoot. Only the inner side needs it — the outer is already
bounded by the existing `5 × width` mitre cap (`line2d.scala:415`).

This is the one worth building, on two conditions:

- **Precise, in any situation.** It has to remove the fold wherever it occurs,
  not below some threshold, and leave everything else bit-identical. Then no
  width and no turn is off-limits any more, and the whole width range stays
  usable at any curvature — which is the point.
- **Opt-in.** A parameter on the geometry build, off by default, so existing
  strokes keep their exact look and a sketch chooses the treatment. Not a silent
  change of what every line does.

Open problems to solve before it qualifies as either:

- Leaves a pinch/notch on the inside of tight corners — the stroke keeps its full
  width but the corner's inner boundary is a compromise, visible as a thinning
  under a hard edge falloff. Whether that reads better than B2's narrowing is an
  open visual question, and it is the thing to prototype first.
- **Desynchronises the `width` attribute from the real half-extent** at clamped
  joins. Under A3 the offset attribute has to carry the _clamped_ offset, not
  `width`, or the corrected `v` is wrong at exactly the corners the clamp
  touched.
- The bevel passes run after the ribs are placed and can reintroduce crossings,
  so the clamp cannot simply be a step in the rib loop and be done.
- Where: the rib placement loop, `line2d.scala:400–459`.

**Both are local, and that is the point.** A stroke that loops back on itself
over a long distance, or crosses a distant part of itself, is untouched by
either — no tight turn is involved, so the predicate never fires. Those overlaps
survive, which is what we want. Being unable to reach them is a feature of B1 and
B2, not a gap in them.

### B3–B5 — the union-of-the-whole-stroke family — out

Three approaches that all compute the stroke's true covered area and shade it
once: trimming the offset outline at its self-intersections the way path-stroking
libraries do (B3); drawing per-segment capsule SDFs unioned with `min` (B4);
stencil-then-cover with nonzero winding (B5).

All three are **out for the same reason, not for their cost**: their defining
property is that the stroke covers every point exactly once, which deletes the
pigment-over-pigment density that makes the stroke read as a brush. They solve a
problem we do not have. (For the record, they also each carry a real price — B3
rebuilds the uv parameterization from scratch, B4 replaces the ribbon renderer
outright, and B5 needs depth-stencil plumbing the painter does not have at all,
no match for `stencil` anywhere under `src/`.)

B4 stays interesting as a **separate second line renderer** some day, for a look
that wants flat coverage — it would also make uv analytic per fragment and solve
A outright. That is a different feature, not a fix for this.

### Does subdivision help B? No.

Subdividing the centerline around a tight corner produces more, smaller
fold-backs whose union area and overdraw depth are essentially unchanged. The
overlap comes from the corner being tighter than the offset distance; it is
scale-invariant with respect to how finely the corner is sampled. Subdivision is
justified for A alone, and A has better options.

---

## 3. What fixes what

The last two columns are the ones we want **kept**, not fixed — a fix that lands
in them is doing damage.

| Fix                 | A zig-zag | A mitres  | A caps | B fold-back | B self-crossing | B split caps   |
| ------------------- | --------- | --------- | ------ | ----------- | --------------- | -------------- |
| A1 subdivision      | partial   | –         | –      | –           | –               | –              |
| A2 projective `v`   | **yes**   | partial   | better | –           | –               | –              |
| A3 offset attribute | **yes**   | **yes**   | better | –           | –               | –              |
| B6 miter limit      | –         | **fixes** | –      | –           | untouched ✓     | untouched ✓    |
| B1 inner clamp      | –         | –         | –      | **yes**     | untouched ✓     | untouched ✓    |
| B2 width limit      | –         | helps     | –      | avoids it   | untouched ✓     | untouched ✓    |
| B0 `BlendOp.Max`    | –         | –         | –      | dims it     | **flattens ✗**  | **flattens ✗** |
| B3–B5 union         | varies    | varies    | varies | yes         | **flattens ✗**  | **flattens ✗** |

("B fold-back" is the inner symptom. B6 is the only row that removes the outward
excursion the fan is built from — which is why the fan went unnoticed until the
render.)

**A2 + A3** for the zig-zag, **B6** for the needle fan. B6 is cheap enough and
targeted enough that it should not wait behind A — it is a bounded change to one
loop, and the fan is the artifact that actually ruins renders. B1/B2 stay
available for the fold-back underneath, which is milder and may not need
anything once the fan is gone.

---

## 4. Suggested order

Both get solved. A first, because nothing else unblocks it.

**A — patterned textures on a width-varying stroke.**

1. **Reproduce the diagnosis, don't trust it.** A debug shade on this study's
   geometry — `fract(v · 20)` as stripes, plus `fract(u · 20)` — makes the kink
   and its location unmistakable, and tells us whether the `balance` interleave
   and the cap ribs contribute enough to matter. The
   `examples/bevel_lines_2d` uv shade is too smooth to show any of it; if we
   want a permanent regression check, that example wants a striped variant
   rather than a gradient.
2. **A2 in the sketch** — two varyings and a divide, no library change. Confirms
   the analysis against the real stroke and settles how much of the remainder is
   mitres, caps and interleave skew rather than the taper.
3. **A3 in the library** — the offset attribute, once A2 has shown what is left.
   This is the version worth keeping: exact at mitres too, and it hands the
   shade a world-unit cross-stroke coordinate, which for bristle-type patterns
   is very likely the better basis than a normalized `v` that stretches with the
   width. Decide at that point whether `v` is derived in a shader-lib helper or
   left to each shade.
4. Re-check the cap ribs (`uv.y = 0.5` on the centerline) separately — with
   `splitAtAngle` in play there is a cap at every sharp corner, so if that
   distortion is still visible after A3 it deserves its own treatment.

**B — the needle fan first, then whatever is left.**

5. **B6 — the miter limit with bevel fallback.** This is now the first B step and
   arguably belongs ahead of A3: it is a bounded change to the rib placement
   loop, it is the artifact that actually ruins renders, and it makes `width` the
   true half-extent again, which A3 wants anyway. Verify it by re-rendering this
   study with the same seed and checking that the fans are gone.
6. **Re-look before doing more.** B6 lands both outlines back at `width`, so the
   fold may be gone with the spike. Whatever remains is local overlap, possibly
   the wanted kind. Decide from a render, not from this document.
7. **B2 by eye** in the meantime — choose widths and a point distribution that
   don't fold. Costs nothing, and constrains what the study may look like, which
   is why it is a stopgap.
8. **B1, if the fold still shows.** Prototype the inner-join clamp on the
   worst corners the study can produce and look at the notch it leaves; that
   image decides whether it is better than B2's narrowing. If it is, build it as
   an opt-in parameter on the geometry build, exact at every corner, with the A3
   offset attribute carrying the clamped value. That is the version that gives
   the full width range back at any curvature.
9. A4 and B0 / B3–B5 are scoped here so we can say no to them on purpose. The B
   ones share one failure: they buy the corner by flattening the stroke's
   overlap with itself, which is the effect the brush look depends on.

The study itself stays valid either way — surfacing these two is what it was
for. But A is the difference between "the line pipeline has a known limit" and
"a patterned brush stroke can change width", so it does not stay a known limit.

---

## 5. Open questions

- **Is `uv.y` meant to be exactly bilinear?** A2/A3 assume that "half way across
  the stroke" means half way in _distance_, at every point. At a mitre the ribs
  are oblique, and there is a second plausible definition (perpendicular from
  the centerline) that differs. Which one the shade wants is a look decision.
- **Should the cross-stroke coordinate be normalized at all?** A3's `d` in
  canvas units may be more useful than `v` for everything except the edge
  falloff. If so, the library should offer both and say which is which.
- **Is `uv.x` allowed to stay affine?** Stated above as a requirement (arc
  length must remain arc length). Worth confirming that nothing wants the
  projective form at joins.
- **Should `cleanup`'s counterpart exist** — a `subdivide`/`resample`
  transformation — independently of this issue? It has uses beyond A1
  (animation along the path, per-vertex data variation) and the study already
  hand-rolls one via `flatMapWithNeighbours`.
- **Where does the width-vs-radius constraint belong?** As a `Line`
  transformation next to `cleanup` / `smoothEdges` (reusable, but it silently
  rewrites widths the caller chose), or as a rule the point generator follows
  (explicit, but re-implemented per sketch). And when it triggers: narrow the
  width, or round out the corner?
- **Where exactly is the line between a wanted overlap and the fold-back?** The
  windowed predicate draws it at "the half-width does not fit in the radius the
  line achieves", which is a clean geometric criterion — but the split-fragment
  caps overlap at those same corners and are wanted. Worth checking on real
  strokes that the predicate fires on the fold and not on the cap overlap that
  sits right next to it.
