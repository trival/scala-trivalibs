# Line2D — UV distortion on width changes, and fold-back overlap on tight turns

Status: **A2 done and verified; A3–A4 approved and next. B approved in shape.**
See section 4 for the step-by-step status.

- **A — approved and started.** A2 (projective `v`) is implemented in study1 and
  **confirmed against many regenerated random geometries: no zig-zag anywhere**.
  That was the condition on the rest, so the full-width semantics change, the
  honest `width` attribute and rib-preserving paired smoothing are unblocked.
- **API naming — `width` means full width, approved** (it is load-bearing for
  A3). The `splitAtAngle` naming question is **deferred** to a later API-surface
  review, noted below.
- **B — under review.** Settled so far: **B6 is rejected** — a sharp miter then
  smoothed is how a brush turn is modelled, so bounding the miter factor forbids
  the corners we want. **B1 and B2 are both wanted**, both programmatic and
  opt-in, as a selectable pair: B1 keeps the width and clamps the inner texture,
  B2 narrows the stroke and keeps the texture complete. Much of the fan falls out
  of A's paired smoothing anyway.

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
  `uv.y = 0.5` (`line2d.scala:500`), compressing the full `0..1` cross range into
  a degenerate first quad. **Deliberate** — it is what gives the contour a corner
  for `smoothEdges` to cut, and therefore what rounds the cap; see A5. The
  distortion it implies has not shown a glitch. The cap problem that _is_ real
  lies in the vertices that rounding inserts.

The first two of these are not separate problems: both are consequences of the
two contours being smoothed independently, and rib-preserving smoothing (under
A3, below) removes them together.

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
- **Approximate at mitres**, where the vertex was placed at
  `width / dot(normal, prevNormal)` (capped at `5 × width`) but the attribute
  says `width`. That is a wrong value in an existing field, not a missing
  field — see A3.
- If it proves out, the natural library form is a small shader-lib helper
  (`shader/lib/` — a `lineUv` block that takes the varyings and returns `v` and
  `d`) plus the `LineAttribs` scaladoc explaining the pair. The only schema
  change A3 then needs is the field's **name**, not its shape.

#### Carried to a later API-surface review: `splitAtAngle`'s threshold

**Not part of this plan.** After these fixes land, the line generator's whole API
gets reviewed for consistency and readability as its own milestone. This belongs
there, together with anything else that review turns up.

The observation, so it is not lost: `splitAtAngle`'s threshold is compared
against `dot(prevDir, dir)` — the angle **between travel directions**, `0`
straight, `π` a full reversal. That is `180° − interior`, so
`splitAtAngle(3π/4)` reads as 135° but splits at a 45° corner.

```
deviation δ = 180° − interior θ
miter factor = 1 / cos(δ/2) = 1 / sin(θ/2)
```

Candidates when it comes up: rename to what it measures (`splitAtTurn`,
`splitAtDeviation`) with the relation documented; or switch to the interior
angle, which reads best but leaves every existing call site numerically valid
and silently different, so it needs a rename regardless; or a
`splitAtMiterLimit(limit)` helper keyed on the quantity that actually predicts
the artifact. Undecided.

Timing note: **cheap now, expensive later** — the API has very few consumers yet.
That argues for scheduling the review soon, not for smuggling the change into
this plan.

The one naming change that _is_ in scope is `width` meaning full width, because
A3 depends on it.

#### A3 — Consistent `width` semantics, and an attribute that tells the truth

A2 is approximate at joins for one reason: the `width` attribute carries the
width the **caller asked for**, while the vertex was actually placed at
`width / cos(θ/2)` along the mitre bisector (`line2d.scala:415`). The attribute
describes a construction input, not the geometry the library produced. Fix that
and A2 gets much closer to exact — no new attribute, no schema change.

The mitre is the largest such gap but not the only one: outline smoothing moves
the produced outline too, and in the other direction. Both are covered below.

**The rule for the pair:** the width field is the **full stroke width the
geometry actually produced at this vertex**; `uv.y` says where across it the
vertex sits (`0` top, `1` bottom, `0.5` centerline). Together they reconstruct
both cross coordinates from the same two varyings:

```
v = V / Q            normalized 0..1, stretches with width
d = V − 0.5·Q        world units, signed distance from the centerline
```

with `V = uv.y · width`, `Q = width`, both full width. Note `d` needs **no
divide** — it is a linear combination of two affine varyings, so it is affine
and exact.

**Which of the two carries a deviation:** `width` carries **intent** — the width
the stroke means to have here. `uv.y` carries **what the geometry actually did**.
Wherever the produced outline falls short of the intended width — a clamped
inner join (B1), a corner pinched by smoothing — the deviation goes into `uv.y`
and `width` is left alone.

`d = V − 0.5·Q` stays exact either way, since it only needs the two to be
consistent with each other. What the convention buys is that `v` reports the
pinch. For an outline vertex retaining a fraction `f` of the half extent
(`f = 1` untouched, `f = 0` on the centerline):

```
uv.y = 0.5 − 0.5·f     top side          uv.y = 0.5 + 0.5·f     bottom side
```

so a top vertex pulled 20% of the half extent toward the centerline carries
`uv.y = 0.1` instead of `0`. The shade can see that the stroke is pinched and
respond — and `width` stays continuous along the stroke, which the world-metric
frame wants. The reverse convention (clamp `width`, keep `uv.y` at 1) throws that
information away and cannot be undone in the shader.

The cost: **`v` no longer always spans `0..1`.** An edge falloff keyed on `v`
will not fully close at pinched places, and the shade cannot recover the local
maximum to renormalize. Deliberate — the compression is the signal.

Consequences worth stating explicitly:

- **Caps stay positive.** A cap vertex sits on the centerline, so its "true
  distance" is 0 — but writing 0 puts `Q = 0` into the `v` divide. It is not
  needed: `uv.y = 0.5` there already gives `d = 0.5·q − 0.5·q = 0` for any
  positive `q`. "On the centerline" is encoded by `uv.y`, not by the width.
  Keep the local width there deliberately.
- **Make `width` mean full width, everywhere.** Today `LineVertex(pos, width)`
  takes a **half**-extent — `top = normal·width`, `bottom = normal·-width`, so
  the stroke is `2 × width` wide. That is a trap: it misreads as full width, and
  it has already caught the library's own author. Fix the semantics rather than
  the attribute name.
  - `LineVertex.width` / `Line.add(pos, width)` mean the **full** stroke width
    in world units.
  - The rib loop halves it once, where it offsets.
  - The attribute keeps the name `width` and carries the **produced full
    width** — `|top_i − bottom_i|` once ribs are paired.

  Nothing gets harder in the shader. `v = V/Q` is unchanged (`Q` cancels), and
  `d` is one multiply-add either way — `V − 0.5·Q` on full width against
  `2·V − Q` on a half-extent. The factor of 2 just moves. What full width buys
  is the quantity a shader author actually reasons about ("how wide is the
  stroke here"), matching `lineWidth` in SVG and Canvas.

  Do it now: the line builder is early and about to be leaned on heavily, and
  **no shader in the repo reads this attribute yet** (`base1`, `tile-strokes`,
  `study1`, `bevel_lines_2d` all use `position` / `uv` / `localUv` only). The
  cost only grows.

  What the change touches:
  - Every sketch's width constants halve — the four above plus `Line`'s
    `defaultWidth` arguments. Mechanical.
  - `cleanup(minLenWidRatio, …)` compares lengths against `avgWidth`, so its
    meaning shifts by 2×. Re-pick the defaults rather than rescaling them
    blindly.
  - The `5 × width` miter cap (`line2d.scala:415`) likewise — and it is being
    a limit worth re-picking against full-width semantics — and generously, since
    sharp miters are wanted (see B6).
  - `smoothMinLength` is a length, unaffected.

- **Outline smoothing shrinks the stroke, so "exact" has a ceiling.** The
  mitre is not the only place the produced geometry departs from the requested
  width. `smoothEdges` at `ratio = 0.25` is corner cutting — Chaikin — and
  `smoothDepth` passes of it converge on a curve lying strictly **inside** the
  outline polygon. Every bevelled corner is therefore pulled toward the
  centerline, and the stroke is genuinely narrower there than `width` says.
  Four things follow:
  - It is **corner-local**: straight runs have no corner to cut, and along a
    straight taper a lerped position and a lerped scale stay consistent, so the
    value remains exact there.
  - It is **asymmetric**. The two outlines are smoothed independently, and at a
    turn the outer outline has the sharper corner, so it is cut harder than the
    inner one. The ribbon's effective centerline drifts and its local thickness
    changes — not just one side's offset.
  - It is **inconsistent between corners**: `smoothMinLength` leaves short
    segments unsmoothed, so some corners shrink and their neighbours do not.
  - So fixing the mitre value raises accuracy from "wrong by up to 2.6×" to
    "wrong by the corner sagitta", which is a large improvement and **not**
    exactness. The doc should not claim exactness for the smoothed case.

  One workaround exists today and costs nothing library-side: **smooth the
  centerline instead of the outlines** — `line.smoothEdges(...)` before
  building, with `smoothDepth = 0` on `toBufferedGeometry`. The outlines are
  then true offsets of a smooth centerline, the produced width is the requested
  `width` by construction, and it reduces B as a side effect (larger radius → less mitre,
  less fold). **But it loses the rounded caps**, which contour smoothing gives
  for free and which are wanted. So it is a comparison worth rendering, not a
  replacement. The real answer is below.

- **Sharp miters are staying** (B6 rejected), so the gap between requested width
  and placed vertex is large by design at corners. That makes this correction
  load-bearing rather than a refinement.

What this unlocks, beyond removing the kink: `d` is a cross-stroke coordinate in
world units that does **not** stretch when the stroke widens. Paired with the
existing `length` attribute (world-unit arc length along the centerline), a
shade gets a full world-metric ribbon frame — see below.

#### Making A3 exact, and the strip simpler: rib-preserving contour smoothing

Contour smoothing is worth keeping — it rounds the **caps** as a side effect,
which centerline smoothing cannot do and which is wanted. What breaks is not
contour smoothing itself but that the two contours are smoothed **independently**
(`line2d.scala:462–473`), so they end up with different vertex counts and the
ribs stop existing as pairs.

Smooth them in **one pass over rib indices instead**: decide per rib, and
whenever one side bevels, emit the matching two vertices on the other side at
the same lerp ratios. On a side with no turn those two points land on its
existing edge, so the shape is unchanged — only the vertex density rises. Both
contours then always have equal counts and matching indices.

What that buys, in order of importance:

- **The produced extent stops being predicted and becomes measured.** With ribs
  paired, `|top_i − bottom_i|` is knowable after all smoothing has run, and the
  rib midpoint is the local center. The mitre gap, the Chaikin shrink and the
  asymmetry all stop being sources of error, because nothing is predicted any
  more. Per the intent/deviation rule, that measurement lands in **`uv.y`** —
  `uv.y_top = 0.5 − |top_i − mid_i| / width_i`, and the mirror for bottom —
  while `width_i` keeps the intended width. `d` then comes out exact, and `v`
  reports how far short of intent the geometry fell. It is also the
  self-consistent frame: `v` is interpolated along the rib, so measuring along
  that same rib is the right axis.
- **Both secondary contributors to A go away.** The `balance` interleave exists
  only to reconcile mismatched contours; with pairs it reduces to
  `emit top_i, bottom_i`, so no skewed quads, no repeated indices, no degenerate
  triangles. And paired vertices carry identical `length`, so `uv.x` no longer
  disagrees across a rib.
- **The strip walk gets shorter**, and the `balance` / `topLen` / `bottomLen`
  bookkeeping can go.
- **Caps still round**, symmetrically on both sides, since the corner that
  rounds them is present on both contours.

Details to settle when building it:

- What triggers a bevel for the pair — either side exceeding the angle
  threshold, or the max of the two? Likewise `smoothMinLength`: the shorter of
  the two sides, or both?
- The paired outlines are no longer symmetric about the centerline after
  smoothing, which is fine and expected — it is exactly why measuring beats
  predicting.
- `Line.smoothEdges` stays as it is for centerline use; this is a new paired
  routine inside `toBufferedGeometry`, not a change to the public
  transformation.
- The stroke is still genuinely narrower at smoothed corners. That is now a
  **look** question rather than a correctness one — the attribute reports it
  truthfully.

#### What A2 + A3 unlock: four coordinate systems, chosen per pattern

A2 + A3 do not impose a cross-stroke convention; they hand the shade both and
let each pattern pick. Crossed with the two along-stroke coordinates that
already exist, that is four frames, all meaningful:

|                       | `v` normalized                          | `d` world units                    |
| --------------------- | --------------------------------------- | ---------------------------------- |
| **`uv.x` normalized** | today's behavior — everything stretches | spans the stroke, fixed cross-size |
| **`length` world**    | fixed spacing along, stretched across   | fully world-metric ribbon frame    |

The cross axis is the interesting choice, and it is a physical one: **`v` is one
brush pressed harder** — hair count fixed, texture stretches across a wider
mark; **`d` is a bigger brush** — hair size fixed, more features appear at the
edges as it widens. Both are wanted, for different marks. Since both are in hand
in the same fragment, `mix(d, v · referenceWidth, k)` dials continuously between
splaying and growing, which is probably closer to a real brush than either
endpoint, and the edge falloff can use `v` while the bristle noise uses `d`.

Two limits on the world-metric frame, neither fixable by attributes:

- **It is not isometric on curves.** `(s, d) → center(s) + d · normal(s)` has
  Jacobian determinant `1 − d·κ(s)`, so a `(length, d)` pattern stretches along
  the outside of a turn and compresses on the inside, and degenerates exactly
  where `d·κ = 1` — the same condition as B's fold. Inherent to any ribbon
  frame, and arguably right for brush texture, which does smear longer around
  the outside of a turn.
- **`swapTextureOrientation` is required, not a hazard.** `toBufferedGeometries`
  passes `i % 2 != 0` (`line2d.scala:585`), so every other fragment runs `uv.y`
  `1→0` instead of `0→1`. That is the brush model: at a reversal the hand stops
  and comes back, **the brush itself does not rotate**, so the physical top edge
  of the mark stays the top edge. Travel direction reverses, so travel-relative
  "left" swaps physical sides — flipping `uv.y` is what keeps `v` and `d`
  referring to the **same physical side** across the split. Without it a
  `d`-keyed pattern would be the discontinuous one.

#### Where the principle stops: `length`

Stated so the principle does not get over-applied. `length` records the
**centerline** accumulated length even at an outer-curve vertex that genuinely
travelled farther, and that is correct: `u` has to be shared stroke progress, or
the two outlines would disagree about where along the stroke they are. So the
rule is not "always record what the geometry produced" — it is **record what the
shader needs to reconstruct the field it wants**. Cross axis: the actual
placement. Along axis: the shared centerline value.

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

| Strategy                  | Exact?                            | Cost                             | Where            | Fixes mitres | Fixes caps |
| ------------------------- | --------------------------------- | -------------------------------- | ---------------- | ------------ | ---------- |
| A1 subdivision            | no (∝ 1/N)                        | vertices, a new pass             | library          | no           | no         |
| **A2 projective `v`**     | yes on tapers                     | 2 varyings + 1 divide            | **sketch first** | approx       | improves   |
| **A3 honest `width`**     | yes, unless outlines are smoothed | semantics fix + one changed line | library          | yes          | improves   |
| **A3 + paired smoothing** | yes, measured not predicted       | rewrite the smoothing pass       | library          | yes          | yes        |
| A4 flat per-segment       | yes                               | high, fragile                    | library + DSL    | yes          | yes        |
| A5 world-space pattern    | n/a                               | none                             | sketch           | n/a          | n/a        |

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
- `splitAtAngle(3π/4)` leaves deviations of up to 135° inside a fragment — a 45°
  interior angle — so the mitre factor reaches `1/cos(67.5°) ≈ 2.6`.
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

It is not a per-vertex curvature test. On a polyline that has been through
`smoothEdges`, a tight corner is a _run_ of small turns, so a fold can be spread
over several vertices none of which turns much on its own. It needs a window —
and the window has a natural size.

**The test.** From each vertex, walk **half the stroke width of arc length in
each direction** and accumulate the turn `Δθ` over that span. The fold condition
is `Δθ > 2` radians (≈ 114.6°).

Where the 2 comes from: over an arc of radius `R`, turn and arc length relate as
`Δθ = s/R`, and the inner offset folds when `R < h` (half-extent). The window
fixes `s = 2h`, so `R < h` becomes `Δθ > 2h/h = 2`. **Nothing to tune** — the
constant falls out of the geometry, and width-dependence survives because the
_window_ scales with the width, not the threshold. A wide stroke inspects a long
span of line and fails on a corner a narrow stroke passes.

Checks at the edges: 2 rad over `3h` → `R = 1.5h`, no fold, and the window sees
only 1.33 rad — no fire. 2 rad over `h` → `R = 0.5h`, folds, and the window sees
the full 2 rad — fires.

Three details:

- Use **the vertex's own width** for its window; it is that vertex's offset that
  folds.
- Under full-width semantics the window is `±width/2` — **one stroke width of
  line, centred on the vertex**.
- **Clamp the window at fragment boundaries.** It must never reach across a
  split: a reversal's overlap is wanted, and the split corner is an endpoint
  with no join to fix.

O(n) with a running turn sum over a sliding window.

Then the strategies diverge on what to do when it fires — B1 clamps the inner
vertex, B2 narrows the stroke. **B6** — bounding the miter factor itself — is
rejected, since the sharp miter is wanted. (Numbering follows discovery order,
not preference.)

### B6 — Bounding the miter factor — rejected

Both forms of this (a bevel fallback, or lowering `splitAtAngle` so the surviving
miter factor `1/cos(θ/2)` stays small) were considered and are **out**. They
remove the artifact by removing the capability: a **sharp miter, then smoothed,
is how an explicit brush turn is modelled**, and that has to work on sharp curves
without artifacts. Capping the miter factor forbids exactly the corners we want
to be able to draw.

Recorded because the arithmetic is worth knowing when choosing a split threshold
deliberately. **`splitAtAngle`'s threshold is a deviation angle** — between the
two travel directions, `0` straight, `π` a full reversal — not the interior angle
at the corner, which is easy to misread:

```
offset = halfWidth / cos(δ/2)      δ = deviation
       = halfWidth / sin(θ/2)      θ = 180° − δ, the interior angle
```

| threshold δ | interior θ | miter factor | excursion at half-extent `1/4` |
| ----------- | ---------- | ------------ | ------------------------------ |
| 3π/4 = 135° | 45°        | 2.61         | 0.65 canvas units — the fans   |
| π/2 = 90°   | 90°        | 1.41         | 0.35                           |
| π/3 = 60°   | 120°       | 1.15         | 0.29                           |

So today's threshold admits a 45° V — sharp, not gentle. Same quantity as SVG's
`miterLimit` (default 4, ≈ 29° interior). Excursions are in today's semantics
where `WidthMax = 1/4` is the half-extent; under the approved full-width change
the same constant halves them.

A bevel join remains a reasonable **separate library feature** for non-brush
lines — it keeps `v` continuous around a corner where a split flips it — but it
is a uv-semantics feature, not a fix, and not in scope here.

**What this leaves.** The fan cannot be blamed on the miter excursion alone,
because the excursion is staying. Re-reading the mechanism: a fan needs many
outer vertices paired against **few** inner ones, and that count mismatch comes
from the inner outline being folded and degenerate at the corner, plus the two
contours being smoothed independently. Both of those are addressed elsewhere:

- **A's rib-preserving paired smoothing** gives every outer vertex its own
  inner partner, so the corner tessellates into ordinary quads instead of
  slivers converging on a point.
- **B1's inner clamp** stops the inner outline folding in the first place.

Together they remove the fan while the sharp outer miter stays exactly as
drawn — a long thin wedge, which is what a brush turn looks like before it is
smoothed. So B is B1 (with B2 as the by-eye stopgap), and the fan is largely a
consequence of work already approved under A.

### B1 and B2 are two different marks, both wanted

Neither is a stopgap for the other. They differ in **where the deviation is
recorded**, which is exactly A3's intent/what-happened rule, and that difference
is visible:

|                   | B1 — clamp the inner vertex        | B2 — narrow the stroke              |
| ----------------- | ---------------------------------- | ----------------------------------- |
| what moves        | inner outline only                 | both outlines, symmetrically        |
| `width` attribute | unchanged (intent stands)          | **reduced** (intent changes)        |
| `uv.y` at outline | **clamped**, never reaches `0`/`1` | stays `0` / `1`                     |
| texture across    | clipped on the inner side          | complete, compressed into less room |
| the mark          | brush stays wide, inner edge bites | brush narrows through the turn      |

`d` stays exact in both. Both are **opt-in**, and which one a stroke wants is a
look decision, so the library offers the choice rather than picking. Per the
project's no-`enum` rule, an opaque type in the geometry API:

```scala
opaque type FoldTreatment = Int
object FoldTreatment:
  val Leave: FoldTreatment = 0
  val ClampInner: FoldTreatment = 1
  val NarrowWidth: FoldTreatment = 2
```

They also compose: a gentle `NarrowWidth` with `ClampInner` catching the
remainder is a third mark worth trying.

### B2 — Narrow the stroke where the turn cannot carry it

Change the **centerline data** before any offsetting: reduce `width` until the
windowed predicate holds. Both offsets then follow symmetrically, there is no
special case in the outline builder, and the ribbon stays a valid ribbon with a
complete texture across it.

Three levels of rigour, and we want the third:

- **By eye.** Pick a `WidthMax` and a point distribution that never fold. Free,
  available now, and what unblocks the study today.
- **Approximately.** A per-vertex `halfWidth ≤ len · tan(θ/2)` clamp in the
  sketch's own generator. Misses corners spread over several small turns.
- **Programmatically**, as a `Line` transformation using the windowed predicate,
  alongside `cleanup` / `smoothEdges`. Its natural home: it rewrites vertex
  data, so the reduced width flows into the attribute by itself and `uv.y`
  needs no special case at all.

The cost is real and is why it is a choice rather than the default: **the stroke
cannot stay wide through a sharp turn**. Whether that reads as a brush easing off
through the turn or as a stroke that lost its weight is the thing to compare
against B1 on a render.

One genuine advantage over B1: the reduced width is a real property of the
vertex, so it flows into the `width` attribute and the A2/A3 uv correction
derives from it with no extra work.

### B1 — Clamp the inner vertex, keep the width

Keep the width the caller asked for; fix the **outline**. When the inner offset
would run past the neighbouring segments, pull the inner vertex back toward the
centerline instead of letting it overshoot. Only the inner side needs it — the
outer miter is wanted and stays.

Two conditions:

- **Precise, in any situation.** It has to remove the fold wherever it occurs,
  not below some threshold, and leave everything else bit-identical. Then no
  width and no turn is off-limits, and the whole width range stays usable at any
  curvature — which is the point.
- **Opt-in.** A `FoldTreatment` parameter on the geometry build, `Leave` by
  default, so existing strokes keep their exact look.

Details:

- Leaves a pinch/notch on the inside of tight corners — the stroke keeps its full
  width but the corner's inner boundary is a compromise, visible as a thinning
  under a hard edge falloff. Whether that reads better than B2's narrowing is
  the comparison to render.
- **Clamp the inner vertex's distance to the centerline** — the centerline is
  the one place that never overlaps, so it is the right thing to measure
  against. Express the clamp in `uv.y`, not in `width`, per A3's "intent vs.
  what happened" rule: a top-side inner vertex pulled 20% of the half extent
  toward the centerline carries `uv.y = 0.1` against an unchanged `width`, so
  `d` stays exact while `v` never reaches its extreme. That compression at the
  notch is a **usable visual signal**, not a defect — the shade sees the corner
  is pinched and can respond, and nothing has been thrown away.
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

| Fix                | A zig-zag | A mitres | A caps | B fold-back | B self-crossing | B split caps              |
| ------------------ | --------- | -------- | ------ | ----------- | --------------- | ------------------------- |
| A1 subdivision     | partial   | –        | –      | –           | –               | –                         |
| A2 projective `v`  | **yes**   | partial  | better | –           | –               | –                         |
| A3 honest `width`  | **yes**   | **yes**  | better | –           | –               | –                         |
| B6 bound the miter | –         | fixes    | –      | –           | untouched ✓     | **forbids sharp turns ✗** |
| B1 clamp inner     | –         | –        | –      | **yes**     | untouched ✓     | untouched ✓               |
| B2 narrow width    | –         | helps    | –      | **yes**     | untouched ✓     | untouched ✓               |
| B0 `BlendOp.Max`   | –         | –        | –      | dims it     | **flattens ✗**  | **flattens ✗**            |
| B3–B5 union        | varies    | varies   | varies | yes         | **flattens ✗**  | **flattens ✗**            |

("B fold-back" is the inner symptom; the needle fan is the tessellation
amplifying it, and A's paired smoothing removes the amplification.)

**A2 + A3 + paired smoothing** for the zig-zag and most of the fan; **B1** for
the fold underneath, with **B2** as the by-eye stopgap until it exists. B6 is
rejected and the union family below the line buys the corner at the price of the
brush.

---

## 4. Implementation plan

| #   | Step                                         | Where   | Status      |
| --- | -------------------------------------------- | ------- | ----------- |
| —   | `tests/line2d-debug` verification sketch     | sketch  | **done**    |
| A2  | Projective `v` proof of concept              | sketch  | **done** \* |
| A3a | `width` means full width                     | library | next        |
| A3b | Attribute carries the produced width         | library | after A3a   |
| A3c | `v` / `d` helper for shades                  | library | after A3b   |
| A4  | Rib-preserving paired contour smoothing      | library | after A3    |
| A5  | Re-check cap ribs                            | —       | after A4    |
| B0  | Re-render, measure what is left of the fan   | sketch  | after A4    |
| B1  | `ClampInner` treatment                       | library | after B0    |
| B2  | `NarrowWidth` treatment                      | library | after B0    |
| B3  | Compare the two marks, pick defaults per use | sketch  | after B1/B2 |

(Step ids are local to this plan and unrelated to the strategy labels in
sections 1–2.)

\* Tapers are fixed and verified. Zig-zag remains at smoothing-rounded caps from
a separate cause — see below; A3b/A4 carry the fix.

### The verification sketch — done

`sketches/tests/line2d-debug/` runs the same geometry pipeline and constants as
study1 under a shade with nothing in it but the uv: one flat color whose alpha is
sine-striped, composited src-over, so overlaps compound and the geometry issues
show themselves. `Mode` at the top selects `Across` (corrected `v`),
`AcrossRawV` (the before/after control), `Along`, `Grid`, `Coverage`.

This is the instrument for every gate below — the real shade's blurred noise was
too soft to judge against.

### A2 — done

Implemented in `sketches/experiments/strokes/study1/StrokeStudy1.scala`:
`LineVaryings` gained `vNum` / `vDen`, the vertex stage writes
`uv.y * width` and `width`, and the fragment stage divides. Both consumers moved
off `uv.y` — the bristle fbm samples `vec2(uv.x, v)` and `edgeFade` uses `v`.
`uv.x` untouched.

**Verified** on `sketches/tests/line2d-debug` (below) as well as study1: across
many regenerated random geometries the cross-stroke bands run smooth and
unbroken through every taper. The analysis in section 1 holds and everything
conditional on it is unblocked.

**Residual, and it is a different mechanism: zig-zag is still visible at
smoothing-rounded caps.** Not an interpolation error — a wrong attribute value.
`writeLineVert` assigns `uv.y` by **index** (`line2d.scala:499`): only the first
and last outline vertex get `0.5`, everything else gets `0` or `1`. Smoothing
rounds a cap by _inserting_ vertices around that corner, and each inserted
vertex inherits the full-edge `0` / `1` while physically curving inward toward
the centerline. A vertex at 20% of the half extent claims to be on the edge.

That is exactly what A3b's rule fixes — `uv.y` carries what the geometry actually
did — once A4 makes the placement measurable
(`0.5 ± |vertex − mid| / width`). So the cap residual is already covered by the
approved plan; it is now a concrete failing case for A3b/A4 rather than a
suspicion.

Note also what A2 does not yet prove: it runs against the current half-extent
`width` attribute, so mitre joins remain approximated. That the taper kink is
invisible even so is evidence the taper term dominated.

### A3a — `width` means full width

The breaking change, in one commit.

- `LineVertex.width`, `Line.add`, `Line.defaultWidth` mean full stroke width.
- The rib loop halves once, where it offsets.
- Halve the width constants in every consumer: `sketches/strokes/base1`,
  `sketches/strokes/tile-strokes`, `sketches/experiments/strokes/study1`,
  `trivalibs/examples/bevel_lines_2d`.
- Re-pick, do not rescale, `cleanup(minLenWidRatio, …)` and the `5 × width`
  miter cap — both shift meaning by 2×.
- `smoothMinLength` is a length; unaffected.

**Gate**: every consumer renders as before at halved constants.

### A3b — the attribute carries the produced width

Write the width the geometry actually produced, not the requested one. Deviations
go into `uv.y` per the intent rule; cap vertices keep a positive width and stay
at `uv.y = 0.5`.

**Gate**: study1 unchanged except that mitre joins stop being approximate — visible
by tightening `edgeFade` until the falloff would show a seam at a corner.

### A3c — a `v` / `d` helper

Once two sketches want it, lift the divide out of the shade into
`shader/lib/`: takes the varying pair, returns `v = V/Q` and
`d = V − 0.5·Q`. Scaladoc on `LineAttribs` explaining the pair. Not before —
see "don't extract unasked".

### A4 — rib-preserving paired contour smoothing

One pass over rib indices; when either side bevels, both emit at the same lerp
ratios. Settle the two open details first: what triggers a bevel for the pair
(either side over the angle threshold, or the max), and which side
`smoothMinLength` tests against.

Then the payoff, all of which is checkable:

- `balance` / `topLen` / `bottomLen` reconciliation deleted, strip walk becomes
  `emit top_i, bottom_i`.
- Paired vertices carry identical `length`, so `uv.x` agrees across a rib.
- `|top_i − bottom_i|` measurable, so A3b's value stops being predicted.
- Caps still round.

**Gate**: no degenerate triangles in the output; study1 renders with the corners
visibly cleaner and the caps still rounded.

### A5 — cap ribs

**The degenerate first quad is deliberate and stays.** The centerline vertex at
`uv.y = 0.5` is what gives the contour a *corner* at the first rib; without it
that rib would be the contour's endpoint, and `smoothEdges` skips endpoints
(`if prev.isNull || next.isNull then Arr(curr.copy)`), so there would be nothing
to cut and no rounded cap. The compressed cross range is the price of the
rounding, it was paid knowingly, and it has produced no visible glitch so far.
Do not "fix" it.

What is a real defect is the index-assigned `uv.y` on the vertices smoothing
*inserts* to round that corner (see A2's residual above) — they claim to be on
the edge while curving inward. That is A3b/A4's job. Re-check after those land;
`splitAtAngle` puts a cap at every sharp corner, so it is not a rare case.

### B0 — measure before building

Paired smoothing removes the vertex-count mismatch the needle fan is built from.
Re-render study1 at the same seed and worst-case corners and see what is actually
left of B before writing any of it.

### B1 / B2 — the two fold treatments

Shared: the windowed predicate — `±width/2` of arc length, fire above 2 radians,
window clamped at fragment boundaries.

- **B1 `ClampInner`** — geometry build. Pull the inner vertex back toward the
  centerline; record it in `uv.y`, leave `width` alone.
- **B2 `NarrowWidth`** — a `Line` transformation. Reduce `width` until the
  predicate holds; `uv.y` stays `0`/`1`.
- `FoldTreatment` opaque type, default `Leave`, so existing strokes are
  untouched.

### B3 — compare

Render the same worst-case corners under both and decide which mark suits which
kind of stroke. Inner texture clipped against stroke narrowed; they also compose.

### Explicitly not doing

A1 (subdivision), A4-flat (per-segment flat varyings), B0-blend (`BlendOp.Max`),
B3–B5 (the union family) and B6 (bounding the miter factor). The union family
buys the corner by flattening the stroke's overlap with itself; B6 buys it by
forbidding sharp turns. Both prices are the brush.

### Deferred to the API-surface review

`splitAtAngle`'s threshold naming, and a `subdivide` / `resample` counterpart to
`cleanup`.

---

## 5. Open questions

None remain. Resolved in the course of the review, kept as a record of what was
decided:

- _Can the fold predicate misfire on the wanted split-fragment cap overlap?_ —
  No, by construction. `splitAtAngle` hands `toBufferedGeometry` separate
  fragments, so a split corner is a fragment **endpoint**: no `prev`/`next` on
  that side, no mitre, nothing for the predicate to see. A reversal's overlap is
  structurally out of reach of B1/B2.

- _Is `uv.y` meant to be exactly bilinear at oblique mitre ribs?_ — Decided by
  construction: `v` is interpolated **along the rib**, so it is measured along
  the rib. Paired smoothing makes that the tessellation's own frame.
- _Should the cross-stroke coordinate be normalized at all?_ — Both. `v` and
  `d` come from the same two varyings; the shade picks per pattern.
- _Is `uv.x` allowed to stay affine?_ — Yes. Arc length stays arc length; only
  `v` takes the divide.
- _Where does the width-vs-radius constraint belong?_ — B2, as an opt-in `Line`
  transformation, with B1 as the alternative treatment. Narrow **or** clamp is
  the caller's choice, not the library's.

Carried to the API-surface review, not to this plan:

- **A `subdivide` / `resample` counterpart to `cleanup`.** Rejected as an A fix
  (A1), but it has uses of its own — animation along the path, per-vertex data
  variation — and the study already hand-rolls one via `flatMapWithNeighbours`.
- **`splitAtAngle`'s threshold naming**, as recorded under section 1.
