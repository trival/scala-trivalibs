# trivalibs painter — comprehensive API documentation & sketch-authoring skill

## Implementation status (2026-05-26) — DONE (Phases 1 & 2); Phase 3 deferred

**Phase 1 — complete.**
- Scaladoc doc-comments across the public surface: `painter/` (factories,
  `Panel`/`Layer`/`Shape`/`Form`/`Instance`/`Bindable`/`BindPair`, enums),
  `shader/`+`dsl/` (`Program` class doc; rest already documented), `buffers/`
  (`BufferBinding`, `allocateAttribs`, `AttribLayout`, `UniformValue`/`Layout`),
  `math/gpu/` (Expr model, op families, constructors, control flow, sampling,
  `Texture2D`/`Sampler`, int/uint), `math/cpu/` (`Vec2-4`/`Mat2-4`/`Quat` +
  three-representation model).
- Manuals under `docs/guide/`: `sketch-authoring-guide.md`,
  `shader-dsl-guide.md`, `gotchas.md`.
- Skill: `docs/skills/write-sketch/SKILL.md` (git-visible; consumption method
  still deferred — see below).
- Tooling: `bun run docs` → `docs/api/html/` (gitignored); CI workflow
  `.github/workflows/deploy-docs.yml` (Scaladoc → GitHub Pages); doc conventions
  + `docs/` vs `documents/` rule in `trivalibs/CLAUDE.md`; READMEs (root +
  trivalibs) with setup + Metals MCP enablement; `.mcp.json` gitignored.
- Metals MCP enabled (`.vscode/settings.json`) and validated end-to-end
  (`get-docs` returns written prose).

**Beyond-plan hardening done in Phase 1:**
- **Encapsulation**: `Panel`/`Layer`/`Shape`/`Form`/`Shade` constructors +
  render-state fields are `private[painter]`; public surface is the idiomatic
  path (`set`/`bind`/`instances`/`binding`/factories). A few members stay public
  by necessity (inline `bind`/`shade` machinery) — recorded in the memory
  `painter-encapsulation-inline-constraint`.
- **`TextureFormat` opaque-type enum** replaces raw format strings (enforced at
  compile time).

**Phase 2 — done (geometry-focused).**
- `geometry/` doc-strings: `Mesh` (+ optional-normal `addFace`, `map`/`flatMap`),
  `toBufferedGeometry`, `MeshBufferType`, `BufferedGeometry`, `WithNormal`,
  `Triangle`/`Quad`, `sphereMesh`, `Grid`, `Position`/`Lerp`/`Plane` (`Box`
  already documented).
- Guide + skill rewritten to promote **`Mesh` + `toBufferedGeometry`** as the
  default 3D-geometry path; `allocateAttribs` demoted to the simple-primitive
  fallback.
- `utils/`: `animate`, `NumExt` documented (`js`/`bufferdata` were already good).
  `scene/` and `dev/` were already well-documented.

**Phase 3 — deferred**: `preact` (type-safe Preact bindings, signals, HTML DSL)
docs + examples. No interactive-UI sketches planned yet; pick up when that work
starts.

**Open follow-ups (not blockers):**
- Skill **consumption method** still to be decided (symlink / copy / CLAUDE.md
  pointer) so the harness loads `docs/skills/write-sketch/`.
- Optional **Markdown API generator** (`docs/api/markdown/`) — prototype + decide
  (MCP-driven vs TASTY vs HTML→Markdown).
- Migrate other consumable docs (e.g. `rust-painter/scala-port-comparison.md`)
  from `documents/` into `docs/` over time; consider renaming `documents/`.

## Context

The painter library is mostly feature-complete and we're about to write many
sketches. Writing the bloom sketch required reading large amounts of library
source (render internals in `painter.scala`, the op catalogs in
`math/gpu/*.scala`, `panel.scala`/`layer.scala`, binding mechanics) because the
public API is unevenly documented. We want a documentation layer that lets a
human **and** an AI agent author sketches without reading the whole source.

Findings from an API/doc audit:

- **Surface is large**: ~70 painter/shader public items, 200+ GPU DSL ops, full
  CPU Vec/Mat/Quat op sets, plus geometry/scene/utils.
- **Coverage is uneven**: scene/ and dev/ are good; CPU math has **no**
  doc-comments; the GPU op catalog and the painter binding mechanics (BindPair
  `:=`, `Panel`/`Layer` mip semantics, the naming-tuple schema rules) are sparse
  to none — precisely what slowed the bloom port.
- **Existing conventions**: curated Markdown under `trivalibs/documents/`
  (`done/`, `scala-reference/`, `rust-painter/`); `examples/` as canonical code;
  no Scaladoc pipeline; no skills dir.
- **Tooling present**: `scala-cli doc` (Scaladoc generator) works (scala-cli
  1.13). **Metals server 1.6.7** is active (VS Code extension 1.66.0; `.metals/`
  - Bloop). Its MCP server is **validated working** — see below.

## The key tooling insight: one source, three consumers

Scaladoc doc-comments (`/** … */`) on public defs are read by **three**
channels, which is why doc-strings are worth investing in even though we also
want manuals:

1. **Metals hover** — the human sees them on hover in the IDE.
2. **Metals MCP server** — an AI agent can query them live during a session.
   This is the "query Metals for reference while developing" path: instead of
   grepping source, the agent asks Metals for the signature+doc of
   `Painter.layer`, `Vec3`, `FloatExpr.smoothstep`, etc.
   - **VALIDATED (2026-05-26)**: enabled via `"metals.startMcpServer": true` +
     `"metals.mcpClient": "claude"` in `.vscode/settings.json`. Metals
     **hot-applied** it (no window reload needed), wrote `.mcp.json` at repo
     root, and listens on a dynamic port. Transport is **streamable HTTP** (not
     SSE): `.mcp.json` =
     `{"mcpServers":{"metals":{"url":"http://localhost:<port>/mcp","type":"http"}}}`.
     A raw MCP `initialize` + `tools/list` + `inspect` handshake succeeded
     (`serverInfo: graphics-metals 1.6.7`);
     `inspect trivalibs.graphics.painter.Painter` returned full method
     signatures (overloads, defaults, type params).
   - **17 tools exposed.** Reference-relevant: **`get-docs`** (ScalaDoc comments
     - params + return + examples), **`inspect`** (members / constructors /
       overload signatures by FQCN), **`get-source`** (full source file, method
       bodies stripped to `???` to save tokens — read library API without
       bodies), **`glob-search`** / **`typed-glob-search`** (find symbols by
       name/kind), **`get-usages`**. Plus `compile-file`/`-module`/`-full`,
       `test`, `find-dep`, `list-modules`, `import-build`, `format-file`, and
       three scalafix tools.
   - **Important**: signatures come from the compiler, so `inspect`/`get-source`
     work **even without doc-strings** — the agent already has a
     no-source-reading path today. Doc-strings upgrade `get-docs`/hover from
     empty to explanatory. This is the core argument for Scaladoc-first.
   - **Session caveat**: Claude Code reads `.mcp.json` at session start and
     prompts to approve the `metals` server. A session started _before_
     `.mcp.json` existed won't have the tools — start a fresh session to use
     them.
3. **`scala-cli doc`** — generate a browsable Scaladoc HTML site on demand.

Consequence: **doc-strings are high-leverage** (serve IDE + agent + site), but
they only carry per-symbol reference. Conceptual/task knowledge ("how do panels,
layers and mips compose", "how to read an external panel while writing a mip")
lives better in **manuals + the skill**. Metals/LSP cannot supply that narrative
layer — so we need both.

## Documentation medium — options (all documented, then a recommendation)

**Option A — Manuals + skill primary; doc-strings only on gotchas.**

- Pros: fastest path to "author without reading source"; least surface to write.
- Cons: weak IDE hover and weak Metals-MCP answers (most symbols return bare
  signatures with no prose); reference and manuals drift independently.

**Option B — Full Scaladoc on every public def + manuals (both complete).**

- Pros: richest hover + MCP + Scaladoc site; reference is authoritative and
  co-located with code; manuals stay conceptual and short.
- Cons: highest authoring cost up front. Mitigated by the fact that math ops are
  stable and rarely change (low maintenance), and comments have **zero JS bundle
  cost** (stripped before bundling), so the usual bundle-size discipline doesn't
  apply to doc-comments.

**Option C — Scaladoc-first, derive the reference manual from it.**

- Pros: single source of truth; no manual/reference drift for per-symbol facts.
- Cons: needs a doc-extraction/assembly step we'd have to build (no pipeline
  today); the `scala-cli doc` HTML site already covers "browsable reference", so
  a derived Markdown reference is largely redundant with it.

**DECIDED — a hybrid of B, leaning Scaladoc-first.**

- Scaladoc doc-comments on **all** Phase-1 public APIs (painter + shader DSL +
  CPU/GPU math). Math ops are stable → document them now; low maintenance.
- `scala-cli doc` is the generated **reference** (no hand-written per-symbol
  reference Markdown to maintain).
- Hand-written **manuals** cover only what Scaladoc/LSP can't: concepts, task
  recipes, the render/mip model, and the gotchas.
- **Metals MCP** is the live agent query layer (reads the same doc-strings).
- A **skill** ties it together with a procedure + template + pointers.

## Deliverables

> **`docs/` vs `documents/` — placement rule.** All **consumable** documentation
> (the public reference for *using* the lib: guides, generated API, skills) lives
> under `trivalibs/docs/`. `trivalibs/documents/` is reserved for **internal**
> feature-planning / contribution docs (writing/extending the lib) — including
> this plan. Existing consumable material currently under `documents/` (e.g.
> `rust-painter/scala-port-comparison.md`) should migrate to `docs/` over time.
> **Consideration**: rename `documents/` to something that states intent better
> (e.g. `dev-docs/`, `design/`, `contributing/`) — deferred, not part of Phase 1.

### 1. Scaladoc doc-comments (Phase 1 scope: painter + shader DSL + all math)

Add `/** … */` to public APIs, prioritised by what a sketch author touches. Each
entry: one-line purpose, params, and any non-obvious behaviour / gotcha.

- **painter/**: `Painter` factories (`shade`/`layerShade` overloads, `form`,
  `shape`, `panel`, `layer`, `binding`, `sampler*`, `paint`/`paintAndShow`/
  `show`/`draw`, `init`, `onResize`, `input`); `Panel` (esp. `mipLevels`/`mips`,
  `format(s)`, `binding(index/mipLevel/depth)`, `set`, the `bind` overloads);
  `Layer` (`blendState`, `mipSource`/`mipTarget` + the auto-mipmap-skip
  interaction); `Shape`, `Form`, `Instance`, `Bindable`/`BindPair` (`:=`),
  `PanelBinding`; `enums.scala` (`BlendState` presets, `CullMode`, `FilterMode`,
  `PrimitiveTopology`, `FrontFace`).
- **shader/ + shader/dsl/**: `shade`/`layerShade` entry semantics, `Program`/
  `LayerProgram` (`vert`/`frag`, typed-locals form), `ctx` accessors
  (`in`/`out`/`bindings`/`textures`/`locals`), marker types (`VertexUniform`/
  `FragmentUniform`/`SharedUniform`, `FragmentPanel`/…, `Sampler`, `Texture2D`),
  `FragOut`, builtins, `WgslFn.raw`/`.dsl`/`.fromSrc`.
- **buffers/**: `BufferBinding` (`set`/`:=`/`update`/`get`, the factory
  overloads), `allocateAttribs`, `AttribLayout` (note the named-tuple schema
  rule: field order = layout index, field name = WGSL name).
- **math/gpu/**: per-type op catalogs — `FloatExpr`, `Vec2/3/4Expr`, `Mat*Expr`,
  int/uint variants: arithmetic,
  `.sin/.cos/.pow/.sqrt/.length/.dot/.normalize/ .clamp/.clamp01/.mix/.smoothstep/.step/.fract/.fit0111/.fit1101`,
  swizzles, constructors (`vec2/3/4`), comparisons → `BoolExpr`,
  `select/when/ifElse/ ifChain`, `Block`, `Let*/Var*/Const*`, texture
  `.sample`/`.sampleLevel`. Document the **Double-on-the-left** operator gotcha
  at the type level.
- **math/cpu/**: `Vec2-4`, `Mat2-4`, `Quat` ops + the three-representation model
  (mutable class / immutable tuple / buffer type) and the conversion givens.

Group-level doc-comments (one good comment per op family) are acceptable where
per-method comments would be noise, as long as Metals hover still shows
something useful for the family's representative methods.

### 2. Manuals (Markdown, under `trivalibs/docs/guide/`)

Conceptual + task-first; link into source and into the Scaladoc site. Keep these
small and stable:

- `sketch-authoring-guide.md` — end-to-end: project/entry (`@main`,
  `Painter.init`, `animate`, `onResize`), the shade→form→shape→panel→paint flow,
  bindings (`binding`, `:=`, instances), panels vs layers vs shapes, the
  **render & mip model** (paint order, ping-pong, mip-target layers, the
  auto-mipmap-skip gate), `draw` shortcut. This is the spine.
- `shader-dsl-guide.md` — writing `shade`/`layerShade`, ctx, locals, control
  flow, `WgslFn`, the naming-tuple schema + visibility wrappers, with worked
  snippets. Complements the per-op Scaladoc/MCP reference (which carries the
  exhaustive op list).
- `gotchas.md` — the cross-cutting traps (Double-on-left, named-tuple/match-type
  rule, `Dict.at`/`Arr` arity, F32-vs-Double buffers, mip semantics, bind slot-0
  auto-injection vs manual external-panel binding).
- Update `rust-painter/scala-port-comparison.md` cross-links to point at the new
  guide instead of source where appropriate (it already is a strong API map).

### 2b. Project README setup instructions

Add comprehensive, first-run setup instructions to the project `README.md` (root
`README.md`, with the lib-specific parts in `trivalibs/README.md`). This is part
of the doc endeavor and is **required**, not optional, because the
machine-specific tooling config is **gitignored**: `.vscode/` (the Metals MCP
settings), `.claude/`, and now `.mcp.json` are all in `.gitignore`, so nothing
about the dev/agent setup is shared via the repo. A new contributor (or a fresh
agent session on another machine) gets none of it unless the README spells it
out. Cover:

- **Prerequisites & build**: `bun install`, the `sketch` / `sketch:watch` /
  `dev` / `build` scripts, the new `docs` script, scala-cli/Metals versions.
- **IDE / Metals**: how to enable the Metals MCP server locally —
  `"metals.startMcpServer": true` + `"metals.mcpClient": "claude"` in
  `.vscode/settings.json` (gitignored, so each dev sets it) — what `.mcp.json`
  is, that it's gitignored and auto-regenerated per machine on a dynamic port.
- **AI-agent workflow**: that a fresh Claude Code session picks up `.mcp.json`
  and prompts to approve the `metals` server; how to query reference via
  `get-docs` / `inspect` / `get-source` / `glob-search`; pointer to the
  authoring guide and the `write-sketch` skill.
- **Scaladoc site**: how to generate/browse it (`bun run docs`).
- **Writing a sketch**: link the authoring guide + `sketches/base-triangle/`
  template.

### 3. AI skill

`trivalibs/docs/skills/write-sketch/SKILL.md` — compact procedure for authoring
a sketch: the canonical template (from `sketches/base-triangle/`), the
shade→panel→paint recipe, when to use `layerShade` vs `shade`, the gotchas, and
**pointers** ("for the exhaustive op list, query Metals `get-docs`/`inspect`, or
read the Scaladoc site / guide"). Keep it lean — it routes to the manuals/LSP
rather than duplicating them.

- **Location DECIDED: `trivalibs/docs/skills/` (git-visible).** Authored there
  so it's committed and shared with the lib, _not_ in `.claude/` (which is
  gitignored). This is authoring location, not necessarily where the harness
  loads it from.
- **Consumption DEFERRED until skills exist.** Once written, evaluate how to
  make it active in a session — symlink
  `/.claude/skills/write-sketch -> trivalibs/docs/skills/write-sketch`, copy on
  setup (README step), a `CLAUDE.md` reference/pointer, or `~/.claude/skills/`.
  Pick after we see the skill in use; the README will document whichever method
  we land on.

### 4. Tooling & conventions

- Add a `docs` script (e.g. `bun run docs`) wrapping
  `scala-cli doc src project.scala -o docs/api/html -f` (exclude examples/tests
  like the other scripts). Two output formats, two audiences:

- **HTML Scaladoc site (rich human browsing) — DECIDED: deploy, don't commit.**
  `scala-cli doc` emits **HTML only**, which is not readable by editors/humans
  as source and would add large diffs, so the generated HTML is **never
  committed**: it's written to `trivalibs/docs/api/html/`, which is gitignored.
  A **GitHub Action** runs `scala-cli doc` and **deploys to GitHub Pages** on
  the trivalibs repo. Link the Pages URL from both READMEs.

- **Markdown API reference (committed to `trivalibs/docs/api/markdown/`; AI +
  GitHub-readable) — optional, needs a custom generator.** Scaladoc has no
  Markdown output, so this requires a script. Value over the HTML site:
  grep-pable by agents _without_ Metals MCP (fresh clone, CI, parent-repo
  sessions), renders natively on GitHub, and surfaces public-API changes in PR
  diffs (API-change tripwire). Three build options (**practicability to be
  validated** with a prototype before choosing):
  1. **Metals-MCP-driven** (reuses the validated server): a `bun` script
     recursively `inspect`s packages from `trivalibs.graphics.*`, then
     `get-docs`/`inspect` per symbol → Markdown. Output equals what an AI sees
     via MCP. Main work: the package-traversal driver + a running Metals.
  2. **TASTY/Scalameta extractor**: a Scala script over `scala.tasty.inspector`
     walking public signatures + ScalaDoc. Standalone (no running Metals), more
     code; existing tools (e.g. `tastydoc`) are unmaintained.
  3. **HTML→Markdown conversion**: reuse the Scaladoc HTML we already generate
     and convert it (e.g. `pandoc`, or an HTML parser script). Pro: no
     signature/docstring extraction logic — Scaladoc already did it; cheapest to
     stand up. Con: Scaladoc HTML is a JS-driven multi-page site with nav/search
     chrome, so converted output needs cleanup/post-processing to read as a
     clean API reference; per-page mapping to a sane Markdown tree is the real
     work.

  Note this format is **redundant with Metals MCP** when the agent _has_ MCP —
  so it's mainly insurance for non-MCP contexts + human GitHub browsing. Decide
  whether that's worth the generator after prototyping; options 1 and 3 are the
  lower-effort starts (1 = structured data, no HTML cleanup; 3 = reuses existing
  HTML, needs cleanup).

- **Status**: HTML→Pages decided. The Markdown generator stays an **optional,
  useful follow-up** — prototype the candidate methods (MCP-driven, TASTY,
  HTML→Markdown) against one well-documented package and compare output quality
  - practicability before committing to one and maintaining it.
- Metals MCP is **already enabled** (`.vscode/settings.json`:
  `metals.startMcpServer` + `mcpClient: "claude"`); Metals auto-writes the
  endpoint to `.mcp.json` (gitignored, dynamic port). Document the agent
  workflow ("query Metals via `get-docs`/`inspect`/`get-source` for signatures +
  docstrings") in the skill, the README, and `trivalibs/CLAUDE.md`.
- Add a short **doc-comment convention** to `trivalibs/CLAUDE.md`: Scaladoc
  `/** */` on public APIs, one-line purpose + gotcha, link style, and the note
  that comments are bundle-free so the size rules don't apply to them.

## Phasing

- **Phase 1 (this effort)**: painter + shader DSL + **all** graphics math (GPU
  _and_ CPU — they're critical and highly integrated for 3D scenes) +
  buffers/bindings. Manuals (3 files), the skill, and the tooling wiring.
- **Phase 2 (later)**: geometry, scene (camera/transform/controllers already
  good), utils, dev — add doc-strings + extend the guide.
- **Phase 3 (deferred)**: `preact` (type-safe Preact bindings, signals, HTML
  DSL) — docs + examples. Deferred because foreseeable sketches focus on painter
  WebGPU canvases only; no interactive UI is planned for now. Pick this up when
  we actually start building interactive UI on top of the canvases.

## Critical files

- Doc-strings (Phase 1): `trivalibs/src/graphics/painter/*.scala`,
  `trivalibs/src/graphics/shader/**/*.scala`,
  `trivalibs/src/graphics/math/gpu/*.scala`,
  `trivalibs/src/graphics/math/cpu/*.scala`,
  `trivalibs/src/graphics/buffers/*.scala`.
- Manuals:
  `trivalibs/docs/guide/{sketch-authoring-guide,shader-dsl-guide,gotchas}.md`.
- README setup instructions: root `README.md` (+ `trivalibs/README.md`).
- Skill: `trivalibs/docs/skills/write-sketch/SKILL.md` (git-visible; consumption
  method deferred).
- Docs dir layout under the visible/committed `trivalibs/docs/api/`:
  - `trivalibs/docs/api/html/` — generated Scaladoc HTML, **gitignored**.
  - `trivalibs/docs/api/markdown/` — optional committed Markdown API reference
    (later, via the generator).
- Tooling: `package.json` (`docs` script), `.gitignore` (`.mcp.json` ignored —
  done), `trivalibs/CLAUDE.md` (conventions), `.vscode/settings.json` (Metals
  MCP — done, but gitignored so README must document it).
- Scaladoc HTML: **not committed** (gitignored output dir in trivalibs) — a
  GitHub Action builds `scala-cli doc` and deploys to GitHub Pages.
- Optional Markdown API generator (follow-up): a `bun` script driving Metals MCP
  `inspect`/`get-docs` over `trivalibs.graphics.*`.
- Reference for canonical patterns: `trivalibs/examples/*` (already exist).

## Verification

1. **Builds**: `scala-cli doc src project.scala` (via the new `bun run docs`)
   completes without errors and produces the HTML site; `bun run examples:build`
   and `bun run sketch …` still compile (doc-comments don't affect output).
2. **Metals hover**: spot-check that hovering `Painter.layer`, `Vec3.normalize`,
   `FloatExpr.smoothstep` shows the new docstrings.
3. **Metals MCP**: DONE — server validated (handshake + `inspect` on `Painter`).
   After writing doc-strings, confirm `get-docs` on a documented symbol returns
   the new prose (not just the signature `inspect` already gives).
4. **Skill loads**: `/write-sketch` (or the skill) is discoverable in a graphics
   repo session.
5. **Cold-start test (the real bar)**: in a fresh session, author a small new
   sketch (e.g. a single full-screen `layerShade` effect with one uniform and a
   sampled panel) using **only** the skill + manuals + Metals MCP, without
   opening library source. If it compiles and renders, the docs achieve the
   goal.
