# Porting the WebGPU library into trivalibs

Status: **largely executed** (branch `trivalibs-port`). The code move, package
rename, examples + tests relocation, and standalone-workspace setup are all done
and building green. Remaining work is docs + the consumer scaffold — see
[Implementation status](#implementation-status) below.

Target: move the painter / math / shader-DSL / geometry / scene / webgpu-facade
code from this repo into the `trivalibs` submodule so other projects can depend
on it the same way they already depend on `trivalibs/utils` and
`trivalibs/preact`.

## Implementation status

As of 2026-05-14. Branch `trivalibs-port` in both this repo and the trivalibs
submodule.

**Done:**

- §1–§3 — All `src/graphics/*` dirs relocated to `trivalibs/src/graphics/*`,
  packages renamed to `trivalibs.graphics.*`, imports rewritten. `animate.scala`
  → `trivalibs/src/utils/animate.scala`. The webgpu facade landed at
  `trivalibs/src/graphics/painter/webgpu.scala` with
  `package trivalibs.graphics.painter` as planned. (trivalibs commit `a982dc6`,
  playground `02e6c33`)
- §1/§3 — `examples/` (17 examples) and `test/` moved into `trivalibs/`. Both
  `test-setup.scala` files deleted; no per-folder setup files remain. (trivalibs
  `664e849`)
- §4 — `trivalibs/project.scala` added as the standalone-workspace single source
  of truth. Playground `project.scala` excludes `trivalibs/project.scala`,
  `trivalibs/test/**`, `trivalibs/examples/**`.
- §4 — `trivalibs/package.json` added. Note: scripts pass `src`, `examples`/
  `test`, and `project.scala` explicitly as invocation inputs (not bare folder
  roots) — needed so scala-cli picks up directives and doesn't hang. `serve.ts`
  (minimal Bun static server) added.
- §6 — bundle-size optimization pass done (playground `c62aaf3`, trivalibs
  `7e3f2cb`); `paintAndShow` helper added (`b805cc7`).
- Playground stripped to a pure consumer (`a091325`).

**Divergence from plan — playground sketch system:**

Instead of keeping `serve_custom.ts` + an `examples/`-as-sketches dir, the
playground now has a dedicated `sketches/` tree with per-sketch isolated builds
driven by `scripts/sketch.ts` (`bun run sketch` / `sketch:watch`).
`serve_custom.ts` is gone; `dev` runs `serve_custom.ts`'s replacement. This
supersedes §1's "examples + dev server split" and §7's `serve_custom.ts`
guidance for the playground side.

**Docs — done:**

- §3 step 7 — `trivalibs/README.md` rewritten: goal statement for an integrated
  interactive-graphics web library, Painter + Preact API summaries with code
  excerpts, updated `graphics/` + `preact/` + `utils/` + `examples/` contents,
  and the `package.json` / `serve.ts` script workflow. Playground `README.md`
  was already current (sketches workflow).
- §3 step 8 — `CLAUDE.md` — playground `CLAUDE.md` rewritten as a
  trivalibs-consumer guide (sketches workflow, `trivalibs.*` namespace, points
  lib work at the submodule). New `trivalibs/CLAUDE.md` added: the full lib-dev +
  bundle-size-optimization reference, with architecture / source layout for the
  relocated `trivalibs/src/graphics/...` tree.

With the docs done, the only open item is moving this plan doc to
`documents/done/` (manual) — the feature branch is otherwise complete.

**Skipped / deferred:**

- §8 step 6 — `trivalibs/examples/_starter/` scaffold dropped. The playground's
  `sketches/base-triangle` already serves as the starter template for new
  consumer projects.
- §8 step 8 — moving this plan doc to `documents/done/` will be done manually.
- §6 — incremental-rebuild measurement + Option 3/4 evaluation deferred to a
  future feature branch. The recent bundle-size optimizations also shortened
  build times considerably, so the 4 s figure should be re-measured fresh before
  deciding on the JAR-publish route.

## 1. Scope of the move

Source dirs to relocate (current → new):

- `src/graphics/math/` → `trivalibs/src/graphics/math/`
- `src/graphics/shader/` → `trivalibs/src/graphics/shader/`
- `src/graphics/buffers/` → `trivalibs/src/graphics/buffers/`
- `src/graphics/geometry/` → `trivalibs/src/graphics/geometry/`
- `src/graphics/scene/` → `trivalibs/src/graphics/scene/`
- `src/graphics/painter/` → `trivalibs/src/graphics/painter/`
- `src/graphics/utils/animate.scala` → `trivalibs/src/utils/animate.scala`
  (rendering-agnostic — wraps `requestAnimationFrame` lifecycle, belongs with
  the other generic JS helpers in `trivalibs.utils`)
- `src/webgpu/facades.scala` → `trivalibs/src/graphics/painter/webgpu.scala`
  (the facades are tailored to the painter's needs, not a general WebGPU
  binding; co-locating them with painter makes that explicit and avoids the
  illusion of a reusable generic facade layer)
- `examples/` → `trivalibs/examples/` (they serve as idiomatic-usage docs, one
  per feature, and should ship with the library)
- `test/` → merge into `trivalibs/test/` (current `test/geometry/` and
  `test/shader/` cover trivalibs code, not playground code; they belong with the
  library they test)

Stays in this repo:

- `serve_custom.ts` (the rebuild-aware dev server, tuned for this playground),
  `project.scala`, `package.json`, `documents/`, `memory/`.
- After the move, this repo becomes a _consumer_ of trivalibs — same role as any
  future downstream project. It is the sketch / experiment playground; once an
  experiment matures into a feature-documenting example, it migrates over to
  `trivalibs/examples/`.

### Examples + dev server split

- `trivalibs/examples/` ships with the library and uses a **minimal Bun dev
  server** (`trivalibs/serve.ts`, plain static file server + scala-cli watch in
  parallel — no custom rebuild hooks). Goal: smallest possible footprint so
  downstream consumers can copy the pattern.
- This repo keeps `serve_custom.ts` (and `serve_bun.ts` if still needed for
  comparison) — the custom server is optimized to pick up scala-cli's
  incremental rebuild artifacts and trigger reloads, which is overkill for the
  examples set but valuable when iterating on sketches here.
- `trivalibs/package.json` only needs `@webgpu/types` + `@types/bun` as dev
  deps. No preact/runtime deps — preact is loaded via ESM CDN inside example
  HTML, matching current practice.

## 2. Package rename

All moved files currently use unprefixed packages (`graphics.*`, `webgpu`).
Rename to fit the `trivalibs.*` namespace:

- `graphics.math` → `trivalibs.graphics.math`
- `graphics.math.cpu` → `trivalibs.graphics.math.cpu`
- `graphics.math.gpu` → `trivalibs.graphics.math.gpu`
- `graphics.shader` → `trivalibs.graphics.shader`
- `graphics.shader.dsl` → `trivalibs.graphics.shader.dsl`
- `graphics.shader.lib.*` → `trivalibs.graphics.shader.lib.*`
- `graphics.buffers` → `trivalibs.graphics.buffers`
- `graphics.geometry` → `trivalibs.graphics.geometry`
- `graphics.scene` → `trivalibs.graphics.scene`
- `graphics.painter` → `trivalibs.graphics.painter`
- `graphics.utils.animation` → `trivalibs.utils.animation`
- `webgpu` → `trivalibs.graphics.painter` (merge facade types into the painter
  package; they aren't a standalone module)

Why keep the `graphics.` segment instead of flattening to `trivalibs.painter`,
`trivalibs.shader`, `trivalibs.math`: the whole rendering stack is one coherent
group; a top-level `trivalibs.graphics` umbrella keeps it discoverable and
leaves space for non-graphics additions (`trivalibs.preact`, `trivalibs.utils`,
`trivalibs.bufferdata`).

The old bare-`webgpu` package gets folded into `trivalibs.graphics.painter`. The
facades aren't a generic WebGPU binding — every type and method in there exists
to serve the painter's needs. Keeping them as a sibling package would falsely
advertise a reusable facade layer; making them painter-internal is honest and
lets us evolve them freely.

## 3. Mechanical migration steps

Do this in a single PR — the package rename is purely textual and the build is
all-or-nothing.

1. `git mv` the directories listed in §1.
2. Rewrite `package` declarations in every moved file (sed by package, ~55
   files).
3. Rewrite imports across the whole repo:
   - `import graphics.` → `import trivalibs.graphics.`
   - `import webgpu.` → `import trivalibs.graphics.painter.`
   - `import graphics.utils.animation.` → `import trivalibs.utils.animation.`
   - Also fix wildcard `import graphics.*` etc. in examples and tests.
4. Move `examples/` → `trivalibs/examples/`. Add `trivalibs/serve.ts` (minimal
   static Bun server) and `trivalibs/package.json` with build/dev scripts
   mirroring this repo's but pointing at the new locations.
5. Merge `test/geometry/` and `test/shader/` into `trivalibs/test/`. Delete both
   `test-setup.scala` files (this repo's and trivalibs's existing one) — their
   directives now live in the new `trivalibs/project.scala` (§4). Delete the
   empty `test/` from this repo. Same goes for any inline `using` directives
   that might exist in `trivalibs/examples/` — none of the example folders need
   their own setup file anymore.
6. Update this repo's `project.scala`: keep `using exclude trivalibs/test/**`
   and also exclude `trivalibs/examples/**` so the playground build doesn't pull
   example sources in.
7. Update `trivalibs/README.md` Contents section to list `graphics/`, `webgpu/`,
   and `examples/`. Document the minimal `serve.ts` dev workflow.
8. Update this repo's `CLAUDE.md` Source Layout — point at
   `trivalibs/src/graphics/...`, drop references to local `src/graphics/...` and
   the example list (examples now live in trivalibs).
9. Run `bun run build` and `scala-cli test trivalibs/test` (in trivalibs) and
   `bun run build` in this repo; rebuild every example in
   `trivalibs/examples/*/index.html` — examples never get deleted per project
   convention, so every one must still compile.

Risk surface is low: no runtime behavior changes, just package paths.

## 4. trivalibs as a standalone buildable unit

**IDE constraint:** Metals gets confused when two `project.scala` files coexist
in the same workspace, and scala-cli is deliberately not a multi-project build
tool. The existing convention already works around this: nested setups
(`test/test-setup.scala`, `trivalibs/test/test-setup.scala`) live in folders
that the outer build excludes via `using exclude …/test/**`. The IDE only sees
one set of directives at a time because only the outer build picks up the outer
`project.scala`.

The library needs to be openable as a **standalone workspace** in the IDE (when
working only on trivalibs, or in its own repo after a future split). That means
trivalibs needs a setup file at its root carrying all the `using` directives.
scala-cli reads directives from any `.scala` file in the project, so the
filename isn't strictly mandatory — but `project.scala` is the canonical
convention, clearest signal of intent, and what most tooling docs assume. We use
it. So:

- `trivalibs/project.scala` is the canonical Metals entry point. It covers
  **everything** in trivalibs — lib + tests + examples — as a single scala-cli
  project. Opening `trivalibs/` directly in VS Code / Metals gives a fully
  type-checked view of the whole library and its examples.
- The outer playground `project.scala` excludes `trivalibs/project.scala` by
  filename plus the test and examples folders, so Metals (when the playground is
  the workspace root) only loads one config:

  ```scala
  //> using exclude trivalibs/project.scala
  //> using exclude trivalibs/test/**
  //> using exclude trivalibs/examples/**
  ```

- Inside trivalibs, the test and examples folders no longer need their own setup
  files when Metals is loading them as part of `trivalibs/project.scala`. But
  scala-cli still needs them as **invocation roots** for separate build outputs:
  - Tests: `cd trivalibs && scala-cli test test` — scala-cli auto-detects the
    `.test.scala` files under `test/`. The directives from `project.scala`
    apply, no separate test setup needed.
  - Examples:
    `cd trivalibs && scala-cli --power package examples --js -o examples/out -f -w`.
    scala-cli uses the root `project.scala` and treats `examples/` as the source
    set to bundle.
- A jar publish run is rare; extra publish-specific options can be passed on the
  command line:
  `cd trivalibs && scala-cli --power publish local src \   --organization me.trival --name trivalibs --project-version 0.1.0-SNAPSHOT`

`trivalibs/project.scala` — single source of truth for the trivalibs workspace:

```scala
//> using scala 3.8.4-RC2
//> using platform js
//> using option -Wconf:msg=differs.only.in.case:s

//> using jsVersion 1.21.0
//> using jsMode full
//> using jsModuleKind es
//> using jsEsVersionStr es2021
//> using jsModuleSplitStyleStr fewestmodules

//> using dep org.scala-js::scalajs-dom::2.8.1

//> using test.dep org.scalameta::munit::1.2.4
```

Trade-off: the lib-only build (e.g. `scala-cli compile src` from `trivalibs/`)
will pull in the example bundle settings too (jsMode full, fewestmodules).
That's fine for type-checking but a wasted setting for `publish local`. We
accept this minor over-specification rather than split into multiple setup files
and lose the single-workspace IDE experience.

### Dedicated `trivalibs/package.json`

Trivalibs gets its own `package.json` so all JS/Bun deps and build invocations
live with the library, not in the playground. Downstream consumers can use
trivalibs's scripts directly, or copy them as a starting point. Sketch:

Trivalibs has no JS entry points of its own — it is consumed only as Scala
sources (or, eventually, a Scala artifact) by other Scala.js projects. So there
is no `build`/`watch` that produces a `trivalibs.js` bundle. Instead the scripts
cover **lib type-checking, examples build, tests, publish**:

```json
{
	"name": "trivalibs",
	"type": "module",
	"scripts": {
		"check": "scala-cli compile src",
		"examples:build": "scala-cli --power package examples --js -o examples/out -f",
		"examples:watch": "scala-cli --power package examples --js -o examples/out -f -w",
		"examples:dev": "bun --bun serve.ts",
		"test": "scala-cli test test",
		"publish:local": "scala-cli --power publish local src --organization me.trival --name trivalibs --project-version 0.1.0-SNAPSHOT"
	},
	"devDependencies": {
		"@types/bun": "latest"
	},
	"dependencies": {
		"@webgpu/types": "^0.1.69"
	}
}
```

Notes:

- All scripts run from the trivalibs root and inherit directives from
  `project.scala`. No `cd` into nested folders, no per-folder setup files.
- `check` uses `scala-cli compile` (not `package`) — type-checks the library in
  isolation without producing a bundle. Useful for CI.
- `examples:watch` + `examples:dev` are meant to run side-by-side (two terminals
  or a process manager) when iterating on lib + examples together. The watch
  step rebuilds JS into `examples/out/` on every change, the dev server serves
  the static `examples/*/index.html` files alongside it.
- `publish:local` produces a Scala artifact in `~/.ivy2/local`. That is the only
  "build output" trivalibs has — everything else is sources consumed by
  downstream Scala.js builds (§5 Option 1) or by the local publish flow (§5
  Option 3).
- The playground repo's own `package.json` keeps its scripts pointing at the
  outer build; it does not depend on trivalibs's `package.json` at all. Same
  pattern as having two `node_modules` trees — each project is self-contained.
- `@webgpu/types` and `@types/bun` migrate from the playground's `package.json`
  to trivalibs's; the playground's `package.json` can keep them as well if any
  playground-only sketches still need them, but the authoritative copy lives
  with the lib.

## 5. Workflow options for downstream projects

The user explicitly wants to compare integration strategies. Four realistic
options, ordered from least to most upfront work:

### Option 1 — Submodule + recompile from source (status quo)

```bash
git submodule add git@github.com:trival/trivalibs.git trivalibs
```

In the consumer's `project.scala`:

```scala
//> using exclude trivalibs/test/**
//> using dep org.scala-js::scalajs-dom::2.8.1
```

Pros: zero packaging ceremony; consumer can edit trivalibs in place; matches how
the user works today. Cons: ~4 s rebuild penalty on every full compile (the
whole library re-compiles into the consumer's JS output).

**Use when:** actively co-evolving trivalibs and a consumer project. This is the
recommended default while trivalibs is pre-1.0.

### Option 2 — Submodule + scala-cli `using filethe`

Same as Option 1 but using `//> using file trivalibs/src` instead of the exclude
trick. Slightly more explicit; scala-cli treats trivalibs as a sibling module
rather than "all sources in the tree". Compile cost identical.

### Option 3 — Local published JAR (`publishLocal`)

scala-cli supports `scala-cli --power publish local trivalibs` which installs an
artifact into `~/.ivy2/local`. Consumers then use:

```scala
//> using dep me.trival::trivalibs::0.1.0-SNAPSHOT
```

Pros: skips recompilation; the JAR is consumed like any Maven dep, so
incremental builds drop the 4 s tax to whatever scala-cli's resolver + linker
overhead is (typically <1 s for an unchanged dep). Cons: every trivalibs change
requires `publish local` again; easy to forget and end up with stale artifacts;
needs a `publish.*` directive block in `trivalibs/project.scala`
(`organization`, `name`, `version`, `licenses`, `url`, `vcs`, `developers`,
`repository`).

**Use when:** trivalibs has stabilized enough that consumer-side edits to it are
rare.

### Option 4 — Published to a real repo (Maven Central / GitHub Packages)

End-state, post-beta. Same `using dep` line as Option 3 but resolvable without
local publish. scala-cli supports this directly via
`scala-cli --power publish trivalibs` once the publish directives and signing
keys are configured.

**Recommended path:** stay on **Option 1** until the API truly settles, then
jump straight to **Option 4**. Option 3 is a useful intermediate if the 4 s cost
starts hurting iteration but the API still churns weekly.

## 6. Addressing the 4-second compile

The 4 s is the **watch-mode incremental rebuild cost per file change**, not cold
start (cold start is longer). So `bun run watch` doesn't help; this is already
the warm path. Mostly Scala.js linking + Scala 3 macros (the shader DSL uses
inline + summonFrom heavily). Worth measuring before optimizing. Likely
contributors, in descending order:

1. **Scala.js linker** re-emitting ES modules for the full reachable graph.
   `fewestmodules` is the cheap option already; it still has to re-link the
   whole reachable set on every change. The linker is the prime suspect for the
   4 s figure since it always runs and scales with library surface.
2. **Macro expansion** in shader derivation (`ShaderDef`, `WGSLType` summons,
   `allocateAttribs`). Only re-runs for files that touch macro call sites, but
   those are common in examples.
3. **Scala 3 typer** on the math library (lots of given chains). Incremental
   compilation skips unchanged files, so this is mostly a cold-start cost.

Mitigations, in order of expected impact:

- **Precompile trivalibs to a JAR** (Option 3/4). Removes the lib from the
  per-change recompile path entirely. Cost (1) shrinks dramatically because the
  linker can treat the JAR as a stable input and only re-link the consumer's own
  changed code. Cost (3) goes away for lib sources. Cost (2) stays because
  macros expand at the _consumer_ call site regardless of whether the macro
  definition is in source or JAR form.
- **Switch to `smallestmodules` only when packaging for production**, leave
  watch builds in `fewestmodules` (already the case). No further tuning to do
  here.
- **Track Scala.js linker caching upstream.**
  [scala-js/scala-js#5352](https://github.com/scala-js/scala-js/pull/5352) adds
  incremental linking support, currently surfaced through sbt 2. Open question
  whether scala-cli's `package --js -w` mode picks it up automatically once it
  lands in a released Scala.js version, or whether scala-cli needs its own
  integration work. If/when this ships end-to-end, the 4 s figure could drop
  dramatically without any of the JAR-publish workarounds. Worth a separate
  spike before committing to Option 3/4.
- **Move heavy macros behind smaller surface APIs** so call sites trigger less
  work. Out of scope for this port; track separately.

Suggested action: do the port first (Option 1), then **measure the incremental
rebuild before and after `publish:local`** to confirm the JAR route actually
shrinks the 4 s. Decide on Option 3/4 based on data.

## 7. Consumer project template

Once the port lands, a fresh scala-cli project consuming trivalibs looks like:

```
my-graphics-sketch/
├── package.json        # bun scripts identical to this repo's
├── project.scala
├── src/main.scala
├── public/index.html
├── trivalibs/          # git submodule
└── out/                # scala-cli --js output
```

`project.scala`:

```scala
//> using scala 3.8.4-RC2
//> using platform js

//> using jsVersion 1.21.0
//> using jsMode full
//> using jsModuleKind es
//> using jsEsVersionStr es2021
//> using jsModuleSplitStyleStr fewestmodules

//> using exclude trivalibs/test/**
//> using exclude trivalibs/examples/**
//> using dep org.scala-js::scalajs-dom::2.8.1
```

Starter scaffold lives at `trivalibs/examples/_starter/` — `cp -r` it to seed a
new sketch. The minimal `trivalibs/serve.ts` is the reference dev server; copy
it if all you need is static serving + reload, or swap in this repo's
`serve_custom.ts` when you want the incremental-rebuild-aware variant.

This is identical to today's setup, only with `graphics.*` imports replaced by
`trivalibs.graphics.*`.

## 8. Migration order (recommended commit sequence)

1. `chore: rename packages graphics.* → trivalibs.graphics.*` — file moves +
   package rewrites + import rewrites in one atomic commit.
2. `chore: relocate webgpu facade to trivalibs.webgpu`.
3. `chore: move examples/ to trivalibs/examples/` — includes adding
   `trivalibs/serve.ts` (minimal Bun static server) and `trivalibs/package.json`
   with build / watch / dev scripts.
4. `chore: merge test/ into trivalibs/test/` — consolidate test suites, collapse
   the two `test-setup.scala` files into one.
5. `chore: add trivalibs/project.scala + trivalibs/package.json` for the
   standalone trivalibs workspace (§4); update outer `project.scala` to exclude
   the nested `project.scala` and the `test/` + `examples/` trees.
6. `chore: add trivalibs/examples/_starter/` scaffold for downstream consumers.
7. `docs: update CLAUDE.md and trivalibs README for new layout`.
8. `docs: add trivalibs-port notes to design docs/done/`.

Each commit independently leaves the build green.
