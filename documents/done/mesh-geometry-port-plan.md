# CPU Geometry Helpers — Index

This plan has been split into two independent feature documents:

- [documents/geometry3d-plan.md](/home/trival/code/personal/scala/webgpu/documents/geometry3d-plan.md)
  — **Grid, Cuboid, Quad factories, Sphere** — 3D geometry helpers + rendered
  scene example.
- [documents/line2d-plan.md](/home/trival/code/personal/scala/webgpu/documents/line2d-plan.md)
  — **2D Line geometry** — variable-width polyline builder, mitre joins, UV
  export + animated stroke example.

## Prerequisites (all done)

The prerequisites that were §2–§2.4 in the original document are fully
implemented:

| Done | What                                                                                    |
| ---- | --------------------------------------------------------------------------------------- |
| ✅   | Additional `Lerp` givens (`Double`, `Vec2`, `Vec4`, `Unit`) in `geometry/package.scala` |
| ✅   | `Mesh.addFaces` batch extension in `mesh.scala`                                         |
| ✅   | `object Mesh.apply(faces, normal, section)` factory in `mesh.scala`                     |
| ✅   | `IntExt[Int]` + standalone `Int.abs` / `.sign` in `trivalibs/utils/numbers.scala`       |
| ✅   | `Quad` corner reorder to `tl(0), bl(1), br(2), tr(3)` in `polygon.scala`                |

## Design principles

The Rust source is shaped by its constraints (no overloading, explicit trait
bounds, `Option` chains). Scala 3 replacements used throughout both plans:

| Rust pattern                              | Scala replacement                                  |
| ----------------------------------------- | -------------------------------------------------- |
| `CoordOpsFn` trait + 4 zero-sized structs | `opaque type CoordOps = Int` + `val` constants     |
| `Quad3D<P>` (separate type from `Quad`)   | Reuse `Quad[T]` opaque; add factories to companion |
| `{front,back,...}_face` + `_face_f` pairs | Six `*Face` methods with optional mapper           |
| `NeighbourList<T>` doubly-linked list     | Inlined `forEachWithNeighbours` in `line2d.scala`  |
| `LineData<EmptyData>` type alias          | `Line[Unit]`                                       |

## Explicit non-goals

- `rust/data/neighbour_list/` container — only the iterator combinators are
  needed, inlined.
- `rust/data/vertex_index.rs` — Scala already uses a stringified `posKey`.
- WebGL vertex layout (`webgl_buffered_geometry`) — replaced by `StructArray` +
  named tuples.
- `gpu_data!` / `bytemuck` derives — handled by `allocateAttribs`.

## Rust source reference

[documents/rust-painter/repomix-trivalibs-core.xml](/home/trival/code/personal/scala/webgpu/documents/rust-painter/repomix-trivalibs-core.xml)

- Lines 119–1755: `data/grid`, `data/neighbour_list`
- Lines 3136–3946: `rendering/line_2d`
- Lines 5307–5893: `rendering/mesh_geometry/utils`, `rendering/shapes/*`
