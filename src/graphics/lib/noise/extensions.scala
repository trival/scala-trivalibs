package trivalibs.graphics.lib.noise

// Shared noise extensions: one definition per name for CPU receivers
// (`Vec2`–`Vec4`) and GPU receivers (`Vec2Expr`–`Vec4Expr`). Scala forbids
// default arguments on more than one overloaded alternative, so each name is a
// single `transparent inline` def that branches on the receiver type at
// compile time. The CPU branch expands to the inline CPU object API — one
// direct kernel call in the linked JS; the GPU branch calls the GPU wrapper.
// The dimension comes from the receiver.

import scala.compiletime.erasedValue
import trivalibs.graphics.lib.*
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.Vec4
import trivalibs.graphics.math.gpu.FloatExpr
import trivalibs.graphics.math.gpu.Vec2Expr
import trivalibs.graphics.math.gpu.Vec3Expr
import trivalibs.graphics.math.gpu.Vec4Expr
import trivalibs.graphics.shader.lib.noise as gpu

// ---- simplex ----

extension [P <: Vec2 | Vec3 | Vec4 | Vec2Expr | Vec3Expr | Vec4Expr](p: P)

  /** Simplex noise value at `p`, in about `[-1, 1]`. An optional `seed`
    * selects an unrelated field.
    */
  transparent inline def simplexNoise(
      inline seed: FloatArg | Null = null,
  ): Double | FloatExpr =
    inline erasedValue[P] match
      case _: Vec2 => Simplex.noise2d(p.asInstanceOf[Vec2], cpuOptD(seed))
      case _: Vec3 => Simplex.noise3d(p.asInstanceOf[Vec3], cpuOptD(seed))
      case _: Vec4 => Simplex.noise4d(p.asInstanceOf[Vec4], cpuOptD(seed))
      case _: Vec2Expr => gpu.Simplex.noise2d(p.asInstanceOf[Vec2Expr], gpuOptF(seed))
      case _: Vec3Expr => gpu.Simplex.noise3d(p.asInstanceOf[Vec3Expr], gpuOptF(seed))
      case _: Vec4Expr => gpu.Simplex.noise4d(p.asInstanceOf[Vec4Expr], gpuOptF(seed))

  /** Simplex fbm: `octaves` layers at `lacunarity`× frequency and `gain`×
    * amplitude, normalized to strictly `[-1, 1]`.
    */
  transparent inline def simplexFbm(
      inline octaves: IntArg = 4,
      inline lacunarity: FloatArg = 2.0,
      inline gain: FloatArg = 0.5,
      inline seed: FloatArg | Null = null,
  ): Double | FloatExpr =
    inline erasedValue[P] match
      case _: Vec2 =>
        Simplex.fbm2d(p.asInstanceOf[Vec2], cpuI(octaves), cpuD(lacunarity), cpuD(gain), cpuOptD(seed))
      case _: Vec3 =>
        Simplex.fbm3d(p.asInstanceOf[Vec3], cpuI(octaves), cpuD(lacunarity), cpuD(gain), cpuOptD(seed))
      case _: Vec4 =>
        Simplex.fbm4d(p.asInstanceOf[Vec4], cpuI(octaves), cpuD(lacunarity), cpuD(gain), cpuOptD(seed))
      case _: Vec2Expr =>
        gpu.Simplex.fbm2d(p.asInstanceOf[Vec2Expr], gpuI(octaves), gpuF(lacunarity), gpuF(gain), gpuOptF(seed))
      case _: Vec3Expr =>
        gpu.Simplex.fbm3d(p.asInstanceOf[Vec3Expr], gpuI(octaves), gpuF(lacunarity), gpuF(gain), gpuOptF(seed))
      case _: Vec4Expr =>
        gpu.Simplex.fbm4d(p.asInstanceOf[Vec4Expr], gpuI(octaves), gpuF(lacunarity), gpuF(gain), gpuOptF(seed))

// ---- simplex torus, 2D only ----

extension [P <: Vec2 | Vec2Expr](p: P)
  /** Seamlessly tiling 2D noise (4D simplex on a torus), `p` in `[0, 1]` per
    * tile, `scale` the frequency.
    */
  transparent inline def simplexTorusNoise(
      inline scale: FloatArg,
      inline seed: FloatArg | Null = null,
  ): Double | FloatExpr =
    inline erasedValue[P] match
      case _: Vec2 => Simplex.torusNoise2d(p.asInstanceOf[Vec2], cpuD(scale), cpuOptD(seed))
      case _: Vec2Expr =>
        gpu.Simplex.torusNoise2d(p.asInstanceOf[Vec2Expr], gpuF(scale), gpuOptF(seed))

// ---- extended (psrdnoise) and worley, 2D / 3D ----

extension [P <: Vec2 | Vec3 | Vec2Expr | Vec3Expr](p: P)

  /** Extended noise, value + gradient: `(value, gradient…)` as `Vec3` (2D) /
    * `Vec4` (3D). Optional `tilingPeriod` (integer components ≤ 289, zero =
    * unwrapped), `rot` (turns) and `seed`.
    */
  transparent inline def extendedNoise(
      inline tilingPeriod: VecArg | Null = null,
      inline rot: FloatArg | Null = null,
      inline seed: FloatArg | Null = null,
  ): Vec3 | Vec4 | Vec3Expr | Vec4Expr =
    inline erasedValue[P] match
      case _: Vec2 =>
        Extended.noise2d(p.asInstanceOf[Vec2], cpuOptV2(tilingPeriod), cpuOptD(rot), cpuOptD(seed))
      case _: Vec3 =>
        Extended.noise3d(p.asInstanceOf[Vec3], cpuOptV3(tilingPeriod), cpuOptD(rot), cpuOptD(seed))
      case _: Vec2Expr =>
        gpu.Extended.noise2d(p.asInstanceOf[Vec2Expr], gpuOptV2(tilingPeriod), gpuOptF(rot), gpuOptF(seed))
      case _: Vec3Expr =>
        gpu.Extended.noise3d(p.asInstanceOf[Vec3Expr], gpuOptV3(tilingPeriod), gpuOptF(rot), gpuOptF(seed))

  /** [[extendedNoise]], value only (cheaper). */
  transparent inline def extendedNoiseValue(
      inline tilingPeriod: VecArg | Null = null,
      inline rot: FloatArg | Null = null,
      inline seed: FloatArg | Null = null,
  ): Double | FloatExpr =
    inline erasedValue[P] match
      case _: Vec2 =>
        Extended.noiseValue2d(p.asInstanceOf[Vec2], cpuOptV2(tilingPeriod), cpuOptD(rot), cpuOptD(seed))
      case _: Vec3 =>
        Extended.noiseValue3d(p.asInstanceOf[Vec3], cpuOptV3(tilingPeriod), cpuOptD(rot), cpuOptD(seed))
      case _: Vec2Expr =>
        gpu.Extended.noiseValue2d(p.asInstanceOf[Vec2Expr], gpuOptV2(tilingPeriod), gpuOptF(rot), gpuOptF(seed))
      case _: Vec3Expr =>
        gpu.Extended.noiseValue3d(p.asInstanceOf[Vec3Expr], gpuOptV3(tilingPeriod), gpuOptF(rot), gpuOptF(seed))

  /** Extended fbm, value + gradient (value strictly `[-1, 1]`). A tiling fbm
    * needs a whole-number `lacunarity`.
    */
  transparent inline def extendedFbm(
      inline octaves: IntArg = 4,
      inline lacunarity: FloatArg = 2.0,
      inline gain: FloatArg = 0.5,
      inline tilingPeriod: VecArg | Null = null,
      inline rot: FloatArg | Null = null,
      inline seed: FloatArg | Null = null,
  ): Vec3 | Vec4 | Vec3Expr | Vec4Expr =
    inline erasedValue[P] match
      case _: Vec2 =>
        Extended.fbm2d(
          p.asInstanceOf[Vec2],
          cpuI(octaves),
          cpuD(lacunarity),
          cpuD(gain),
          cpuOptV2(tilingPeriod),
          cpuOptD(rot),
          cpuOptD(seed),
        )
      case _: Vec3 =>
        Extended.fbm3d(
          p.asInstanceOf[Vec3],
          cpuI(octaves),
          cpuD(lacunarity),
          cpuD(gain),
          cpuOptV3(tilingPeriod),
          cpuOptD(rot),
          cpuOptD(seed),
        )
      case _: Vec2Expr =>
        gpu.Extended.fbm2d(
          p.asInstanceOf[Vec2Expr],
          gpuI(octaves),
          gpuF(lacunarity),
          gpuF(gain),
          gpuOptV2(tilingPeriod),
          gpuOptF(rot),
          gpuOptF(seed),
        )
      case _: Vec3Expr =>
        gpu.Extended.fbm3d(
          p.asInstanceOf[Vec3Expr],
          gpuI(octaves),
          gpuF(lacunarity),
          gpuF(gain),
          gpuOptV3(tilingPeriod),
          gpuOptF(rot),
          gpuOptF(seed),
        )

  /** [[extendedFbm]], value only (cheaper). */
  transparent inline def extendedFbmValue(
      inline octaves: IntArg = 4,
      inline lacunarity: FloatArg = 2.0,
      inline gain: FloatArg = 0.5,
      inline tilingPeriod: VecArg | Null = null,
      inline rot: FloatArg | Null = null,
      inline seed: FloatArg | Null = null,
  ): Double | FloatExpr =
    inline erasedValue[P] match
      case _: Vec2 =>
        Extended.fbmValue2d(
          p.asInstanceOf[Vec2],
          cpuI(octaves),
          cpuD(lacunarity),
          cpuD(gain),
          cpuOptV2(tilingPeriod),
          cpuOptD(rot),
          cpuOptD(seed),
        )
      case _: Vec3 =>
        Extended.fbmValue3d(
          p.asInstanceOf[Vec3],
          cpuI(octaves),
          cpuD(lacunarity),
          cpuD(gain),
          cpuOptV3(tilingPeriod),
          cpuOptD(rot),
          cpuOptD(seed),
        )
      case _: Vec2Expr =>
        gpu.Extended.fbmValue2d(
          p.asInstanceOf[Vec2Expr],
          gpuI(octaves),
          gpuF(lacunarity),
          gpuF(gain),
          gpuOptV2(tilingPeriod),
          gpuOptF(rot),
          gpuOptF(seed),
        )
      case _: Vec3Expr =>
        gpu.Extended.fbmValue3d(
          p.asInstanceOf[Vec3Expr],
          gpuI(octaves),
          gpuF(lacunarity),
          gpuF(gain),
          gpuOptV3(tilingPeriod),
          gpuOptF(rot),
          gpuOptF(seed),
        )

  /** Cellular noise: `(F1, F2)`, distances to the nearest and second-nearest
    * feature point. `jitter` in `[0, 1]` (lower = more regular).
    */
  transparent inline def worleyNoise(
      inline jitter: FloatArg = 1.0,
      inline seed: FloatArg | Null = null,
  ): Vec2 | Vec2Expr =
    inline erasedValue[P] match
      case _: Vec2 => Worley.noise2d(p.asInstanceOf[Vec2], cpuD(jitter), cpuOptD(seed))
      case _: Vec3 => Worley.noise3d(p.asInstanceOf[Vec3], cpuD(jitter), cpuOptD(seed))
      case _: Vec2Expr =>
        gpu.Worley.noise2d(p.asInstanceOf[Vec2Expr], gpuF(jitter), gpuOptF(seed))
      case _: Vec3Expr =>
        gpu.Worley.noise3d(p.asInstanceOf[Vec3Expr], gpuF(jitter), gpuOptF(seed))
