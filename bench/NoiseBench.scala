package bench

// CPU noise throughput, called from bench/noise.bench.ts. Each export runs its
// own loop over `n` sample points and returns a checksum, so the measurement is
// the kernel, not the JS → Scala call.

import trivalibs.graphics.lib.noise.{*, given}
import trivalibs.graphics.math.cpu.{*, given}

import scala.scalajs.js.annotation.*

private inline def loop2(n: Int)(inline f: (Double, Double) => Double): Double =
  var sum = 0.0
  var i = 0
  while i < n do
    sum += f(i * 0.0137, i * 0.0071)
    i += 1
  sum

private inline def loop3(n: Int)(inline f: (Double, Double, Double) => Double): Double =
  var sum = 0.0
  var i = 0
  while i < n do
    sum += f(i * 0.0137, i * 0.0071, i * 0.0053)
    i += 1
  sum

@JSExportTopLevel("simplex2d", moduleID = "noise_bench")
def simplex2d(n: Int): Double = loop2(n)((x, y) => Simplex.kernel.noise2d(x, y))

@JSExportTopLevel("simplex2dSeeded", moduleID = "noise_bench")
def simplex2dSeeded(n: Int): Double =
  loop2(n)((x, y) => Simplex.kernel.noise2dSeeded(x, y, 3.0))

@JSExportTopLevel("simplex3d", moduleID = "noise_bench")
def simplex3d(n: Int): Double = loop3(n)((x, y, z) => Simplex.kernel.noise3d(x, y, z))

@JSExportTopLevel("simplex3dSeeded", moduleID = "noise_bench")
def simplex3dSeeded(n: Int): Double =
  loop3(n)((x, y, z) => Simplex.kernel.noise3dSeeded(x, y, z, 3.0))

@JSExportTopLevel("simplex4d", moduleID = "noise_bench")
def simplex4d(n: Int): Double =
  loop3(n)((x, y, z) => Simplex.kernel.noise4d(x, y, z, x - z))

@JSExportTopLevel("simplexFbm2dVia", moduleID = "noise_bench")
def simplexFbm2dVia(n: Int): Double =
  loop2(n)((x, y) => Vec2(x, y).simplexFbm(octaves = 4))

@JSExportTopLevel("extendedValue2d", moduleID = "noise_bench")
def extendedValue2d(n: Int): Double =
  loop2(n)((x, y) => Extended.kernel.noiseValue2d(x, y, 0.0, 0.0, 0.0))

@JSExportTopLevel("extendedValue3d", moduleID = "noise_bench")
def extendedValue3d(n: Int): Double =
  loop3(n)((x, y, z) => Extended.kernel.noiseValue3d(x, y, z, 0.0, 0.0, 0.0, 0.0))

@JSExportTopLevel("extendedValue3dRot", moduleID = "noise_bench")
def extendedValue3dRot(n: Int): Double =
  loop3(n)((x, y, z) => Extended.kernel.noiseValue3d(x, y, z, 0.0, 0.0, 0.0, 0.5))

private val out4 = new Vec4()

@JSExportTopLevel("extendedGrad3d", moduleID = "noise_bench")
def extendedGrad3d(n: Int): Double =
  loop3(n)((x, y, z) => Extended.kernel.noise3dInto(x, y, z, 0.0, 0.0, 0.0, 0.0, out4).y)

private val out2 = new Vec2()

@JSExportTopLevel("worley2d", moduleID = "noise_bench")
def worley2d(n: Int): Double =
  loop2(n)((x, y) => Worley.kernel.noise2dInto(x, y, 1.0, out2).x)

@JSExportTopLevel("worley3d", moduleID = "noise_bench")
def worley3d(n: Int): Double =
  loop3(n)((x, y, z) => Worley.kernel.noise3dInto(x, y, z, 1.0, out2).x)
