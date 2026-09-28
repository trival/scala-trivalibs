// CPU noise benchmark: trivalibs' Scala.js kernels against
// jwagner/simplex-noise.js as the yardstick. Run with `bun run bench:noise`
// (builds bench/NoiseBench.scala in full-opt mode first).

import { createNoise2D, createNoise3D, createNoise4D } from "simplex-noise"
import * as tl from "./noise_bench.out.js"

const N = 2_000_000
const ROUNDS = 5

function measure(name: string, run: (n: number) => number) {
	run(N / 10) // warm-up / JIT
	let best = Infinity
	let checksum = 0
	for (let r = 0; r < ROUNDS; r++) {
		const t0 = performance.now()
		checksum += run(N)
		const dt = performance.now() - t0
		best = Math.min(best, dt)
	}
	const mops = N / best / 1000
	console.log(`${name.padEnd(34)} ${mops.toFixed(1).padStart(7)} Mops/s   (checksum ${checksum.toFixed(3)})`)
}

const js2 = createNoise2D(() => 0.5)
const js3 = createNoise3D(() => 0.5)
const js4 = createNoise4D(() => 0.5)

function jsLoop2(n: number) {
	let s = 0
	for (let i = 0; i < n; i++) s += js2(i * 0.0137, i * 0.0071)
	return s
}
function jsLoop3(n: number) {
	let s = 0
	for (let i = 0; i < n; i++) s += js3(i * 0.0137, i * 0.0071, i * 0.0053)
	return s
}
function jsLoop4(n: number) {
	let s = 0
	for (let i = 0; i < n; i++) {
		const x = i * 0.0137
		const z = i * 0.0053
		s += js4(x, i * 0.0071, z, x - z)
	}
	return s
}

console.log("--- simplex ---")
measure("simplex-noise.js 2D", jsLoop2)
measure("trivalibs Simplex 2D", tl.simplex2d)
measure("trivalibs Simplex 2D seeded", tl.simplex2dSeeded)
measure("simplex-noise.js 3D", jsLoop3)
measure("trivalibs Simplex 3D", tl.simplex3d)
measure("trivalibs Simplex 3D seeded", tl.simplex3dSeeded)
measure("simplex-noise.js 4D", jsLoop4)
measure("trivalibs Simplex 4D", tl.simplex4d)
measure("trivalibs p.simplexFbm (4 oct)", tl.simplexFbm2dVia)
console.log("--- extended (psrdnoise) ---")
measure("trivalibs Extended value 2D", tl.extendedValue2d)
measure("trivalibs Extended value 3D", tl.extendedValue3d)
measure("trivalibs Extended value 3D rot", tl.extendedValue3dRot)
measure("trivalibs Extended grad 3D", tl.extendedGrad3d)
console.log("--- worley ---")
measure("trivalibs Worley 2D", tl.worley2d)
measure("trivalibs Worley 3D", tl.worley3d)
