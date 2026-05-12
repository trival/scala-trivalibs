package trivalibs.graphics.shader.lib.random

// Imported and ported from https://www.shadertoy.com/view/WttXWX

import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.Vec4
import trivalibs.graphics.math.gpu.UInt
import trivalibs.graphics.math.gpu.UVec2
import trivalibs.graphics.math.gpu.UVec3
import trivalibs.graphics.math.gpu.UVec4
import trivalibs.graphics.shader.dsl.WgslFn
import trivalibs.graphics.shader.given

object Hash:

  // Private helper: normalise a u32 into the [0, 1) range.
  // Used by all float-valued hash wrappers below.
  val u32ToF32: WgslFn[(x: UInt), Float] =
    WgslFn.raw("u32_to_f32"):
      "  return f32(x) / f32(0xffffffffu);"

  // ---------------------------------------------------------------------------
  // Scalar u32 hashes
  // ---------------------------------------------------------------------------

  // from Chris Wellons https://nullprogram.com/blog/2018/07/31/
  // https://github.com/skeeto/hash-prospector

  /** bias: 0.10760229515479501. Has excellent results if tested here:
    * https://www.shadertoy.com/view/XlGcRh
    */
  val hash1i: WgslFn[(x: UInt), UInt] =
    WgslFn.raw("hash1i"):
      """  var v = x;
  v ^= v >> 16u;
  v = v * 0x21f0aaadu;
  v ^= v >> 15u;
  v = v * 0xd35a2d97u;
  return v ^ (v >> 15u);"""

  /** bias: 0.020888578919738908 = minimal theoretic limit Probably hash1i is
    * good enough for most cases.
    */
  val hash1iTriple32: WgslFn[(x: UInt), UInt] =
    WgslFn.raw("hash1i_triple32"):
      """  var v = x;
  v ^= v >> 17u;
  v = v * 0xed5ad4bbu;
  v ^= v >> 11u;
  v = v * 0xac4c1b51u;
  v ^= v >> 15u;
  v = v * 0x31848babu;
  return v ^ (v >> 14u);"""

  /** Hash a u32 to a normalised f32 in [0, 1). Calls hash1i and u32_to_f32.
    */
  val hash1: WgslFn[(x: UInt), Float] =
    WgslFn
      .raw("hash1")("  return u32_to_f32(hash1i(x));")
      .withDeps(hash1i, u32ToF32)

  // ---------------------------------------------------------------------------
  // 2D → scalar (Inigo Quilez)
  // ---------------------------------------------------------------------------

  /*
   * The MIT License
   * Copyright © 2017, 2024 Inigo Quilez
   * Permission is hereby granted, free of charge, to any person obtaining a
   * copy of this software and associated documentation files (the "Software"),
   * to deal in the Software without restriction, including without limitation
   * the rights to use, copy, modify, merge, publish, distribute, sublicense,
   * and/or sell copies of the Software, and to permit persons to whom the
   * Software is furnished to do so, subject to the following conditions: The
   * above copyright notice and this permission notice shall be included in all
   * copies or substantial portions of the Software. THE SOFTWARE IS PROVIDED
   * "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
   * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
   * PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   *
   * Applies to: hash21i, hash21
   */

  // ported from https://www.shadertoy.com/view/4tXyWN by Inigo Quilez

  /** Hash UVec2 to a u32. */
  val hash21i: WgslFn[(p: UVec2), UInt] =
    WgslFn.raw("hash21i"):
      """  var v = p;
  v = v * vec2<u32>(73333u, 7777u);
  v = v ^ (vec2<u32>(3333777777u, 3333777777u) >> (v >> vec2<u32>(28u, 28u)));
  let n = v.x * v.y;
  return n ^ (n >> 15u);"""

  /** Hash UVec2 to a normalised f32 in [0, 1). */
  val hash21: WgslFn[(p: UVec2), Float] =
    WgslFn
      .raw("hash21")("  return u32_to_f32(hash21i(p));")
      .withDeps(hash21i, u32ToF32)

  // ---------------------------------------------------------------------------
  // 2D → 2D integer / float
  // ---------------------------------------------------------------------------

  // https://www.pcg-random.org/
  // http://www.jcgt.org/published/0009/03/02/

  // see https://www.shadertoy.com/view/XlGcRh

  /** Hash UVec2 to UVec2. */
  val hash2i: WgslFn[(v: UVec2), UVec2] =
    WgslFn.raw("hash2i"):
      """  var h = v;
  h = h * vec2<u32>(1664525u, 1013904223u);
  h.x = h.x + h.y * 1664525u;
  h.y = h.y + h.x * 1664525u;
  h = h ^ (h >> vec2<u32>(16u, 16u));
  h.x = h.x + h.y * 1664525u;
  h.y = h.y + h.x * 1664525u;
  h = h ^ (h >> vec2<u32>(16u, 16u));
  return h;"""

  /** Hash UVec2 to a Vec2 with components in [0, 1). */
  val hash2: WgslFn[(v: UVec2), Vec2] =
    WgslFn
      .raw("hash2")(
        """  let h = hash2i(v);
  return vec2<f32>(u32_to_f32(h.x), u32_to_f32(h.y));""",
      )
      .withDeps(hash2i, u32ToF32)

  // ---------------------------------------------------------------------------
  // 3D → 3D integer / float
  // ---------------------------------------------------------------------------

  /** Hash UVec3 to UVec3. */
  val hash3i: WgslFn[(v: UVec3), UVec3] =
    WgslFn.raw("hash3i"):
      """  var h = v;
  h = h * vec3<u32>(1664525u, 1013904223u, 1013904223u);
  h.x = h.x + h.y * h.z;
  h.y = h.y + h.z * h.x;
  h.z = h.z + h.x * h.y;
  h = h ^ (h >> vec3<u32>(16u, 16u, 16u));
  h.x = h.x + h.y * h.z;
  h.y = h.y + h.z * h.x;
  h.z = h.z + h.x * h.y;
  return h;"""

  /** Hash UVec3 to a Vec3 with components in [0, 1). */
  val hash3: WgslFn[(v: UVec3), Vec3] =
    WgslFn
      .raw("hash3")(
        """  let h = hash3i(v);
  return vec3<f32>(u32_to_f32(h.x), u32_to_f32(h.y), u32_to_f32(h.z));""",
      )
      .withDeps(hash3i, u32ToF32)

  // ---------------------------------------------------------------------------
  // 4D → 4D integer / float
  // ---------------------------------------------------------------------------

  /** Hash UVec4 to UVec4. */
  val hash4i: WgslFn[(v: UVec4), UVec4] =
    WgslFn.raw("hash4i"):
      """  var h = v;
  h = h * vec4<u32>(1664525u, 1013904223u, 1013904223u, 1013904223u);
  h.x = h.x + h.y * h.w;
  h.y = h.y + h.z * h.x;
  h.z = h.z + h.x * h.y;
  h.w = h.w + h.y * h.z;
  h = h ^ (h >> vec4<u32>(16u, 16u, 16u, 16u));
  h.x = h.x + h.y * h.w;
  h.y = h.y + h.z * h.x;
  h.z = h.z + h.x * h.y;
  h.w = h.w + h.y * h.z;
  return h;"""

  /** Hash UVec4 to a Vec4 with components in [0, 1). */
  val hash4: WgslFn[(v: UVec4), Vec4] =
    WgslFn
      .raw("hash4")(
        """  let h = hash4i(v);
  return vec4<f32>(u32_to_f32(h.x), u32_to_f32(h.y), u32_to_f32(h.z), u32_to_f32(h.w));""",
      )
      .withDeps(hash4i, u32ToF32)

  // ---------------------------------------------------------------------------
  // Float-input ergonomic wrappers
  // Callers with Vec* positions use these to avoid writing bitcast manually.
  // Both the wrapper and its underlying hash must be registered via program.fn.
  // ---------------------------------------------------------------------------

  /** Hash a Float to a normalised f32 in [0, 1) by bit-reinterpreting as u32.
    */
  val hash1f: WgslFn[(x: Float), Float] =
    WgslFn
      .raw("hash1f")("  return hash1(bitcast<u32>(x));")
      .withDeps(hash1)

  /** Hash a Vec2 to a Vec2 in [0, 1) by bit-casting to UVec2 first. */
  val hash2f: WgslFn[(v: Vec2), Vec2] =
    WgslFn
      .raw("hash2f")("  return hash2(bitcast<vec2<u32>>(v));")
      .withDeps(hash2)

  /** Hash a Vec3 to a Vec3 in [0, 1) by bit-casting to UVec3 first. */
  val hash3f: WgslFn[(v: Vec3), Vec3] =
    WgslFn
      .raw("hash3f")("  return hash3(bitcast<vec3<u32>>(v));")
      .withDeps(hash3)

  /** Hash a Vec4 to a Vec4 in [0, 1) by bit-casting to UVec4 first. */
  val hash4f: WgslFn[(v: Vec4), Vec4] =
    WgslFn
      .raw("hash4f")("  return hash4(bitcast<vec4<u32>>(v));")
      .withDeps(hash4)
