package trivalibs.graphics.shader.lib.random

// Ported from https://github.com/stegu/psrdnoise (Stefan Gustavson, Ian McEwan)
// MIT License. © 2021-2022 Stefan Gustavson and Ian McEwan.

import trivalibs.graphics.math.cpu.{Vec2, Vec3, Vec4}
import trivalibs.graphics.shader.{given}
import trivalibs.graphics.shader.dsl.WgslFn

object Psrdnoise:

  // ---------------------------------------------------------------------------
  // Private helpers — mod289 variants and permutation used by psrdnoise only
  // ---------------------------------------------------------------------------

  val mod289v3f: WgslFn[(x: Vec3), Vec3] =
    WgslFn.raw("mod289v3f")("  return x - floor(x / 289.0) * 289.0;")

  val mod289v4f: WgslFn[(x: Vec4), Vec4] =
    WgslFn.raw("mod289v4f")("  return x - floor(x / 289.0) * 289.0;")

  val permute289v4f: WgslFn[(i: Vec4), Vec4] =
    WgslFn
      .raw("permute289v4f")("""
  var im: vec4<f32> = mod289v4f(i);
  return mod289v4f((im*34.0 + 10.0)*im);""")
      .withDeps(mod289v4f)

  // ---------------------------------------------------------------------------
  // 2D tiling + rotating psrdnoise — returns vec3(noise, grad.x, grad.y)
  // ---------------------------------------------------------------------------

  val tilingRotNoise2d: WgslFn[(pos: Vec2, period: Vec2, normRot: Float), Vec3] =
    WgslFn
      .raw("tiling_rot_noise_2d")("""
  let alpha = normRot * 6.28318530718;
  var uv: vec2<f32>;
  var f0: vec2<f32>;
  var i0: vec2<f32>;
  var i1: vec2<f32>;
  var i2: vec2<f32>;
  var o1: vec2<f32>;
  var v0: vec2<f32>;
  var v1: vec2<f32>;
  var v2: vec2<f32>;
  var x0: vec2<f32>;
  var x1: vec2<f32>;
  var x2: vec2<f32>;
  uv = vec2<f32>(pos.x+pos.y*0.5, pos.y);
  i0 = floor(uv);
  f0 = uv - i0;
  o1 = select(vec2<f32>(0.0,1.0), vec2<f32>(1.0, 0.0), f0.x > f0.y);
  i1 = i0 + o1;
  i2 = i0 + vec2<f32>(1.0, 1.0);
  v0 = vec2<f32>(i0.x - i0.y*0.5, i0.y);
  v1 = vec2<f32>(v0.x + o1.x - o1.y*0.5, v0.y + o1.y);
  v2 = vec2<f32>(v0.x + 0.5, v0.y + 1.0);
  x0 = pos - v0;
  x1 = pos - v1;
  x2 = pos - v2;
  var iu: vec3<f32>;
  var iv: vec3<f32>;
  var xw: vec3<f32>;
  var yw: vec3<f32>;
  if(any(period > vec2<f32>(0.0, 0.0))) {
    xw = vec3<f32>(v0.x, v1.x, v2.x);
    yw = vec3<f32>(v0.y, v1.y, v2.y);
    if(period.x > 0.0) {
      xw = xw - floor(vec3<f32>(v0.x, v1.x, v2.x) / period.x) * period.x;
    }
    if(period.y > 0.0) {
      yw = yw - floor(vec3<f32>(v0.y, v1.y, v2.y) / period.y) * period.y;
    }
    iu = floor(xw + 0.5*yw + 0.5);
    iv = floor(yw + 0.5);
  } else {
    iu = vec3<f32>(i0.x, i1.x, i2.x);
    iv = vec3<f32>(i0.y, i1.y, i2.y);
  }
  var hash: vec3<f32>;
  var psi: vec3<f32>;
  var gx: vec3<f32>;
  var gy: vec3<f32>;
  var g0: vec2<f32>;
  var g1: vec2<f32>;
  var g2: vec2<f32>;
  hash = mod289v3f(iu);
  hash = mod289v3f((hash*51.0 + 2.0)*hash + iv);
  hash = mod289v3f((hash*34.0 + 10.0)*hash);
  psi = hash*0.07482 + alpha;
  gx = cos(psi);
  gy = sin(psi);
  g0 = vec2<f32>(gx.x, gy.x);
  g1 = vec2<f32>(gx.y, gy.y);
  g2 = vec2<f32>(gx.z, gy.z);
  var w: vec3<f32>;
  var w2: vec3<f32>;
  var w4: vec3<f32>;
  var gdotx: vec3<f32>;
  var n: f32;
  w = 0.8 - vec3<f32>(dot(x0, x0), dot(x1, x1), dot(x2, x2));
  w = max(w, vec3<f32>(0.0, 0.0, 0.0));
  w2 = w*w;
  w4 = w2*w2;
  gdotx = vec3<f32>(dot(g0, x0), dot(g1, x1), dot(g2, x2));
  n = 10.9*dot(w4, gdotx);
  var w3: vec3<f32>;
  var dw: vec3<f32>;
  var dn0: vec2<f32>;
  var dn1: vec2<f32>;
  var dn2: vec2<f32>;
  var grad: vec2<f32>;
  w3 = w2*w;
  dw = -8.0*w3*gdotx;
  dn0 = w4.x*g0 + dw.x*x0;
  dn1 = w4.y*g1 + dw.y*x1;
  dn2 = w4.z*g2 + dw.z*x2;
  grad = 10.9*(dn0 + dn1 + dn2);
  return vec3<f32>(n, grad.x, grad.y);""")
      .withDeps(mod289v3f)

  val tilingNoise2d: WgslFn[(pos: Vec2, period: Vec2), Vec3] =
    WgslFn
      .raw("tiling_noise_2d")("  return tiling_rot_noise_2d(pos, period, 0.0);")
      .withDeps(tilingRotNoise2d)

  val rotNoise2d: WgslFn[(pos: Vec2, normRot: Float), Vec3] =
    WgslFn
      .raw("rot_noise_2d")("  return tiling_rot_noise_2d(pos, vec2<f32>(0.0, 0.0), normRot);")
      .withDeps(tilingRotNoise2d)

  // ---------------------------------------------------------------------------
  // 3D tiling + rotating psrdnoise — returns vec4(noise, grad.x, grad.y, grad.z)
  // ---------------------------------------------------------------------------

  val tilingRotNoise3d: WgslFn[(pos: Vec3, period: Vec3, normRot: Float), Vec4] =
    WgslFn
      .raw("tiling_rot_noise_3d")("""
  let alpha = normRot * 6.28318530718;
  let M = mat3x3<f32>(0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0);
  let Mi = mat3x3<f32>(-0.5, 0.5, 0.5, 0.5,-0.5, 0.5, 0.5, 0.5,-0.5);
  var uvw: vec3<f32>;
  var i0: vec3<f32>;
  var i1: vec3<f32>;
  var i2: vec3<f32>;
  var i3: vec3<f32>;
  var f0: vec3<f32>;
  var gt_: vec3<f32>;
  var lt_: vec3<f32>;
  var gt: vec3<f32>;
  var lt: vec3<f32>;
  var o1: vec3<f32>;
  var o2: vec3<f32>;
  var v0: vec3<f32>;
  var v1: vec3<f32>;
  var v2: vec3<f32>;
  var v3: vec3<f32>;
  var x0: vec3<f32>;
  var x1: vec3<f32>;
  var x2: vec3<f32>;
  var x3: vec3<f32>;
  uvw = M * pos;
  i0 = floor(uvw);
  f0 = uvw - i0;
  gt_ = step(f0.xyx, f0.yzz);
  lt_ = 1.0 - gt_;
  gt = vec3<f32>(lt_.z, gt_.xy);
  lt = vec3<f32>(lt_.xy, gt_.z);
  o1 = min(gt, lt);
  o2 = max(gt, lt);
  i1 = i0 + o1;
  i2 = i0 + o2;
  i3 = i0 + vec3<f32>(1.0, 1.0, 1.0);
  v0 = Mi * i0;
  v1 = Mi * i1;
  v2 = Mi * i2;
  v3 = Mi * i3;
  x0 = pos - v0;
  x1 = pos - v1;
  x2 = pos - v2;
  x3 = pos - v3;
  var vx: vec4<f32>;
  var vy: vec4<f32>;
  var vz: vec4<f32>;
  if(any(period > vec3<f32>(0.0))) {
    vx = vec4<f32>(v0.x, v1.x, v2.x, v3.x);
    vy = vec4<f32>(v0.y, v1.y, v2.y, v3.y);
    vz = vec4<f32>(v0.z, v1.z, v2.z, v3.z);
    if(period.x > 0.0) {
      vx = vx - floor(vx / period.x) * period.x;
    }
    if(period.y > 0.0) {
      vy = vy - floor(vy / period.y) * period.y;
    }
    if(period.z > 0.0) {
      vz = vz - floor(vz / period.z) * period.z;
    }
    i0 = floor(M * vec3<f32>(vx.x, vy.x, vz.x) + 0.5);
    i1 = floor(M * vec3<f32>(vx.y, vy.y, vz.y) + 0.5);
    i2 = floor(M * vec3<f32>(vx.z, vy.z, vz.z) + 0.5);
    i3 = floor(M * vec3<f32>(vx.w, vy.w, vz.w) + 0.5);
  }
  var hash: vec4<f32>;
  var theta: vec4<f32>;
  var sz: vec4<f32>;
  var psi: vec4<f32>;
  var St: vec4<f32>;
  var Ct: vec4<f32>;
  var sz_: vec4<f32>;
  hash = permute289v4f(permute289v4f(permute289v4f(
    vec4<f32>(i0.z, i1.z, i2.z, i3.z))
    + vec4<f32>(i0.y, i1.y, i2.y, i3.y))
    + vec4<f32>(i0.x, i1.x, i2.x, i3.x));
  theta = hash * 3.883222077;
  sz = hash * -0.006920415 + 0.996539792;
  psi = hash * 0.108705628;
  Ct = cos(theta);
  St = sin(theta);
  sz_ = sqrt(1.0 - sz*sz);
  var gx: vec4<f32>;
  var gy: vec4<f32>;
  var gz: vec4<f32>;
  var px: vec4<f32>;
  var py: vec4<f32>;
  var pz: vec4<f32>;
  var Sp: vec4<f32>;
  var Cp: vec4<f32>;
  var Ctp: vec4<f32>;
  var qx: vec4<f32>;
  var qy: vec4<f32>;
  var qz: vec4<f32>;
  var Sa: vec4<f32>;
  var Ca: vec4<f32>;
  if(alpha != 0.0) {
    px = Ct * sz_;
    py = St * sz_;
    pz = sz;
    Sp = sin(psi);
    Cp = cos(psi);
    Ctp = St*Sp - Ct*Cp;
    qx = mix(Ctp*St, Sp, sz);
    qy = mix(-Ctp*Ct, Cp, sz);
    qz = -(py*Cp + px*Sp);
    Sa = vec4<f32>(sin(alpha));
    Ca = vec4<f32>(cos(alpha));
    gx = Ca*px + Sa*qx;
    gy = Ca*py + Sa*qy;
    gz = Ca*pz + Sa*qz;
  } else {
    gx = Ct * sz_;
    gy = St * sz_;
    gz = sz;
  }
  var g0: vec3<f32>;
  var g1: vec3<f32>;
  var g2: vec3<f32>;
  var g3: vec3<f32>;
  var w: vec4<f32>;
  var w2: vec4<f32>;
  var w3: vec4<f32>;
  var gdotx: vec4<f32>;
  var n: f32;
  g0 = vec3<f32>(gx.x, gy.x, gz.x);
  g1 = vec3<f32>(gx.y, gy.y, gz.y);
  g2 = vec3<f32>(gx.z, gy.z, gz.z);
  g3 = vec3<f32>(gx.w, gy.w, gz.w);
  w = 0.5 - vec4<f32>(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3));
  w = max(w, vec4<f32>(0.0, 0.0, 0.0, 0.0));
  w2 = w * w;
  w3 = w2 * w;
  gdotx = vec4<f32>(dot(g0,x0), dot(g1,x1), dot(g2,x2), dot(g3,x3));
  n = 39.5 * dot(w3, gdotx);
  var dw: vec4<f32> = -6.0 * w2 * gdotx;
  var dn0: vec3<f32> = w3.x * g0 + dw.x * x0;
  var dn1: vec3<f32> = w3.y * g1 + dw.y * x1;
  var dn2: vec3<f32> = w3.z * g2 + dw.z * x2;
  var dn3: vec3<f32> = w3.w * g3 + dw.w * x3;
  var grad: vec3<f32> = 39.5 * (dn0 + dn1 + dn2 + dn3);
  return vec4<f32>(n, grad.x, grad.y, grad.z);""")
      .withDeps(permute289v4f)

  val tilingNoise3d: WgslFn[(pos: Vec3, period: Vec3), Vec4] =
    WgslFn
      .raw("tiling_noise_3d")("  return tiling_rot_noise_3d(pos, period, 0.0);")
      .withDeps(tilingRotNoise3d)

  val rotNoise3d: WgslFn[(pos: Vec3, normRot: Float), Vec4] =
    WgslFn
      .raw("rot_noise_3d")("  return tiling_rot_noise_3d(pos, vec3<f32>(0.0, 0.0, 0.0), normRot);")
      .withDeps(tilingRotNoise3d)
