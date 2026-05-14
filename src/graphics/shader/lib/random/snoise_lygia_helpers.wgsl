/*
contributors: [Stefan Gustavson, Ian McEwan]
description: grad4, used for snoise(vec4 v)
*/

fn grad4(j: f32, ip: vec4f) -> vec4f {
    let ones = vec4(1.0, 1.0, 1.0, -1.0);
    var xyz = floor( fract (vec3(j) * ip.xyz) * 7.0) * ip.z - 1.0;
    let w = 1.5 - dot(abs(xyz), ones.xyz);
    let s = select(vec4(0.0), vec4(1.0), vec4(xyz, w) < vec4(0.0));
    xyz = xyz + (s.xyz*2.0 - 1.0) * s.www;
    return vec4f(xyz, w);
}
#include "mod289.wgsl"

/*
contributors: [Stefan Gustavson, Ian McEwan]
description: permute
*/

fn permute(x: f32) -> f32 { return mod289(((x * 34.0) + 1.0) * x); }
fn permute2(x: vec2f) -> vec2f { return mod289_2(((x * 34.0) + 1.0) * x); }
fn permute3(x: vec3f) -> vec3f { return mod289_3(((x * 34.0) + 1.0) * x); }
fn permute4(x: vec4f) -> vec4f { return mod289_4(((x * 34.0) + 1.0) * x); }
/*
contributors: [Stefan Gustavson, Ian McEwan]
description: modulus of 289
*/

fn mod289(x: f32) -> f32 { return x - floor(x * (1. / 289.)) * 289.; }
fn mod289_2(x: vec2f) -> vec2f { return x - floor(x * (1. / 289.)) * 289.; }
fn mod289_3(x: vec3f) -> vec3f { return x - floor(x * (1. / 289.)) * 289.; }
fn mod289_4(x: vec4f) -> vec4f { return x - floor(x * (1. / 289.)) * 289.; }
