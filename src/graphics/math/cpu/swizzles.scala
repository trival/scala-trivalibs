package trivalibs.graphics.math.cpu

// Swizzles: consecutive sub-vector splits + full-arity reverse.
// Available on the mutable Vec class and the Tuple variants. Each receiver
// type's swizzles preserve its own family (Vec → Vec*; Tuple → Vec*Tuple).
// Buffer types are not covered — buffer slots are read out by component
// before swizzling.
//
// Import via `trivalibs.graphics.math.cpu.*`.

extension (v: Vec2) inline def yx: Vec2 = Vec2(v.y, v.x)

extension (v: Vec3)
  inline def xy: Vec2 = Vec2(v.x, v.y)
  inline def yz: Vec2 = Vec2(v.y, v.z)
  inline def zyx: Vec3 = Vec3(v.z, v.y, v.x)
  inline def rg: Vec2 = v.xy
  inline def gb: Vec2 = v.yz
  inline def bgr: Vec3 = v.zyx

extension (v: Vec4)
  inline def xy: Vec2 = Vec2(v.x, v.y)
  inline def yz: Vec2 = Vec2(v.y, v.z)
  inline def zw: Vec2 = Vec2(v.z, v.w)
  inline def xyz: Vec3 = Vec3(v.x, v.y, v.z)
  inline def yzw: Vec3 = Vec3(v.y, v.z, v.w)
  inline def wzyx: Vec4 = Vec4(v.w, v.z, v.y, v.x)
  inline def rg: Vec2 = v.xy
  inline def gb: Vec2 = v.yz
  inline def ba: Vec2 = v.zw
  inline def rgb: Vec3 = v.xyz
  inline def gba: Vec3 = v.yzw
  inline def abgr: Vec4 = v.wzyx

extension (v: Vec2Tuple)
  @annotation.targetName("vec2t_yx") inline def yx: Vec2Tuple = (v._2, v._1)

extension (v: Vec3Tuple)
  @annotation.targetName("vec3t_xy") inline def xy: Vec2Tuple = (v._1, v._2)
  @annotation.targetName("vec3t_yz") inline def yz: Vec2Tuple = (v._2, v._3)
  @annotation.targetName("vec3t_zyx") inline def zyx: Vec3Tuple =
    (v._3, v._2, v._1)
  @annotation.targetName("vec3t_rg") inline def rg: Vec2Tuple = v.xy
  @annotation.targetName("vec3t_gb") inline def gb: Vec2Tuple = v.yz
  @annotation.targetName("vec3t_bgr") inline def bgr: Vec3Tuple = v.zyx

extension (v: Vec4Tuple)
  @annotation.targetName("vec4t_xy") inline def xy: Vec2Tuple = (v._1, v._2)
  @annotation.targetName("vec4t_yz") inline def yz: Vec2Tuple = (v._2, v._3)
  @annotation.targetName("vec4t_zw") inline def zw: Vec2Tuple = (v._3, v._4)
  @annotation.targetName("vec4t_xyz") inline def xyz: Vec3Tuple =
    (v._1, v._2, v._3)
  @annotation.targetName("vec4t_yzw") inline def yzw: Vec3Tuple =
    (v._2, v._3, v._4)
  @annotation.targetName("vec4t_wzyx") inline def wzyx: Vec4Tuple =
    (v._4, v._3, v._2, v._1)
  @annotation.targetName("vec4t_rg") inline def rg: Vec2Tuple = v.xy
  @annotation.targetName("vec4t_gb") inline def gb: Vec2Tuple = v.yz
  @annotation.targetName("vec4t_ba") inline def ba: Vec2Tuple = v.zw
  @annotation.targetName("vec4t_rgb") inline def rgb: Vec3Tuple = v.xyz
  @annotation.targetName("vec4t_gba") inline def gba: Vec3Tuple = v.yzw
  @annotation.targetName("vec4t_abgr") inline def abgr: Vec4Tuple = v.wzyx
