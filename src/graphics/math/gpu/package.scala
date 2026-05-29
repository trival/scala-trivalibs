package trivalibs.graphics.math.gpu

// Re-export CPU class names for shader contract definitions
// (e.g., type Attribs = (position: Vec2, color: Vec4))
export trivalibs.graphics.math.cpu.{Vec2, Vec3, Vec4, Mat2, Mat3, Mat4}
