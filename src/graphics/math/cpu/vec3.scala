package trivalibs.graphics.math.cpu

import trivalibs.graphics.math.*
import trivalibs.bufferdata.F32
import trivalibs.bufferdata.F64
import trivalibs.bufferdata.StructRef

// === implementations for common vector types ===

// ==== Buffer types ====
// Note: Vec3Buffer uses F32 (for GPU upload), Vec3dBuffer uses F64

type Vec3Buffer = (F32, F32, F32)

object Vec3Buffer:
  given vec3MutableBuffer: Vec3Mutable[StructRef[Vec3Buffer]]:
    extension (v: StructRef[Vec3Buffer])
      inline def x = v.getAt(0): Double
      inline def y = v.getAt(1): Double
      inline def z = v.getAt(2): Double
      inline def x_=(value: Double) = v.setAt(0)(value.toFloat)
      inline def y_=(value: Double) = v.setAt(1)(value.toFloat)
      inline def z_=(value: Double) = v.setAt(2)(value.toFloat)

  given vec3MutableOpsBuffer: Vec3MutableOps[StructRef[Vec3Buffer]] =
    new Vec3MutableOps[StructRef[Vec3Buffer]] {}

type Vec3dBuffer = (F64, F64, F64)

object Vec3dBuffer:
  given vec3MutableDBuffer: Vec3Mutable[StructRef[Vec3dBuffer]]:
    extension (v: StructRef[Vec3dBuffer])
      inline def x = v.getAt(0): Double
      inline def y = v.getAt(1): Double
      inline def z = v.getAt(2): Double
      inline def x_=(value: Double) = v.setAt(0)(value)
      inline def y_=(value: Double) = v.setAt(1)(value)
      inline def z_=(value: Double) = v.setAt(2)(value)

  given vec3MutableOpsDBuffer: Vec3MutableOps[StructRef[Vec3dBuffer]] =
    new Vec3MutableOps[StructRef[Vec3dBuffer]] {}

// ===== Tuple and class types =====

type Vec3Tuple = (Double, Double, Double)

object Vec3Tuple extends Vec3ImmutableOps[Vec3Tuple]:
  inline def create(x: Double, y: Double, z: Double) = (x, y, z)
  given Vec3ImmutableOps[Vec3Tuple] = Vec3Tuple
  val zero: Vec3Tuple = (0.0, 0.0, 0.0)
  val X: Vec3Tuple = (1.0, 0.0, 0.0)
  val Y: Vec3Tuple = (0.0, 1.0, 0.0)
  val Z: Vec3Tuple = (0.0, 0.0, 1.0)

  given Vec3Base[Vec3Tuple]:
    extension (v: Vec3Tuple)
      inline def x = v._1
      inline def y = v._2
      inline def z = v._3

class Vec3(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)

object Vec3 extends Vec3ImmutableOps[Vec3]:
  inline def create(x: Double, y: Double, z: Double) = new Vec3(x, y, z)
  given Vec3ImmutableOps[Vec3] = Vec3
  def zero: Vec3 = new Vec3(0.0, 0.0, 0.0)
  def X: Vec3 = new Vec3(1.0, 0.0, 0.0)
  def Y: Vec3 = new Vec3(0.0, 1.0, 0.0)
  def Z: Vec3 = new Vec3(0.0, 0.0, 1.0)

  given Vec3Mutable[Vec3]:
    extension (v: Vec3)
      inline def x = v.x
      inline def y = v.y
      inline def z = v.z
      inline def x_=(value: Double) = v.x = value
      inline def y_=(value: Double) = v.y = value
      inline def z_=(value: Double) = v.z = value

  given Vec3MutableOps[Vec3] = new Vec3MutableOps[Vec3] {}
