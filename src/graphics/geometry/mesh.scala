package trivalibs.graphics.geometry

import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.given
import scala.scalajs.js
import trivalibs.utils.js.*

/** A mesh face: either a [[Triangle]] or a [[Quad]] of vertices `T`. */
type Face[T] = Triangle[T] | Quad[T]

class PositionFaceRef(val faceIndex: Int, val vertexSlot: Int)

class VertexPosition[T](val position: Vec3, val faces: Arr[PositionFaceRef])

class FaceData(var normal: Opt[Vec3], val section: Int)

/** A collection of [[Face]]s (triangles/quads) with shared-position tracking —
  * the idiomatic way to build 3D geometry. Construct from faces with
  * `Mesh(faces)` / `Mesh.apply`, transform with [[map]] / [[flatMap]], then hand
  * to [[toBufferedGeometry]] for `painter.form`. The vertex type `T` needs a
  * [[Position]] (any named tuple with a `position: Vec3` field). Generators like
  * [[Box]] and [[sphereMesh]] / [[Grid]] produce faces or meshes directly.
  */
class Mesh[T: Position]:
  val faces: Arr[Face[T]] = Arr()
  val faceData: Arr[FaceData] = Arr()
  val positions: Arr[VertexPosition[T]] = Arr()
  val positionMap: Dict[Int] = Dict()

  /** Add a [[Face]] (triangle/quad). `normal` is **optional** — pass it only if
    * you already know it (a cheap shortcut); otherwise [[toBufferedGeometry]]
    * computes face/vertex normals itself when a `…WithNormal` buffer type is
    * requested. `section` tags faces for [[newFromSection]]. */
  def addFace(face: Face[T], normal: Opt[Vec3] = null, section: Int = 0): Unit =
    val faceIdx = faces.length
    faces.push(face)
    faceData.push(FaceData(normal, section))
    val arr = face.asInstanceOf[Arr[T]]
    val n = arr.length
    var slot = 0
    while slot < n do
      val v = arr(slot)
      val key = posKey(v.pos)
      if js.Object.hasProperty(positionMap.asInstanceOf[js.Object], key) then
        positions(positionMap(key)).faces.push(PositionFaceRef(faceIdx, slot))
      else
        val idx = positions.length
        positionMap(key) = idx
        positions.push(
          VertexPosition(v.pos, Arr(PositionFaceRef(faceIdx, slot))),
        )
      slot += 1

  def removeFace(faceIdx: Int): Unit =
    // Unregister position refs for the face being removed
    val arr = faces(faceIdx).asInstanceOf[Arr[T]]
    val n = arr.length
    var slot = 0
    while slot < n do
      val key = posKey(arr(slot).pos)
      if js.Object.hasProperty(positionMap.asInstanceOf[js.Object], key) then
        val vp = positions(positionMap(key))
        var i = 0
        while i < vp.faces.length do
          val ref = vp.faces(i)
          if ref.faceIndex == faceIdx && ref.vertexSlot == slot then
            vp.faces.splice(i, 1)
          else i += 1
      slot += 1

    val lastIdx = faces.length - 1
    if faceIdx != lastIdx then
      // Rewrite refs for the last face (it will be moved to faceIdx)
      val lastArr = faces(lastIdx).asInstanceOf[Arr[T]]
      val lN = lastArr.length
      var ls = 0
      while ls < lN do
        val key = posKey(lastArr(ls).pos)
        if js.Object.hasProperty(positionMap.asInstanceOf[js.Object], key) then
          val vp = positions(positionMap(key))
          var i = 0
          while i < vp.faces.length do
            val ref = vp.faces(i)
            if ref.faceIndex == lastIdx && ref.vertexSlot == ls then
              vp.faces(i) = PositionFaceRef(faceIdx, ls)
            i += 1
        ls += 1
      faces(faceIdx) = faces(lastIdx)
      faceData(faceIdx) = faceData(lastIdx)

    faces.pop()
    faceData.pop()

  def getPosition(v: Vec3): Opt[VertexPosition[T]] =
    val key = posKey(v)
    if js.Object.hasProperty(positionMap.asInstanceOf[js.Object], key) then
      positions(positionMap(key))
    else null

  def adjacentFaces(v: Vec3): Arr[Face[T]] =
    val vp = getPosition(v)
    if vp == null then Arr()
    else
      val result = Arr[Face[T]]()
      var i = 0
      while i < vp.faces.length do
        result.push(faces(vp.faces(i).faceIndex))
        i += 1
      result

  // Compute and cache face normals for any face that doesn't have one yet.
  // Returns true if the mesh contains any quads.
  def ensureFaceNormals(): Boolean =
    var hasQuads = false
    var i = 0
    while i < faces.length do
      val arr = faces(i).asInstanceOf[Arr[T]]
      if faceData(i).normal == null then
        faceData(i).normal =
          if arr.length == 3 then faces(i).asInstanceOf[Triangle[T]].normal
          else
            hasQuads = true
            faces(i).asInstanceOf[Quad[T]].normal

      i += 1
    hasQuads

  /** A new mesh with each face mapped by `f` (e.g. to recompute vertex
    * attributes); section + normal are carried over. */
  def map[U: Position](f: Face[T] => Face[U]): Mesh[U] =
    val m = new Mesh[U]()
    var i = 0
    while i < faces.length do
      m.addFace(f(faces(i)), faceData(i).normal, faceData(i).section)
      i += 1
    m

  /** A new mesh where each face expands to zero or more faces (e.g.
    * subdivision). */
  def flatMap[U: Position](f: Face[T] => Arr[Face[U]]): Mesh[U] =
    val m = new Mesh[U]()
    var i = 0
    while i < faces.length do
      val newFaces = f(faces(i))
      var j = 0
      while j < newFaces.length do
        m.addFace(newFaces(j), null, faceData(i).section)
        j += 1
      i += 1
    m

  def newFromSection(section: Int): Mesh[T] =
    val m = new Mesh[T]()
    var i = 0
    while i < faces.length do
      if faceData(i).section == section then
        m.addFace(faces(i), faceData(i).normal, section)
      i += 1
    m

extension [T: Position](m: Mesh[T])
  def addFaces[F <: Face[T]](
      faces: Arr[F],
      normal: Opt[Vec3] = null,
      section: Int = 0,
  ): Unit =
    var i = 0
    while i < faces.length do
      m.addFace(faces(i), normal, section)
      i += 1

object Mesh:
  /** Build a mesh from an array of faces (the usual constructor):
    * `Mesh(box.faces(...))` or `Mesh(grid.quads)`. Optionally tag them with a
    * face `normal` and a `section` id. */
  def apply[T: Position, F <: Face[T]](
      faces: Arr[F],
      normal: Opt[Vec3] = null,
      section: Int = 0,
  ): Mesh[T] =
    val m = new Mesh[T]()
    m.addFaces(faces, normal, section)
    m
