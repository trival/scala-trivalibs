package trivalibs.graphics.geometry

import trivalibs.bufferdata.F32
import trivalibs.bufferdata.StructArray
import trivalibs.bufferdata.StructRef
import trivalibs.graphics.math.*
import trivalibs.graphics.math.cpu.Vec2
import trivalibs.graphics.math.cpu.Vec2Buffer
import trivalibs.graphics.math.cpu.given
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

// ===========================================================================
// LineVertex
// ===========================================================================

/** One vertex of a [[Line]]: a position with a stroke `width`, the length and
  * direction of the segment leading to the **next** vertex (`len` / `dir`, both
  * filled in by [[Line.addVert]] once that next vertex arrives), plus arbitrary
  * user `data`. Use `LineVertex(pos, width)` for data-less lines.
  */
class LineVertex[T](
    val pos: Vec2,
    var width: Double,
    var len: Double,
    var dir: Vec2,
    val data: T,
):
  /** A detached copy — line transformations never alias vertices between the
    * source and the result line.
    */
  def copy: LineVertex[T] = new LineVertex(pos, width, len, dir, data)

  /** Aim this vertex at `point`: sets `len` to the distance and `dir` to the
    * normalized direction towards it.
    */
  def pointTo(point: Vec2): Unit =
    val vx = point.x - pos.x
    val vy = point.y - pos.y
    val l = (vx * vx + vy * vy).sqrt
    len = l
    dir = Vec2(vx / l, vy / l)

  /** Replace a sharp corner with two bevel vertices, lerped `ratio` of the way
    * back towards `prev` and forward towards `next`. Returns a single copy of
    * this vertex when the turn is flatter than `angleThreshold` (measured as
    * `1 - dot(dir, prev.dir)`).
    */
  def smoothEdge(
      prev: LineVertex[T],
      next: LineVertex[T],
      ratio: Double,
      angleThreshold: Double,
  )(using Lerp[T]): Arr[LineVertex[T]] =
    val d = 1.0 - dir.dot(prev.dir)
    if d > angleThreshold then
      Arr(lerpVert(prev, this, 1.0 - ratio), lerpVert(this, next, ratio))
    else Arr(copy)

object LineVertex:
  def apply[T](pos: Vec2, width: Double, data: T): LineVertex[T] =
    new LineVertex(pos, width, 0.0, Vec2.zero, data)

  def apply(pos: Vec2, width: Double): LineVertex[Unit] =
    new LineVertex(pos, width, 0.0, Vec2.zero, ())

/** Interpolates position, width and data. `len` / `dir` are left at zero — they
  * are re-derived when the vertex is added to a line.
  */
private def lerpVert[T: Lerp](
    a: LineVertex[T],
    b: LineVertex[T],
    t: Double,
): LineVertex[T] =
  LineVertex(
    a.pos.lerp(b.pos, t),
    a.width + (b.width - a.width) * t,
    a.data.lerp(b.data, t),
  )

// ===========================================================================
// Line
// ===========================================================================

/** A variable-width 2D polyline. Build it by adding vertices — each `add` links
  * the previous vertex to the new one, filling in its segment length and
  * direction and accumulating [[totalLength]]. The transformation methods
  * ([[smoothEdges]], [[cleanup]], [[splitAtAngle]]) are non-mutating and return
  * fresh lines. Convert to GPU geometry with `toBufferedGeometry` (single line)
  * or `toBufferedGeometries` (an `Arr` of fragments, e.g. from
  * [[splitAtAngle]]).
  *
  * `lenOffset` is where this line starts along a longer conceptual stroke —
  * [[splitAtAngle]] threads it through the fragments so `uv.x` stays continuous
  * across the whole stroke.
  *
  * Construct via the companion: `Line(20.0)` for a data-less `Line[Unit]`,
  * `Line(20.0, 0.0, myDefaultData)` for a `Line[T]`.
  */
class Line[T](
    val defaultWidth: Double,
    val lenOffset: Double,
    val defaultData: T,
):
  /** The vertices, in order. Mutating this array directly desyncs
    * [[totalLength]] — prefer the `add*` methods.
    */
  val verts: Arr[LineVertex[T]] = Arr()

  /** Sum of all segment lengths (excluding [[lenOffset]]). */
  var totalLength: Double = 0.0

  /** The length this line will have once fully built, if it is known ahead of
    * time. `localUv.x` is normalised against it instead of [[totalLength]].
    *
    * Set it when rendering a line that is still being built — e.g. animating a
    * brush travelling along its path. Without it `localUv.x` spans 0..1 over
    * whatever has been added so far, so anything keyed on it (an end fade, a
    * texture) rescales and slides every time a vertex is appended. With it,
    * `localUv.x` only reaches the fraction actually drawn, and the geometry's
    * own end cap tapers the unfinished tip.
    */
  var plannedLength: Opt[Double] = null

  def vertCount: Int = verts.length
  def get(i: Int): LineVertex[T] = verts(i)
  def getOpt(i: Int): Opt[LineVertex[T]] =
    if i >= 0 && i < verts.length then verts(i) else null
  def first: LineVertex[T] = verts(0)
  def last: LineVertex[T] = verts(verts.length - 1)

  // --- building ---

  def add(pos: Vec2): Unit =
    addVert(LineVertex(pos, defaultWidth, defaultData))

  def add(pos: Vec2, width: Double): Unit =
    addVert(LineVertex(pos, width, defaultData))

  def add(pos: Vec2, width: Double, data: T): Unit =
    addVert(LineVertex(pos, width, data))

  /** Append `vert`, linking it to the current last vertex — that vertex gets
    * its `len` / `dir` towards `vert`, and `vert` inherits its direction.
    */
  def addVert(vert: LineVertex[T]): Unit =
    val n = verts.length
    if n > 0 then
      val prev = verts(n - 1)
      prev.pointTo(vert.pos)
      totalLength += prev.len
      vert.dir = prev.dir
    verts.push(vert)

  /** Append `vert` without re-linking — keeps the `len` / `dir` it already
    * carries. Used when re-assembling vertices whose directions are already
    * known (see [[splitAtAngle]]).
    */
  def addVertRaw(vert: LineVertex[T]): Unit =
    val n = verts.length
    if n > 0 then totalLength += verts(n - 1).len
    verts.push(vert)

  // --- transformations ---

  /** Rebuild the line, replacing each vertex by the vertices `f` returns for it
    * (`prev` / `next` are `null` at the ends). Returning an empty `Arr` drops
    * the vertex; returning several inserts them.
    */
  def flatMapWithNeighbours(
      f: (
          prev: Opt[LineVertex[T]],
          curr: LineVertex[T],
          next: Opt[LineVertex[T]],
      ) => Arr[LineVertex[T]],
  ): Line[T] =
    val line = new Line[T](defaultWidth, lenOffset, defaultData)
    val n = verts.length
    var i = 0
    while i < n do
      val prev: Opt[LineVertex[T]] = if i == 0 then null else verts(i - 1)
      val next: Opt[LineVertex[T]] = if i == n - 1 then null else verts(i + 1)
      val res = f(prev, verts(i), next)
      var j = 0
      while j < res.length do
        line.addVert(res(j))
        j += 1
      i += 1
    line

  /** Bevel sharp corners — every vertex whose turn exceeds `angleThreshold` is
    * replaced by two vertices at `ratio` along its neighbouring segments.
    * Segments shorter than `minDist` are left alone. Apply repeatedly for
    * rounder joins.
    */
  def smoothEdges(
      ratio: Double,
      minDist: Double,
      angleThreshold: Double = 0.0,
  )(using Lerp[T]): Line[T] =
    flatMapWithNeighbours: (prev, curr, next) =>
      if prev.isNull || next.isNull then Arr(curr.copy)
      else
        val p = prev.get
        if p.len < minDist || curr.len < minDist then Arr(curr.copy)
        else curr.smoothEdge(p, next.get, ratio, angleThreshold)

  /** Drop vertices that carry no shape information: those closer together than
    * `avgWidth * minLenWidRatio` (but at least `minLenFloor`), and those whose
    * width and direction barely differ from both neighbours (`widthThreshold`
    * as a relative width difference, `angleThreshold` as `1 - dot` of the
    * directions). The first and last vertex are always kept.
    *
    * `minLenFloor` defaults to `1.0`, i.e. "never bother below one pixel" for a
    * line measured in pixels. A line in any other unit has to say so — left at
    * the default, a line laid out in normalized units is thinned down to its
    * first and last vertex.
    */
  def cleanup(
      minLenWidRatio: Double,
      widthThreshold: Double,
      angleThreshold: Double,
      minLenFloor: Double = 1.0,
  )(using Lerp[T]): Line[T] =
    var travelled = 0.0
    flatMapWithNeighbours: (prev, curr, next) =>
      if prev.isNull || next.isNull then Arr(curr.copy)
      else
        val p = prev.get
        val nx = next.get
        val len = p.len + curr.len + travelled
        val avgWidth = (p.width + curr.width * 2.0 + nx.width) / 4.0
        val minLen = (avgWidth * minLenWidRatio).max(minLenFloor)

        if len < minLen then
          // too close to the last kept vertex — skip, but remember the
          // distance already travelled so it counts towards the next one
          travelled += p.len
          Arr[LineVertex[T]]()
        else if p.len + travelled < minLen then
          // the gap only clears minLen part way into the next segment —
          // emit an interpolated vertex exactly there
          val dist = curr.len - (len - minLen)
          travelled = -dist
          Arr(lerpVert(curr, nx, dist / curr.len))
        else
          travelled = 0.0
          val sameWidthPrev = p.width == curr.width ||
            (1.0 - p.width / curr.width).abs < widthThreshold
          val sameWidthNext = curr.width == nx.width ||
            (1.0 - nx.width / curr.width).abs < widthThreshold
          val sameDirection = 1.0 - p.dir.dot(curr.dir) < angleThreshold
          if sameWidthPrev && sameWidthNext && sameDirection then
            Arr[LineVertex[T]]()
          else Arr(curr.copy)

  /** Split the line into fragments wherever it turns by more than
    * `angleThreshold` radians. The corner vertex is duplicated into both
    * fragments (ending one, starting the next), and each fragment's
    * [[lenOffset]] continues where the previous one ended — so
    * `toBufferedGeometries` can render them as one continuous stroke.
    */
  def splitAtAngle(angleThreshold: Double): Arr[Line[T]] =
    val lines = Arr[Line[T]]()
    val cosThreshold = angleThreshold.cos
    var line = new Line[T](defaultWidth, lenOffset, defaultData)
    var prev: Opt[LineVertex[T]] = null
    var offset = lenOffset
    var i = 0
    while i < verts.length do
      val v = verts(i)
      line.addVertRaw(v.copy)
      if prev.notNull then
        val p = prev.get
        if v.dir.dot(p.dir) <= cosThreshold then
          offset += line.totalLength
          // the fragment ends here, so its last vertex points backwards along
          // the incoming segment rather than around the corner
          line.last.dir = p.dir
          lines.push(line)
          line = new Line[T](defaultWidth, offset, defaultData)
          line.addVertRaw(v.copy)
      prev = v
      i += 1
    lines.push(line)
    lines

// ===========================================================================
// Line -> BufferedGeometry
// ===========================================================================

/** Vertex schema produced by `toBufferedGeometry` — use it as the shader
  * `Attribs`. `uv.x` runs `0..1` along the whole stroke (all fragments),
  * `localUv.x` along this fragment alone; `y` is `0`/`1` across the stroke and
  * `0.5` at the two end caps. `length` is the accumulated distance in the
  * line's own units.
  */
type LineAttribs = (
    position: Vec2,
    width: Double,
    length: Double,
    uv: Vec2,
    localUv: Vec2,
)

/** The buffer-field tuple `LineAttribs` derives — spelled out so
  * `toBufferedGeometry` can name its return type.
  */
type LineAttribsBuffer =
  Vec2Buffer *: (F32 *: EmptyTuple) *: (F32 *: EmptyTuple) *: Vec2Buffer *:
    Vec2Buffer *: EmptyTuple

private def normalOf(dir: Vec2): Vec2 = Vec2(dir.y, -dir.x)

private def cross2d(a: Vec2, b: Vec2): Double = a.x * b.y - a.y * b.x

private def writeLineVert(
    ref: StructRef[LineAttribsBuffer],
    pos: Vec2,
    width: Double,
    length: Double,
    uvX: Double,
    uvY: Double,
    localUvX: Double,
): Unit =
  ref.set0((pos.x, pos.y))
  ref.set1(Tuple1(width.toFloat))
  ref.set2(Tuple1(length.toFloat))
  ref.set3((uvX, uvY))
  ref.set4((localUvX, uvY))

object Line:
  def apply(defaultWidth: Double): Line[Unit] =
    new Line(defaultWidth, 0.0, ())

  def apply(defaultWidth: Double, lenOffset: Double): Line[Unit] =
    new Line(defaultWidth, lenOffset, ())

  def apply[T](
      defaultWidth: Double,
      lenOffset: Double,
      defaultData: T,
  ): Line[T] = new Line(defaultWidth, lenOffset, defaultData)

  /** A `Line[Unit]` of uniform `width` through `points`. */
  def fromPoints(width: Double, points: Arr[Vec2]): Line[Unit] =
    val line = new Line(width, 0.0, ())
    var i = 0
    while i < points.length do
      line.add(points(i))
      i += 1
    line

  extension [T](line: Line[T])
    /** Expand the line into a triangle-strip quad mesh with mitre joins, ready
      * for `painter.form(geometry = …, topology =
      * PrimitiveTopology.TriangleStrip)`. The shader `Attribs` is
      * [[LineAttribs]].
      *
      * The last four parameters describe this line's place in a longer stroke;
      * `toBufferedGeometries` fills them in for you.
      *
      * @param smoothDepth
      *   how many bevel passes to run over the generated outline (0 = hard
      *   mitres)
      * @param smoothAngleThreshold
      *   minimum turn (as `1 - dot`) a corner needs before it gets bevelled
      * @param smoothMinLength
      *   outline segments shorter than this are never bevelled
      * @param totalLength
      *   stroke length `uv.x` is normalized against; defaults to this line's
      *   own length. Set it to the sum over all fragments for a continuous
      *   `uv.x`.
      * @param prevDirection
      *   direction the preceding fragment arrives with — extends the start cap
      *   so the two fragments meet without a gap
      * @param nextDirection
      *   direction the following fragment leaves with, for the end cap
      * @param swapTextureOrientation
      *   flips `uv.y`, alternated per fragment by `toBufferedGeometries`
      */
    def toBufferedGeometry(
        smoothDepth: Int = 0,
        smoothAngleThreshold: Double = 0.05,
        smoothMinLength: Double = 3.0,
        totalLength: Opt[Double] = null,
        prevDirection: Opt[Vec2] = null,
        nextDirection: Opt[Vec2] = null,
        swapTextureOrientation: Boolean = false,
    ): BufferedGeometry[LineAttribsBuffer] =
      // The two outlines of the stroke, carrying the accumulated stroke length
      // as their vertex data so it survives the bevel passes below.
      var topLine = new Line[Double](line.defaultWidth, 0.0, 0.0)
      var bottomLine = new Line[Double](line.defaultWidth, 0.0, 0.0)
      var lineLength = line.lenOffset

      val src = line.verts
      val n = src.length
      var i = 0
      while i < n do
        val v = src(i)
        val hasPrev = i > 0
        val hasNext = i < n - 1

        // --- mitre positions: offset along the bisector of the two segment
        // normals, capped at 5x the width so needle-sharp turns stay finite
        val nextNormal = normalOf(v.dir)
        var normal = nextNormal
        var offset = v.width
        if hasPrev then
          val prevDir = src(i - 1).dir
          if prevDir.x != v.dir.x || prevDir.y != v.dir.y then
            val prevNormal = normalOf(prevDir)
            normal = (nextNormal + prevNormal).normalize
            offset = (v.width / normal.dot(prevNormal)).min(v.width * 5.0)

        var top = normal * offset + v.pos
        var bottom = normal * -offset + v.pos

        if !hasPrev then
          // start cap: a degenerate vertex on the centre line
          topLine.add(v.pos, v.width, lineLength)
          bottomLine.add(v.pos, v.width, lineLength)

          if prevDirection.notNull then
            // extend the cap so it meets the preceding fragment's end
            val prevDir = prevDirection.get
            val c = v.width / (prevDir * -1.0 + v.dir).normalize.dot(v.dir)
            val a = (c * c - v.width * v.width).sqrt
            if a > 0.001 then
              if cross2d(v.dir, prevDir) > 0.0 then
                top = top + v.dir * -a
                bottom = bottom + v.dir * a
              else
                top = top + v.dir * a
                bottom = bottom + v.dir * -a

        if !hasNext && nextDirection.notNull then
          val nextDir = nextDirection.get
          val c = v.width / (v.dir * -1.0 + nextDir).normalize.dot(nextDir)
          val a = (c * c - v.width * v.width).sqrt
          if a > 0.001 then
            if cross2d(nextDir, v.dir) > 0.0 then
              top = top + v.dir * a
              bottom = bottom + v.dir * -a
            else
              top = top + v.dir * -a
              bottom = bottom + v.dir * a

        topLine.add(top, v.width, lineLength)
        bottomLine.add(bottom, v.width, lineLength)

        if !hasNext then
          // end cap
          topLine.add(v.pos, v.width, lineLength)
          bottomLine.add(v.pos, v.width, lineLength)

        lineLength += v.len
        i += 1

      var d = 0
      while d < smoothDepth do
        topLine = topLine.smoothEdges(
          0.25,
          smoothMinLength,
          smoothAngleThreshold,
        )
        bottomLine = bottomLine.smoothEdges(
          0.25,
          smoothMinLength,
          smoothAngleThreshold,
        )
        d += 1

      val uvLength = totalLength.getOr(lineLength)
      val localLength = line.plannedLength.getOr(line.totalLength)
      val topCount = topLine.vertCount
      val bottomCount = bottomLine.vertCount
      val out = StructArray.allocate[LineAttribsBuffer](topCount + bottomCount)
      val indices = Arr[Int]()

      // Zig-zag the two outlines into one triangle strip. `balance` keeps the
      // strip from skewing when the outlines have different vertex counts (the
      // bevel passes above add vertices to each side independently): whichever
      // side lags in accumulated length is the one that advances.
      var topIdx = 0
      var bottomIdx = 0
      var nextIdx = 0
      var topLen = 0.0
      var bottomLen = 0.0
      var balance = 0.0
      var topI = 0
      var bottomI = 0

      while topI < topCount || bottomI < bottomCount do
        if topI < topCount && balance <= 0.0 then
          val tv = topLine.get(topI)
          topLen = tv.data
          val uvY =
            if topI == 0 || topI == topCount - 1 then 0.5
            else if swapTextureOrientation then 1.0
            else 0.0
          writeLineVert(
            out(nextIdx),
            tv.pos,
            tv.width,
            topLen,
            topLen / uvLength,
            uvY,
            (topLen - line.lenOffset) / localLength,
          )
          indices.push(nextIdx)
          topIdx = nextIdx
          nextIdx += 1
          topI += 1
        else indices.push(topIdx)

        if bottomI < bottomCount && balance >= 0.0 then
          val bv = bottomLine.get(bottomI)
          bottomLen = bv.data
          val uvY =
            if bottomI == 0 || bottomI == bottomCount - 1 then 0.5
            else if swapTextureOrientation then 0.0
            else 1.0
          writeLineVert(
            out(nextIdx),
            bv.pos,
            bv.width,
            bottomLen,
            bottomLen / uvLength,
            uvY,
            (bottomLen - line.lenOffset) / localLength,
          )
          indices.push(nextIdx)
          bottomIdx = nextIdx
          nextIdx += 1
          bottomI += 1
        else indices.push(bottomIdx)

        balance = topLen - bottomLen

      BufferedGeometry(out, makeIndexArray(indices, topCount + bottomCount))

  extension [T](lines: Arr[Line[T]])
    /** Expand stroke fragments (typically from [[Line.splitAtAngle]]) into one
      * geometry each, threading the shared stroke length and the neighbouring
      * fragments' directions through so the result reads as a single continuous
      * brush mark. Feed straight into
      * `painter.form(geometries = …, topology = PrimitiveTopology.TriangleStrip)`.
      *
      * `totalLength` overrides the length `uv.x` is normalised against, which is
      * otherwise the sum of `lines`. Pass the finished stroke's length when
      * rendering a **partial** stroke — e.g. animating a brush travelling along
      * its path — so `uv.x` keeps its final scale instead of restretching over
      * the growing prefix every frame.
      */
    def toBufferedGeometries(
        smoothDepth: Int = 0,
        smoothAngleThreshold: Double = 0.05,
        smoothMinLength: Double = 3.0,
        totalLength: Opt[Double] = null,
    ): Arr[BufferedGeometry[LineAttribsBuffer]] =
      var total = 0.0
      if totalLength.notNull then total = totalLength.get
      else
        var i = 0
        while i < lines.length do
          total += lines(i).totalLength
          i += 1

      val out = Arr[BufferedGeometry[LineAttribsBuffer]]()
      var i = 0
      while i < lines.length do
        val prevDir: Opt[Vec2] = if i == 0 then null else lines(i - 1).last.dir
        val nextDir: Opt[Vec2] =
          if i == lines.length - 1 then null else lines(i + 1).first.dir
        out.push(
          lines(i).toBufferedGeometry(
            smoothDepth = smoothDepth,
            smoothAngleThreshold = smoothAngleThreshold,
            smoothMinLength = smoothMinLength,
            totalLength = total,
            prevDirection = prevDir,
            nextDirection = nextDir,
            swapTextureOrientation = i % 2 != 0,
          ),
        )
        i += 1
      out
