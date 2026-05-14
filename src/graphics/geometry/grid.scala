package trivalibs.graphics.geometry

import trivalibs.utils.js.*
import trivalibs.utils.numbers.IntExt.given

opaque type CoordOps = Int

object CoordOps:
  val ClampToEdge: CoordOps = 0
  val CircleCols: CoordOps = 1
  val CircleRows: CoordOps = 2
  val CircleAll: CoordOps = 3

  extension (c: CoordOps)
    inline def circleCols: Boolean = (c & 1) != 0
    inline def circleRows: Boolean = (c & 2) != 0

    def adjust(x: Int, y: Int, w: Int, h: Int): (Int, Int) =
      val nx = if c.circleCols then ((x % w) + w) % w else x.clamp(0, w - 1)
      val ny = if c.circleRows then ((y % h) + h) % h else y.clamp(0, h - 1)
      (nx, ny)

class Vertex[T](
    val x: Int,
    val y: Int,
    val value: T,
    private val grid: Grid[T],
):
  def next(dx: Int, dy: Int): Opt[Vertex[T]] =
    val v = grid.vertex(x + dx, y + dy)
    if v.x == x && v.y == y then null else v
  inline def left: Opt[Vertex[T]] = next(-1, 0)
  inline def right: Opt[Vertex[T]] = next(1, 0)
  inline def top: Opt[Vertex[T]] = next(0, -1)
  inline def bottom: Opt[Vertex[T]] = next(0, 1)

class Grid[T](val coordOps: CoordOps = CoordOps.ClampToEdge):
  private val _cols: Arr[Arr[T]] = Arr()

  def width: Int = _cols.length
  def height: Int = if _cols.length == 0 then 0 else _cols(0).length

  def get(x: Int, y: Int): T =
    val (nx, ny) = coordOps.adjust(x, y, width, height)
    _cols(nx)(ny)

  def set(x: Int, y: Int, v: T): Unit =
    val (nx, ny) = coordOps.adjust(x, y, width, height)
    _cols(nx)(ny) = v

  def vertex(x: Int, y: Int): Vertex[T] =
    val (nx, ny) = coordOps.adjust(x, y, width, height)
    Vertex(nx, ny, _cols(nx)(ny), this)

  def col(x: Int): Arr[T] =
    val (nx, _) = coordOps.adjust(x, 0, width, height)
    _cols(nx)

  def row(y: Int): Arr[T] =
    val (_, ny) = coordOps.adjust(0, y, width, height)
    val result = Arr[T]()
    var i = 0
    while i < width do
      result.push(_cols(i)(ny))
      i += 1
    result

  def firstCol: Arr[T] = _cols(0)
  def lastCol: Arr[T] = _cols(width - 1)
  def firstRow: Arr[T] = row(0)
  def lastRow: Arr[T] = row(height - 1)

  def addCol(vals: Arr[T]): Unit =
    if width > 0 && vals.length != height then
      throw new IllegalArgumentException(
        s"addCol: length ${vals.length} != height $height",
      )
    _cols.push(vals)

  def addRow(vals: Arr[T]): Unit =
    if vals.length != width then
      throw new IllegalArgumentException(
        s"addRow: length ${vals.length} != width $width",
      )
    var i = 0
    while i < width do
      _cols(i).push(vals(i))
      i += 1

  def map[B](f: Vertex[T] => B): Grid[B] =
    val g = new Grid[B](coordOps)
    var x = 0
    while x < width do
      val newCol = Arr[B]()
      var y = 0
      while y < height do
        newCol.push(f(vertex(x, y)))
        y += 1
      g._cols.push(newCol)
      x += 1
    g

  def flatMapCols[B](f: Arr[Vertex[T]] => Arr[Arr[B]]): Grid[B] =
    val g = new Grid[B](coordOps)
    var x = 0
    while x < width do
      val colVerts = Arr[Vertex[T]]()
      var y = 0
      while y < height do
        colVerts.push(vertex(x, y))
        y += 1
      val newCols = f(colVerts)
      var i = 0
      while i < newCols.length do
        g._cols.push(newCols(i))
        i += 1
      x += 1
    g

  def flatMapRows[B](f: Arr[Vertex[T]] => Arr[Arr[B]]): Grid[B] =
    val g = new Grid[B](coordOps)
    var x = 0
    while x < width do
      g._cols.push(Arr[B]())
      x += 1
    var y = 0
    while y < height do
      val rowVerts = Arr[Vertex[T]]()
      var i = 0
      while i < width do
        rowVerts.push(vertex(i, y))
        i += 1
      val newRows = f(rowVerts)
      var j = 0
      while j < newRows.length do
        val newRow = newRows(j)
        var c = 0
        while c < g._cols.length do
          g._cols(c).push(newRow(c))
          c += 1
        j += 1
      y += 1
    g

  def quadCount: (Int, Int) =
    val qw = if coordOps.circleCols then width else width - 1
    val qh = if coordOps.circleRows then height else height - 1
    (qw, qh)

  def ccwQuads: Arr[Quad[T]] =
    val (qw, qh) = quadCount
    val result = Arr[Quad[T]]()
    var x = 0
    while x < qw do
      var y = 0
      while y < qh do
        result.push(
          Quad(get(x, y), get(x, y + 1), get(x + 1, y + 1), get(x + 1, y)),
        )
        y += 1
      x += 1
    result

  def cwQuads: Arr[Quad[T]] =
    val (qw, qh) = quadCount
    val result = Arr[Quad[T]]()
    var x = 0
    while x < qw do
      var y = 0
      while y < qh do
        result.push(
          Quad(get(x, y), get(x + 1, y), get(x + 1, y + 1), get(x, y + 1)),
        )
        y += 1
      x += 1
    result

  inline def quads: Arr[Quad[T]] = ccwQuads

object Grid:
  def fromCols[T](
      cols: Arr[Arr[T]],
      coordOps: CoordOps = CoordOps.ClampToEdge,
  ): Grid[T] =
    val g = new Grid[T](coordOps)
    var i = 0
    while i < cols.length do
      g.addCol(cols(i))
      i += 1
    g

  def fromRows[T](
      rows: Arr[Arr[T]],
      coordOps: CoordOps = CoordOps.ClampToEdge,
  ): Grid[T] =
    val g = new Grid[T](coordOps)
    if rows.length == 0 then return g
    val w = rows(0).length
    var x = 0
    while x < w do
      val newCol = Arr[T]()
      var y = 0
      while y < rows.length do
        newCol.push(rows(y)(x))
        y += 1
      g._cols.push(newCol)
      x += 1
    g

extension [T: Lerp](g: Grid[T])
  def subdivide(countX: Int, countY: Int): Grid[T] =
    val g1 =
      if countX <= 0 then g
      else
        val newCols = Arr[Arr[T]]()
        var x = 0
        while x < g.width do
          val srcCol = Arr[T]()
          var y = 0
          while y < g.height do
            srcCol.push(g.get(x, y))
            y += 1
          newCols.push(srcCol)
          if x < g.width - 1 then
            var k = 1
            while k <= countX do
              val t = k.toDouble / (countX + 1)
              val interpolated = Arr[T]()
              var yy = 0
              while yy < g.height do
                interpolated.push(g.get(x, yy).lerp(g.get(x + 1, yy), t))
                yy += 1
              newCols.push(interpolated)
              k += 1
          x += 1
        Grid.fromCols(newCols, g.coordOps)
    if countY <= 0 then g1
    else
      val newRows = Arr[Arr[T]]()
      var y = 0
      while y < g1.height do
        newRows.push(g1.row(y))
        if y < g1.height - 1 then
          var k = 1
          while k <= countY do
            val t = k.toDouble / (countY + 1)
            val interpolated = Arr[T]()
            var x = 0
            while x < g1.width do
              interpolated.push(g1.get(x, y).lerp(g1.get(x, y + 1), t))
              x += 1
            newRows.push(interpolated)
            k += 1
        y += 1
      Grid.fromRows(newRows, g1.coordOps)
