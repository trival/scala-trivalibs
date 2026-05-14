package examples.geometry3d_scene

import trivalibs.graphics.buffers.*
import trivalibs.graphics.geometry.{*, given}
import trivalibs.graphics.math.cpu.*
import trivalibs.graphics.math.cpu.Mat4
import trivalibs.graphics.math.cpu.Vec3
import trivalibs.graphics.math.cpu.given
import trivalibs.graphics.math.gpu.{*, given}
import trivalibs.graphics.painter.*
import trivalibs.graphics.shader.dsl.{*, given}
import trivalibs.graphics.shader.{*, given}
import org.scalajs.dom.HTMLCanvasElement
import org.scalajs.dom.document
import trivalibs.utils.js.*
import trivalibs.utils.numbers.NumExt.given

import scala.scalajs.js.annotation.*

@JSExportTopLevel("main", moduleID = "geometry3d_scene")
def main(): Unit =
  val canvas = document.getElementById("canvas").asInstanceOf[HTMLCanvasElement]

  Painter.init(canvas): painter =>
    type SceneAttribs = (position: Vec3, normal: Vec3)
    type SceneVaryings = (normal: Vec3)
    type SceneUniforms = (viewProj: VertexUniform[Mat4])

    val shade = painter.shade[SceneAttribs, SceneVaryings, SceneUniforms]:
      program =>
        program.vert: ctx =>
          Block(
            ctx.out.position := ctx.bindings.viewProj * vec4(
              ctx.in.position,
              1.0,
            ),
            ctx.out.normal := ctx.in.normal,
          )
        program.frag[(n: Vec3, diffuse: Float, c: Vec3)]: ctx =>
          val n = ctx.locals.n
          val diffuse = ctx.locals.diffuse
          val c = ctx.locals.c
          val light = vec3(1.0, 2.0, 1.0).normalize
          Block(
            n := ctx.in.normal.normalize,
            diffuse := n.dot(light).max(0.0),
            c := vec3(0.75, 0.65, 0.45) * (diffuse * 0.85 + 0.15),
            ctx.out.color := vec4(c, 1.0),
          )

    // -------------------------------------------------------------------------
    // Terrain — 24×24 wave grid with smooth vertex normals
    // -------------------------------------------------------------------------

    val segments = 24
    val terrainGrid = new Grid[Vec3]()
    for gx <- 0 to segments do
      val col = Arr[Vec3]()
      for gz <- 0 to segments do
        val wx = (gx - segments / 2).toDouble
        val wz = (gz - segments / 2).toDouble
        val wy = (wx * 0.45).sin * (wz * 0.35).cos * 0.9
        col.push(Vec3(wx, wy, wz))
      terrainGrid.addCol(col)

    val terrainMesh = Mesh(terrainGrid.ccwQuads)

    val terrainGeo = toBufferedGeometry(
      terrainMesh,
      MeshBufferType.FaceVerticesWithVertexNormal,
    )
    val terrainShape = painter.shape(
      painter.form(geometry = terrainGeo),
      shade,
      cullMode = CullMode.Back,
    )

    // -------------------------------------------------------------------------
    // Boxes — flat face normals
    // -------------------------------------------------------------------------

    def makeBoxShape(box: Box) =
      val mesh = new Mesh[Vec3]()
      val faces = box.faces
      faces.foreach: (face, normal) =>
        mesh.addFace(face, normal)

      val geo = toBufferedGeometry(
        mesh,
        MeshBufferType.FaceVerticesWithFaceNormal,
      )
      painter.shape(
        painter.form(geometry = geo),
        shade,
        cullMode = CullMode.Back,
      )

    val towerShape = makeBoxShape(Box(Vec3(3.0, 1.5, 3.0), 1.0, 3.0, 1.0))
    val platformShape =
      makeBoxShape(Box(Vec3(8.0, 0.5, 4.5), 3.0, 1.0, 2.0))
    val wallShape = makeBoxShape(Box(Vec3(4.5, 0.5, 10.0), 1.5, 1.0, 4.0))

    // -------------------------------------------------------------------------
    // Spheres — smooth vertex normals
    // -------------------------------------------------------------------------

    def makeSphereShape(
        vSegs: Int,
        hSegs: Int,
        center: Vec3,
        radius: Double,
    ) =
      val mesh = sphereMesh(vSegs, hSegs)((p, _) => p * radius + center)
      val geo = toBufferedGeometry(
        mesh,
        MeshBufferType.FaceVerticesWithVertexNormal,
      )
      painter.shape(
        painter.form(geometry = geo),
        shade,
        cullMode = CullMode.Back,
      )

    val largeSphereShape = makeSphereShape(14, 20, Vec3(6.0, 2.5, 6.0), 0.9)
    val smallSphereShape = makeSphereShape(10, 14, Vec3(10.0, 1.5, 9.0), 0.5)

    // -------------------------------------------------------------------------
    // Camera and panel
    // -------------------------------------------------------------------------

    val sceneShapes = Arr(
      terrainShape,
      towerShape,
      platformShape,
      wallShape,
      largeSphereShape,
      smallSphereShape,
    )

    val panel = painter
      .panel(
        clearColor = (0.05, 0.07, 0.12, 1.0),
        depthTest = true,
        shapes = sceneShapes,
      )

    val view = Mat4.lookAt(-5.0, 15.0, 25.0, 3.0, 1.0, 5.0, 0.0, 1.0, 0.0)

    painter.onResize: (w, h) =>
      val p = Mat4.perspective(Math.PI / 4.0, w / h, 0.1, 200.0)
      panel.bind("viewProj" := p * view)
      painter.paint(panel)
      painter.show(panel)
