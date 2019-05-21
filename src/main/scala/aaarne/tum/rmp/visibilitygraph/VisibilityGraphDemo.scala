package aaarne.tum.rmp.visibilitygraph

import aaarne.tum.rmp.geometry.{Plotter, Polygon}
import breeze.plot.Figure

trait VisibilityGraphDemoSettings {
  val verticesPerPolygon = 6
  val nConvexPolygons = 3
  val nConcavePolygons = 3
}

class VisibilityGraphDemo extends RandomPolygons
  with VisibilityGraph
  with VisibilityGraphDemoSettings {
  val convexPolygons = sampleConvexPolygons(nConvexPolygons, verticesPerPolygon)
  val concavePolygons = sampleConcavePolygons(nConcavePolygons, verticesPerPolygon)

  override val polygons: List[Polygon] = convexPolygons ++ concavePolygons
}

object ShowVisibilityGraph extends VisibilityGraphDemo
  with Runnable
  with Plotter {

  override def run(): Unit = {
    println(
      s"""There are ${convexPolygons.size} convex and ${concavePolygons.size} polygons with $verticesPerPolygon vertices each.
         |In total there are ${(0 /: polygons) (_ + _.size)} vertices.
         |The visibility graph consists of ${visibilityGraph.size} edges.
       """.stripMargin)

    val f = Figure("Random Polygon")

    f subplot 0 ++= convexPolygons.zipWithIndex map {
      case (poly, i) => plot("green", s"Convex Polygon ${i + 1}")(poly)
    }
    f subplot 0 ++= concavePolygons.zipWithIndex map {
      case (poly, i) => plot("blue", s"Concave Polygon ${i + 1}")(poly)
    }
    f subplot 0 ++= plotLineSegments(visibilityGraph, "red")

    f.refresh()
  }
}
