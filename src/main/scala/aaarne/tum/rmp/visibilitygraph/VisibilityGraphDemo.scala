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

  override val polygons: List[Polygon] = removeCollidingPolygons(convexPolygons ++ concavePolygons)
}

object ShowVisibilityGraph extends VisibilityGraphDemo
  with Runnable
  with Plotter {

  override def run(): Unit = {
    println(
      s"""There are ${polygons.size} polygons with $verticesPerPolygon vertices each.
         |In total there are ${(0 /: polygons) (_ + _.size)} vertices.
         |The visibility graph consists of ${visibilityGraph.size} edges.
         |Convex polygons are shown in green, concave polygons in red and the visibility graph in gray.
       """.stripMargin)

    val f = Figure("Random Polygon")

    f subplot 0 ++= polygons map { p =>
      plot(if (p.convex) "g" else "r")(p)
    }
    f subplot 0 ++= plotLineSegments(visibilityGraph, "180,180,180")

    f.refresh()
  }
}
