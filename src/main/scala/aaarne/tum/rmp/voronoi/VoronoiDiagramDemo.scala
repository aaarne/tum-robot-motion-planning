package aaarne.tum.rmp.voronoi

import java.awt.Color

import aaarne.tum.rmp.geometry.{Plotter, Rectangle}
import aaarne.tum.rmp.visibilitygraph.RandomPolygons
import breeze.linalg._
import breeze.plot.{Figure, PaintScale, image}
import breeze.stats.distributions.Uniform

trait VoronoiDiagramDemoSettings {
  val nPolygons = 2

  val mapSize = ((-1.0, 1.0), (-1.0, 1.0))
  val resolution = 500

  val maxRadius = 0.5
}

trait VoronoiDiagramDemo extends VoronoiDiagram with RandomPolygons with VoronoiDiagramDemoSettings {

  override val positionSampling = Uniform(mapSize._1._1, mapSize._1._2)
  override val radiusSampling = Uniform(0.1, maxRadius)

  val polygons = Stream.continually(samplePolygon()) take nPolygons

  val x = linspace(mapSize._1._1, mapSize._1._2, resolution)
  val y = linspace(mapSize._2._1, mapSize._2._2, resolution)

  val mapBorder = Rectangle(DenseVector(0.0, 0.0), 2.0, 2.0)

  override val map = DenseMatrix.tabulate(x.size, y.size) {
    case (i, j) => polygons exists (_ contains DenseVector(x(i), y(j)))
  }

}

object ShowVoronoiDiagramDemo extends VoronoiDiagramDemo with Plotter with Runnable {
  override def run(): Unit = {

    val f = Figure("Raw map")
    f subplot 0 ++= polygons map plot(Color.BLACK)
    f subplot 0 += plot(Color.RED)(mapBorder)

    val f2 = Figure("Discretized")
    f2 subplot 0 += image((map map (if (_) 1.0 else 0.0)).t)

    val f3 = Figure("Voronoi Diagram")
    f3 subplot 0 += image(-1.0 * voronoi.t, scale = Utils.createColorcode(-1.0 * voronoi, PaintScale.Heat))

    f.refresh()
    f2.refresh()
    f3.refresh()
  }
}
