package aaarne.tum.rmp.pathplanning

import java.awt.Color

import aaarne.tum.rmp.geometry.{LineSegment, Plotter, Polygon}
import aaarne.tum.rmp.visibilitygraph.RandomPolygons
import breeze.linalg._
import breeze.plot.{Figure, Plot, scatter}
import breeze.stats.distributions.Uniform

case class Point(x: Double, y: Double)

trait PathPlanner {

  val xdim: (Double, Double) = (-10.0, 10.0)
  val ydim: (Double, Double) = (-10.0, 10.0)


  implicit def point2vec(p: Point): Vector[Double] =
    DenseVector(p.x, p.y)

  val obstacles: List[Polygon]

  type Path = List[Point]

  def sampleFreePoint: Point = {
    val xSampler = Uniform(xdim._1, xdim._2)
    val ySampler = Uniform(ydim._1, ydim._2)

    Stream.continually(Point(xSampler.sample(), ySampler.sample())).filter {
      p => obstacles forall (o => !o.contains(p))
    }.head
  }

  def plan(start: Point, destination: Point): Option[Path]
}

trait PathPlanningEvaluation extends PathPlanner with Plotter with Runnable with RandomPolygons {

  override val positionSampling: Uniform = Uniform(-8, 8)
  override val radiusSampling: Uniform = Uniform(1, 3)

  override val obstacles: List[Polygon] =
    removeCollidingPolygons(Stream.continually(samplesArbitraryPolygon()).take(20).toList)


  def plotGraph(f: Plot, verbose: Boolean = false): Unit


  override def run(): Unit = {
    val f = Figure("Path Planning Map")

    f subplot 0 ++= obstacles map plot(color = "red")

    plotGraph(f subplot 0)

    List(Color.RED, Color.ORANGE, Color.GREEN).zipWithIndex foreach {
      case (color, i) =>

        val start: Point = sampleFreePoint
        val destination: Point = sampleFreePoint

        f subplot 0 += scatter(DenseVector(start.x), DenseVector(start.y), _ => 0.4, _ => color)
        f subplot 0 += scatter(DenseVector(destination.x), DenseVector(destination.y), _ => 0.4, _ => color)

        plan(start, destination) match {
          case None => println(s"No Solution found for the $i. path.")
          case Some(solution) =>
            f subplot 0 ++= plotLineSegments((solution zip solution.tail) map {
              case (p1, p2) => LineSegment(p1, p2)
            }, color = s"${color.getRed},${color.getGreen},${color.getBlue}")
        }
    }

    f.refresh()
  }

}


