package aaarne.tum.rmp.pathplanning

import java.awt.Color

import aaarne.tum.rmp.geometry.{LineSegment, Plotter, Polygon, Rectangle}
import aaarne.tum.rmp.visibilitygraph.RandomPolygons
import breeze.linalg._
import breeze.plot._
import breeze.stats.distributions.Uniform

trait PathPlannerDemo extends PathPlanner with Plotter with Runnable with RandomPolygons {

  val nObstacles = 18
  val title = "Path Planning Map"
  val verbose = false
  val trials = List(Color.GREEN, Color.ORANGE, Color.RED) // number of queries. Add a color for each trial here

  override val positionSampling: Uniform = Uniform(-8, 8)
  override val radiusSampling: Uniform = Uniform(1, 3)

  override val obstacles: List[Polygon] = {
    val noObstacles: List[Polygon] = Nil
    Stream.iterate(noObstacles)(obs => sampleNonCollidingPolygon(obs) :: obs)
      .take(nObstacles).last
  }

  def plotSingleQuery(f: Plot, c: Color, verbose: Boolean = false): Unit = {}

  def plotMultipleQuery(f: Plot, verbose: Boolean = false): Unit = {}

  override def run(): Unit = {

    println(s"---------- $title ---------")
    val f = Figure(title)

    f subplot 0 ++= obstacles map plot(color = Color.BLACK)
    f subplot 0 += plot(color = Color.LIGHT_GRAY)(Rectangle(
      center = DenseVector((xdim._1 + xdim._2) / 2, (ydim._1 + ydim._2) / 2),
      width = xdim._2 - xdim._1,
      height = ydim._2 - ydim._1)
    )

    trials.zipWithIndex foreach { case (color, i) =>

        val start: Point = sampleFreePoint
        val destination: Point = sampleFreePoint

      f subplot 0 += scatter(DenseVector(start.x), DenseVector(start.y), _ => 0.5, _ => color)
      f subplot 0 += scatter(DenseVector(destination.x), DenseVector(destination.y), _ => 0.5, _ => color.darker())

        plan(start, destination) match {
          case None => println(s"No Solution found for the ${i + 1}. path.")
          case Some(solution) =>
            println(s"Solution containing ${solution.size} path segments found for the ${i + 1}. path.")
            f subplot 0 ++= plotLineSegments((solution zip solution.tail) map {
              case (p1, p2) => LineSegment(p1, p2)
            }, color = color.darker())
        }

      plotSingleQuery(f subplot 0, color, verbose = verbose)
    }

    plotMultipleQuery(f subplot 0, verbose = verbose)

    f.refresh()
    println()
  }
}
