package aaarne.tum.rmp.pathplanning

import aaarne.tum.rmp.geometry.Polygon
import breeze.linalg._
import breeze.stats.distributions.Uniform

case class Point(x: Double, y: Double)

trait PathPlanner {

  val xdim: (Double, Double) = (-10.0, 10.0)
  val ydim: (Double, Double) = (-10.0, 10.0)


  implicit def point2vec(p: Point): Vector[Double] = DenseVector(p.x, p.y)

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



