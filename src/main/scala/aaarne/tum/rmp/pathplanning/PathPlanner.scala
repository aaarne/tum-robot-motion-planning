package aaarne.tum.rmp.pathplanning

import aaarne.tum.rmp.geometry.Polygon
import breeze.linalg._
import breeze.numerics.sqrt
import breeze.stats.distributions.Uniform

case class Point(x: Double, y: Double) {
  def dist(other: Point): Double = sqrt((x - other.x) * (x - other.x) + (y - other.y) * (y - other.y))
}

trait PathPlanner {

  val xdim: (Double, Double) = (-10.0, 10.0)
  val ydim: (Double, Double) = (-10.0, 10.0)


  implicit def point2vec(p: Point): Vector[Double] = DenseVector(p.x, p.y)

  val obstacles: List[Polygon]
  type Path = List[Point]

  val xSampler = Uniform(xdim._1, xdim._2)
  val ySampler = Uniform(ydim._1, ydim._2)

  def samplePoint: Point = Point(xSampler.sample(), ySampler.sample())

  def sampleFreePoint: Point =
    Stream.continually(samplePoint).filter {
      p => obstacles forall (o => !o.contains(p))
    }.head

  def plan(start: Point, destination: Point): Option[Path]
}



