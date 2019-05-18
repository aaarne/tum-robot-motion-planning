package ex1.configuration_space

import breeze.linalg._

case class Rectangle(center: Vector[Double], width: Double, height: Double) {

  def corners: List[Vector[Double]] = {
    for {
      xsign <- List(-1, 1)
      ysign <- List(-1, 1)
    } yield DenseVector[Double](center(0) + xsign*0.5*width, center(1) + ysign*0.5*height)
  }

  def doesPointCollide(point: Vector[Double]): Boolean = {
    (List(0, 1) zip List(width, height)) forall {
      case (i, w) => point(i) >= center(i) - 0.5 * w && point(i) <= center(i) + 0.5 * w
    }
  }

  def doesLineCollide(p1: Vector[Double], p2: Vector[Double]): Boolean = {
    val x = linspace(p1(0), p2(0))
    val y = linspace(p1(1), p2(1))
    (x.valuesIterator zip y.valuesIterator) exists {
      case (x1, y1) => doesPointCollide(DenseVector(x1, y1))
    }
  }


}
