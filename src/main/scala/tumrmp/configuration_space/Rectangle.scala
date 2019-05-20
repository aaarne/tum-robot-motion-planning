package tumrmp.configuration_space

import breeze.linalg._

case class Rectangle(center: Vector[Double], width: Double, height: Double) {

  List(width, height) foreach (d => if (d < 0) throw new IllegalArgumentException("Must not be negative"))

  lazy val corners: List[Vector[Double]] = {
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

  def doesLineCollide(l: LineSegment): Boolean =
    lineSegments exists l.intersects


  lazy val lineSegments: List[LineSegment] = {
    val c = List(0, 2, 3, 1, 0) map corners
    (c zip c.tail) map {
      case (p1, p2) => LineSegment(p1, p2)
    }
  }

}
