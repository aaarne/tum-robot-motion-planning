package tumrmp.geometry

import breeze.linalg._

trait Polygon extends Plottable {

  override lazy val lineSegments: List[LineSegment] = {
    (vertices zip (vertices.tail :+ vertices.head)) map {
      case (p1, p2) => LineSegment(p1, p2)
    }
  }

  val vertices: List[Vector[Double]]

  def doesLineCollide(l: LineSegment): Boolean =
    lineSegments exists l.intersects

  lazy val complex: Boolean =
    lineSegments exists {l =>
      lineSegments.filterNot(l2 => l == l2) exists l.intersects
    }

}

case class Rectangle(center: Vector[Double], width: Double, height: Double) extends Polygon {

  List(width, height) foreach (d => if (d < 0) throw new IllegalArgumentException("Must not be negative"))

  override lazy val vertices: List[Vector[Double]] = {
    val unordered = for {
      xsign <- List(-1, 1)
      ysign <- List(-1, 1)
    } yield DenseVector[Double](center(0) + xsign * 0.5 * width, center(1) + ysign * 0.5 * height)
    List(0, 1, 3, 2) map unordered
  }

  def doesPointCollide(point: Vector[Double]): Boolean = {
    (List(0, 1) zip List(width, height)) forall {
      case (i, w) => point(i) >= center(i) - 0.5 * w && point(i) <= center(i) + 0.5 * w
    }
  }

}
