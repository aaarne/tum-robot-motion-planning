package tumrmp.geometry

import breeze.linalg._
import tumrmp.configuration_space.Plottable

trait ClosedShape extends Plottable {

  override lazy val lineSegments: List[LineSegment] = {
    (vertices zip (vertices.tail :+ vertices.head)) map {
      case (p1, p2) => LineSegment(p1, p2)
    }
  }
  val vertices: List[Vector[Double]]

  def doesLineCollide(l: LineSegment): Boolean =
    lineSegments exists l.intersects

}

