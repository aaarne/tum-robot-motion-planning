package tumrmp.geometry

import breeze.linalg.{DenseVector, Vector}

case class Rectangle(center: Vector[Double], width: Double, height: Double) extends ClosedShape {

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
