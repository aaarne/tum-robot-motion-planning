package aaarne.tum.rmp.geometry

import breeze.linalg._

trait Shape {

  def lineSegments: List[LineSegment]

  def lineCollides(l: LineSegment): Boolean =
    lineSegments exists l.intersects

  def dist(p: Vector[Double]): Double =
    (lineSegments map (l => l dist p)).min

  lazy val complex: Boolean =
    lineSegments exists { l =>
      lineSegments filterNot (l2 => l == l2) exists l.intersects
    }
}

trait Polygon extends Shape with ConvexityCheck {

  val vertices: List[Vector[Double]]

  def size: Int = vertices.size

  override lazy val lineSegments: List[LineSegment] = {
    (vertices zip (vertices.tail :+ vertices.head)) map {
      case (p1, p2) => LineSegment(p1, p2)
    }
  }

  def contains(p: Vector[Double]): Boolean = {

    def f(c: Boolean, ls: LineSegment): Boolean = ls match {
      case LineSegment(p1, p2) =>
        p1(1) > p(1) match {
          case t if t != (p2(1) > p(1)) =>
            (p2(0) - p1(0)) * (p(1)-p1(1)) / (p2(1)-p1(1)) + p1(0) match {
              case x if x > p(0) => !c
              case _ => c
            }
          case _ => c
        }
    }

    (false /: lineSegments)(f)
  }

  def collidesWith(other: Polygon): Boolean = lineSegments exists other.lineCollides

  lazy val convex: Boolean = !complex && isConvex(vertices)

}

object Polygon {
  def apply(v: List[Vector[Double]]): Polygon = {
    new Polygon {
      override val vertices: List[Vector[Double]] = v
    }
  }
  def unapply(arg: Polygon): Option[List[Vector[Double]]] = Some(arg.vertices)
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


