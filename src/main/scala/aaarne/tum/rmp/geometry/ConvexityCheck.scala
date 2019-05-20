package aaarne.tum.rmp.geometry

import breeze.linalg._
import breeze.numerics.{abs, atan2, round}

trait ConvexityCheck {

  def isConvex(vertices: List[Vector[Double]]): Boolean = {

    val v1 = vertices(vertices.size - 2)
    val v2 = vertices.last
    val initialDirection = atan2(v2(1) - v1(1), v2(0) - v1(0))

    type AngleTraverseAccumulator = (Double, Vector[Double], List[Double]) // direction, previous vertex, angles

    def traverseVertices(acc: AngleTraverseAccumulator, v: Vector[Double]): AngleTraverseAccumulator =
      acc match {
        case (prev_dir, prev_vertex, a) =>
          val dir = atan2(v(1) - prev_vertex(1), v(0) - prev_vertex(0))
          val angle: Double = dir - prev_dir match {
            case x if x <= -math.Pi => x + 2 * math.Pi
            case x if x > math.Pi => x - 2 * math.Pi
            case x => x
          }
          (dir, v, angle :: a)
      }

    val initial = (atan2(v2(1) - v1(1), v2(0) - v1(0)), v2, List[Double]())

    val angles = (initial /: vertices) (traverseVertices)._3.reverse

    val orientation = if (angles.head > 0.0) 1.0 else -1.0
    val orientationOk = angles forall (a => orientation * a > 0.0)

    val angle_sum = (0.0 /: angles) (_ + _)

    orientationOk && abs(round(angle_sum / (2 * math.Pi))) == 1
  }

}
