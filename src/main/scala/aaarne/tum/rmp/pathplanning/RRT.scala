package aaarne.tum.rmp.pathplanning

import aaarne.tum.rmp.geometry.LineSegment
import breeze.linalg._

trait RRT extends PathPlanner {

  val stepSize = 0.5

  case class RRTTree(tree: Tree[Int], coordinates: Map[Int, Point]) {

    def addSample(point: Point): RRTTree = {
      val closest = coordinates.minBy(_._2 dist point)
      val p1 = DenseVector(closest._2.x, closest._2.y)
      val p2: Vector[Double] = point
      val newCoordinate = p1 + stepSize * (p2 - p1)
      if (obstacles exists (_.lineCollides(LineSegment(newCoordinate, p1)))) this
      else {
        val i = coordinates.keys.max + 1
        RRTTree(tree.add(closest._1, i), coordinates + (i -> Point(newCoordinate(0), newCoordinate(1))))
      }
    }
  }

  override def plan(start: Point, destination: Point): Option[Path] = {
    val tree = RRTTree(Leaf(1), Map(1 -> start))

    val result = Stream.iterate(tree)(t => t.addSample(samplePoint)).find {
      case RRTTree(_, coordinates) => coordinates.values exists { p =>
        obstacles forall (o => !o.lineCollides(LineSegment(p, destination)))
      }
    }

    result map {
      case RRTTree(t, c) =>
        val finalNode = c.filter {
          case (i, p) => obstacles forall (o => !o.lineCollides(LineSegment(p, destination)))
        }.head._1

        (t pathTo finalNode map c) :+ destination
    }
  }
}

object RRTDemo extends PathPlannerDemo with RRT {
  override val title: String = "RRT Pathplanning"
}
