package aaarne.tum.rmp.pathplanning

import java.awt.Color

import aaarne.tum.rmp.geometry.LineSegment
import breeze.linalg._
import breeze.plot._

trait RRT extends PathPlanner {

  val stepSize = 0.2

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

  var pastTrees: List[RRTTree] = Nil

  override def plan(start: Point, destination: Point): Option[Path] = {
    val tree = RRTTree(Leaf(0), Map(0 -> start))

    val result = Stream.iterate(tree)(t => t.addSample(samplePoint)).find {
      case RRTTree(_, coordinates) => coordinates.values exists { p =>
        obstacles forall (o => !o.lineCollides(LineSegment(p, destination)))
      }
    }

    pastTrees = result match {
      case None => pastTrees
      case Some(t) => pastTrees :+ t
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

  override def plotGraph(f: Plot, verbose: Boolean): Unit = {

    pastTrees.zipWithIndex foreach {
      case (tree, index) => println(s"Tree ${index + 1} has ${tree.coordinates.keys.max + 1} nodes.")
    }

    val points = pastTrees flatMap {
      case RRTTree(tree, coordinates) => coordinates.values
    }

    if (verbose)
      f += scatter(DenseVector(points map (_.x): _*), DenseVector(points map (_.y): _*), _ => 0.3, _ => Color.LIGHT_GRAY)
  }
}
