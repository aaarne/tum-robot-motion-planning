package aaarne.tum.rmp.pathplanning

import java.awt.Color

import aaarne.tum.rmp.geometry.LineSegment
import breeze.linalg._
import breeze.plot._

trait RandomTreePathPlanner extends PathPlanner {

  val stepSize = 0.2
  val maxTrials = 10000

  case class RRTTree(tree: Tree[Int], coordinates: Map[Int, Point]) {

    def grow: RRTTree = {
      val randomPoint: Point = samplePoint
      val (n, closest) = coordinates.minBy(_._2 dist randomPoint)

      val p1: Vector[Double] = closest
      val p2: Vector[Double] = randomPoint
      val newVector = p1 + stepSize * (p2 - p1) / norm(p2 - p1)
      val newPoint = Point(newVector(0), newVector(1))
      val newIndex = coordinates.keys.max + 1

      if (haveEyeContact(newPoint, closest))
        RRTTree(tree.add(parent = n, value = newIndex), coordinates + (newIndex -> newPoint))
      else
        this.grow // try again tail-recursively
    }
  }

  object RRTTreeStream {
    /**
      * Generate infinite stream of RRT Trees rooted at start
      *
      * @return
      */
    def from(start: Point): Stream[RRTTree] =
      Stream.iterate(RRTTree(Leaf(0), Map(0 -> start)))(tree => tree.grow)
  }

  var pastTrees: List[RRTTree] = Nil

  def haveEyeContact(p1: Point, p2: Point): Boolean =
    obstacles forall (o => !o.lineCollides(LineSegment(p1, p2)))

  def canSeePoint(point: Point)(rrt: RRTTree): Boolean = rrt match {
    case RRTTree(_, coordinates) => coordinates.values exists hasEyeContactTo(point)
  }

  def hasEyeContactTo(p: Point)(q: Point): Boolean = haveEyeContact(p, q)
}

object RandomTreeGrowthDemo extends RandomTreePathPlanner with PathPlannerDemo {

  val n = 500
  override val stepSize: Double = 0.5

  override def run(): Unit = {
    val f = Figure("RRT Demo")

    val startPoint = sampleFreePoint

    plotWorld(f subplot 0)
    f subplot 0 += scatter(DenseVector(startPoint.x), DenseVector(startPoint.y), _ => 0.5, _ => Color.RED)

    val rrt = RRTTreeStream.from(startPoint).take(n).last

    def traverseTree(t: Tree[Int]): List[Int] = t match {
      case Leaf(v) => v :: Nil
      case Branch(v, children) => children.flatMap(c => (v :: traverseTree(c)) :+ v)
    }

    val orderedPoints = traverseTree(rrt.tree) map rrt.coordinates

    f subplot 0 += breeze.plot.plot(
      x = orderedPoints.map(_.x),
      y = orderedPoints.map(_.y),
      style = '-',
      colorcode = "r"
    )
  }

  override def plan(start: Point, destination: Point): Option[Path] = ???
}
