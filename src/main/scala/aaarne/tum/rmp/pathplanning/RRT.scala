package aaarne.tum.rmp.pathplanning

import java.awt.Color

import aaarne.tum.rmp.geometry.LineSegment
import breeze.linalg._
import breeze.plot._

trait RandomTreePathPlanner extends PathPlanner {

  val stepSize = 0.2
  val maxTrials = 10000

  case class RRTTree(tree: Tree[Int], coordinates: Map[Int, Point]) {

    def addSample: RRTTree = {
      val randomPoint: Point = samplePoint
      val (n, closest) = coordinates.minBy(_._2 dist randomPoint)

      val p1: Vector[Double] = closest
      val p2: Vector[Double] = randomPoint
      val newPoint = p1 + stepSize * (p2 - p1)
      val sampleImpossible = obstacles exists (_.lineCollides(LineSegment(newPoint, p1)))

      if (sampleImpossible)
        this.addSample // try again tail-recursively
      else {
        val newIndex = coordinates.keys.max + 1
        RRTTree(tree.add(n, newIndex), coordinates + (newIndex -> Point(newPoint(0), newPoint(1))))
      }
    }
  }

  object RRTTree {
    /**
      * Generate infinite stream of RRT Trees rooted at start
      *
      * @return
      */
    def from(start: Point): Stream[RRTTree] =
      Stream.iterate(RRTTree(Leaf(0), Map(0 -> start)))(tree => tree.addSample)
  }

  var pastTrees: List[RRTTree] = Nil

  def hasEyeContact(p1: Point, p2: Point): Boolean =
    obstacles forall (o => !o.lineCollides(LineSegment(p1, p2)))

  def isPointVisible(point: Point)(rrt: RRTTree): Boolean = rrt match {
    case RRTTree(_, coordinates) => coordinates.values exists hasEyeContactTo(point)
  }

  def hasEyeContactTo(p: Point)(q: Point): Boolean = hasEyeContact(p, q)
}

/**
  * RRT implementation starting a single RRT at the start node
  */
trait SimpleRRT extends RandomTreePathPlanner {

  override def plan(start: Point, destination: Point): Option[Path] =
    RRTTree.from(start) take maxTrials find isPointVisible(destination) map computePath(destination)

  def computePath(destination: Point)(finalTree: RRTTree): Path = {
    pastTrees = finalTree :: Nil
    val finalNode = finalTree.coordinates.filter { case (_, p) => hasEyeContact(destination, p) }.keys.head
    val pathToFinalNode = finalTree.tree pathTo finalNode map finalTree.coordinates

    pathToFinalNode :+ destination
  }

}

/**
  * RRT implementation starting a RRT on both, the start and the goal node
  */
trait RRT extends RandomTreePathPlanner {

  override def plan(start: Point, destination: Point): Option[Path] = {

    val treeStream = RRTTree.from(start) zip RRTTree.from(destination)

    treeStream take maxTrials find connectable map computePath
  }

  def connectable(trees: (RRTTree, RRTTree)): Boolean = {

    def lastAddedPoint(t: RRTTree): Point = t.coordinates(t.coordinates.keys.max) // just for performance...

    val lastPointTests = List(lastAddedPoint(trees._1), lastAddedPoint(trees._2)) map isPointVisible

    (lastPointTests zip List(trees._2, trees._1)) exists {
      case (pointVisibleFrom, tree) => pointVisibleFrom(tree)
    }
  }

  def computePath(finalState: (RRTTree, RRTTree)): Path = finalState match {
    case (RRTTree(t1, points1), RRTTree(t2, points2)) =>
      val paths = for {
        (finalNode1, p1) <- points1.toStream
        (finalNode2, p2) <- points2.toStream
        if hasEyeContact(p1, p2)
      } yield {
        val path1 = t1 pathTo finalNode1 map points1
        val path2 = t2 pathTo finalNode2 map points2
        path1 ++ path2.reverse
      }
      pastTrees = RRTTree(t1, points1) :: RRTTree(t2, points2) :: Nil
      paths.head
  }

}

trait RRTViz extends PathPlannerDemo with RandomTreePathPlanner {

  override def plotSingleQuery(f: Plot, color: Color, verbose: Boolean): Unit = {

    pastTrees.zipWithIndex foreach {
      case (tree, index) => println(s"Tree ${index + 1} has ${tree.coordinates.keys.max + 1} nodes.")
    }

    val points = pastTrees flatMap {
      case RRTTree(tree, coordinates) => coordinates.values
    }

    if (verbose)
      f += scatter(
        x = DenseVector(points map (_.x): _*),
        y = DenseVector(points map (_.y): _*),
        size = _ => 0.3,
        colors = _ => new Color(color.getRed, color.getGreen, color.getBlue, 60)
      )
  }
}

object SimpleRRTDemo extends RRTViz with SimpleRRT {
  override val title = "RRT Path Planning (single tree)"
}

object RRTDemo extends RRTViz with RRT {
  override val title = "RRT Path Planning (symmetric)"
}
