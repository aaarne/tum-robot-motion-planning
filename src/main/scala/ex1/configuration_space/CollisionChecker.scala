package ex1.configuration_space

import breeze.linalg._

trait CollisionChecker {

  private def pointsToLineSegments(points: List[Vector[Double]]): List[(Vector[Double], Vector[Double])] = points match {
    case Nil => Nil
    case p :: Nil => Nil
    case p :: rest => (p, rest.head) :: pointsToLineSegments(rest)
  }

  def checkCollisionWithRect(robot: Robot, jointAngles: List[Double], rectangle: Rectangle): Boolean =
    pointsToLineSegments(robot points jointAngles).exists {
      case (p1, p2) => rectangle.doesLineCollide(p1, p2)
    }

  def checkCollision(robot: Robot, jointAngles: List[Double], rectangles: List[Rectangle]): Boolean =
    checkCollision(robot, jointAngles, rectangles, ignoreTable = false)

  def checkCollision(robot: Robot, jointAngles: List[Double], rectangles: List[Rectangle], ignoreTable: Boolean): Boolean = {
    val rects = rectangles exists (r => checkCollisionWithRect(robot, jointAngles, r))
    rects || (!ignoreTable && checkTable(robot, jointAngles))
  }

  def checkTable(robot: Robot, jointAngles: List[Double]): Boolean =
    (robot points jointAngles) exists {p =>
      p(1) < 0
    }
}
