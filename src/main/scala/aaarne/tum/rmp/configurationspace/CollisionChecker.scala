package aaarne.tum.rmp.configurationspace

import aaarne.tum.rmp.geometry.Rectangle

trait CollisionChecker {

  def checkCollision(robot: RobotState, rectangles: List[Rectangle], ignoreTable: Boolean = false): Boolean = {
    val rects = rectangles exists checkCollisionWithRect(robot)
    rects || (!ignoreTable && checkTable(robot))
  }

  def checkCollisionWithRect(robot: RobotState)(rectangle: Rectangle): Boolean =
    robot.lineSegments exists rectangle.doesLineCollide

  def checkTable(robot: RobotState): Boolean = robot.points exists (p => p(1) < 0)
}
