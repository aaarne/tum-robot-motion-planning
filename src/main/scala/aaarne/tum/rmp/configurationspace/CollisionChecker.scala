package aaarne.tum.rmp.configurationspace

import aaarne.tum.rmp.geometry.Polygon

trait CollisionChecker {

  def checkCollision(robot: RobotState, polys: List[Polygon], ignoreTable: Boolean = false): Boolean = {
    val rects = polys exists checkCollisionWithRect(robot)
    rects || (!ignoreTable && checkTable(robot))
  }

  def checkCollisionWithRect(robot: RobotState)(poly: Polygon): Boolean =
    robot.lineSegments exists poly.lineCollides

  def checkTable(robot: RobotState): Boolean = robot.points exists (p => p(1) < 0)
}
