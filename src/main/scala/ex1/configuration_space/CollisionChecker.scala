package ex1.configuration_space

trait CollisionChecker {

  def checkCollision(robot: RobotState, rectangles: List[Rectangle], ignoreTable: Boolean = false): Boolean = {
    val rects = rectangles exists checkCollisionWithRect(robot)
    rects || (!ignoreTable && checkTable(robot))
  }

  def checkCollisionWithRect(robot: RobotState)(rectangle: Rectangle): Boolean =
    (robot.points zip robot.points.tail) exists {
      case (p1, p2) => rectangle.doesLineCollide(p1, p2)
    }

  def checkTable(robot: RobotState): Boolean = robot.points exists (p => p(1) < 0)
}
