package tumrmp.configuration_space

import breeze.linalg._
import breeze.plot._

trait Plotter {

  private def plotPointList(p: Plot, color: String, points: List[Vector[Double]]): Unit = {
    (points zip points.tail) map {
      case (p1, p2) => LineSegment(p1, p2)
    } foreach plotLineSegment(p, color)

  }

  private def plotLineSegment(p: Plot, color: String)(lineSegment: LineSegment) = lineSegment match {
    case LineSegment(p1, p2) =>
      val x = List(p1(0), p2(0))
      val y = List(p1(1), p2(1))
      p += plot(x, y, colorcode = color)
      p2
  }

  def plotRobot(robot: RobotState)(implicit p: Plot) =
    plotPointList(p, "blue", robot.points)

  def plotRectangle(rect: Rectangle)(implicit p: Plot) =
    rect.lineSegments foreach plotLineSegment(p, "red")
}
