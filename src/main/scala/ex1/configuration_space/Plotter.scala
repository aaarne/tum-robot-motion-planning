package ex1.configuration_space

import breeze.linalg._
import breeze.plot._

trait Plotter {

  private def plotPointList(p: Plot, points: List[Vector[Double]]): Unit = {
    points reduce ((p1: Vector[Double], p2: Vector[Double]) => {
      val x = List(p1(0), p2(0))
      val y = List(p1(1), p2(1))
      p += plot(x, y)
      p2
    })
  }

  def plotRobot(robot: RobotState)(implicit p: Plot) =
    plotPointList(p, robot.points)

  def plotRectangle(rect: Rectangle)(implicit p: Plot) = {
    plotPointList(p, List(0, 2, 3, 1, 0) map rect.corners)
  }
}
