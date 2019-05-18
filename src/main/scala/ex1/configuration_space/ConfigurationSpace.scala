package ex1.configuration_space

import breeze.linalg._
import breeze.numerics.toRadians
import breeze.plot._

class ConfigurationSpace extends CollisionChecker with RandomRects {

  val robot = new Robot(List(1, 1))
  val rects = createRandomRects(3).toList

  def computeConfigurationSpace: DenseMatrix[Double] = {
    val q1 = toRadians(linspace(0, 180, 181))
    val q2 = toRadians(linspace(0, 359, 360))

    DenseMatrix.tabulate(q1.size, q2.size){
      case (i, j) => if (checkCollision(robot, List(q1(i), q2(j)), rects)) 0.0 else 1.0
    }
  }
}

class ShowConfigurationSpace extends ConfigurationSpace with Plotter {

  val f = Figure("Robot Visualizer")

  val p = f subplot 0
  plotRobot(p, robot, List(.25*math.Pi, .25*math.Pi))
  rects foreach (r => plotRectangle(p, r))
  println("Rectangles:")
  rects foreach println

  val confspace = computeConfigurationSpace
  println(sum(confspace)/confspace.size)
  println(s"\nSize of matrix: (${confspace.rows}, ${confspace.cols})")

  val f2 = Figure("Configuration Space")
  f2 subplot 0 += image(-1.0 * confspace.t)

  f.refresh()
  f2.refresh()

}
