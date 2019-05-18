package ex1.configuration_space

import breeze.linalg._
import breeze.numerics.toRadians
import breeze.plot._

class ConfigurationSpace extends CollisionChecker with RandomRects {

  lazy val confspace: DenseMatrix[Double] = {
    val q1 = toRadians(linspace(0, 180, 181))
    val q2 = toRadians(linspace(0, 359, 360))

    DenseMatrix.tabulate(q1.size, q2.size){
      case (i, j) => if (checkCollision(robot moveTo List(q1(i), q2(j)), rects)) 0.0 else 1.0
    }
  }
  val rects: List[Rectangle] = createRandomRects(2).toList
  val robot = new Robot(List(1, 1))
}

class ShowConfigurationSpace extends ConfigurationSpace with Plotter {

  val f = Figure("Robot Visualizer")

  implicit val p: Plot = f subplot 0
  plotRobot(robot moveTo List(.25 * math.Pi,.25 * math.Pi))
  rects foreach plotRectangle

  println(s"Percentage of confspace collision-free: ${100*sum(confspace)/confspace.size}%")

  val f2 = Figure("Configuration Space")
  val p2: Plot = f2 subplot 0
  p2 += image(-1.0 * confspace.t)
  p2.xlabel = "q1"
  p2.ylabel = "q2"

  f.refresh()
  f2.refresh()

}
