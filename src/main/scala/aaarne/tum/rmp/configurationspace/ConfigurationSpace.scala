package aaarne.tum.rmp.configurationspace

import aaarne.tum.rmp.geometry.{Plotter, Rectangle}
import breeze.linalg._
import breeze.numerics.toRadians
import breeze.plot.{Figure, Plot, image}

trait ConfigurationSpace extends CollisionChecker {

  val rects: List[Rectangle]
  val robot: Robot

  lazy val confspace: DenseMatrix[Double] = {
    val q1 = toRadians(linspace(0, 180, 181))
    val q2 = toRadians(linspace(0, 359, 360))

    DenseMatrix.tabulate(q1.size, q2.size){
      case (i, j) => if (checkCollision(robot moveTo List(q1(i), q2(j)), rects)) 0.0 else 1.0
    }
  }
}

