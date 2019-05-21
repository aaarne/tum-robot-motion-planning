package aaarne.tum.rmp.configurationspace

import aaarne.tum.rmp.geometry.{Plotter, Rectangle}
import breeze.plot.{Figure, Plot, image}
import breeze.linalg._

trait ConfigurationSpaceDemoSettings {
  val robotLinkLengths = List(.7, .7)
  val amountOfObstacles = 2
}

trait ConfigurationSpaceDemo extends ConfigurationSpace with RandomRects with ConfigurationSpaceDemoSettings {

  override val rects: List[Rectangle] = createRandomRects(amountOfObstacles).toList
  override val robot = new Robot(robotLinkLengths)
}

object ShowConfigurationSpace extends ConfigurationSpaceDemo with Plotter with Runnable {

  override def run(): Unit = {
    val f = Figure("Robot Visualizer")

    f subplot 0 += plot(robot moveTo List(.25 * math.Pi, 0.25 * math.Pi), "blue", "Robot")
    f subplot 0 ++= rects.zipWithIndex map {
      case (shape, i) => plot(shape, "red", s"Obstacle $i")
    }

    f.subplot(0).legend = true

    println(s"Percentage of confspace collision-free: ${100*sum(confspace)/confspace.size}%")

    val f2 = Figure("Configuration Space")
    val p2: Plot = f2 subplot 0
    p2 += image(-1.0 * confspace.t)
    p2.xlabel = "q1"
    p2.ylabel = "q2"

    f.refresh()
    f2.refresh()

  }

}
