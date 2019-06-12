package aaarne.tum.rmp.configurationspace

import java.awt.Color

import aaarne.tum.rmp.geometry.{Plotter, Polygon}
import breeze.linalg._
import breeze.plot.{Figure, Plot, image}

trait ConfigurationSpaceDemoSettings {
  val robotLinkLengths = List(.7, .7)
  val amountOfObstacles = 2
}

trait ConfigurationSpaceDemo extends ConfigurationSpace with RandomRects with ConfigurationSpaceDemoSettings {

  override val obstacles: List[Polygon] = createRandomRects(amountOfObstacles).toList
  override val robot = new Robot(robotLinkLengths)
}

object ShowConfigurationSpace extends ConfigurationSpaceDemo with Runnable with Plotter {

  def plotConfspace(): Unit = {
    val f = Figure("Robot Visualizer")

    f subplot 0 += plot(Color.BLUE, "Robot")(robot moveTo List(.25 * math.Pi, 0.25 * math.Pi))
    f subplot 0 ++= obstacles.zipWithIndex map {
      case (shape, i) => plot(Color.RED, s"Obstacle $i")(shape)
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

  override def run(): Unit = plotConfspace()
}
