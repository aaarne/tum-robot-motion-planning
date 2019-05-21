package aaarne.tum.rmp

import aaarne.tum.rmp.configurationspace.ShowConfigurationSpace
import aaarne.tum.rmp.visibilitygraph.ShowVisibilityGraph
import aaarne.tum.rmp.voronoi.ShowVoronoiDiagramDemo

import scala.collection.immutable.Stream.continually

object Main extends App {

  println(
    """
      |1. Homework 1:
      | 1.1 Configuration Space
      | 1.2 Visibility Graph
      | 1.3 Voronoi Diagram
    """.stripMargin)

  continually(scala.io.StdIn.readLine("Select exercise: ")) foreach {
    case "1.1" => ShowConfigurationSpace.run()
    case "1.2" => ShowVisibilityGraph.run()
    case "1.3" => ShowVoronoiDiagramDemo.run()

    case _ => sys.exit()
  }


}
