package tumrmp

import tumrmp.configuration_space.ShowConfigurationSpace
import tumrmp.visibility_graph.ShowVisibilityGraph
import tumrmp.voronoi.ShowVoronoiDiagram


object Main extends App {

  println("""
      |1. Homework 1:
      | 1.1 Configuration Space
      | 1.2 Visibility Graph
      | 1.3 Voronoi Diagram
    """.stripMargin)

  def loop(): Unit =
    scala.io.StdIn.readLine("Select exercise: ") match {
      case "1.1" => ShowConfigurationSpace.run()
      case "1.2" => ShowVisibilityGraph.run()
      case "1.3" => ShowVoronoiDiagram.run()

      case _ => sys.exit()
    }

  while(true) loop()

}
