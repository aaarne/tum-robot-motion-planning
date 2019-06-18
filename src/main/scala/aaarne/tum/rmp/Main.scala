package aaarne.tum.rmp

import aaarne.tum.rmp.configurationspace.ShowConfigurationSpace
import aaarne.tum.rmp.pathplanning.{PRMDemo, RRTDemo, RandomTreeGrowthDemo, SimpleRRTDemo}
import aaarne.tum.rmp.visibilitygraph.ShowVisibilityGraph
import aaarne.tum.rmp.voronoi.ShowVoronoiDiagramDemo

import scala.collection.immutable.Stream.continually
import scala.io.StdIn.readLine

object Main extends App {

  println(
    """
      |1. Part 1:
      | 1.1 Configuration Space
      | 1.2 Visibility Graph
      | 1.3 Voronoi Diagram
      |
      |2. Part 2:
      | 2.1 Probabilistic Roadmap (PRM)
      | 2.2 Rapidly Exploring Random Tree (RRT) (single tree mode)
      | 2.3 Rapidly Exploring Random Tree (RRT) (symmetric bi-tree mode)
      | 2.4 RRT Growth Demo
      |
      |Type 'all' to run all.
    """.stripMargin)

  val exercises: Map[String, Runnable] = Map(
    "1.1" -> ShowConfigurationSpace,
    "1.2" -> ShowVisibilityGraph,
    "1.3" -> ShowVoronoiDiagramDemo,

    "2.1" -> PRMDemo,
    "2.2" -> SimpleRRTDemo,
    "2.3" -> RRTDemo,
    "2.4" -> RandomTreeGrowthDemo,
  )

  val commands: Map[String, Runnable] = exercises ++ Map(
    "all" -> new Runnable {
      override def run(): Unit = exercises.values foreach (_.run())
    }
  )

  continually(readLine("Select exercise (e.g. '2.3') -> ")) map commands.get forall {
    case Some(ex) => ex.run(); true
    case None => false
  }

  println("Bye")
}

