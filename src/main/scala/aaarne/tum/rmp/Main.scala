package aaarne.tum.rmp

import aaarne.tum.rmp.configurationspace.ShowConfigurationSpace
import aaarne.tum.rmp.pathplanning.PRMDemo
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
      |
      |2. Homework 2:
      | 2.1 Probabilistic Roadmap (PRM)
    """.stripMargin)

  val exercises: Map[String, Runnable] = Map(
    "1.1" -> ShowConfigurationSpace,
    "1.2" -> ShowVisibilityGraph,
    "1.3" -> ShowVoronoiDiagramDemo,
    "2.1" -> PRMDemo,
  )

  continually(scala.io.StdIn.readLine("Select exercise: ")) map exercises.get forall {
    case Some(ex) => ex.run(); true
    case None => false
  }

  println("Bye")
}
