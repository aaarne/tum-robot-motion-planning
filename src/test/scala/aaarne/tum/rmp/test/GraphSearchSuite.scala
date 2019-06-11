package aaarne.tum.rmp.test

import aaarne.tum.rmp.pathplanning.Graphs._
import org.scalatest.{FlatSpec, Matchers}

class GraphSearchSuite extends FlatSpec with Matchers {

  "BFS search" should "select 1 -> 3 -> 4 -> 9 for query 1 -> 9" in new GraphSearchTestEnvironment {
    bfs(testGraph, 1, 9) match {
      case Some(solution) => solution should be(List(1, 3, 4, 9))
      case None => fail()
    }
  }

  it should "find no solution for query 1 -> 10" in new GraphSearchTestEnvironment {
    bfs(testGraph, 1, 10) match {
      case Some(_) => fail
      case None => succeed
    }
  }

  it should "select path 5->4->9->3->12 for query 5 -> 12" in new GraphSearchTestEnvironment {
    bfs(testGraph, 5, 12) match {
      case None => fail
      case Some(solution) => solution should be(List(5, 4, 9, 3, 12))
    }
  }

  it should "find no solution for query 11->10" in new GraphSearchTestEnvironment {
    bfs(testGraph, 11, 10) match {
      case None => succeed
      case Some(_) => fail()
    }
  }

  "The connectivity check" should "identify the test graph as unconnected with 4 components" in new GraphSearchTestEnvironment {
    checkConnectivity(testGraph) should be(3)
  }

  it should "identify the connectedGraph as connected" in new GraphSearchTestEnvironment {
    checkConnectivity(connectedGraph) should be(0)
  }


  trait GraphSearchTestEnvironment {
    val testGraph = Map(
      1 -> List(2, 3),
      2 -> List(3, 5, 7, 12),
      3 -> List(4, 5, 8, 12),
      4 -> List(9),
      5 -> List(4, 6, 8),
      6 -> List(7),
      7 -> Nil,
      8 -> Nil,
      9 -> List(3),
      10 -> List(11),
      11 -> List(3),
      12 -> Nil,
    )

    val connectedGraph = Map(
      1 -> List(2, 3),
      2 -> List(3, 1),
      3 -> List(1, 2),
    )
  }

}
