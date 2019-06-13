package aaarne.tum.rmp.test

import aaarne.tum.rmp.graphs.Graphs._
import breeze.numerics.abs
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

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

  it should "also work on a graph with string vertices" in new GraphSearchTestEnvironment {
    bfs(anotherTestGraph, "a", "f") match {
      case None => fail()
      case Some(sol) => sol should be(List("a", "c", "f"))
    }
  }

  it should "work for the ladders problem" in new LaddersEnvironment {
    val sol = bfs(neighbors, countChars("croissant"), countChars("baritone")) match {
      case None => fail()
      case Some(solution) => solution map wordDB map (_.head)
    }
    sol should be(List("croissant", "carotins", "aroints", "notaries", "baritones", "baritone"))
  }

  "The connectivity check" should "identify the test graph as unconnected with 3 components" in new GraphSearchTestEnvironment {
    countCommunities(testGraph) should be(3)
  }

  it should "identify the connectedGraph as connected" in new GraphSearchTestEnvironment {
    countCommunities(connectedGraph) should be(1)
  }

  it should "identify two components in the graph with two components" in new GraphSearchTestEnvironment {
    countCommunities(twoComponentsGraph) should be(2)
  }

  it should "identity one components when adding a link in the two component graph" in new GraphSearchTestEnvironment {
    countCommunities(twoComponentsGraph + (1 -> List(1, 2, 6))) should be(1)
  }

  "The UCS" should "return the path 1->3->2->4 on the cost graph" in new GraphSearchTestEnvironment {
    ucs(graphWithCost, 1, 4) match {
      case None => fail()
      case Some(solution) => solution should be (List(1, 3, 2, 4))
    }
  }

  "The A* search" should "also work on the ladders problem" in new LaddersEnvironment {
    val start = countChars("croissant")
    val goal = countChars("baritone")

    val sol = astar((v: Vertex) => addUnitCost(neighbors(v)), nDifferentCharacters(goal), start, goal) match {
      case None => fail()
      case Some(solution) => solution map wordDB map (_.head)
    }
    sol should be(List("croissant", "carotins", "aroints", "notaries", "baritones", "baritone"))
  }


  trait GraphSearchTestEnvironment {
    val testGraph: Map[Int, List[Int]] = Map(
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

    val anotherTestGraph: Map[String, List[String]] = Map(
      "a" -> List("b", "c"),
      "b" -> List("a", "d"),
      "c" -> List("f"),
      "d" -> List("e"),
      "e" -> List("f"),
      "f" -> Nil,
    )

    val connectedGraph: Map[Symbol, List[Symbol]] = Map(
      'node1 -> List('node2, 'node3),
      'node2 -> List('node1, 'node3),
      'node3 -> List('node1, 'node2),
    )

    val twoComponentsGraph: Map[Int, List[Int]] = Map(
      1 -> List(2, 3),
      2 -> List(1, 3),
      3 -> List(1, 2),
      4 -> List(5, 6),
      5 -> List(4, 6),
      6 -> List(4, 5),
    )

    val graphWithCost: Map[Int, List[(Int, Double)]] = Map(
      1 -> List((2, 3), (3, 1), (4, 50)),
      2 -> List((4, 2)),
      3 -> List((4, 4), (2, 1)),
      4 -> Nil,
    )
  }

  trait LaddersEnvironment {
    type Vertex = Map[Char, Int]

    def countChars(s: String): Map[Char, Int] =
      ('a' to 'z').map{
        c => c -> s.count(p => p == c)
      }.toMap

    var exploredNodes = 0

    lazy val wordDB: Map[Vertex, List[String]] = {

      val source = Source.fromURL(getClass.getResource("/wordList.txt"))
      val words = source.getLines().toList
      source.close()
      words groupBy countChars
    }

    lazy val existingWordsLookup = wordDB.keys.toSet

    def neighbors(v: Vertex): List[Vertex] = {
      val withoutOne = v flatMap {
        case (_, 0) => None
        case (c: Char, i: Int) => Some(v + (c -> (i - 1)))
      }
      val addEachChar = v map {
        case (c, i) => v + (c -> (i+1))
      }
      (withoutOne ++ addEachChar).toList filter existingWordsLookup.contains
    }

    def nDifferentCharacters(v1: Vertex)(v2: Vertex): Double =
      v1.map{
        case (c1, n1) => abs(v2(c1) - n1)
      }.sum


    def addUnitCost(nbhd: List[Vertex]): List[(Vertex, Double)] =
      nbhd map ((_, 1.0))
  }
}
