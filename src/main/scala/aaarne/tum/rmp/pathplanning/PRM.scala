package aaarne.tum.rmp.pathplanning

import java.awt.Color

import aaarne.tum.rmp.geometry.LineSegment
import aaarne.tum.rmp.pathplanning.Graphs._
import breeze.linalg._
import breeze.plot._


trait PRM extends PathPlanner {

  val nVertices: Int = 50

  case class Graph(vertices: List[Point], edges: List[(Int, Int)]) {
    def extend(vertex: Point): Graph = extendGraph(this, vertex)

    def toVertexListFormat: Map[Int, List[Int]] = {
      val raw = edges.groupBy(_._1).mapValues(l => l.map(_._2))
      val missingKeys = vertices.indices diff raw.keys.toList
      raw ++ Map(missingKeys map (i => i -> Nil): _*)
    }
  }

  def extendGraph(graph: Graph, vertex: Point): Graph = {
    val newIndex = graph.vertices.size
    val newEdges = graph.vertices.zipWithIndex.filter {
      case (v, i) =>
        val ls = LineSegment(v, vertex)
        obstacles forall (o => !o.lineCollides(ls))
    }.flatMap {
      case (_, i) => List(i -> newIndex, newIndex -> i)
    }

    Graph(graph.vertices :+ vertex, graph.edges ++ newEdges)
  }

  lazy val prm: Graph = {
    val randomPoints = (1 to nVertices) map (_ => sampleFreePoint)

    (Graph(Nil, Nil) /: randomPoints) (extendGraph)
  }

  override def plan(start: Point, destination: Point): Option[Path] = {
    val graph = prm.extend(start).extend(destination)
    val i_start = graph.vertices.size - 2
    val i_dest = graph.vertices.size - 1

    bfs(graph.toVertexListFormat, i_start, i_dest) map (solution => solution map graph.vertices)
  }

}

object PRMDemo extends PathPlannerDemo with PRM {

  override val title = "PRM Pathplanning"

  override def plotMultipleQuery(f: Plot, verbose: Boolean): Unit = {

    println(s"Using $nVertices non-colliding points to construct the PRM")

    countCommunities(prm.toVertexListFormat) match {
      case 1 => println("The probabilistic roadmap is connected :)")
      case n if n > 1 => println(s"PRM is not connected :( There are $n independent graphs.")
    }

    f += scatter(
      DenseVector(prm.vertices map (v => v.x): _*),
      DenseVector(prm.vertices map (v => v.y): _*),
      _ => 0.3, _ => Color.LIGHT_GRAY
    )

    if (verbose) {
      val pathLineSegments = prm.edges map {
        case (v1, v2) => LineSegment(prm.vertices(v1), prm.vertices(v2))
      }
      f ++= plotLineSegments(pathLineSegments, color = new Color(180, 180, 180))
    }
  }

}
