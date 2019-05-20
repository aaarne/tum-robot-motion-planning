package tumrmp.configuration_space

import breeze.plot._
import tumrmp.geometry.LineSegment

trait Plottable {
  def lineSegments: List[LineSegment]
}

trait Plotter {

  def plot(color: String)(shape: Plottable): Seq[Series] = {
    plotShape(breeze.plot.plot(_, _, style = '-', colorcode = color))(shape)
  }

  def plotShape(f: (List[Double], List[Double]) => Series)(shape: Plottable): Seq[Series] = {
    shape.lineSegments map plotLineSegment(f)
  }

  private def plotLineSegment(f: (List[Double], List[Double]) => Series)(lineSegment: LineSegment): Series =
    lineSegment match {
      case LineSegment(p1, p2) =>
        val x = List(p1(0), p2(0))
        val y = List(p1(1), p2(1))
        f(x, y)
    }
}
