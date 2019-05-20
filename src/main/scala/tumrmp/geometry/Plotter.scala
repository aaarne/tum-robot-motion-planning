package tumrmp.geometry

import breeze.plot._

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
      case LineSegment(p1, p2) => f(List(p1(0), p2(0)), List(p1(1), p2(1)))
    }
}
