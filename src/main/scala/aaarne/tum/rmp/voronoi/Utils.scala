package aaarne.tum.rmp.voronoi

import java.awt.Color

import breeze.linalg.Matrix
import breeze.plot.{GradientPaintScale, GradientPaintScaleFactory}

object Utils {

  def createColorcode(m: Matrix[Double], colors: Array[Color]): GradientPaintScale[Double] =
    GradientPaintScaleFactory[Double](gradient = colors)
      .apply(m.valuesIterator.toList)
      .asInstanceOf[GradientPaintScale[Double]]

}
