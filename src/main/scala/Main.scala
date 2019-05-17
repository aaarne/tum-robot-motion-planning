import breeze.linalg._
import breeze.plot._

object Main extends App {

  val d = 1.0

  val f = Figure("Test")
  val x = linspace(0.0, 1.0)

  f.subplot(1, 2, 0).title = "Some Lines"
  f.subplot(1, 2, 1).title = "Fancy image"

  f subplot 0 += plot(x, x ^:^ 2.0)
  f subplot 0 += plot(x, x ^:^ 3.0)

  f subplot 1 += image(DenseMatrix.rand(200, 200))

  f.refresh()
}
