package tumrmp

import tumrmp.configuration_space.ShowConfigurationSpace

object Main extends App {

  println(
    """Implemented Exercises:
      |1. Homework 1:
      | 1.1 Configuration Space
    """.stripMargin)

  def loop(): Unit =
    scala.io.StdIn.readLine("Select exercise: ") match {
      case "1.1" => new ShowConfigurationSpace
      case _ => sys.exit()
    }

  Stream.from(1) foreach {
    _ => loop()
  }

}
