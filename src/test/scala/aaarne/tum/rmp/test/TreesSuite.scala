package aaarne.tum.rmp.test

import aaarne.tum.rmp.pathplanning._
import org.scalatest.{FlatSpec, Matchers}

class TreesSuite extends FlatSpec with Matchers {

  "The tree" should "be built correctly" in new TestEnvironment {
    println(testTree)
  }

  it should "contain 2" in new TestEnvironment {
    testTree contains 2 should be(true)
  }

  it should "contain 3" in new TestEnvironment {
    testTree contains 3 should be(true)
  }

  "The path to 6" should "be 1->2->5->6" in new TestEnvironment {
    testTree pathTo 6 should be(List(1, 2, 5, 6))
  }

  trait TestEnvironment {
    val testTree = Leaf(1)
      .add(1, 2)
      .add(1, 3)
      .add(2, 4)
      .add(2, 5)
      .add(5, 6)
  }

}
