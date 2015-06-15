package com.github.parkl.wh1le

import org.scalatest.{Matchers, FunSpec}

/**
 * Created by bernd on 5/21/15.
 */
class AvailableExpressionTest extends FunSpec with Matchers {
  import WhileSyntax._
  val aPlusB = BinaryAExp("a", "+", "b")
  val aTimesB = BinaryAExp("a", "*", "b")
  val aPlus1 = BinaryAExp("a", "+", 1)
  val example = List[Statement](
    Assignment("x", aPlusB, l = 1),
    Assignment("y", aTimesB, l = 2),
    While(ROpBExp("y", ">", aPlusB), 3, List[Statement](
      Assignment("a", aPlus1, 4),
      Assignment("x", aPlusB, 5)
      )
    )
  )

  describe("AvailableExpression analysis") {
    it("syntax should properly compute aExpStar") {
      aExpStar(example) should equal (Set(aPlusB, aTimesB, aPlus1))
    }
    it("should properly compute kill") {
      val ae = AvailableExpression(example)
      ae.kill(1) should equal(Set.empty)
      ae.kill(2) should equal(Set.empty)
      ae.kill(3) should equal(Set.empty)
      ae.kill(4) should equal(Set(aPlusB, aTimesB, aPlus1))
      ae.kill(5) should equal(Set.empty)
    }
    it("should properly compute gen sets") {
      val ae = AvailableExpression(example)
      ae.gen(1) should equal(Set(aPlusB))
      ae.gen(2) should equal(Set(aTimesB))
      ae.gen(3) should equal(Set(aPlusB))
      ae.gen(4) should equal(Set.empty)
      ae.gen(5) should equal(Set(aPlusB))
    }
    it("should compute the solution using the algorithm") {
      val (entry, exit) = AvailableExpression(example).solve()
      entry(1) should equal(Set.empty)
      entry(2) should equal(Set(aPlusB))
      entry(3) should equal(Set(aPlusB))
      entry(4) should equal(Set(aPlusB))
      entry(5) should equal(Set.empty)

      exit(1) should equal(Set(aPlusB))
      exit(2) should equal(Set(aPlusB, aTimesB))
      exit(3) should equal(Set(aPlusB))
      exit(4) should equal(Set.empty)
      exit(5) should equal(Set(aPlusB))
    }
  }
}
