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
  val example = assignLabels(List[Statement](
    Assignment("x", aPlusB),
    Assignment("y", aTimesB),
    While(ROpBExp("y", ">", aPlusB), List[Statement](
      Assignment("a", aPlus1),
      Assignment("x", aPlusB)
      )
    ))
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
