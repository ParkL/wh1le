package com.github.parkl.wh1le

import org.scalatest.{Matchers, FunSpec}

/**
 * Created by bernd on 5/21/15.
 */
class AvailableExpressionTest extends FunSpec with Matchers {
  import WhileSyntax._
  val aPlusB = BinaryAExp("a", "+", "b")
  val aTimesX = BinaryAExp("a", "*", "x")
  val aPlus1 = BinaryAExp("a", "+", 1)
  val example = List[Statement](
    Assignment("x", aPlusB, l = 1),
    Assignment("y", aTimesX, l = 2),
    While(ROpBExp("y", ">", aPlusB), 3, List[Statement](
      Assignment("a", aPlus1, 4),
      Assignment("x", aPlusB, 5)
      )
    )
  )

  describe("AvailableExpression analysis") {
    it("syntax should properly compute aExpStar") {
      aExpStar(example) should equal (Set(aPlusB, aTimesX, aPlus1))
    }
    it("should properly compute kill sets (ATTN weird!!)") {
      val ae = AvailableExpression(example)
//      ae.kill(1) should equal(Set.empty)
      ae.kill(1) should equal(Set(aTimesX))
      ae.kill(2) should equal(Set.empty)
      ae.kill(3) should equal(Set.empty)
      ae.kill(4) should equal(Set(aPlusB, aTimesX, aPlus1))
//      ae.kill(5) should equal(Set.empty)
      ae.kill(5) should equal(Set(aTimesX))
    }
    it("should properly compute gen sets") {
      val ae = AvailableExpression(example)
      ae.gen(1) should equal(Set(aPlusB))
      ae.gen(2) should equal(Set(aTimesX))
      ae.gen(3) should equal(Set(aPlusB))
      ae.gen(4) should equal(Set.empty)
      ae.gen(5) should equal(Set(aPlusB))
    }
    it("should compute the solution using the algorithm") {
      val ae = AvailableExpression(example)
      val solve: (AvailableExpression#ResultMap, AvailableExpression#ResultMap) = ae.solve()
      val AeIn = solve._1
      val AeOut = solve._2
      AeIn(1) should equal(Set.empty)
      AeIn(2) should equal(aPlusB)
      AeIn(3) should equal(aPlusB)
      AeIn(4) should equal(aPlusB)
      AeIn(5) should equal(Set.empty)
    }
  }
}
