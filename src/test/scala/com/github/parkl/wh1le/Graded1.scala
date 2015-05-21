package com.github.parkl.wh1le

import com.github.parkl.wh1le.AvailableExpression
import org.scalatest.{Matchers, FunSpec}

/**
  * Created by bernd on 5/21/15.
 */
class Graded1 extends FunSpec with Matchers {
  import com.github.parkl.wh1le.AvailableExpression
  import com.github.parkl.wh1le.WhileSyntax._

  def assProg:List[Statement] = {
    val fiveTimesX = BinaryAExp(5, "*", "x")
    val fiveTimesXMinusY = BinaryAExp(fiveTimesX, "-", "y")
    val exp1 = ROpBExp(fiveTimesXMinusY, "<", 5)

    val yPlus4 = BinaryAExp("y", "+", 4)
    val exp2 = BinaryAExp(yPlus4, "*", "x")

    val exp3 = BinaryAExp(fiveTimesX, "*", yPlus4)

    val xPlusY = BinaryAExp("x", "+", "y")
    val xPlusYGt1 = ROpBExp(xPlusY, ">", 1)
    val exp4 = xPlusYGt1

    val yBy2 = BinaryAExp("y", "/", 2)
    val yBy2MinusX = BinaryAExp(yBy2, "-", "x")
    val exp5 = yBy2MinusX

    val exp6 = BinaryAExp(fiveTimesX, "+", yPlus4)

    List[Statement](
      If(exp1, l = 1,
        Assignment("z", exp2, l = 2),
        Assignment("z", exp3, l = 3)),
      While(exp4, l = 4, Assignment("y", exp5, l = 5)),
      Assignment("z", exp6, l = 6)
    )
  }

  describe("Graded1 program") {
    it("flow graph shoudl be ...") {
      flow(assProg) should equal (
        Set((1,2), (1,3), (2,4), (3,4), (4,5), (5,4), (4,6))
      )
    }
    it("should have 6 blocks") {
      blocks(assProg).map(_.l) should equal ((1 to 6).toSet)
    }
//    it("Available expression kill list should be") {
//      val ae = AvailableExpression(assProg)
//      for {
//        i <- 1 to blocks(assProg).size
//
//      }
//    }

  }

}
