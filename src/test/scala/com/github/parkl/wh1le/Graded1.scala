package com.github.parkl.wh1le

import org.scalatest.{FunSpec, Matchers}

/**
  * Created by bernd on 5/21/15.
 */
class Graded1 extends FunSpec with Matchers {
  import WhileSyntax._
  import TestPrograms._
  describe("Graded1 program") {
    it("flow graph should be ...") {
      flow(program2) should equal (
        Set((1,2), (1,3), (2,4), (3,4), (4,5), (5,4), (4,6))
      )
    }
    it("should have 6 blocks") {
      blocks(program2).map(_.l) should equal ((1 to 6).map(Some(_)).toSet)
    }
//    it("doit display stuff") {
//      println(assProg)
//      val ae = AvailableExpression(assProg)
//      val aes = aExpStar(assProg)
//      // for(i <- 1 to 6) println(s"kill($i): ${ae.kill(i)}\ngen($i): ${ae.gen(i)}")
//      val solve: (AvailableExpression#ResultMap, AvailableExpression#ResultMap, List[String]) = ae.solve()
//      for(x <- solve._3) println(x)
//      // for(i <- 1 to 6) println(s"AEEnter($i): ${result._1(i)}.")
//      // for(i <- 1 to 6) println(s"AELeave($i): ${result._2(i)}.")
//    }
  }
}
