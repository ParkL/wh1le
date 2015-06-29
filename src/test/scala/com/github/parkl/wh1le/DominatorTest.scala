package com.github.parkl.wh1le

import org.scalatest.concurrent.Timeouts
import org.scalatest.{FunSpec, Matchers}

/**
 * Created by bernd on 18.06.15.
 */
class DominatorTest extends FunSpec with Matchers with Timeouts {
  import TestPrograms._

  describe("The Dominator Analysis") {
    it("should somehow work") {
      // println(sheet9); System.exit(0)
      import org.scalatest.time.SpanSugar._
      failAfter(1 second) {
        val (enter, exit) = Dominator(program4).solve()
        enter(1) should equal(Set.empty)
        exit(1) should equal(Set(1))
        enter(2) should equal(exit(1))
        exit(2) should equal(Set(1,2))
        enter(3) should equal(exit(2))
        exit(3) should equal(Set(1,2,3))
        enter(4) should equal(exit(2))
        exit(4) should equal(Set(1,2,4))
        enter(5) should equal(Set(1,2))
        exit(5) should equal(Set(1,2,5))
        enter(6) should equal(Set(1,2,5))
        exit(6) should equal(Set(1,2,5,6))
        enter(7) should equal(Set(1,2,5,6))
        exit(7) should equal(Set(1,2,5,6,7))
        enter(8) should equal(Set(1,2,5))
        exit(8) should equal(Set(1,2,5,8))
      }
    }
  }
}
