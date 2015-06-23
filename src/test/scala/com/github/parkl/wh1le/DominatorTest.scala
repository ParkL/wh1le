package com.github.parkl.wh1le

import org.scalatest.{Matchers, FunSpec}

/**
 * Created by bernd on 18.06.15.
 */
class DominatorTest extends FunSpec with Matchers {
  import WhileSyntax._
  val sheet9ExplicitLabels = program(List[Statement](
    Assignment("y", 0, Some(1)),
    If(ROpBExp("x", ">", 2),
      Assignment("x", "x", Some(3)), // ATTN actually is -x
      Assignment("x", BinaryAExp(2, "*", "x"), Some(4))
      , Some(2)),
    While(ROpBExp("x", ">", 0),
      List[Statement](
        Assignment("z", BinaryAExp("z", "-", 1), Some(6)),
        Assignment("x", BinaryAExp("x", "+", "z"), Some(7))
      ), Some(5)),
    Assignment("a", "x", Some(8))
  ))

  val sheet9 = assignLabels(List[Statement](
    Assignment("y", 0),
    If(ROpBExp("x", ">", 2),
      Assignment("x", "x"), // ATTN actually is -x
      Assignment("x", BinaryAExp(2, "*", "x"))),
    While(ROpBExp("x", ">", 0),
      List[Statement](
        Assignment("z", BinaryAExp("z", "-", 1)),
        Assignment("x", BinaryAExp("x", "+", "z"))
      )),
    Assignment("a", "x"))
  )

  describe("The Dominator Analysis") {
    it("should somehow work") {
      // val (enter, leave) = Dominator(sheet9).solve()
      // TODO
    }
  }
}
