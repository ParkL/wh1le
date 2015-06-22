package com.github.parkl.wh1le
import org.scalatest.{Matchers, FunSpec}

/**
  * Created by bernd on 15.06.15.
 */
class VeryBusyExpressionsTest extends FunSpec with Matchers {
  import WhileSyntax._

  private val aMinusB: BinaryAExp = BinaryAExp("a", "-", "b")
  private val bMinusA: BinaryAExp = BinaryAExp("b", "-", "a")
  val example = assignLabels(List[Statement](
    If(ROpBExp("a", ">", "b"),
      List[Statement](
        Assignment("x", bMinusA),
        Assignment("y", aMinusB)
      ),
      List[Statement](
        Assignment("y", bMinusA),
        Assignment("x", aMinusB)
      )
    )
  ))

  describe("Very Busy Expression Analysis") {
    it("should generate kill set properly") {
      val vbe = VeryBusyExpression(example)
      vbe.kill(1) should equal(Set.empty)
      vbe.kill(2) should equal(Set.empty)
      vbe.kill(3) should equal(Set.empty)
      vbe.kill(4) should equal(Set.empty)
      vbe.kill(5) should equal(Set.empty)
    }
    it("should generate gen set properly") {
      val vbe = VeryBusyExpression(example)
      vbe.gen(1) should equal(Set.empty)
      vbe.gen(2) should equal(Set(bMinusA))
      vbe.gen(3) should equal(Set(aMinusB))
      vbe.gen(4) should equal(Set(bMinusA))
      vbe.gen(5) should equal(Set(aMinusB))
    }
    it("should solve properly") {
      val (exit, enter) = VeryBusyExpression(example).solve()
      enter(1) should equal(Set(aMinusB, bMinusA))
      enter(2) should equal(Set(aMinusB, bMinusA))
      enter(3) should equal(Set(aMinusB))
      enter(4) should equal(Set(aMinusB, bMinusA))
      enter(5) should equal(Set(aMinusB))

      exit(1) should equal(Set(aMinusB, bMinusA))
      exit(2) should equal(Set(aMinusB))
      exit(3) should equal(Set.empty)
      exit(4) should equal(Set(aMinusB))
      exit(5) should equal(Set.empty)

    }
  }
}
