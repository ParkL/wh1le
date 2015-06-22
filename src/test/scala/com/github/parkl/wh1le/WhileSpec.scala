package com.github.parkl.wh1le

import org.scalatest.{Matchers, FunSpec}

/**
 * Created by bernd on 5/21/15.
 */
class WhileSpec extends FunSpec with Matchers {
  import WhileSyntax._
  val prog1:Statement = assignLabels(List[Statement](
    Assignment("z", 1),
    While(
      ROpBExp("x", Gt, 0), Composition(
        Assignment("z", BinaryAExp("z", "*", "y")),
        Assignment("x", BinaryAExp("x", "-", "1"))
      )
    )
  ))

  describe("Syntax") {
    describe("AExp") {
      it("should support implicit conversion of AOps") {
        BinaryAExp("x", "+", 3) should equal (BinaryAExp(Ide("x"), Plus, Number(3)))
      }
    }
    describe("Statements") {
      it("should support implicit conversions from lists of statements to composition") {
        val ass1: Statement = Assignment("x", 2, Some(1))
        val ass2: Statement = Assignment("y", 3, Some(3))
        val myProgram: List[Statement] = List(ass1, Skip(Some(2)), ass2)
        val expected: Composition = Composition(ass1, Composition(Skip(Some(2)), ass2))
        program(myProgram) should equal(expected)
        def _test(s:Statement): Unit = {
          s should equal(expected)
        }
        _test(myProgram)
      }
    }
    describe("Functions") {
      it("should find labels properly") {
        labels(prog1) should equal(Set(1,2,3,4))

      }
      it("should find init(S) properly") {
        init(prog1) should equal(1)
      }
      it("should find finals properly") {
        f1nal(prog1) should equal(Set(2))
      }
      it("should properly calc flow") {
        flow(prog1) should equal(
          Set((1,2), (2,3), (3,4), (4,2))
        )
      }
      it("should properly calc flowR") {
        flowR(prog1) should equal(
          Set((2,1), (2,4), (3,2), (4,3))
        )
      }
      it("should properly compute blocks") {
        import language.postfixOps
        blocks(prog1).map(_.l) should equal((1 to 4) map (Some(_)) toSet)
      }
      it("should properly compute available expressions") {
        val fiveTimesX = BinaryAExp(5, "*", "x")
        val fiveTimesXMinusY = BinaryAExp(fiveTimesX, "-", "y")
        val exp1 = ROpBExp(fiveTimesXMinusY, "<", 5)
        aExp(exp1) should equal (
          Set(BinaryAExp(5, "*", "x"), BinaryAExp(fiveTimesX, "-", "y"))
        )
      }
      it("should properly compute free variables") {
        val fiveTimesX = BinaryAExp(5, "*", "x")
        val fiveTimesXMinusY = BinaryAExp(fiveTimesX, "-", "y")
        val exp1 = ROpBExp(fiveTimesXMinusY, "<", 5)
        fv(exp1) should equal(Set(Ide("x"), Ide("y")))
      }
    }
  }
}