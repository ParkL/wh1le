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
        Assignment("x", BinaryAExp("x", "-", 1))
      )
    )
  ))

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

  val fvWeirdBug = assignLabels(List[Statement](
    Assignment("x", 5),
    Assignment("y", 1),
    While(ROpBExp("x", ">", 1),
      List[Statement](
        Assignment("y", BinaryAExp("x", "*", "y")),
        Assignment("x", BinaryAExp("x", "-", 1))
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
      it("should automatically assign labels correctly") {
        sheet9ExplicitLabels should equal(sheet9)
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
      it("should compute fv of statements") {
        fv(fvWeirdBug) should equal(Set(Ide("x"), Ide("y")))
      }
    }
  }
}