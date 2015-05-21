package com.github.parkl.wh1le

import org.scalatest.{Matchers, FunSpec}

/**
 * Created by bernd on 5/21/15.
 */
class WhileSpec extends FunSpec with Matchers {
  import WhileSyntax._
  val prog1:List[Statement] = List(
    Assignment("z", 1, 1),
    While(
      ROpBExp("x", Gt, 0), 2, Composition(
        Assignment("z", BinaryAExp("z", "*", "y"), 3),
        Assignment("x", BinaryAExp("x", "-", "1"), 4)
      )
    )
  )

  describe("Syntax") {
    describe("AExp") {
      it("should support implicit conversion of AOps") {
        BinaryAExp("x", "+", 3) should equal (BinaryAExp(Ide("x"), Plus, Number(3)))
      }
    }
    describe("Statements") {
      it("should support implicit conversions from lists of statements to composition") {
        val ass1: Statement = Assignment("x", 2, 1)
        val ass2: Statement = Assignment("y", 3, 3)
        val myProgram: List[Statement] = List(ass1, Skip(2), ass2)
        val expected: Composition = Composition(ass1, Composition(Skip(2), ass2))
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
//      it("should properly compute blocks") {
//        blocks(prog1) should equal(Set(1,2,3,4))
//      }
    }
  }
}