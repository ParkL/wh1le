package com.github.parkl.wh1le

import org.scalatest.{Matchers, FunSpec}

/**
 * Created by bernd on 5/21/15.
 */
class SyntaxSpec extends FunSpec with Matchers {
  import WhileSyntax._
  describe("Syntax") {
    describe("AExp") {
      it("should support implicit conversion of AOps") {
        BinaryAExp("x", "+", 3) should equal (BinaryAExp(Ide("x"), Plus, Number(3)))
      }
    }
  }
}