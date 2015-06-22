package com.github.parkl.wh1le

import org.scalatest.{Matchers, FunSpec}
/**
 * Created by bernd on 15.06.15.
 */
class LiveVariablesTest extends FunSpec with Matchers {
  import WhileSyntax._
  private val yTimesY: BinaryAExp = BinaryAExp("y", "*", "y")

  val example = assignLabels(List[Statement](
    Assignment("x", 2),
    Assignment("y", 4),
    Assignment("x", 1),
    If(ROpBExp("y", ">", "x"),
      Assignment("z", "y"),
      Assignment("z", yTimesY)),
    Assignment("x", "z")
  ))

  describe("LiveVariablesAnalysis") {
    it("should properly create kill lists") {
      val lv = LiveVariables(example)
      lv.kill(1) should equal(Set[Ide]("x"))
      lv.kill(2) should equal(Set[Ide]("y"))
      lv.kill(3) should equal(Set[Ide]("x"))
      lv.kill(4) should equal(Set.empty)
      lv.kill(5) should equal(Set[Ide]("z"))
      lv.kill(6) should equal(Set[Ide]("z"))
      lv.kill(7) should equal(Set[Ide]("x"))
    }
    it("should properly create gen lists") {
      val lv = LiveVariables(example)
      lv.gen(1) should equal(Set.empty)
      lv.gen(2) should equal(Set.empty)
      lv.gen(3) should equal(Set.empty)
      lv.gen(4) should equal(Set[Ide]("x", "y"))
      lv.gen(5) should equal(Set[Ide]("y"))
      lv.gen(6) should equal(Set[Ide]("y"))
      lv.gen(7) should equal(Set[Ide]("z"))
    }
    it("should solve correctly") {
      val (exit, entry) = LiveVariables(example).solve()
      entry(1) should equal(Set.empty[Ide])
      entry(2) should equal(Set.empty[Ide])
      entry(3) should equal(Set[Ide]("y"))
      entry(4) should equal(Set[Ide]("x","y"))
      entry(5) should equal(Set[Ide]("y"))
      entry(6) should equal(Set[Ide]("y"))
      entry(7) should equal(Set[Ide]("z"))

      exit(1) should equal(Set.empty[Ide])
      exit(2) should equal(Set[Ide]("y"))
      exit(3) should equal(Set[Ide]("x", "y"))
      exit(4) should equal(Set[Ide]("y"))
      exit(5) should equal(Set[Ide]("z"))
      exit(6) should equal(Set[Ide]("z"))
      exit(7) should equal(Set.empty[Ide])
    }
  }
  
}
