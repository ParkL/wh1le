package com.github.parkl.wh1le

import org.scalatest.{FunSpec, Matchers}

/**
 * Created by bernd on 6/23/15.
 */
class ReachingDefinitionAnalysisTest extends FunSpec with Matchers {
  import WhileSyntax._

  val example = assignLabels(List[Statement](
    Assignment("x", 5),
    Assignment("y", 1),
    While(ROpBExp("x", ">", 1),
      List[Statement](
        Assignment("y", BinaryAExp("x", "*", "y")),
        Assignment("x", BinaryAExp("x", "-", 1))
      )
    )
  ))

  val udduexample = assignLabels(List[Statement](
      Assignment("x", 0),
      Assignment("x", 3),
      If(ROpBExp("z", "=", "x"),
        Assignment("z", 0),
        Assignment("z", "x")
      ),
      Assignment("y", "x"),
      Assignment("x", BinaryAExp("y", "+", "z"))
    )
  )

  describe("Reaching definition analysis ") {
    def lift(s:Set[(String, Int)]):Set[(Ide, Int)] = s.map {case (i, n) => (Ide(i), n)}
    it("should compute i") {
      ReachingDefinition(example).i should equal(Set((Ide("x"),-1), (Ide("y"), -1)))
    }
    it("should compute kill sets") {
      val rd = ReachingDefinition(example)
      rd.kill(1) should equal(lift(Set(("x", -1), ("x", 1), ("x", 5))))
      rd.kill(2) should equal(lift(Set(("y", -1), ("y", 2), ("y", 4))))
      rd.kill(3) should equal(Set.empty)
      rd.kill(4) should equal(lift(Set(("y", -1), ("y", 2), ("y", 4))))
      rd.kill(5) should equal(lift(Set(("x", -1), ("x", 1), ("x", 5))))
    }
    it("should compute gen sets") {
      val rd = ReachingDefinition(example)
      rd.gen(1) should equal(lift(Set(("x", 1))))
      rd.gen(2) should equal(lift(Set(("y", 2))))
      rd.gen(3) should equal(Set.empty)
      rd.gen(4) should equal(lift(Set(("y", 4))))
      rd.gen(5) should equal(lift(Set(("x", 5))))
    }
    it("should compute the smallest solution") {
      val (enter, exit) = ReachingDefinition(example).solve()
      enter(1) should equal(lift(Set(("x", -1), ("y", -1))))
      enter(2) should equal(lift(Set(("x", 1), ("y", -1))))
      enter(3) should equal(lift(Set(("x", 1), ("y", 2), ("y", 4), ("x", 5))))
      enter(4) should equal(lift(Set(("x", 1), ("y", 2), ("y", 4), ("x", 5))))

      exit(1) should equal(lift(Set(("x", 1), ("y", -1))))
      exit(2) should equal(lift(Set(("x", 1), ("y", 2))))
      exit(3) should equal(lift(Set(("x", 1), ("y", 2), ("y", 4), ("x", 5))))
      // exit(4) should equal(lift(Set(("y", 4), ("x", 5)))) // TODO says so in the slides but I think it's wrong..
      exit(4) should equal(lift(Set(("y", 4), ("x", 5), ("x", 1)))) // I think this is correct ...
    }
    it("should properly compute ud sets") {
      val df = ReachingDefinition(udduexample)
      val udx: (Int) => Set[Int] = df.ud("x", _:Int)
      val udy: (Int) => Set[Int] = df.ud("y", _:Int)
      val udz: (Int) => Set[Int] = df.ud("z", _:Int)
      for(l <- List(1,2,4,7)) udx(l) should equal(Set.empty)
      for(l <- List(3,5,6)) udx(l) should equal(Set(2))

      for(l <- 1 to 6) udy(l) should equal(Set.empty)
      udy(7) should equal(Set(6))

      for(l <- ((1 to 2) ++ (4 to 6))) udz(l) should equal(Set.empty)
      udz(3) should equal(Set(-1))
      udz(7) should equal(Set(4,5))
    }
    it("should properly compute du sets") {
      val df = ReachingDefinition(udduexample)
      val dux = df.du("x", _:Int)
      val duy = df.du("y", _:Int)
      val duz = df.du("z", _:Int)
      dux(1) should equal(Set.empty)
      dux(2) should equal(Set(3,5,6))
      for(l <- 3 to 7) dux(l) should equal(Set.empty)
      dux(-1) should equal(Set.empty)

      for(l <- 1 to 5) duy(l) should equal(Set.empty)
      duy(6) should equal(Set(7))
      duy(7) should equal(Set.empty)
      duy(-1) should equal(Set.empty)

      for(l <- 1 to 3) duz(l) should equal(Set.empty)
      for(l <- 4 to 5) duz(l) should equal(Set(7))
      for(l <- 6 to 7) duz(l) should equal(Set.empty)
      duz(-1) should equal(Set(3))
    }
  }
}
