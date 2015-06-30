package com.github.parkl.wh1le

import com.github.parkl.wh1le.WhileSyntax._

/**
 * Created by bernd on 6/29/15.
 */
object TestPrograms {

  //  z:=1;
  //  while (y>0) do
  //    x:=x-1
  //  od;
  //  x:=2
  val program1 = assignLabels(List[Statement](
    Assignment("z", 1),
    While(ROpBExp("y", ">", 0),
      Assignment("x", BinaryAExp("x", "+", 1))),
    Assignment("x", 2)
  ))


  def program2:Statement = {
    val fiveTimesX = BinaryAExp(5, "*", "x")
    val fiveTimesXMinusY = BinaryAExp(fiveTimesX, "-", "y")
    val exp1 = ROpBExp(fiveTimesXMinusY, "<", 5)

    val yPlus4 = BinaryAExp("y", "+", 4)
    val exp2 = BinaryAExp(yPlus4, "*", "x")

    val exp3 = BinaryAExp(fiveTimesX, "*", yPlus4)

    val xPlusY = BinaryAExp("x", "+", "y")
    val xPlusYGt1 = ROpBExp(xPlusY, ">", 1)
    val exp4 = xPlusYGt1

    val yBy2 = BinaryAExp("y", "/", 2)
    val yBy2MinusX = BinaryAExp(yBy2, "-", "x")
    val exp5 = yBy2MinusX

    val exp6 = BinaryAExp(fiveTimesX, "+", yPlus4)

    assignLabels(List[Statement](
      If(exp1,
        Assignment("z", exp2),
        Assignment("z", exp3)),
      While(exp4, Assignment("y", exp5)),
      Assignment("z", exp6)
    ))
  }

  private val aPLusB: BinaryAExp = BinaryAExp("a", "+", "b")
  //  x:=a+b;
//  while (y>0)
//    do
//      if (y>5) then y:=a+b
//      else z:=a+b
//    ;
//  y:=y-1
//  od
  val program3 = assignLabels(List[Statement](
    Assignment("x", aPLusB),
    While(ROpBExp("y", ">", 0), List[Statement](
      If(ROpBExp("y", ">", 5),
        Assignment("y", aPLusB),
        Assignment("z", aPLusB)
      ),
      Assignment("y", BinaryAExp("y", "-", 1))
    ))
  ))

//  y:=0;
//  if (x>2)
//    then x := 0-x
//  else x := 2*x
//  fi;
//  while (x>0)
//    do
//      z:=z-1;
//  x:=x+z
//  od;
//  a:=x
  val program4 = assignLabels(List[Statement](
    Assignment("y", 0),
    If(ROpBExp("x", ">", 2),
      Assignment("x", "x"), // ATTN actually is -x which is not really defined in our while
      Assignment("x", BinaryAExp(2, "*", "x"))),
    While(ROpBExp("x", ">", 0),
      List[Statement](
        Assignment("z", BinaryAExp("z", "-", 1)),
        Assignment("x", BinaryAExp("x", "+", "z"))
      )),
    Assignment("a", "x"))
  )

//  z:=0;
//  while (not (x==y) )
//    do
//      z:=z+1;
//  if (x>y) then x:=x-y
//  else y:=y-x
//  od;
//  w:=x

  val program5 = assignLabels(List[Statement](
    Assignment("z", 0),
    While(Not(ROpBExp("x", "=", "y")), List[Statement](
      Assignment("z", BinaryAExp("z", "+", 1)),
      If(ROpBExp("x", ">", "y"),
        Assignment("x", BinaryAExp("x", "-", "y")),
        Assignment("y", BinaryAExp("y", "-", "x"))
      )
    )),
    Assignment("w", "x")
  ))

  val program6 = assignLabels(List[Statement](
    Assignment("x", "y"),
    Assignment("y", 25),
    While(ROpBExp("y", ">", "x"),
      While(ROpBExp("x", "<", 5),
        Assignment("x", BinaryAExp("x", "+", "y"))
      )
    ),
    Assignment("x", BinaryAExp(2, "*", "y"))
  ))
}
