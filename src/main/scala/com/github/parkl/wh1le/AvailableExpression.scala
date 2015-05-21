package com.github.parkl.wh1le

import WhileSyntax._

/**
 * Created by bernd on 5/21/15.
 */
object AvailableExpression {
  def apply(s: Statement) = new AvailableExpression(s)
}

class AvailableExpression(s:Statement) {
  val bx = block(s)_

  def kill(i:Int):Set[AExp] = bx(i).get match {
    case Assignment(id, exp, l) => for {
      aPrime <- aExpStar(s, i)
      if fv(aPrime).contains(id)
    } yield aPrime
    case _ => Set.empty
  }
  def gen(i:Int):Set[AExp] = bx(i).get match {
    case Assignment(id, exp, l) => for {
      aPrime <- aExpr(exp)
      if !(fv(aPrime).contains(id))
    } yield aPrime
    case Skip(l) => Set.empty
    case If(b, l, s1, s2) => aExpr(b)
    case While(cond, l, s) => aExpr(cond)
    case _ => Set.empty
  }
}
