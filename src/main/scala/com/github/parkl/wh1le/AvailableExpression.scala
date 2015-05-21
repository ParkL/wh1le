package com.github.parkl.wh1le

import WhileSyntax._

/**
 * Created by bernd on 5/21/15.
 */
object AvailableExpression {
  def apply(s: Statement) = new AvailableExpression(s)
}

class AvailableExpression(s:Statement) {
  def kill(i:Int):Set[AExp] = blocks(s).find(_.l == i).get match {
    case Assignment(id, exp, l) => for {
      aPrime <- aExpStar(s, i)
      if fv(aPrime).contains(id)
    } yield aPrime
    case _ => Set.empty
  }
  //def gen(i:Int):Set[AExp] = blocks(s).
}
