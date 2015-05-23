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
      aPrime <- aExpStar(s)
      if fv(aPrime).contains(id)
    } yield aPrime
    case _ => Set.empty
  }
  def gen(i:Int):Set[AExp] = bx(i).get match {
    case Assignment(id, exp, l) => for {
      aPrime <- aExp(exp)
      if !(fv(aPrime).contains(id))
    } yield aPrime
    case Skip(l) => Set.empty
    case If(b, l, s1, s2) => aExp(b)
    case While(cond, l, s) => aExp(cond)
    case _ => Set.empty
  }

  type ResultMap = Map[Int, Set[AExp]]

  def solve():(ResultMap, ResultMap) = {
    var W = flow(s).toList
    var enter:ResultMap = (for {
      l <- labels(s)
      v = if(l == init(s))
            Set.empty[AExp]
          else
            aExpStar(s)
    } yield(l -> v)).toMap

    var leave:ResultMap = Map.empty

    while(W.nonEmpty) {
      val (l, ll) = W.head
      W = W.tail
      val lvl = (enter(l) -- kill(l)) ++ gen(l)
      leave = leave + (l -> lvl)
      if(enter(ll).subsetOf(lvl)) {
        enter = enter + (ll -> (enter(ll) intersect lvl))
        flow(s).filter(_._1 == ll).foreach{ p =>
          W = p :: W
        }
      }
    }
    (enter, leave)
  }

}
