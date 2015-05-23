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

  type EnterLeaveSet = scala.collection.mutable.Map[Int, Set[AExp]]

  def solve() = {
    var W = List[(Int, Int)]()
    val AEEnter = scala.collection.mutable.HashMap.empty[Int, Set[AExp]]
    val AELeave = scala.collection.mutable.HashMap.empty[Int, Set[AExp]]

    W = flow(s).toList
    labels(s).foreach( l =>
      if(l == init(s)) AEEnter += (l -> Set.empty)
      else AEEnter += (l -> aExpStar(s))
    )

    while(W.nonEmpty) {
      val (l, lp) = W.head
      W = W.tail
      AELeave += (l -> ((AEEnter(l) -- kill(l)) ++ gen(l)))
      if(AEEnter(lp).subsetOf(AELeave(l))) {
        AEEnter += (lp -> (AEEnter(l).intersect(AELeave(l))))
        flow(s).filter(_._1 == lp).foreach(t => W = t :: W)
      }
    }
    (AEEnter, AELeave)
  }

}
