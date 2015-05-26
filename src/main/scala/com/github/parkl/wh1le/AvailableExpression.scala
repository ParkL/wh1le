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

  def solve():(ResultMap, ResultMap, List[String]) = {
    def render(m:ResultMap):String =
      m.toList
        .sortBy(_._1)
        .map({case (k, v) => s"$k -> $v"})
        .foldLeft("")({case (m, e) => s"$m\n$e \\\\"})

    var log = List.empty[String]
    var W = flow(s).toList.sortBy(_._1)
    var after:ResultMap = (for{
      l <- labels(s)
    } yield (l -> Set.empty[AExp])).toMap
    var before:ResultMap = (for {
      l <- labels(s)
      v = if(l == init(s))
            Set.empty[AExp]
          else
            aExpStar(s)
    } yield(l -> v)).toMap

    while(W.nonEmpty) {
      val (l, ll) = W.head
      W = W.tail
      log = (s"Current: ${(l,ll)} :: ${W} \\\\") :: log
      val lvl = (before(l) -- kill(l)) union gen(l) // AEBullet(l)
      after = after + (l -> lvl)
      if(!before(ll).subsetOf(lvl)) {
        before = before + (ll -> (before(ll) intersect lvl))
        flow(s).filter(_._1 == ll).foreach{ p =>
          W = p :: W
        }
      }
      log = s"Before list: \\\\ ${render(before)} \\" :: log
      log = s"After list: \\\\  ${render(after)} \\" :: log
    }
    (before, after, log.reverse)
  }

}
