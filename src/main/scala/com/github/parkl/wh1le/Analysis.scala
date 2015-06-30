package com.github.parkl.wh1le

import WhileSyntax._
import language.implicitConversions

/**
 * Created by bernd on 5/21/15.
 */

abstract class Analysis(s:Statement) { self =>
  type L
  def F: Set[FlowElement]
  def E: Set[Int]
  def i: L
  def bottom: L
  def <= : (L, L) => Boolean
  def cup: (L, L) => L

  def fl(A: Map[Int, L], l: Int): L

  // I can haz binary operators on stuff
  implicit class CupProvider(s1: L) {
    def cup(s2: L):L = self.cup(s1, s2)
  }

  implicit class NotSubSomethingProvider(s1: L) {
    def <=(s2: L) = self.<=(s1, s2)
  }

  type ResultMap = Map[Int, L]
  type Logger = ( FlowElement,        // Current element
                  List[FlowElement],  // Rest of worklist,
                  ResultMap,          // A
                  ResultMap)          // fl(A)
                    => Unit

  private val emptyLogger: (Logger) = (_,_,_,_) => {}
  protected val bx = block(s)_

  def solve(logger:Logger = emptyLogger):(ResultMap, ResultMap) = {
    def flA(A: ResultMap) = A collect { case (l, v) => l -> fl(A, l) }
    var W = F.toList //.sortBy(_._1)
    var A:ResultMap = (for {
      l <- labels(s)
      v = if(E.contains(l)) i else bottom
    } yield(l -> v)).toMap
    while(W.nonEmpty) {
      val ((l, ll) :: ws) = W
      W = ws
      if(! (fl(A, l) <= A(ll))) {
        A = A + (ll -> (A(ll) cup fl(A, l)))
        F.filter(_._1 == ll).foreach{ p => W = p :: W }
      }
      logger((l,ll), W, A, flA(A))
    }
    (A, flA(A))
  }
}

trait GenKill[SET_TYPE] { self: Analysis =>
  type L = Set[SET_TYPE]
  override def fl(A:Map[Int, L], l:Int) = (A(l) -- kill(l)) union gen(l)
  def kill(i:Int): L
  def gen(i:Int): L
}

object AvailableExpression {
  def apply(s: Statement) = new AvailableExpression(s)
}
class AvailableExpression(s: Statement) extends Analysis(s) with GenKill[AExp] {
  def kill(i:Int):L = bx(i).get match {
    case Assignment(id, exp, l) => for {
      aPrime <- aExpStar(s)
      if fv(aPrime).contains(id)
    } yield aPrime
    case _ => Set.empty
  }
  def gen(i:Int):L = bx(i).get match {
    case Assignment(id, exp, l) => for {
      aPrime <- aExp(exp)
      if !(fv(aPrime).contains(id))
    } yield aPrime
    case Skip(l) => Set.empty
    case If(b, s1, s2, _) => aExp(b)
    case While(cond, s, _) => aExp(cond)
    case _ => Set.empty
  }

  override def F: Set[FlowElement] = flow(s)
  override def E: Set[Int] = Set(init(s))
  override def i: L = Set.empty
  override def bottom: L = aExpStar(s)
  override def <= : (L, L) => Boolean = (s1, s2) => s2 subsetOf s1
  override def cup: (L, L) => L = (s1, s2) => s1 intersect s2
}

object LiveVariables {
  def apply(s:Statement) = new LiveVariables(s)
}

class LiveVariables(s:Statement) extends Analysis(s) with GenKill[Ide] {
  def kill(i: Int):L = bx(i).get match {
    case Assignment(id, exp, l) => Set(id)
    case Skip(l) => Set.empty
    case If(b, s1, s2, _) => Set.empty
    case While(cond, s, _) => Set.empty
  }

  def gen(i: Int): L = bx(i).get match {
    case Assignment(id, exp, l) => fv(exp)
    case Skip(l) => Set.empty
    case If(b, s1, s2, _) => fv(b)
    case While(cond, s, _) => fv(cond)
  }

  override def bottom: L = Set.empty
  override def cup: (L, L) => L = _ union _
  override def F: Set[FlowElement] = flowR(s)
  override def i: L = Set.empty
  override def E: Set[Int] = f1nal(s)
  override def <= : (L, L) => Boolean = (s1, s2) => s1 subsetOf s2
}

object VeryBusyExpression {
  def apply(s:Statement) = new VeryBusyExpression(s)
}

class VeryBusyExpression(s:Statement) extends Analysis(s) with GenKill[AExp] {
  def kill(i: Int): L = bx(i).get match {
    case Assignment(id, exp, l) => for {
      aP <- aExpStar(s)
      if (fv(aP).contains(id))
    } yield aP
    case Skip(l) => Set.empty
    case If(b, s1, s2, _) => Set.empty
    case While(cond, s, _) => Set.empty
  }
  def gen(i: Int): L = bx(i).get match {
    case Assignment(id, exp, l) => aExp(exp)
    case Skip(l) => Set.empty
    case If(b, s1, s2, _) => aExp(b)
    case While(cond, s, _) => aExp(cond)
  }

  override def bottom: L = aExpStar(s)
  override def cup: (L, L) => L = _ intersect _
  override def <= : (L, L) => Boolean = (s1, s2) => s2 subsetOf s1
  override def F: Set[FlowElement] = flowR(s)
  override def i: L = Set.empty
  override def E: Set[Int] = f1nal(s)
}

object Dominator {
  def apply(s:Statement) = new Dominator(s)
}
class Dominator(s:Statement) extends Analysis(s) with GenKill[Int]{
  def gen(i: Int): L = Set(i)
  def kill(i: Int): L = Set.empty

  override def bottom: L = blocks(s).flatMap(_.l)
  override def cup: (L, L) => L = _ intersect _
  override def F: Set[(Int, Int)] = flow(s)
  override def <= : (L, L) => Boolean = (s1, s2) => s2 subsetOf s1
  override def i: L = Set.empty
  override def E: Set[Int] = Set(init(s))
}

object ReachingDefinition {
  def apply(s:Statement) = new ReachingDefinition(s)
}

class ReachingDefinition(s:Statement) extends Analysis(s) with GenKill[(Ide,Int)]{
  def used(b:Block):Set[Ide] = b match {
    case Assignment(id, exp, l) => fv(exp)
    case Skip(l) => Set.empty
    case If(b, s1, s2, l) => fv(b)
    case While(cond, s, l) => fv(cond)
  }

  def ud(p: (Ide, Int)):Set[Int] = {
    val (id, i)  = p
    val (enter, _) = solve()
    if(used(bx(i).get).contains(id)) for {
      (x, ll) <- enter(i)
      if id == x
    } yield ll
    else Set.empty
  }

  def du(p: (Ide, Int)):Set[Int] = {
    val (x, i) = p
    for {
      bs <- blocks(s)
      ll <- bs.l
      if ud((x, ll)).contains(i)
    } yield ll
  }

  def maybeLabelIfAssignmentToX(b:Block, x:Ide):Option[Int] = b match {
    case Assignment(`x`, exp, Some(l))  => Some(l)
    case _ => None
  }
  
  def blockNosWithAssignmentToX(x:Ide):Set[Int] = for {
    b <- blocks(s)
    x <- maybeLabelIfAssignmentToX(b, x)
  } yield x

  def kill(i: Int): L = bx(i).get match {
    case Assignment(id, exp, l) => Set((id, -1)) union (for {
      ll <- blockNosWithAssignmentToX(id)
    } yield (id, ll))
    case _ => Set.empty
  }
  def gen(i: Int): L = bx(i).get match {
    case Assignment(id, exp, Some(l)) => Set((id, l))
    case _ => Set.empty
  }

  override def bottom: L = Set.empty
  override def cup: (L, L) => L = _ union _
  override def <= : (L, L) => Boolean = (s1, s2) => s1 subsetOf s2
  override def F: Set[FlowElement] = flow(s)
  override def i: L = for {
    v <- fv(s)
  } yield (v, -1)
  override def E: Set[Int] = Set(init(s))
}