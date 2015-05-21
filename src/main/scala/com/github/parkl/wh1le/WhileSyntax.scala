package com.github.parkl.wh1le

object WhileSyntax {
  type FlowMember = (Int, Int)
  trait Block {
    val l:Int
  }

  sealed abstract trait AOp
  case object Plus extends AOp
  case object Minus extends AOp
  case object Times extends AOp
  case object Div extends AOp

  sealed abstract trait AExp
  case class Ide(x: String) extends AExp
  case class Number(n: Int) extends AExp
  case class BinaryAExp(a1:AExp, op: AOp, a2:AExp) extends AExp

  implicit def str2AOp(op:String) = op match {
    case "+" => Plus
    case "-" => Minus
    case "*" => Times
    case "/" => Div
  }

  implicit def string2Ide(s:String) = Ide(s)
  implicit def int2Number(n:Int) = Number(n)

  sealed abstract trait BOp
  case object eq extends BOp

  sealed abstract trait ROp
  case object Lt extends ROp
  case object Gt extends ROp

  implicit def str2ROp(s:String): ROp = s match {
    case "<" => Lt
    case ">" => Gt
  }

  sealed abstract trait BExp
  case object True extends BExp
  case object False extends BExp
  case class Not(b: BExp) extends BExp
  case class BOpBExp(b1:BExp, bOp: BOp, b2:BExp) extends BExp
  case class ROpBExp(a1: AExp, rOp: ROp, a2: AExp) extends BExp


  sealed abstract trait Statement
  case class Assignment(id:String, exp:AExp, l:Int) extends Statement with Block
  case class Skip(l:Int) extends Statement with Block
  case class Composition(s1: Statement, s2:Statement) extends Statement
  case class If(b: BExp, l: Int, s1: Statement, s2: Statement) extends Statement with Block
  case class While(cond: BExp, l:Int, s: Statement) extends Statement with Block

  // TODO make tail
  implicit def program(prog: List[Statement]):Statement = prog match {
    case s :: Nil => s
    case s :: ss => Composition(s, program(ss))
  }

  def labels(s: Statement): Set[Int] = s match {
    case Assignment(id, exp, l) => Set(l)
    case Skip(l) => Set(l)
    case Composition(s1, s2) => labels(s1) ++ labels(s2)
    case If(i, l, t, e) => Set(l) ++ labels(t) ++ labels(e)
    case While(cond, l, s) => Set(l) ++ labels(s)
  }

  def init(s:Statement): Int = s match {
    case Assignment(id, exp, l) => l
    case Skip(l) => l
    case Composition(s1, s2) => init(s1)
    case If(i, l, t, e) => l
    case While(cond, l, s) => l
  }

  def f1nal(s:Statement): Set[Int] = s match {
    case Assignment(id, exp, l) => Set(l)
    case Skip(l) => Set(l)
    case Composition(s1, s2) => f1nal(s2)
    case If(i, l, t, e) => f1nal(t) ++ f1nal(e)
    case While(cond, l, s) => Set(l)
  }

  def flow(s:Statement): Set[FlowMember] = s match {
    case Assignment(id, exp, l) => Set.empty
    case Skip(l) => Set.empty
    case Composition(s1, s2) =>
      flow(s1) ++ flow(s2) ++ (for {
        l <- f1nal(s1)
      } yield (l, init(s2)))
    case If(b, l, s1, s2) => flow(s1) ++ flow(s2) ++ Set((l, init(s1)), (l, init(s2)))
    case While(cond, l, s) => Set((l, init(s))) ++ flow(s) ++ (for {
        lPrime <- f1nal(s)
      } yield(lPrime, l))
  }

  def flowR(s:Statement): Set[FlowMember] = flow(s).collect({case (l1, l2) => (l2, l1)})

  def blocks(s:Statement): Set[Block] = s match {
    case a @ Assignment(id, exp, l) => Set(a)
    case s @ Skip(l) => Set(s)
    case Composition(s1, s2) => blocks(s1) ++ blocks(s2)
    case i @ If(b, l, s1, s2) => Set(i) ++ blocks(s1) ++ blocks(s2)
    case w @ While(cond, l, s) => Set(w) ++ blocks(s)
  }

  def block(s:Statement)(l:Int):Option[Block] = blocks(s).find(_.l == l)

  def aExpr(a: AExp):Set[AExp] = a match {
    case Ide(x) => Set.empty
    case Number(n) => Set.empty
    case a @ BinaryAExp(a1, op, a2) => Set(a) ++ aExpr(a1) ++ aExpr(a2)
  }

  def aExpr(b: BExp):Set[AExp] = b match {
    case True => Set.empty
    case False => Set.empty
    case Not(b) => aExpr(b)
    case BOpBExp(b1, bOp, b2) => Set.empty
    case ROpBExp(a1, rOp, a2) => aExpr(a1) ++ aExpr(a2)
  }

  def aExpStar(s:Statement, upToBlock:Int):Set[AExp] = s match {
      case Assignment(id, exp, l) if l <= upToBlock => aExpr(exp)
      case Skip(l) if l <= upToBlock => Set.empty
      case Composition(s1, s2) => aExpStar(s1, upToBlock) ++ aExpStar(s2, upToBlock)
      case If(b, l, s1, s2) if l <= upToBlock => aExpr(b) ++ aExpStar(s1, upToBlock) ++ aExpStar(s2, upToBlock)
      case While(cond, l, s) if l <= upToBlock => aExpr(cond) ++ aExpStar(s, upToBlock)
      case _ => Set.empty
    }

  def aExpStar(s:Statement):Set[AExp] = aExpStar(s, blocks(s).size)

  def fv(aExp: AExp):Set[Ide] = aExp match {
    case i:Ide => Set(i)
    case Number(n) => Set.empty
    case BinaryAExp(a1, op, a2) => fv(a1) ++ fv(a2)
  }

  def fv(bExp: BExp):Set[Ide] = bExp match {
    case ROpBExp(a1, rOp, a2) => fv(a1) ++ fv(a2)
    case True => Set.empty
    case False => Set.empty
    case Not(b) => fv(b)
    case BOpBExp(b1, bOp, b2) => fv(b1) ++ fv(b2)
  }
}

