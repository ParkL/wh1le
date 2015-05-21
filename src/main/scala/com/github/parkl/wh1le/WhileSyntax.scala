package com.github.parkl.wh1le

object WhileSyntax {
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
  
  sealed abstract trait BExp
  case object True extends BExp
  case object False extends BExp
  case class Not(b: BExp) extends BExp
  case class BinaryBOpBExp(b1:BExp, bOp: BOp, b2:BExp) extends BExp
  case class BinaryRopBexp(a1: AExp, rOp: ROp, a2: AExp) extends BExp

  sealed abstract trait Statement
  case class Assignment(id:String, exp:AExp) extends Statement
  case object Skip extends Statement
  case class Composition(s1: Statement, s2:Statement) extends Statement
  case class Cond(i: BExp, t: Statement, e: Statement) extends Statement
  case class Loop(cond: BExp, s: Statement) extends Statement

}

