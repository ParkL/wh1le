package com.github.parkl.wh1le

object WhileSyntax {
  type FlowMember = (Int, Int)
  trait Block {
    val l:Int
  }

  sealed abstract trait AOp
  case object Plus extends AOp { override def toString() = "+" }
  case object Minus extends AOp { override def toString() = "-" }
  case object Times extends AOp { override def toString() = "*"}
  case object Div extends AOp { override def toString() = "/" }

  sealed abstract trait AExp
  case class Ide(x: String) extends AExp {override def toString() = x}
  case class Number(n: Int) extends AExp { override def toString() = s"$n"}
  case class BinaryAExp(a1:AExp, op: AOp, a2:AExp) extends AExp {
    override def toString() = {
      def parens(ae:AExp) = ae match {
        case i:Ide => i
        case n:Number => n
        case b:BinaryAExp => s"($b)"
      }
      val ls = parens(a1)
      val rs = parens(a2)
      s"$ls $op $rs"
    }
  }

  implicit def str2AOp(op:String) = op match {
    case "+" => Plus
    case "-" => Minus
    case "*" => Times
    case "/" => Div
  }

  implicit def string2Ide(s:String) = Ide(s)
  implicit def int2Number(n:Int) = Number(n)

  sealed abstract trait BOp
  case object eq extends BOp { override def toString() = "="}

  sealed abstract trait ROp
  case object Lt extends ROp { override def toString() = "<"}
  case object Gt extends ROp { override def toString() = ">"}

  implicit def str2ROp(s:String): ROp = s match {
    case "<" => Lt
    case ">" => Gt
  }

  sealed abstract trait BExp
  case object True extends BExp { override def toString() = "True" }
  case object False extends BExp { override def toString() = "False"}
  case class Not(b: BExp) extends BExp { override def toString() = s"not($b)"}
  case class BOpBExp(b1:BExp, bOp: BOp, b2:BExp) extends BExp {
    override def toString() = {
      def parens(be:BExp) = be match {
        case b: BOpBExp => s"($b)"
        case r: ROpBExp => s"($r)"
        case x @ _ => x
      }
      val lhs = parens(b1)
      val rhs = parens(b2)
      s"$lhs $bOp $rhs"
    }
  }
  case class ROpBExp(a1: AExp, rOp: ROp, a2: AExp) extends BExp {
    override def toString() = {
      def parens(ae:AExp) = ae match {
        case i:Ide => i
        case n:Number => n
        case b:BinaryAExp => s"($b)"
      }
      val lhs = parens(a1)
      val rhs = parens(a2)
      s"$lhs $rOp $rhs"
    }
  }


  sealed abstract trait Statement
  case class Assignment(id:String, exp:AExp, l:Int) extends Statement with Block {
    override def toString() = s"[$id := $exp]^$l"
  }
  case class Skip(l:Int) extends Statement with Block {
    override def toString() = s"[skip]^$l"
  }
  case class Composition(s1: Statement, s2:Statement) extends Statement {
    override def toString() = s"$s1 ; $s2"
  }
  case class If(b: BExp, l: Int, s1: Statement, s2: Statement) extends Statement with Block {
    override def toString() = s"if [$b]^$l then $s1 else $s2"
  }
  case class While(cond: BExp, l:Int, s: Statement) extends Statement with Block {
    override def toString() = s"while [$cond]^$l do $s od"
  }

  // TODO make tail
  implicit def program(prog: List[Statement]):Statement = prog match {
    case Nil => ??? // empty program is undefined
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

  def aExp(a: AExp):Set[AExp] = a match {
    case Ide(x) => Set.empty
    case Number(n) => Set.empty
    case a @ BinaryAExp(a1, op, a2) => Set(a) ++ aExp(a1) ++ aExp(a2)
  }

  def aExp(b: BExp):Set[AExp] = b match {
    case True => Set.empty
    case False => Set.empty
    case Not(b) => aExp(b)
    case BOpBExp(b1, bOp, b2) => Set.empty
    case ROpBExp(a1, rOp, a2) => aExp(a1) ++ aExp(a2)
  }

  def aExpStar(s:Statement):Set[AExp] = s match {
    case Assignment(id, exp, l) => aExp(exp)
    case Skip(l) => Set.empty
    case Composition(s1, s2) => aExpStar(s1) ++ aExpStar(s2)
    case If(b, l, s1, s2) => aExp(b) ++ aExpStar(s1) ++ aExpStar(s2)
    case While(cond, l, s) => aExp(cond) ++ aExpStar(s)
  }

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

