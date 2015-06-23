package com.github.parkl.wh1le
import language.implicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

object WhileSyntax extends JavaTokenParsers{
  type FlowElement = (Int, Int)
  sealed trait Block {
    val l:Option[Int]
  }

  sealed abstract trait AOp
  case object Plus extends AOp { override def toString() = "+" }
  case object Minus extends AOp { override def toString() = "-" }
  case object Times extends AOp { override def toString() = "*"}
  case object Div extends AOp { override def toString() = "/" }

  sealed abstract trait AExp
  case class Ide(x: String) extends AExp { override def toString() = x}
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
  case object Eq extends ROp { override def toString() = "="}
  implicit def str2ROp(s:String): ROp = s match {
    case "<" => Lt
    case ">" => Gt
    case "=" => Eq
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
  case class Assignment(id:Ide, exp:AExp, l:Option[Int] = None) extends Statement with Block {
    override def toString() = s"[$id := $exp]^$l"
  }
  case class Skip(l: Option[Int] = None) extends Statement with Block {
    override def toString() = s"[skip]^$l"
  }
  case class Composition(s1: Statement, s2:Statement) extends Statement {
    override def toString() = s"$s1 ; $s2"
  }
  case class If(b: BExp, s1: Statement, s2: Statement, l: Option[Int] = None) extends Statement with Block {
    override def toString() = s"if [$b]^$l then $s1 else $s2"
  }
  case class While(cond: BExp, s: Statement, l: Option[Int] = None) extends Statement with Block {
    override def toString() = s"while [$cond]^$l do $s od"
  }

  // TODO make tail
  implicit def program(prog: List[Statement]):Statement = prog match {
    case Nil => ??? // empty program is undefined
    case s :: Nil => s
    case s :: ss => Composition(s, program(ss))
  }

  def labels(s: Statement): Set[Int] = s match {
    case Assignment(id, exp, Some(l)) => Set(l)
    case Skip(Some(l)) => Set(l)
    case Composition(s1, s2) => labels(s1) ++ labels(s2)
    case If(i, t, e, Some(l)) => Set(l) ++ labels(t) ++ labels(e)
    case While(cond, s, Some(l)) => Set(l) ++ labels(s)
    case _ => ???
  }

  def init(s:Statement): Int = s match {
    case Assignment(id, exp, Some(l)) => l
    case Skip(Some(l)) => l
    case Composition(s1, s2) => init(s1)
    case If(i, t, e, Some(l)) => l
    case While(cond, s, Some(l)) => l
    case _ => ???
  }

  def f1nal(s:Statement): Set[Int] = s match {
    case Assignment(id, exp, Some(l)) => Set(l)
    case Skip(Some(l)) => Set(l)
    case Composition(s1, s2) => f1nal(s2)
    case If(i, t, e, Some(l)) => f1nal(t) ++ f1nal(e)
    case While(cond, s, Some(l)) => Set(l)
    case _ => ???
  }

  def flow(s:Statement): Set[FlowElement] = s match {
    case Assignment(id, exp, Some(l)) => Set.empty
    case Skip(Some(l)) => Set.empty
    case Composition(s1, s2) =>
      flow(s1) ++ flow(s2) ++ (for {
        l <- f1nal(s1)
      } yield (l, init(s2)))
    case If(b, s1, s2, Some(l)) => flow(s1) ++ flow(s2) ++ Set((l, init(s1)), (l, init(s2)))
    case While(cond, s, Some(l)) => Set((l, init(s))) ++ flow(s) ++ (for {
        lPrime <- f1nal(s)
      } yield(lPrime, l))
    case _ => ???
  }

  def flowR(s:Statement): Set[FlowElement] = flow(s).collect({case (l1, l2) => (l2, l1)})

  def blocks(s:Statement): Set[Block] = s match {
    case a @ Assignment(id, exp, Some(l)) => Set(a)
    case s @ Skip(Some(l)) => Set(s)
    case Composition(s1, s2) => blocks(s1) ++ blocks(s2)
    case i @ If(b, s1, s2, Some(l)) => Set(i) ++ blocks(s1) ++ blocks(s2)
    case w @ While(cond, s, Some(l)) => Set(w) ++ blocks(s)
    case _ => ???
  }

  def block(s:Statement)(l:Int):Option[Block] = blocks(s).find(_.l == Some(l))

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
    case If(b, s1, s2, _) => aExp(b) ++ aExpStar(s1) ++ aExpStar(s2)
    case While(cond, s, _) => aExp(cond) ++ aExpStar(s)
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

  def fv(s:Statement):Set[Ide] = s match {
    case Assignment(id, exp, _) => Set(id) ++ fv(exp)
    case Skip(_) => Set.empty
    case Composition(s1, s2) => fv(s1) ++ fv(s2)
    case If(b, s1, s2, _) => fv(b) ++ fv(s1) ++ fv(s2)
    case While(cond, s, _) => fv(cond) ++ fv(s)
  }

  def assignLabels(s:Statement):Statement = {
    def al(s:Statement, i:Int = 1):(Statement, Int) = s match {
      case a @ Assignment(id, exp, None) => (a.copy(l = Some(i)), i + 1)
      case s @ Skip(None) => (s.copy(l = Some(i)), i + 1)
      case c @ Composition(s1, s2) => {
        val (s1WithLabel, j) = al(s1, i)
        val (s2WithLabel, k) = al(s2, j)
        (c.copy(s1 = s1WithLabel, s2 = s2WithLabel), k)
      }
      case c @ If(b, s1, s2, None) => {
        val ifI = i
        val (s1WithLabel, j) = al(s1, ifI + 1)
        val (s2WithLabel, k) = al(s2, j)
        (c.copy(l = Some(ifI), s1 = s1WithLabel, s2 = s2WithLabel), k)
      }
      case w @ While(cond, s, None) => {
        val whileI = i
        val (sWithLabel, j) = al(s, i + 1)
        (w.copy(s = sWithLabel, l = Some(whileI)), j)
      }
      case _ => throw new IllegalArgumentException(s"Label already assigned: $s")
    }
    val (sFinal, _) = al(s)
    sFinal
  }
}

