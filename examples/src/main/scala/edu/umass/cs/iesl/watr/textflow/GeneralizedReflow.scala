package edu.umass.cs.iesl.watr
package textflow


import scalaz._, Scalaz.{fix => _, _}

import matryoshka._
import matryoshka.data._

import FunctorT.ops._


class TextReflowAtomOps(
  val chars: Seq[Char]
) {
  override def toString = chars.mkString
}


sealed trait TextReflowF[+A]

object TextReflowF {
  case class Atom(c: Any, ops: TextReflowAtomOps)         extends TextReflowF[Nothing]
  case class Insert(value: String)                        extends TextReflowF[Nothing]
  case class Rewrite[A](from: A, to: String)              extends TextReflowF[A]
  case class Bracket[A](pre: String, post: String, a: A)  extends TextReflowF[A]
  case class Flow[A](labels: Set[String], as: List[A])     extends TextReflowF[A]
  case class Labeled[A](labels: Set[String], a: A)         extends TextReflowF[A]

  implicit val ReflowTraverse: Traverse[TextReflowF] = new Traverse[TextReflowF] {
    def traverseImpl[G[_], A, B](fa: TextReflowF[A])(f: A => G[B])(implicit G: Applicative[G]): G[TextReflowF[B]] = fa match {
      case Atom(c, ops)               => G.point(Atom(c, ops))
      case Insert(value)              => G.point(Insert(value))
      case Rewrite(from, to)          => f(from).map(Rewrite(_, to))
      case Bracket(pre, post, a)      => f(a).map(Bracket(pre, post, _))
      case Flow(labels, atoms)        => atoms.traverse(f).map(Flow(labels, _))
      case Labeled(labels, a)         => f(a).map(Labeled(labels, _))
    }
  }

  implicit val show: Delay[Show, TextReflowF] = new Delay[Show, TextReflowF] {
    def apply[A](show: Show[A]) = Show.show {
      case Atom(c, ops)               => ops.toString
      case Insert(value)              => s"+'$value'"
      case Rewrite(from, to)          => s"-+'${to}'"
      case Bracket(pre, post, a)      => s"""${pre}`${a.toString}`{post} """
      case Flow(ls, atoms)            => s"""flow${ls.mkString(":#", " #", "")}"""
      case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""
    }
  }


}


// TODO rename this to avoid object name/type clashes
object TextReflow {
  import TextReflowF._

  type TextReflow = Fix[TextReflowF]

  type TextReflowU = TextReflowF[Fix[TextReflowF]]

  def fixf = Fix[TextReflowF](_)

  // def atom[AtomT](c: AtomT, ops:TextReflowAtomOps) = fixf(Atom(c, ops))

  // dbef flow(as:TextReflow*) = flows(as)

  def flows(as: Seq[TextReflow]) = fixf(Flow(Set(), as.toList))

  def bracket(pre: Char, post: Char, a:TextReflow) = fixf(
    Bracket(pre.toString, post.toString, a)
  )
  def bracket(pre: String, post: String, a:TextReflow) = fixf(
    Bracket(pre, post, a)
  )

  def labeled(l: String, a:TextReflow) = fixf(Labeled(Set(l), a))
  def insert(s: String) = fixf(Insert(s))
  def space() = insert(" ")



  def addLabel(l: String): TextReflow => TextReflow = tr => fixf(tr.unFix match {
    case f @ Flow(ls, as)    => f.copy(labels = ls + l)
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  })


  def hasLabel(l: String): TextReflowU => Boolean = _ match {
    case Labeled(labels, _) if labels.contains(l) => true
    case _ => false
  }


  def everySequence(r: TextReflow)(f: List[TextReflow] => List[TextReflow]): TextReflow = {
    def atFlows: TextReflowU => TextReflowU = r => r match {
      case fl @ Flow(labels: Set[String], as: List[TextReflow]) =>
        fl.copy(as = f(as))
      case fl => fl
    }

    r.transCata(atFlows)
  }

  def everyLabel(l: String, r: TextReflow)(f: TextReflow => TextReflow): TextReflow = {
    def ifLabeled(r:TextReflowU): TextReflowU =  {
      if (hasLabel(l)(r)) holes(r) match {
        case Labeled(labels, (a, fWhole)) => fWhole(f(a))
        case _ => r
      } else r
    }

    r.transCata(ifLabeled)
  }

  def everywhere(r: TextReflow)(f: TextReflowU => TextReflowU): TextReflow = {
    r.transCata(f)
  }

  // def prettyPrintTree(reflow: TextReflow): TB.Box = {
  //   import utils.ScalazTreeImplicits._
  //   reflow.cata(toTree).draw
  // }
  def prettyPrintCofree[B](cof: Cofree[TextReflowF, B])(implicit
    BS: Show[B],
    CS: Delay[Show, Cofree[TextReflowF, ?]]
  ): String = {
    CS(BS).shows(cof)
  }
  // Tree.Node(c.head, c.tail.map(from(_)))
  def cofreeToTree[A](c: Cofree[TextReflowF, A]): Tree[A] = {
    Tree.Node(
      c.head,
      c.tail.toStream.map(cofreeToTree(_))
    )
  }
  // def printCofree[B](cof: Cofree[TextReflowF, B])(implicit
  //   BS: Show[B],
  //   CS: Delay[Show, Cofree[TextReflowF, ?]]
  // ): TB.Box = {
  //   import utils.ScalazTreeImplicits._
  //   // CS(BS).shows(cof)
  //   cofreeToTree(cof).draw
  // }

  private def mkPad(s: String): TextReflow = insert(s)

  def join(sep:String)(bs:TextReflow*): TextReflow =
    joins(sep)(bs.toSeq)

  def joins(sep:String)(bs:Seq[TextReflow]): TextReflow =
    concat(bs.toList intersperse mkPad(sep))

  def concat(bs: Seq[TextReflow]): TextReflow = {
    flows(bs)
  }

}
