package spsc

import Algebra._

class AdvancedTreeBuilder(prog: Program) extends BasicTreeBuilder(prog) {

  def abstractNode(t: Tree, a: Node, term: Term,
                   bs: List[(String, Term)]): Tree = {
    t.decompose(a, term, bs)
  }

  def splitNode(t: Tree, n: Node): Tree = n.term match {
    case term: CFG =>
      val ns = term.args.map(_ => freshVarName())
      val term1 = term.copy(args = ns.map(Var))
      val bs = ns.zip(term.args)
      t.decompose(n, term1, bs)
  }

  def generalizeAlphaOrSplit(t: Tree, b: Node, a: Node): Tree = {
    val g: Gen = MSG.msg(a.term, b.term)
    g.t match {
      case _: Var =>
        splitNode(t, b)
      case _ =>
        abstractNode(t, a, g.t, g.m1.toList)
    }
  }

  override def buildStep(t: Tree, b: Node): Tree = {
    t.findFuncAncestor(b) match {
      case Some(a) =>
        t.setBack(b, a)
      case None =>
        t.findAMoreGeneralAncestor(b) match {
          case Some(a) =>
            generalizeNode(t, b, a)
          case None =>
            t.findAnEmbeddedAncestor(b) match {
              case Some(a) =>
                generalizeAlphaOrSplit(t, b, a)
              case None =>
                driveNode(t, b)
            }
        }
    }
  }

}
