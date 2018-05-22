package spsc

import spsc.SLL._

class AdvancedTreeBuilder(task: Task) extends BasicTreeBuilder(task) {

  val msgen = new MSGen(ng)

  def abstractNode(t: Tree, a: Node, term: Term,
                   bs: List[(Name, Term)]): Tree =
    t.decompose(a, term, bs)

  def splitNode(t: Tree, n: Node): Tree =
    (n.term: @unchecked) match {
      case term: CFG =>
        val ns = term.args.map(_ => ng.freshName(prefix = "v"))
        val term1 = term.copy(args = ns.map(Var))
        val bs = ns.zip(term.args)
        t.decompose(n, term1, bs)

    }

  def generalizeAlphaOrSplit(t: Tree, b: Node, a: Node): Tree = {
    val g: Gen = msgen.msg(a.term, b.term)
    g.t match {
      case _: Var =>
        splitNode(t, b)
      case _ =>
        abstractNode(t, a, g.t, g.m1.toList)
    }
  }

  override def buildStep(t: Tree, b: Node): Tree =
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
