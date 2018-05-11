package spsc

import Algebra._

class AdvancedTreeBuilder(p: Program) extends BasicTreeBuilder(p) {

  def abs(t: Tree, a: Node, b: Node): Tree = {
    val g: Gen = MSG.msg(a.term, b.term)
    t.replace(a, Let(g.t, g.m1.toList))
  }

  def split(t: Tree, n: Node): Tree = n.term match {
    case term: CFG =>
      val vs = term.args map freshVar
      t.replace(n, Let(term.replaceArgs(vs), vs zip term.args))
  }

  override def buildStep(t: Tree, b: Node): Tree = {
    if (!isFGCall(b.term)) {
      t.addChildren(b, driveTerm(b.term))
    } else {
      b.ancestors.find(a => isFGCall(a.term) && HE.embeddedIn(a.term, b.term))
      match {
        case None =>
          t.addChildren(b, driveTerm(b.term))
        case Some(a) =>
          if (instOf(b.term, a.term))
            abs(t, b, a)
          else if (equiv(MSG.msg(a.term, b.term).t, Var("z")))
            split(t, b)
          else
            abs(t, a, b)
      }
    }
  }

}
