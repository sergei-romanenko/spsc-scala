package spsc

import scala.annotation.tailrec

import Algebra._

class BasicTreeBuilder(p: Program) extends TreeBuilder {

  def freshPat(p: Pat) = Pat(p.name, p.params map freshVar)

  def driveTerm(term: Term): List[(Term, Contraction)] = term match {
    case Ctr(name, args) =>
      args.map((_, null))
    case FCall(name, args) =>
      List((applySubst(Map(p.f(name).params.zip(args): _*), p.f(name).term), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = p.g(name, cname)
      List((applySubst(Map((g.pat.params ::: g.params) zip (cargs ::: args): _*), g.term), null))
    case gCall@GCall(name, (v: Var) :: args) =>
      for (g <- p.gs(name); fp = freshPat(g.pat); ctr = Ctr(fp.name, fp.params))
        yield driveTerm(applySubst(Map(v -> ctr), gCall)) match {
          case (k, _) :: _ => (k, Contraction(v, fp))
          case _ => sys.error("BasicTreeBuilder")
        }
    case GCall(name, args) =>
      driveTerm(args.head) map { case (k, v) => (GCall(name, k :: args.tail), v) }
    case Let(term0, bs) =>
      (term0, null) :: bs.map { case (_, v) => (v, null) }
  }

  def buildStep(t: Tree, b: Node): Tree = {
    b.ancestors.find(a => isFGCall(a.term) && instOf(b.term, a.term)) match {
      case None =>
        t.addChildren(b, driveTerm(b.term))
      case Some(a) =>
        t.replace(b, Let(a.term, matchAgainst(a.term, b.term).toList))
    }
  }

  @tailrec
  private def buildLoop(t: Tree): Tree = {
    t.leaves.find(!_.isProcessed) match {
      case None => t
      case Some(b) => buildLoop(buildStep(t, b))
    }
  }

  def buildProcessTree(term: Term): Tree =
    buildLoop(Tree.create(term))

}
