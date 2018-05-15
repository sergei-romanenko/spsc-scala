package spsc

import scala.annotation.tailrec

import Algebra._

class BasicTreeBuilder(prog: Program) extends TreeBuilder {

  def freshPat(p: Pat) = Pat(p.name, p.params.map(_ => freshVarName()))

  def driveTerm(term: Term): List[(Term, Contraction)] = term match {
    case Ctr(name, args) =>
      args.map((_, null))
    case FCall(name, args) =>
      List((applySubst(Map(prog.f(name).params.zip(args): _*),
        prog.f(name).term), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = prog.g(name, cname)
      val subst = Map(g.allParams.zip(cargs ::: args): _*)
      List((applySubst(subst, g.term), null))
    case gCall@GCall(name, (v: Var) :: args) =>
      for (g <- prog.gs(name);
           fp = freshPat(g.pat);
           ctr = Ctr(fp.name, fp.params.map(Var)))
        yield driveTerm(applySubst(Map(v.name -> ctr), gCall)) match {
          case (k, _) :: _ => (k, Contraction(v.name, fp))
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
