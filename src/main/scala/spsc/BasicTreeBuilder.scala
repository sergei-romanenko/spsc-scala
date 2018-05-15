package spsc

import scala.annotation.tailrec

import Algebra._

class BasicTreeBuilder(prog: Program) extends TreeBuilder {

  def freshPat(p: Pat) = Pat(p.name, p.params.map(_ => freshVarName()))

  def applyContr(c: Contraction)(term: Term): Term = {
    if (c == null)
      term
    else {
      val cargs = c.pat.params.map(Var)
      val subst = Map(c.n -> Ctr(c.pat.name, cargs))
      applySubst(subst, term)
    }
  }

  def driveTerm(term: Term) : List[(Term, Contraction)] = term match {
    case Ctr(name, args) =>
      args.map((_, null))
    case FCall(name, args) =>
      val f = prog.f(name)
      val subst = Map(f.params.zip(args): _*)
      List((applySubst(subst, f.term), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = prog.g(name, cname)
      val subst = Map(g.allParams.zip(cargs ::: args): _*)
      List((applySubst(subst, g.term), null))
    case GCall(name, (v: Var) :: args) =>
      for(g <- prog.gs(name)) yield {
        val p = freshPat(g.pat)
        val c = Contraction(v.name, p)
        val cargs = p.params.map(Var)
        val args1 = args.map(applyContr(c))
        val subst = Map(g.allParams.zip(cargs ::: args1): _*)
        (applySubst(subst, g.term), c)
      }
    case GCall(name, arg0 :: args) =>
      val bs = driveTerm(arg0)
      for ((t, c) <- bs) yield
        (GCall(name, t :: args.map(applyContr(c))), c)
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
