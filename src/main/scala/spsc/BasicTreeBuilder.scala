package spsc

import scala.annotation.tailrec
import Algebra._
import Tree._

class BasicTreeBuilder(prog: Program) extends TreeBuilder {

  def freshPat(p: Pat) = Pat(p.name, p.params.map(_ => freshVarName()))

  def applyContr(oc: Option[Contraction])(term: Term): Term = oc match {
    case None =>
      term
    case Some(c) =>
      val cargs = c.pat.params.map(Var)
      val subst = Map(c.n -> Ctr(c.pat.name, cargs))
      applySubst(subst, term)
  }

  def driveTerm(term: Term) : List[Branch] = term match {
    case Ctr(name, args) =>
      args.map((_, None))
    case FCall(name, args) =>
      val f = prog.f(name)
      val subst = Map(f.params.zip(args): _*)
      List((applySubst(subst, f.term), None))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = prog.g(name, cname)
      val subst = Map(g.allParams.zip(cargs ::: args): _*)
      List((applySubst(subst, g.term), None))
    case GCall(name, (v: Var) :: args) =>
      for(g <- prog.gs(name)) yield {
        val p = freshPat(g.pat)
        val c = Some(Contraction(v.name, p))
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
      sys.error("driveTerm")
  }

  def buildStep(t: Tree, b: Node): Tree =
    b.ancestors.find(a => isFGCall(a.term) && instOf(b.term, a.term)) match {
      case None =>
        t.addChildren(b, driveTerm(b.term))
      case Some(a) =>
        t.decompose(b, a.term, matchAgainst(a.term, b.term).toList)
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
