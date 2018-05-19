package spsc

import scala.annotation.tailrec
import Algebra._
import Tree._

class BasicTreeBuilder(prog: Program) {
  val ng = new NameGen(Seq())

  def applyContr(oc: Option[Contraction])(term: Term): Term = oc match {
    case None =>
      term
    case Some(c) =>
      val cargs = c.pat.params.map(Var)
      val subst = Map(c.n -> Ctr(c.pat.name, cargs))
      applySubst(subst)(term)
  }

  def driveTerm(term: Term): List[Branch] = term match {
    case Ctr(name, args) =>
      args.map((_, None))
    case FCall(name, args) =>
      val f = prog.f(name)
      val subst = Map(f.params.zip(args): _*)
      List((applySubst(subst)(f.term), None))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = prog.g(name, cname)
      val subst = Map(g.allParams.zip(cargs ::: args): _*)
      List((applySubst(subst)(g.term), None))
    case GCall(name, (v: Var) :: args) =>
      for (g <- prog.gs(name)) yield {
        val p1 = g.pat.copy(params = g.pat.params.map(ng.freshName))
        val c = Some(Contraction(v.name, p1))
        val cargs1 = p1.params.map(Var)
        val args1 = args.map(applyContr(c))
        val subst = Map(g.allParams.zip(cargs1 ::: args1): _*)
        (applySubst(subst)(g.term), c)
      }
    case GCall(name, arg0 :: args) =>
      val bs = driveTerm(arg0)
      for ((t, c) <- bs) yield
        (GCall(name, t :: args.map(applyContr(c))), c)
    case Let(term0, bs) =>
      sys.error("driveTerm")
  }

  // -- The basic build step.

  // This function applies a driving step to the node's expression,
  // and, in general, adds children to the node.

  def driveNode(t: Tree, n: Node): Tree = {
    val branches = driveTerm(n.term)
    t.addChildren(n, branches)
  }

  // If beta `instOf` alpha, we generalize beta by introducing
  // a let-expression, in order to make beta the same as alpha
  // (modulo variable names).

  def generalizeNode(t: Tree, b: Node, a: Node): Tree = {
    val bindings = matchAgainst(a.term, b.term).get.toList
    t.decompose(b, a.term, bindings)
  }

  def buildStep(t: Tree, b: Node): Tree =
    t.findFuncAncestor(b) match {
      case Some(a) =>
        t.setBack(b, a)
      case None =>
        t.findAMoreGeneralAncestor(b) match {
          case Some(a) => generalizeNode(t, b, a)
          case None => driveNode(t, b)
        }
    }

  // -- The main loop.

  @tailrec
  private def buildLoop(t: Tree): Tree = {
    t.leaves.find(!t.isProcessed(_)) match {
      case None => t
      case Some(b) => buildLoop(buildStep(t, b))
    }
  }

  def buildProcessTree(term: Term): Tree =
    buildLoop(Tree.create(term))

}
