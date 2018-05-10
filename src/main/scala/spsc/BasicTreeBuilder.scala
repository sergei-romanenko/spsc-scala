package spsc

import Algebra._

class BasicTreeBuilder(p: Program) {

  def driveTerm(term: Term): List[(Term, Contraction)] = term match {
    case Ctr(name, args) => args.map((_, null))
    case FCall(name, args) =>
      List((applySubst(Map(p.f(name).args.zip(args): _*), p.f(name).term), null))
    case GCall(name, Ctr(cname, cargs) :: args) =>
      val g = p.g(name, cname)
      List((applySubst(Map((g.p.args ::: g.args) zip (cargs ::: args): _*), g.term), null))
    case gCall@GCall(name, (v: Var) :: args) =>
      for (g <- p.gs(name); fp = freshPat(g.p); ctr = Ctr(fp.name, fp.args))
        yield driveTerm(applySubst(Map(v -> ctr), gCall)) match {
          case (k, _) :: _ => (k, Contraction(v, fp))
          case _ => sys.error("BasicSupercompiler")
        }
    case GCall(name, args) =>
      driveTerm(args.head) map { case (k, v) => (GCall(name, k :: args.tail), v) }
    case Let(term0, bs) => (term0, null) :: bs.map { case (_, v) => (v, null) }
  }

  def buildProcessTree(term: Term): Tree = {
    var t = Tree.create(term)
    while (t.leaves.exists {
      !_.isProcessed
    }) {
      val b = t.leaves.find(!_.isProcessed).get
      t = b.ancestors.find(a => isFGCall(a.term) && instOf(b.term, a.term)) match {
        case Some(a) => t.replace(b, Let(a.term, matchAgainst(a.term, b.term).toList))
        case None => t.addChildren(b, driveTerm(b.term)) // drive
      }
    }
    t
  }

  def freshPat(p: Pat) = Pat(p.name, p.args map freshVar)
}
