package spsc

import Algebra._

import scala.annotation.tailrec

// Most Specific Generalization.

case class Gen(t: Term, m1: Subst, m2: Subst)

class MSGen(val ng: NameGen) {

  @tailrec
  private def msgLoop(g: Gen): Gen = {
    val g1 = commonSubst(commonFun(g))
    if (g.t == g1.t) g1 else msgLoop(g1)
  }

  def msg(t1: Term, t2: Term): Gen = {
    val n = ng.freshName(prefix = "v")
    val g = Gen(Var(n), Map(n -> t1), Map(n -> t2))
    msgLoop(g)
  }

  def commonFun(g: Gen): Gen = {
    for (n <- g.m1.keys)
      (g.m1(n), g.m2(n)) match {
        case (t1: CFG, t2: CFG) if shallowEq(t1, t2) =>
          val ns = t1.args.map(_ => ng.freshName(prefix = "v"))
          val t = applySubst(Map(n -> t1.copy(args = ns.map(Var))))(g.t)
          return Gen(t,
            g.m1 - n ++ ns.zip(t1.args),
            g.m2 - n ++ ns.zip(t2.args))
        case _ =>
      }
    g
  }

  def commonSubst(gen: Gen): Gen = {
    for ((n1, t1) <- gen.m1; (n2, t2) <- gen.m1)
      if ((n1 != n2 && t1 == t2) && (gen.m2(n1) == gen.m2(n2)))
        return Gen(applySubst(Map(n1 -> Var(n2)))(gen.t), gen.m1 - n1, gen.m2 - n1)
    gen
  }
}
