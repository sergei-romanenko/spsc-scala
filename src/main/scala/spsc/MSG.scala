package spsc

import Algebra._

case class Gen(t: Term, m1: Map[String, Term], m2: Map[String, Term])

object MSG {

  def msg(t1: Term, t2: Term): Gen = {
    val n = freshVarName()
    var g = Gen(Var(n), Map(n -> t1), Map(n -> t2))
    var term: Term = g.t
    do {term = g.t; g = commonSubst(commonFun(g))} while (term != g.t)
    g
  }
  
  def commonFun(g: Gen): Gen = {
    for (n <- g.m1.keys) (g.m1(n), g.m2(n)) match {
      case (t1:CFG, t2:CFG) if shellEq(t1, t2) =>
        val ns = t1.args.map(_ => freshVarName())
        val t = applySubst(Map(n -> t1.replaceArgs(ns.map(Var))), g.t)
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
        return Gen(applySubst(Map(n1 -> Var(n2)), gen.t), gen.m1 - n1, gen.m2 - n1)
    gen
  }
}
