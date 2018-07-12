package spsc

import Algebra._

object HE {

  def embeddedIn(t1: Term, t2: Term): Boolean =
    heByDiving(t1, t2) || heByCoupling(t1, t2)

  private def heByDiving(t1: Term, t2: Term): Boolean = t2 match {
    case t: CFG => t.args exists (embeddedIn(t1, _))
    case _ => false
  }

  private def heByCoupling: (Term, Term) => Boolean = {
    case (e1:CFG, e2:CFG) if shallowEq(e1, e2) =>
      (e1.args, e2.args).zipped.forall(embeddedIn)
    case (Var(_), Var(_)) => true
    case _ => false
  }
}
