package spsc

import Algebra._

object HE {
  def embeddedIn(t1: Term, t2: Term): Boolean =
    aVarIsUnderAttack(t1) == aVarIsUnderAttack(t2) && he(t1, t2)
  
  def he(t1: Term, t2: Term) = heByDiving(t1, t2) || heByCoupling(t1, t2)
  
  private def heByDiving(t1: Term, t2: Term): Boolean = t2 match {
    case e: CFG => e.args exists (he(t1, _))
    case _ => false
  }
  
  private def heByCoupling(t1: Term, t2: Term): Boolean = (t1, t2) match {
    case (e1:CFG, e2:CFG) if shellEq(e1, e2) => (e1.args, e2.args).zipped.forall(he)
    case (Var(_), Var(_)) => true
    case _ => false
  }
  
  def aVarIsUnderAttack(t: Term): Boolean = t match {
    case GCall(_, args) => aVarIsUnderAttack(args.head)
    case Var(_) => true
    case _ => false
  } 
}