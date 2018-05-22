package spsc

import spsc.SLL._

import scala.annotation.tailrec

object Algebra {

  type Subst = Map[Name, Term]

  def shellowEq(t1: CFG, t2: CFG): Boolean =
    t1.kind == t2.kind &&
      t1.name == t2.name &&
      t1.args.length == t2.args.length

  def applySubst(m: Subst)(term: Term): Term = (term: @unchecked) match {
    case v: Var => m.getOrElse(v.name, v)
    case t: CFG => t.copy(args = t.args.map(applySubst(m)))
  }

  // The result is `List[Name]`, rather than `Set[Name]`,
  // in order to preserve the order in which the variables appear in the term.
  // (Just to make the residual program more similar to the input one.)

  def termVars(term: Term): List[Name] = (term: @unchecked) match {
    case v: Var => List(v.name)
    case t: CFG =>
      (List[Name]() /: t.args) { (ns, term) => (ns ::: termVars(term)).distinct }
  }

  def termNames: Term => Set[Name] = {
    case Var(name) => Set(name)
    case CFG(kind, name, args) =>
      (Set(name) /: args.map(termNames)) (_ ++ _)
    case Let(term0, bs) =>
      ((termNames(term0) ++ bs.map(_._1)) /: bs.map(_._2).map(termNames)) (_ ++ _)
  }

  def ruleNames: Rule => Set[Name] = {
    case FRule(name, params, term) =>
      Set(name) ++ params ++ termNames(term)
    case GRule(name, pat, params, term) =>
      Set(name) + pat.name ++ pat.params ++ params ++ termNames(term)
  }

  def taskNames(task: Task): Set[Name] =
    (termNames(task.term) /: task.rules.map(ruleNames)) (_ ++ _)

  def matchLoop(m: Subst): List[(Term, Term)] => Option[Subst] = {
    case Nil =>
      Some(m)
    case hd :: tl =>
      hd match {
        case (v: Var, t2) => m.get(v.name) match {
          case None => matchLoop(m + (v.name -> t2))(tl)
          case Some(t0) => if (t2 == t0) matchLoop(m)(tl) else None
        }
        case (t1: CFG, t2: CFG) if shellowEq(t1, t2) =>
          matchLoop(m)((t1.args zip t2.args) ::: tl)
        case _ => None
      }
  }

  def matchAgainst(t1: Term, t2: Term): Option[Subst] =
    matchLoop(Map())((t1, t2) :: Nil)

  def instOf(t1: Term, t2: Term): Boolean =
    matchAgainst(t2, t1).isDefined

  def equiv(t1: Term, t2: Term): Boolean =
    instOf(t1, t2) && instOf(t2, t1)

}

// Generating variable names.

class NameGen(val reserved: Seq[Name]) {
  private var i: Int = 0
  private val used = scala.collection.mutable.Set[Name]() ++ reserved

  @tailrec
  final def freshName(prefix: String): Name = {
    i += 1
    val name = prefix + i
    if (used.contains(name))
      freshName(prefix)
    else {
      used += name
      name
    }
  }
}
