package spsc

import scala.annotation.tailrec

object Algebra {

  type Subst = Map[String, Term]

  def shellowEq(t1: CFG, t2: CFG): Boolean =
    t1.kind == t2.kind &&
      t1.name == t2.name &&
      t1.args.length == t2.args.length

  def applySubst(m: Subst)(term: Term): Term = (term: @unchecked) match {
    case v: Var => m.getOrElse(v.name, v)
    case t: CFG => t.copy(args = t.args.map(applySubst(m)))
  }

  def vars(term: Term): List[String] = (term: @unchecked) match {
    case v: Var => List(v.name)
    case t: CFG =>
      (List[String]() /: t.args) { (ns, term) => (ns ::: vars(term)).distinct }
  }

  def names(term: Term): Set[String] = term match {
    case Var(name) => Set(name)
    case CFG(kind, name, args) =>
      (Set(name) /: args.map(names)) (_ ++ _)
    case Let(term0, bs) =>
      ((names(term0) ++ bs.map(_._1)) /: bs.map(_._2).map(names))(_++_)
  }

  def names: Rule => Set[String] = {
    case FRule (name, params, term) =>
      Set(name) ++ params ++ names(term)
    case GRule (name, pat, params, term) =>
      Set(name) + pat.name ++ pat.params ++ params ++ names(term)
  }

  def names(task: Task) : Set[String] =
    (names(task.term) /: task.prog.rules.map(names))(_++_)

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

class NameGen(val reserved: Seq[String]) {
  private var i: Int = 0
  private val used = scala.collection.mutable.Set[String]() ++ reserved

  @tailrec
  final def freshName(prefix: String): String = {
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