package spsc

import spsc.SLL._

object SLL {
  type Name = String
  type Params = List[Name]
}

sealed abstract class Term

case class Var(name: Name) extends Term {
  override def toString: String = name
}

object TKind extends Enumeration {
  val Ctr, FCall, GCall = Value
}

case class CFG(kind: TKind.Value, name: Name, args: List[Term]) extends Term {

  override def toString: String =
    if (kind == TKind.Ctr && args.isEmpty)
      name
    else
      name + args.mkString("(", ",", ")")
}

sealed abstract class CFGObject(kind: TKind.Value)
  extends Function2[Name, List[Term], CFG] {

  def apply(name: Name, args: List[Term]) = CFG(kind, name, args)

  def unapply(t: CFG): Option[(Name, List[Term])] =
    if (t.kind == kind) Some(t.name, t.args) else None
}

object Ctr extends CFGObject(TKind.Ctr)

object FCall extends CFGObject(TKind.FCall)

object GCall extends CFGObject(TKind.GCall)

case class Let(term: Term, bindings: List[(Name, Term)]) extends Term {
  override def toString: String = {
    val nts = bindings map { case (n, t) => s"$n=$t" }
    s"let ${nts.mkString(",")} in $term"
  }
}

case class Pat(name: Name, params: Params) {
  override def toString: String =
    if (params.isEmpty)
      name
    else
      name + params.mkString("(", ",", ")")
}

sealed abstract class Rule {
  def name: Name
}

case class FRule(name: Name, params: Params, term: Term) extends Rule {
  override def toString: String =
    s"$name${params.mkString("(", ",", ")")}=$term;"
}

case class GRule(name: Name, pat: Pat, params: Params, term: Term) extends Rule {
  val allParams: Params = pat.params ::: params

  override def toString: String =
    s"$name${(pat :: params).mkString("(", ",", ")")}=$term;"
}

case class Task(term: Term, rules: List[Rule]) {
  override def toString: String =
    s"$term where ${rules.mkString("")}"
}
