package spsc

abstract class Term

case class Var(name: String) extends Term {
  override def toString: String = name
}

object TKind extends Enumeration {
  val Ctr, FCall, GCall = Value
}

case class CFG(kind: TKind.Value, name: String, args: List[Term]) extends Term {

  def replaceArgs(newArgs: List[Term]) = CFG(kind, name, newArgs)

  override def toString: String =
    if (kind == TKind.Ctr && args.isEmpty)
      name
    else
      name + args.mkString("(", ",", ")")
}

abstract class CFGObject(kind: TKind.Value)
  extends Function2[String, List[Term], CFG] {

  def apply(name: String, args: List[Term]) = CFG(kind, name, args)

  def unapply(t: CFG): Option[(String, List[Term])] =
    if (t.kind == kind) Some(t.name, t.args) else None
}

object Ctr extends CFGObject(TKind.Ctr)
object FCall extends CFGObject(TKind.FCall)
object GCall extends CFGObject(TKind.GCall)

case class Let(term: Term, bindings: List[(Var, Term)]) extends Term {
  val bindings_s: List[String] =
    bindings map {case (v, e) => v.toString() + "=" + e.toString}
  override def toString: String =
    "let " + bindings_s.mkString(",") + " in " + term.toString
}

case class Pat(name: String, params: List[Var]) {
  override def toString: String =
    if (params.isEmpty)
      name
    else
      name + params.mkString("(", ",", ")")
}

abstract class Rule {def name: String}

case class FRule(name: String, params: List[Var], term: Term) extends Rule {
  override def toString: String =
    name + params.mkString("(",",",")") + "=" + term + ";"
}

case class GRule(name: String, pat: Pat, params: List[Var], term: Term) extends Rule {
  val allParams: List[Var] = pat.params ++ params
  override def toString: String =
    name + (pat :: params).mkString("(",",",")")  + "=" + term + ";"
}

case class Program(rules: List[Rule]){

  val f: Map[String, FRule] = (rules :\ Map[String, FRule]()) {
    case (r: FRule, m) => m + (r.name -> r);
    case (_, m) => m
  }

  val g: Map[(String, String), GRule] = (rules :\ Map[(String, String), GRule]()) {
    case (r: GRule, m) => m + ((r.name, r.pat.name) -> r)
    case (_, m) => m
  }

  val gs: Map[String, List[GRule]] = (rules :\ Map[String, List[GRule]]()) {
    case (r: GRule, m) => m + (r.name -> (r :: m.getOrElse(r.name, Nil)))
    case (_, m) => m
  }

  override def toString: String = rules.mkString("")
}

case class Task(term: Term, prog: Program) {
  override def toString: String = term.toString + " where " + prog.toString
}
