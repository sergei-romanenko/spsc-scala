package spsc

abstract class Term

case class Var(name: String) extends Term {
  override def toString: String = name
}

object TKind extends Enumeration {
  val Ctr, FCall, GCall = Value
}

case class CFG(kind: TKind.Value, name: String, args: List[Term])
    extends Term {
  def replaceArgs(newArgs: List[Term]) = CFG(kind, name, newArgs)
  override def toString: String = name + args.mkString("(",",",")")
}

abstract class CFGObject(kind: TKind.Value)
    extends ((String, List[Term]) => CFG) {
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

case class Pat(name: String, args: List[Var]) {
  override def toString: String = name + args.mkString("(",",",")")
}

abstract class Rule {def name: String}

case class FRule(name: String, args: List[Var], term: Term) extends Rule {
  override def toString: String =
    name + args.mkString("(",",",")") + "=" + term + ";"
}

case class GRule(name: String, p: Pat, args: List[Var], term: Term) extends Rule {
  override def toString: String =
    name + (p :: args).mkString("(",",",")")  + "=" + term + ";"
}

case class Program(rules: List[Rule]){

  val f: Map[String, FRule] = (rules :\ Map[String, FRule]())
    {case (d: FRule, m) => m + (d.name -> d); case (_, m) => m}

  val g: Map[(String, String), GRule] = (rules :\ Map[(String, String), GRule]())
    {case (d: GRule, m) => m + ((d.name, d.p.name) -> d); case (_, m) => m}

  val gs: Map[String, List[GRule]] = (g :\ Map[String, List[GRule]]())
    {case (((n, _), d), m) => m + (n -> (d :: m.getOrElse(n, Nil)))}

  override def toString: String = rules.mkString("")
}

case class Task(term: Term, prog: Program) {
  override def toString: String = term.toString + " where " + prog.toString
}
