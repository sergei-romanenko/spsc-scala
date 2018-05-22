package spsc

import spsc.SLL._
import spsc.SLLCheck._

import scala.annotation.tailrec
import scala.collection.mutable

class SLLCheck(val task: Task) {

  // A name belongs to one and only one of the sets:
  //   gNames, fNames, pNames, cNames.
  // A variable in a pattern can appear only once.
  // All constructors in the definition of a g-function must be different.
  // A variable appearing in the right hand side must be a parameter.
  // A function name appearing in the right hand side must have a definition.
  // The arities of constructors and functions must be consistent.

  val fRules: List[FRule] =
    task.rules.flatMap { case r: FRule => Some(r); case _ => None; }

  val gRules: List[GRule] =
    task.rules.flatMap { case r: GRule => Some(r); case _ => None; }

  val fNames: List[Name] = fRules.map(_.name)
  val gNames: List[Name] = gRules.map(_.name)
  val hNames: List[Name] = fNames union gNames
  val fParams: List[Name] = fRules.flatMap(_.params)
  val gParams: List[Name] = gRules.flatMap(_.allParams)
  val pNames: List[Name] = vTerm(task.term) union (fParams union gParams)

  // Disjointness of name sets.

  def fgIntersection: Option[Msg] =
    for (f <- fNames.intersect(gNames).headOption) yield
      s"$f is both f- and g-function"

  def hpIntersection: Option[Msg] =
    for (f <- hNames.intersect(pNames).headOption) yield
      s"$f is both a function and a parameter"

  // Collecting variable names.

  def vTerm(term: Term): List[Name] = (term: @unchecked) match {
    case Var(name) =>
      List(name)
    case CFG(kind, name, args) =>
      args.flatMap(vTerm)
  }

  // Repeated names.

  @tailrec
  private def repeatedName(names: List[Name]): Option[Name] = names match {
    case Nil => None
    case n :: ns =>
      if (ns.contains(n)) Some(n) else repeatedName(ns)
  }

  def rnFRule(fRule: FRule): Option[Msg] = {
    for (n <- repeatedName(fRule.params))
      yield s"$n is repeated in the parameters of ${fRule.name}"
  }

  def rnGRule(gRule: GRule): Option[Msg] = {
    for (n <- repeatedName(gRule.allParams))
      yield s"$n is repeated in the parameters of ${gRule.name}"
  }

  def rnTask: Option[Msg] =
    fRules.flatMap(rnFRule).headOption orElse
      gRules.flatMap(rnGRule).headOption

  // Repeated constructor names in g-rules.

  def rcGRules(name: Name): Option[Msg] = {
    val cs = for (r <- gRules if name == r.name) yield r.pat.name
    for (c <- repeatedName(cs))
      yield s"In the definition of $name, $c appears twice in the patterns"
  }

  def rcTask: Option[Msg] =
    gNames.flatMap(rcGRules).headOption

  // A variable must be a parameter.

  def pvFRule(fRule: FRule): Option[Msg] = {
    val ps = fRule.params
    val vs = vTerm(fRule.term)
    for (v <- vs.find(!ps.contains(_))) yield
      s"In the definition of ${fRule.name}, $v is not a parameter"
  }

  def pvGRule(gRule: GRule): Option[Msg] = {
    val ps = gRule.allParams
    val vs = vTerm(gRule.term)
    for (v <- vs.find(!ps.contains(_))) yield
      s"In the definition of ${gRule.name}, $v is not a parameter"
  }

  def pvTask: Option[Msg] =
    fRules.flatMap(pvFRule).headOption orElse
      gRules.flatMap(pvGRule).headOption

  // Collecting function names.

  // We already know that fNames and gNames are disjoint,
  // and all calls to g-functions are marked as GCalls.
  // So, we only need to check that there are rules for FCalls.

  def fTerm(term: Term): List[Name] = (term: @unchecked) match {
    case Var(_) =>
      Nil
    case CFG(kind, name, args) =>
      val fs = args.flatMap(fTerm)
      if (kind == TKind.FCall) name :: fs else fs
  }

  // Functions called in terms must be defined in the program.

  def dTerm(term: Term): Option[Name] =
    fTerm(term).find(!fNames.contains(_))

  def uFRule(fRule: FRule): Option[Msg] =
    for (f <- dTerm(fRule.term)) yield
      s"In the definition of ${fRule.name}" +
        s", there is a call to an undefined function $f"

  def uGRule(gRule: GRule): Option[Msg] =
    for (f <- dTerm(gRule.term)) yield
      s"In the definition of ${gRule.name}" +
        s", there is a call to an undefined function $f"

  def uTerm(term: Term): Option[Msg] =
    for (f <- dTerm(term)) yield
      s"In the initial term, there is a call to an undefined function $f"

  def uTask: Option[Msg] =
    fRules.flatMap(uFRule).headOption orElse
      (gRules.flatMap(uGRule).headOption orElse
        uTerm(task.term))

  // The arities of constructors and functions must be consistent.

  def arTask: Option[Msg] =
    CArChecker().caTask(task.term, fRules, gRules) orElse
      HArChecker().haTask(task.term, fRules, gRules)

  // All checks.

  def checkTask: Option[Msg] =
    fgIntersection orElse (hpIntersection orElse
      (rnTask orElse (rcTask orElse
        (pvTask orElse (uTask orElse arTask)))))
}

object SLLCheck {

  type Msg = String

  def checkTask(task: Task): Option[Msg] = {
    val checker = new SLLCheck(task)
    checker.checkTask
  }
}

// Arities

trait ArChecker {

  val ar: mutable.Map[Name, Int] = mutable.HashMap[Name, Int]()

  def updAr(name: Name, k: Int): Option[Msg] = {
    ar.get(name) match {
      case None =>
        ar += (name -> k)
        None
      case Some(k1) =>
        if (k == k1)
          None
        else Some(s"$name has inconsistent arity: $k and $k1")
    }
  }
}

// Arities of constructors.

case class CArChecker() extends ArChecker {

  def caTerm(term: Term): Option[Msg] = (term: @unchecked) match {
    case Var(name) =>
      None
    case CFG(TKind.Ctr, name, args) =>
      updAr(name, args.length) orElse caArgs(args)
    case CFG(_, _, args) =>
      caArgs(args)
  }

  def caArgs(args: List[Term]): Option[Msg] =
    args.flatMap(caTerm).headOption

  def caFRules(rs: List[FRule]): Option[Msg] =
    rs.flatMap(r => caTerm(r.term)).headOption

  def caGRule(r: GRule): Option[Msg] =
    updAr(r.pat.name, r.pat.params.length) orElse caTerm(r.term)

  def caGRules(rs: List[GRule]): Option[Msg] =
    rs.flatMap(caGRule).headOption

  def caTask(term: Term,
             fRules: List[FRule], gRules: List[GRule]): Option[Msg] =
    caTerm(term) orElse (caFRules(fRules) orElse caGRules(gRules))
}

// Arities of functions.
// (We already know that fNames and gNames are disjoint.)

case class HArChecker() extends ArChecker {

  def haTerm(term: Term): Option[Msg] = (term: @unchecked) match {
    case Var(name) =>
      None
    case CFG(TKind.Ctr, name, args) =>
      haArgs(args)
    case CFG(_, name, args) =>
      updAr(name, args.length) orElse haArgs(args)
  }

  def haArgs(args: List[Term]): Option[Msg] =
    args.flatMap(haTerm).headOption

  def haFRule(fRule: FRule): Option[Msg] =
    updAr(fRule.name, fRule.params.length) orElse haTerm(fRule.term)

  def haFRules(rs: List[FRule]): Option[Msg] =
    rs.flatMap(haFRule).headOption

  def haGRule(gRule: GRule): Option[Msg] =
    updAr(gRule.name, gRule.params.length + 1) orElse haTerm(gRule.term)

  def haGRules(gRules: List[GRule]): Option[Msg] =
    gRules.flatMap(haGRule).headOption

  def haTask(term: Term,
             fRules: List[FRule], gRules: List[GRule]): Option[Msg] =
    haTerm(term) orElse (haFRules(fRules) orElse haGRules(gRules))

}
