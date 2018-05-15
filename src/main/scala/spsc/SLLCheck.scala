package spsc

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

class SLLCheck(val task: Task) {

  // A name belongs to one and only one of the sets:
  //   gNames, fNames, pNames, cNames.
  // A variable in a pattern can appear only once.
  // All constructors in the definition of a g-function must be different.
  // A variable appearing in the right hand side must be a parameter.
  // A function name appearing in the right hand side must have a definition.
  // The arities of constructors and functions must be consistent.

  val fRules: List[FRule] =
    task.prog.rules.flatMap { case r: FRule => Some(r); case _ => None; }

  val gRules: List[GRule] =
    task.prog.rules.flatMap { case r: GRule => Some(r); case _ => None; }

  val fNames: List[String] = fRules.map(_.name)
  val gNames: List[String] = gRules.map(_.name)
  val hNames: List[String] = fNames union gNames
  val fParams: List[String] = fRules.flatMap(_.params)
  val gParams: List[String] = gRules.flatMap(_.allParams)
  val pNames: List[String] = vTerm(task.term) union (fParams union gParams)

  // Disjointness of name sets.

  def ifgNames(): Option[String] =
    for (f <- fNames.intersect(gNames).headOption) yield
      f"$f is both f- and g-function"

  def ihpNames(): Option[String] =
    for (f <- hNames.intersect(pNames).headOption) yield
      f"$f is both a function and a parameter"

  // Collecting variable names.

  def vTerm(term: Term): List[String] = term match {
    case Var(name) =>
      List(name)
    case CFG(kind, name, args) =>
      args.flatMap(vTerm)
  }

  // Repeated names.

  @tailrec
  private def repeatedName(names: List[String]): Option[String] = names match {
    case Nil => None
    case n :: ns =>
      if (ns.contains(n)) Some(n) else repeatedName(ns)
  }

  def rnFRule(fRule: FRule): Option[String] = {
    for (n <- repeatedName(fRule.params))
      yield f"$n is repeated in the parameters of ${fRule.name}"
  }

  def rnGRule(gRule: GRule): Option[String] = {
    for (n <- repeatedName(gRule.allParams))
      yield f"$n is repeated in the parameters of ${gRule.name}"
  }

  def rcGRules(name: String): Option[String] = {
    val cs = for (r <- gRules if name == r.name) yield r.pat.name
    for (c <- repeatedName(cs))
      yield f"In the definition of $name, $c appears twice in the patterns"
  }

  // A variable must be a parameter.

  def pvFRule(fRule: FRule): Option[String] = {
    var ps = fRule.params
    var vs = vTerm(fRule.term)
    for (v <- vs.find(!ps.contains(_))) yield
      f"In the definition of ${fRule.name}, $v is not a parameter"
  }

  def pvGRule(gRule: GRule): Option[String] = {
    var ps = gRule.allParams
    var vs = vTerm(gRule.term)
    for (v <- vs.find(!ps.contains(_))) yield
      f"In the definition of ${gRule.name}, $v is not a parameter"
  }

  // Collecting function names.

  // We already know that fNames and gNames are disjoint,
  // and all calls to g-functions are marked as GCalls.
  // So, we only need to check that there are rules for FCalls.

  def fTerm(term: Term): List[String] = term match {
    case Var(_) =>
      Nil
    case CFG(kind, name, args) =>
      val fs = args.flatMap(fTerm)
      if (kind == TKind.FCall) name :: fs else fs
  }

  def dTerm(term: Term): Option[String] =
    fTerm(term).find(!fNames.contains(_))

  def uFRule(fRule: FRule): Option[String] =
    for (f <- dTerm(fRule.term)) yield
      f"In the definition of ${fRule.name}" +
        f", there is a call to an undefined function $f"

  def uGRule(gRule: GRule): Option[String] =
    for (f <- dTerm(gRule.term)) yield
      f"In the definition of ${gRule.name}" +
        f", there is a call to an undefined function $f"

  def uTerm(term: Term): Option[String] =
    for (f <- dTerm(term)) yield
      f"In the initial term, there is a call to an undefined function $f"

  def checkTask(): Option[String] =
    ifgNames() orElse
      (ihpNames() orElse
        (fRules.flatMap(rnFRule).headOption orElse
          (gRules.flatMap(rnGRule).headOption orElse
            (gNames.flatMap(rcGRules).headOption orElse
              (fRules.flatMap(pvFRule).headOption orElse
                (gRules.flatMap(pvGRule).headOption orElse
                  (fRules.flatMap(uFRule).headOption orElse
                    (gRules.flatMap(uGRule).headOption orElse
                      (uTerm(task.term) orElse
                        (CArChecker().caTask(task.term, fRules, gRules) orElse
                          HArChecker().haTask(task.term, fRules, gRules)))))))))))

}

object SLLCheck {
  def checkTask(task: Task): Option[String] = {
    val checker = new SLLCheck(task)
    checker.checkTask()
  }
}

// Arities

trait ArChecker {

  val ar: mutable.Map[String, Int] = mutable.HashMap[String, Int]()

  def updAr(name: String, k: Int): Option[String] = {
    ar.get(name) match {
      case None =>
        ar += (name -> k)
        None
      case Some(k1) =>
        if (k == k1)
          None
        else Some(f"$name has inconsistent arity: $k and $k1")
    }
  }
}

// Arities of constructors.

case class CArChecker() extends ArChecker {

  def caTerm(term: Term): Option[String] = term match {
    case Var(name) =>
      None
    case CFG(TKind.Ctr, name, args) =>
      updAr(name, args.length) orElse caArgs(args)
    case CFG(_, _, args) =>
      caArgs(args)
  }

  def caArgs(args: List[Term]): Option[String] =
    args.flatMap(caTerm).headOption

  def caFRules(rs: List[FRule]): Option[String] =
    rs.flatMap(r => caTerm(r.term)).headOption

  def caGRule(r: GRule): Option[String] =
    updAr(r.pat.name, r.pat.params.length) orElse caTerm(r.term)

  def caGRules(rs: List[GRule]): Option[String] =
    rs.flatMap(caGRule).headOption

  def caTask(term: Term,
             fRules: List[FRule], gRules: List[GRule]): Option[String] =
    caTerm(term) orElse (caFRules(fRules) orElse caGRules(gRules))
}

// Arities of functions.
// (We already know that fNames and gNames are disjoint.)

case class HArChecker() extends ArChecker {

  def haTerm(term: Term): Option[String] = term match {
    case Var(name) =>
      None
    case CFG(TKind.Ctr, name, args) =>
      haArgs(args)
    case CFG(_, name, args) =>
      updAr(name, args.length) orElse haArgs(args)
  }

  def haArgs(args: List[Term]): Option[String] =
    args.flatMap(haTerm).headOption

  def haFRule(fRule: FRule): Option[String] =
    updAr(fRule.name, fRule.params.length) orElse haTerm(fRule.term)

  def haFRules(rs: List[FRule]): Option[String] =
    rs.flatMap(haFRule).headOption

  def haGRule(gRule: GRule): Option[String] =
    updAr(gRule.name, gRule.params.length + 1) orElse haTerm(gRule.term)

  def haGRules(gRules: List[GRule]): Option[String] =
    gRules.flatMap(haGRule).headOption

  def haTask(term: Term,
             fRules: List[FRule], gRules: List[GRule]): Option[String] =
    haTerm(term) orElse (haFRules(fRules) orElse haGRules(gRules))

}
