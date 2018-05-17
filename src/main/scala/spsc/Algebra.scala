package spsc

object Algebra {

  type Subst = Map[String, Term]

  def shellEq(t1: CFG, t2: CFG): Boolean =
    t1.kind == t2.kind &&
      t1.name == t2.name &&
      t1.args.length == t2.args.length

  def applySubst(m: Subst, term: Term): Term = term match {
    case v: Var => m.getOrElse(v.name, v)
    case t: CFG => t.replaceArgs(t.args.map(applySubst(m, _)))
  }

  def vars: Term => List[String] = {
    case v: Var => List(v.name)
    case t: CFG =>
      (List[String]() /: t.args) { (ns, term) => (ns ::: vars(term)).distinct }
  }

  def isFGCall: Term => Boolean = {
    case FCall(_, _) => true
    case GCall(_, _) => true
    case _ => false
  }

  def matchAgainst(t1: Term, t2: Term): Option[Subst] = {
    var m = Map[String, Term]()

    def walk(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Var, _) => m.get(v1.name) match {
        case None => m += (v1.name -> t2); true
        case Some(t3) => t2 == t3
      }
      case (e1: CFG, e2: CFG) if shellEq(e1, e2) =>
        (e1.args, e2.args).zipped.forall(walk)
      case _ => false
    }

    if (walk(t1, t2)) Some(m) else None
  }

  def instOf(t1: Term, t2: Term): Boolean =
    matchAgainst(t2, t1).isDefined

  def equiv(t1: Term, t2: Term): Boolean =
    instOf(t1, t2) && instOf(t2, t1)

  // Generating variable names.

  private var i = 0

  def resetVarGen(): Unit = {
    i = 0
  }

  def freshVarName(): String = {
    i += 1
    "v" + i
  }

}
