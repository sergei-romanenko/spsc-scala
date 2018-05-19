package spsc

object Algebra {

  type Subst = Map[String, Term]

  def shellowEq(t1: CFG, t2: CFG): Boolean =
    t1.kind == t2.kind &&
      t1.name == t2.name &&
      t1.args.length == t2.args.length

  def applySubst(m: Subst): Term => Term = {
    case v: Var => m.getOrElse(v.name, v)
    case t: CFG => t.copy(args = t.args.map(applySubst(m)))
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
