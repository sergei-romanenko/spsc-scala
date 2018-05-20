package spsc

// Separating f-functions from g-functions.

case class FGSeparator(isGName: String => Boolean) {

  def toTerm(term: Term): Term = term match {
    case Var(name) =>
      Var(name)
    case Ctr(name, args) =>
      Ctr(name, args.map(toTerm))
    case FCall(name, args) =>
      val args1 = args.map(toTerm)
      if (isGName(name)) GCall(name, args1) else FCall(name, args1)
  }

  def toFRule(r: FRule): FRule =
    FRule(r.name, r.params, toTerm(r.term))

  def toGRule(r: GRule): GRule =
    GRule(r.name, r.pat, r.params, toTerm(r.term))

  def toRule(r: Rule): Rule = r match {
    case FRule(name, params, term) =>
      FRule(name, params, toTerm(term))
    case GRule(name, pat, params, term) =>
      GRule(name, pat, params, toTerm(term))
  }

  def toProgram(prog: Program): Program =
    Program(prog.rules.map(toRule))

  def toTask(task: Task): Task =
    Task(toTerm(task.term), toProgram(task.prog))
}

object FGSeparator {

  def startsWithG(name: String): Boolean = {
    name.length > 0 && name.charAt(0) == 'g'
  }

  def getGNames(rules: List[Rule]): List[String] =
    rules.flatMap {
      case FRule(name, params, term) => None;
      case GRule(name, pat, params, term) => Some(name)
    }

  def isGNameInProg(prog: Program): String => Boolean = {
    val gNames = getGNames(prog.rules)
    gNames.contains(_)
  }

}
