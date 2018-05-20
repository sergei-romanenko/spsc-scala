package spsc

// Separating f-functions from g-functions.

case class FGSeparator(isGName: String => Boolean) {

  def fgSep(term: Term): Term = term match {
    case Var(name) =>
      Var(name)
    case Ctr(name, args) =>
      Ctr(name, args.map(fgSep))
    case FCall(name, args) =>
      val args1 = args.map(fgSep)
      if (isGName(name)) GCall(name, args1) else FCall(name, args1)
  }

  def fgSep(r: Rule): Rule = r match {
    case FRule(name, params, term) =>
      FRule(name, params, fgSep(term))
    case GRule(name, pat, params, term) =>
      GRule(name, pat, params, fgSep(term))
  }

  def fgSep(prog: Program): Program =
    Program(prog.rules.map(fgSep))

  def fgSep(task: Task): Task =
    Task(fgSep(task.term), fgSep(task.prog))
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
