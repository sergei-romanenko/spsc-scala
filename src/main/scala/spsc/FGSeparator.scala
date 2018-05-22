package spsc

import spsc.SLL._

// Separating f-functions from g-functions.

case class FGSeparator(isGName: Name => Boolean) {

  def fgSepTerm: Term => Term = {
    case Var(name) =>
      Var(name)
    case Ctr(name, args) =>
      Ctr(name, args.map(fgSepTerm))
    case FCall(name, args) =>
      val args1 = args.map(fgSepTerm)
      if (isGName(name)) GCall(name, args1) else FCall(name, args1)
  }

  def fgSepRule: Rule => Rule = {
    case FRule(name, params, term) =>
      FRule(name, params, fgSepTerm(term))
    case GRule(name, pat, params, term) =>
      GRule(name, pat, params, fgSepTerm(term))
  }

  def fgSepTask(task: Task): Task =
    Task(fgSepTerm(task.term), task.rules.map(fgSepRule))
}

object FGSeparator {

  def startsWithG(name: Name): Boolean =
    name.length > 0 && name.charAt(0) == 'g'

  def getGNames(rules: List[Rule]): List[Name] =
    rules.flatMap {
      case FRule(name, params, term) => None;
      case GRule(name, pat, params, term) => Some(name)
    }

  def isGNameInRules(rules: List[Rule]): Name => Boolean = {
    val gNames = getGNames(rules)
    gNames.contains(_)
  }

}
