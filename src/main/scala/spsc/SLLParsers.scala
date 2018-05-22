package spsc

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.RegexParsers
import spsc.FGSeparator.{isGNameInRules, startsWithG}

import scala.util.matching.Regex

object SLLParsers extends RegexParsers with ImplicitConversions {

  override val whiteSpace: Regex = """(\s|--.*)+""".r

  def task: SLLParsers.Parser[Task] =
    term ~ ("where" ~> rules) ^^ Task

  def rules: SLLParsers.Parser[List[Rule]] =
    rep(definition)

  def definition: SLLParsers.Parser[Rule] =
    gRule | fRule

  def term: SLLParsers.Parser[Term] =
    ctr | call | vrb

  def uid: SLLParsers.Parser[String] =
    """[A-Z]\w*""".r

  def lid: SLLParsers.Parser[String] =
    """[a-z]\w*""".r

  def vrb: SLLParsers.Parser[Var] =
    lid ^^ Var

  def patParams: SLLParsers.Parser[List[String]] =
    opt("(" ~> repsep(lid, ",") <~ ")") ^^ {
      _.getOrElse(Nil)
    }

  def pat: SLLParsers.Parser[Pat] =
    uid ~ patParams ^^ Pat

  def fRule: SLLParsers.Parser[FRule] =
    lid ~ ("(" ~> repsep(lid, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FRule

  def gRule: SLLParsers.Parser[GRule] =
    lid ~ ("(" ~> pat) ~ (rep("," ~> lid) <~ ")") ~ ("=" ~> term <~ ";") ^^ GRule

  def ctrArgs: SLLParsers.Parser[List[Term]] =
    opt("(" ~> repsep(term, ",") <~ ")") ^^ {
      _.getOrElse(Nil)
    }

  def ctr: SLLParsers.Parser[CFG] =
    uid ~ ctrArgs ^^ Ctr

  def call: SLLParsers.Parser[CFG] =
    lid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall


  def runParser[T](p: Parser[T], s: String): T = {
    parseAll(p, s) match {
      case Success(result, _) => result
      case NoSuccess(err, next) =>
        val msg =
          "Failed to parse the input task " +
          s"(line ${next.pos.line}, column ${next.pos.column}):\n" +
          s"$err\n${next.pos.longString}"
        sys.error(msg)
    }
  }

  // `parseTerm` and `parseProg` are only used for testing purposes.

  def parseTerm(s: String): Term = {
    val rawTerm = runParser(term, s)
    val fg = new FGSeparator(startsWithG)
    fg.fgSepTerm(rawTerm)
  }

  // `parseTask` classifies function names according to their definitions,
  // rather than the first letter of a function name.

  def parseTask(s: String): Task = {
    val rawTask = runParser(task, s)
    val fg = new FGSeparator(isGNameInRules(rawTask.rules))
    fg.fgSepTask(rawTask)
  }

}
