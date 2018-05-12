package spsc

import scala.language.postfixOps
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{CharSequenceReader => Reader}

import spsc.FGSeparator.{isGNameInProg, startsWithG}

object SLLParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")
  lexical.reserved += "where"

  def task: SLLParsers.Parser[Task] =
    term ~ ("where" ~> prog) ^^ Task

  def prog : SLLParsers.Parser[Program] =
    (definition*) ^^ Program

  def definition: SLLParsers.Parser[Rule] =
    gRule | fRule

  def term: SLLParsers.Parser[Term] =
    ctr | call | vrb

  def uid: SLLParsers.Parser[String] =
    ident ^? {case id if id.charAt(0).isUpper => id}

  def lid: SLLParsers.Parser[String] =
    ident ^? {case id if id.charAt(0).isLower => id}

  def vrb: SLLParsers.Parser[Var] =
    lid ^^ Var

  def patParams : SLLParsers.Parser[List[Var]] =
    (("(" ~> repsep(vrb, ",") <~ ")") ?) ^^ {_.getOrElse(Nil)}

  def pat: SLLParsers.Parser[Pat] =
    uid ~ patParams ^^ Pat

  def fRule: SLLParsers.Parser[FRule] =
    lid ~ ("(" ~> repsep(vrb, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FRule

  def gRule: SLLParsers.Parser[GRule] =
    lid ~ ("(" ~> pat) ~ ((("," ~> vrb)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GRule

  def ctrArgs: SLLParsers.Parser[List[Term]] =
    (("(" ~> repsep(term, ",") <~ ")") ?) ^^ {_.getOrElse(Nil)}

  def ctr: SLLParsers.Parser[CFG] =
    uid ~ ctrArgs ^^ Ctr

  def call: SLLParsers.Parser[CFG] =
    lid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall

  // `parseTerm` and `parseProg` are only used for testing purposes.

  def parseTerm(s: String): Term = {
    val rawTerm = term(new lexical.Scanner(new Reader(s))).get
    val fg = new FGSeparator(startsWithG)
    fg.toTerm(rawTerm)
  }

  def parseProg(s: String): Program = {
    val rawProg = prog(new lexical.Scanner(new Reader(s))).get
    val fg = new FGSeparator(startsWithG)
    fg.toProgram(rawProg)
  }

  // `parseTask` classifies function names according to their definitions,
  // rather than the first letter of a function name.

  def parseTask(s: String): Task = {
    val rawTask = task(new lexical.Scanner(new Reader(s))).get
    val fg = new FGSeparator(isGNameInProg(rawTask.prog))
    fg.toTask(rawTask)
  }
}
