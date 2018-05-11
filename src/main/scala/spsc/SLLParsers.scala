package spsc

import scala.language.postfixOps
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{CharSequenceReader => Reader}

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
    fcall | gcall | ctr | vrb

  def uid: SLLParsers.Parser[String] =
    ident ^? {case id if id.charAt(0).isUpper => id}

  def lid: SLLParsers.Parser[String] =
    ident ^? {case id if id.charAt(0).isLower => id}

  def fid: SLLParsers.Parser[String] =
    ident ^? {case id if id.charAt(0) == 'f' => id}

  def gid: SLLParsers.Parser[String] =
    ident ^? {case id if id.charAt(0) == 'g' => id}

  def vrb: SLLParsers.Parser[Var] =
    lid ^^ Var

  def patParams : SLLParsers.Parser[List[Var]] =
    (("(" ~> repsep(vrb, ",") <~ ")") ?) ^^ {_.getOrElse(Nil)}

  def pat: SLLParsers.Parser[Pat] =
    uid ~ patParams ^^ Pat

  def fRule: SLLParsers.Parser[FRule] =
    fid ~ ("(" ~> repsep(vrb, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FRule

  def gRule: SLLParsers.Parser[GRule] =
    gid ~ ("(" ~> pat) ~ ((("," ~> vrb)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GRule

  def ctrArgs: SLLParsers.Parser[List[Term]] =
    (("(" ~> repsep(term, ",") <~ ")") ?) ^^ {_.getOrElse(Nil)}

  def ctr: SLLParsers.Parser[CFG] =
    uid ~ ctrArgs ^^ Ctr

  def fcall: SLLParsers.Parser[CFG] =
    fid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall

  def gcall: SLLParsers.Parser[CFG] =
    gid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ GCall

  def parseTerm(s: String): Term =
    term(new lexical.Scanner(new Reader(s))).get

  def parseProg(s: String): Program =
    prog(new lexical.Scanner(new Reader(s))).get

  def parseTask(s: String): Task = {
    task(new lexical.Scanner(new Reader(s))).get
  }
}
