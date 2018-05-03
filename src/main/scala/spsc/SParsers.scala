package spsc

import scala.language.postfixOps
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{CharSequenceReader => Reader}

object SParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")

  def prog: SParsers.Parser[List[Rule]] =
    definition*

  def definition: Parser[Rule] =
    gRule | fRule

  def term: Parser[Term] =
    fcall | gcall | ctr | vrb

  def uid: SParsers.Parser[String] =
    ident ^? {case id if id.charAt(0).isUpper => id}

  def lid: SParsers.Parser[String] =
    ident ^? {case id if id.charAt(0).isLower => id}

  def fid: SParsers.Parser[String] =
    ident ^? {case id if id.charAt(0) == 'f' => id}

  def gid: SParsers.Parser[String] =
    ident ^? {case id if id.charAt(0) == 'g' => id}

  def vrb: SParsers.Parser[Var] =
    lid ^^ Var

  def pat: SParsers.Parser[Pat] =
    uid ~ ("(" ~> repsep(vrb, ",") <~ ")") ^^ Pat

  def fRule: SParsers.Parser[FRule] =
    fid ~ ("(" ~> repsep(vrb, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FRule

  def gRule: SParsers.Parser[GRule] =
    gid ~ ("(" ~> pat) ~ ((("," ~> vrb)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GRule

  def ctr: SParsers.Parser[CFG] =
    uid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ Ctr

  def fcall: SParsers.Parser[CFG] =
    fid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall

  def gcall: SParsers.Parser[CFG] =
    gid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ GCall

  def parseProg(s: String) =
    Program(prog(new lexical.Scanner(new Reader(s))).get)

  def parseTerm(s: String): Term =
    term(new lexical.Scanner(new Reader(s))).get
}
