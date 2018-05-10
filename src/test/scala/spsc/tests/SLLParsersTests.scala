package spsc.tests

import org.scalatest.FunSuite
import spsc._

class SLLParsersTests  extends FunSuite {

  def pTerm(g: String, e: String): Unit = {
    assert(SLLParsers.parseTerm(g).toString == e)
  }

  def pProg(g: String, e: String): Unit = {
    assert(SLLParsers.parseProg(g).toString == e)
  }

  def pTask(g: String, e: String): Unit = {
    assert(SLLParsers.parseTask(g).toString == e)
  }

  test(testName = "parseTerm") {
    pTerm("x", "x")
    pTerm("C()", "C()")
    pTerm("C(x)", "C(x)")
    pTerm("C(x,y)", "C(x,y)")
    pTerm("fX(x,y)", "fX(x,y)")
  }

  test(testName = "parseProg") {
    pProg("f(x,y) = f(y, x);g(C(x),y) = g(y, x);",
      "f(x,y)=f(y,x);\ng(C(x),y)=g(y,x);")
  }

  test(testName = "parseTask") {
    pTask("f(x) where f(x) = f(x);",
      "f(x) where f(x)=f(x);")
  }

}
