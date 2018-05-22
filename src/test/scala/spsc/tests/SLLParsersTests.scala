package spsc.tests

import org.scalatest.FunSuite
import spsc.SLLParsers.parseTerm
import spsc._

class SLLParsersTests extends FunSuite {

  def pTerm(g: String, e: String): Unit = {
    assert(parseTerm(g).toString == e)
  }

  def pTask(g: String, e: String): Unit = {
    assert(SLLParsers.parseTask(g).toString == e)
  }

  test(testName = "parseTerm") {
    pTerm("x", "x")
    pTerm(" -- 111\nx --222\n --333", "x")
    pTerm("C", "C")
    pTerm("C()", "C")
    pTerm("C(x)", "C(x)")
    pTerm("C(x,y)", "C(x,y)")
    pTerm("fX(x,y)", "fX(x,y)")
    pTerm("fX()", "fX()")
    pTerm("gX(x,y)", "gX(x,y)")
    pTerm("gX()", "gX()")
  }

  test(testName = "parseTask") {

    pTask("a where f(x,y) = f(y, x);g(C(x),y) = g(y, x);",
      "a where f(x,y)=f(y,x);g(C(x),y)=g(y,x);")

    pTask("a where g(Z,y)= y;", "a where g(Z,y)=y;")

    pTask("f(x) where f(x) = f(x);",
      "f(x) where f(x)=f(x);")

    pTask("h(Z) where h(Z) = h(Z);",
      "h(Z) where h(Z)=h(Z);")
  }

}
