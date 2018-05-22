package spsc.tests

import org.scalatest.FunSuite
import spsc.HE._
import spsc.SLLParsers.parseTerm
import spsc._

class HeTests extends FunSuite {

  def heTrue(t1: String, t2: String): Unit = {
    val term1 = parseTerm(t1)
    val term2 = parseTerm(t2)
    assert(embeddedIn(term1, term2))
  }

  def heFalse(t1: String, t2: String): Unit = {
    val term1 = parseTerm(t1)
    val term2 = parseTerm(t2)
    assert(!embeddedIn(term1, term2))
  }

  test(testName = "201 he") {
    heTrue(t1 = "v1", t2 = "v2")
  }

  test(testName = "202 he") {
    heTrue(t1 = "v1", t2 = "F(v2)")
  }

  test(testName = "203 he") {
    heFalse(t1 = "F(v2)", t2 = "v1")
  }

  test(testName = "204 he diving") {
    heTrue(t1 = "F(v1)", t2 = "G(v0,F(H(v2)))")
  }

  test(testName = "205 he coupling1") {
    heTrue(t1 = "F(v1,G(v2))", t2 = "F(H(w1),G(w2))")
  }

  test(testName = "206 he coupling") {
    heFalse(t1 = "f(v1)", t2 = "g(w1)")
  }

}
