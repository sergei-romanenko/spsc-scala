package spsc.tests

import org.scalatest.FunSuite
import spsc.HE._
import spsc._

class HeTests extends FunSuite {

  def heTrue(t1: String, t2: String): Unit = {
    val e1 = SLLParsers.parseTerm(t1)
    val e2 = SLLParsers.parseTerm(t2)
    assert(he(e1, e2))
  }

  def heFalse(t1: String, t2: String): Unit = {
    val e1 = SLLParsers.parseTerm(t1)
    val e2 = SLLParsers.parseTerm(t2)
    assert(!he(e1, e2))
  }

  def varAttackTrue(t: String): Unit = {
    val e = SLLParsers.parseTerm(t)
    assert(aVarIsUnderAttack(e))
  }

  def varAttackFalse(s: String): Unit = {
    val e = SLLParsers.parseTerm(s)
    assert(!aVarIsUnderAttack(e))
  }

  test(testName = "101 aVarIsUnderAttack") {
    varAttackTrue("x")
    varAttackFalse("A()")
    varAttackFalse("f(x)")
    varAttackTrue("g(x,y)")
    varAttackTrue("g1(g2(x))")
    varAttackFalse("g(A())")
    varAttackFalse("g(f(x))")
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
