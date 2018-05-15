package spsc.tests

import org.scalatest.FunSuite
import spsc.Algebra._
import spsc._

class AlgebraTests extends FunSuite {

  test(testName = "101 shellEq") {
    assert(shellEq(Ctr("A", Nil), Ctr("A", Nil)))
    assert(shellEq(FCall("A", Nil), FCall("A", Nil)))
    assert(shellEq(GCall("A", Nil), GCall("A", Nil)))
    assert(!shellEq(Ctr("A", Nil), Ctr("B", Nil)))
    assert(!shellEq(Ctr("A", Nil), FCall("A", Nil)))
    assert(!shellEq(Ctr("A", Nil), Ctr("A", List(Var("y")))))
  }

  test(testName = "201 applySubst") {
    val e1 = SLLParsers.parseTerm("E1")
    val e2 = SLLParsers.parseTerm("E2")
    val e = SLLParsers.parseTerm("Cons(x1,Cons(x2,Cons(x3,Nil())))")
    val subst = Map("x1" -> e1, "x2" -> e2)
    assert(applySubst(subst, e).toString
      == "Cons(E1,Cons(E2,Cons(x3,Nil)))")
  }

  test(testName = "302 vars") {
    val e = SLLParsers.parseTerm("A(x,B(y,z),a)")
    assert(vars(e)
      == List("x", "y", "z", "a"))

    val e1 = SLLParsers.parseTerm("A(x,B(y,x),a)")
    assert(vars(e1)
      == List("x", "y", "a"))
  }

  def substToString(subst: Map[String, Term]): String = {
    if (subst == null)
      null
    else {
      val acc = for((n, e) <- subst) yield f"$n->${e.toString};"
      acc.mkString("")
    }
  }

  def matchOK(pat: String, term: String, expected: String): Unit = {
    val subst = matchAgainst(SLLParsers.parseTerm(pat), SLLParsers.parseTerm(term))
    assert(substToString(subst) == expected)
  }

  def matchNo(pat: String, term: String): Unit = {
    val subst = matchAgainst(SLLParsers.parseTerm(pat), SLLParsers.parseTerm(term))
    assert(substToString(subst) == null)
  }

  test(testName = "401 matchAgainst") {
    matchOK(pat = "x", term = "S(Z)", expected = "x->S(Z);")
  }

  test(testName = "402 matchAgainst") {
    matchNo(pat = "Z", term = "x")
  }

  test(testName = "403 matchAgainst") {
    matchOK(pat = "C(x,y)", term = "C(A,B)", expected = "x->A;y->B;")
  }

  test(testName = "404 matchAgainst") {
    matchNo(pat = "C(x,y)", term = "D(A(),B())")
  }

  test(testName = "405 matchAgainst") {
    matchNo(pat = "C(x,y)", term = "f(A(),B())")
  }

  test(testName = "406 matchAgainst") {
    matchOK(pat = "C(x,x)", term = "C(A,A)", expected = "x->A;")
  }

  test(testName = "407 matchAgainst") {
    matchNo(pat = "C(x,y)", term = "C(A,B,C)")
  }

  test(testName = "408 matchAgainst") {
    matchNo(pat = "C(x,y,z)", term = "C(A,B)")
  }

  def equivYes(e1: String, e2: String): Unit = {
    assert(equiv(SLLParsers.parseTerm(e1), SLLParsers.parseTerm(e2)))
  }

  def equivNo(e1: String, e2: String): Unit = {
    assert(!equiv(SLLParsers.parseTerm(e1), SLLParsers.parseTerm(e2)))
  }

  test(testName = "501 equiv") {
    equivYes(e1 = "gA(fB(x,y),C)", e2 = "gA(fB(a,b),C)")
  }

  test(testName = "502 equiv") {
    equivNo(e1 = "gA(fB(x,y),x)", e2 = "gA(fB(a,a),b)")
  }
}
