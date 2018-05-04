package spsc.tests

import org.scalatest.FunSuite
import spsc.Algebra._
import spsc._

class AlgebraTests extends FunSuite {

  test(testName = "101 shellEq") {
    assert(shellEq(Ctr("A", List()), Ctr("A", List())))
    assert(shellEq(FCall("A", List()), FCall("A", List())))
    assert(shellEq(GCall("A", List()), GCall("A", List())))
    assert(!shellEq(Ctr("A", List()), Ctr("B", List())))
    assert(!shellEq(Ctr("A", List()), FCall("A", List())))
    assert(!shellEq(Ctr("A", List()), Ctr("A", List(Var("y")))))
  }

  test(testName = "201 applySubst") {
    val e1 = SParsers.parseTerm("E1()")
    val e2 = SParsers.parseTerm("E2()")
    val e = SParsers.parseTerm("Cons(x1,Cons(x2,Cons(x3,Nil())))")
    val subst = Map(Var("x1") -> e1, Var("x2") -> e2)
    assert(applySubst(subst, e).toString
      == "Cons(E1(),Cons(E2(),Cons(x3,Nil())))")
  }

  test(testName = "302 vars") {
    val e = SParsers.parseTerm("A(x,B(y,z),a)")
    assert(vars(e)
      == List(Var("x"), Var("y"), Var("z"), Var("a")))

    val e1 = SParsers.parseTerm("A(x,B(y,x),a)")
    assert(vars(e1)
      == List(Var("x"), Var("y"), Var("a")))
  }

  def substToString(subst: Map[Var, Term]): String = {
    if (subst == null)
      null
    else {
      var acc = ""
      for ((v, e) <- subst)
        acc += v.toString() + "->" + e.toString + ";"
      acc.mkString("")
    }
  }

  def matchOK(pat: String, exp: String, expected: String): Unit = {
    val subst = matchAgainst(SParsers.parseTerm(pat), SParsers.parseTerm(exp))
    assert(substToString(subst) == expected)
  }

  def matchNo(pat: String, exp: String): Unit = {
    val subst = matchAgainst(SParsers.parseTerm(pat), SParsers.parseTerm(exp))
    assert(substToString(subst) == null)
  }

  test(testName = "401 matchAgainst") {
    matchOK(pat = "x", exp = "S(Z())", expected = "x->S(Z());")
  }

  test(testName = "402 matchAgainst") {
    matchNo(pat = "Z()", exp = "x")
  }

  test(testName = "403 matchAgainst") {
    matchOK(pat = "C(x,y)", exp = "C(A(),B())", expected = "x->A();y->B();")
  }

  test(testName = "404 matchAgainst") {
    matchNo(pat = "C(x,y)", exp = "D(A(),B())")
  }

  test(testName = "405 matchAgainst") {
    matchNo(pat = "C(x,y)", exp = "f(A(),B())")
  }

  test(testName = "406 matchAgainst") {
    matchOK(pat = "C(x,x)", exp = "C(A(),A())", expected = "x->A();")
  }

  test(testName = "407 matchAgainst") {
    matchNo(pat = "C(x,y)", exp = "C(A(),B(),C())")
  }

  test(testName = "408 matchAgainst") {
    matchNo(pat = "C(x,y,z)", exp = "C(A(),B())")
  }

  def equivYes(e1: String, e2: String): Unit = {
    assert(equiv(SParsers.parseTerm(e1), SParsers.parseTerm(e2)))
  }

  def equivNo(e1: String, e2: String): Unit = {
    assert(!equiv(SParsers.parseTerm(e1), SParsers.parseTerm(e2)))
  }

  test(testName = "501 equiv") {
    equivYes(e1 = "gA(fB(x,y),C())", e2 = "gA(fB(a,b),C())")
  }

  test(testName = "502 equiv") {
    equivNo(e1 = "gA(fB(x,y),x)", e2 = "gA(fB(a,a),b)")
  }
}
