package spsc.tests

import org.scalatest.FunSuite
import spsc.Algebra._
import spsc._

class AlgebraTests extends FunSuite {

  test(testName = "101 shellEq") {
    assert(shellowEq(Ctr("A", Nil), Ctr("A", Nil)))
    assert(shellowEq(FCall("A", Nil), FCall("A", Nil)))
    assert(shellowEq(GCall("A", Nil), GCall("A", Nil)))
    assert(!shellowEq(Ctr("A", Nil), Ctr("B", Nil)))
    assert(!shellowEq(Ctr("A", Nil), FCall("A", Nil)))
    assert(!shellowEq(Ctr("A", Nil), Ctr("A", List(Var("y")))))
  }

  test(testName = "201 applySubst") {
    val e1 = SLLParsers.parseTerm("E1")
    val e2 = SLLParsers.parseTerm("E2")
    val e = SLLParsers.parseTerm("Cons(x1,Cons(x2,Cons(x3,Nil())))")
    val subst = Map("x1" -> e1, "x2" -> e2)
    assert(applySubst(subst)(e).toString
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

  def substToString(os: Option[Subst]): Option[String] =
    for (subst <- os) yield {
      (for ((n, t) <- subst) yield s"$n->$t;").mkString("")
    }

  def matchOK(pat: String, term: String, expected: String): Unit = {
    val subst = matchAgainst(SLLParsers.parseTerm(pat), SLLParsers.parseTerm(term))
    assert(substToString(subst).contains(expected))
  }

  def matchNo(pat: String, term: String): Unit = {
    val subst = matchAgainst(SLLParsers.parseTerm(pat), SLLParsers.parseTerm(term))
    assert(substToString(subst).isEmpty)
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

  test(testName = "NameGen") {
    val ng = new NameGen(Seq("B2", "D5"))
    assert(List("A", "B", "C", "D", "E").map(ng.freshName)
      == List("A1", "B3", "C4", "D6", "E7"))
  }

  test(testName = "taskNames") {
    val sTask = "a where f(x)=x; g(C(p),q) = f(R(p,q));"
    val task = SLLParsers.parseTask(sTask)
    val ns = names(task).toList.sortWith(_<_)
    assert(ns.mkString(",") == "C,R,a,f,g,p,q,x")
  }
}
