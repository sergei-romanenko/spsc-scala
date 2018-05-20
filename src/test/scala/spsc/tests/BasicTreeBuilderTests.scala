package spsc.tests

import org.scalatest.FunSuite

import spsc.Algebra._
import spsc._

class BasicTreeBuilderTests extends FunSuite {

  def varAttackTrue(t: String): Unit = {
    val e = SLLParsers.parseTerm(t)
    assert(Tree.aVarIsUnderAttack(e))
  }

  def varAttackFalse(s: String): Unit = {
    val e = SLLParsers.parseTerm(s)
    assert(!Tree.aVarIsUnderAttack(e))
  }

  test(testName = "aVarIsUnderAttack") {
    varAttackTrue("x")
    varAttackFalse("A()")
    varAttackFalse("f(x)")
    varAttackTrue("g(x,y)")
    varAttackTrue("g1(g2(x))")
    varAttackFalse("g(A())")
    varAttackFalse("g(f(x))")
  }

  val pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
  val pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

  def drStep(prog: String, e: String, expected: String): Unit =
    drStep0(SLLParsers.parseProg(prog), SLLParsers.parseTerm(e), expected)

  def drStep0(prog: Program, term: Term, expected: String): Unit = {
    val builder = new BasicTreeBuilder(Task(term, prog))
    val branches = builder.driveTerm(term)
    val branches_s = (branches map { case (exp, oc) =>
      "(" + exp.toString + "," +
        (if (oc.isEmpty) "*" else oc.get.toString) + ")"
    }).mkString("+")
    assert(branches_s == expected)
  }

  test(testName = "101 Ctr") {
    drStep(prog = "", e = "C(a,b)", expected = "(a,*)+(b,*)")
  }

  test(testName = "102 FCall") {
    drStep(prog = "f(x)=x;", e = "f(A(z))", expected = "(A(z),*)")
  }

  test(testName = "103 GCallCtr") {
    drStep(prog = pAddAcc, e = "gAddAcc(S(S(Z)),Z)",
      expected = "(gAddAcc(S(Z),S(Z)),*)")
  }

  test(testName = "104 GCallVar") {
    drStep(prog = pAddAcc, e = "gAddAcc(a,b)",
      expected = "(b,a=Z)+(gAddAcc(x1,S(b)),a=S(x1))")
  }

  test(testName = "105 GCallGeneral") {
    drStep(prog = pAddAcc, e = "gAddAcc(gAddAcc(a,b),c)",
      expected = "(gAddAcc(b,c),a=Z)+(gAddAcc(gAddAcc(x1,S(b)),c),a=S(x1))")
  }

//  test(testName = "106 Let") {
//    drStep0(prog = Program(Nil),
//      term = Let(Ctr("C", List(Var("x"), Var("y"))),
//        List(("x", Var("a")), ("y", Var("b")))),
//      expected = "(C(x,y),*)+(a,*)+(b,*)")
//  }

  test(testName = "107 a a") {
    drStep(pAdd, "gAdd(a,a)",
      expected = "(Z,a=Z)+(S(gAdd(x1,S(x1))),a=S(x1))")
  }
}
