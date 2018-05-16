package spsc.tests

import org.scalatest.FunSuite

import spsc.Algebra._
import spsc._

class BasicTreeBuilderTests extends FunSuite {

  val pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
  val pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"

  def drStep(prog: String, e: String, expected: String): Unit =
    drStep0(SLLParsers.parseProg(prog), SLLParsers.parseTerm(e), expected)

  def drStep0(prog: Program, term: Term, expected: String): Unit = {
    resetVarGen()
    val builder = new BasicTreeBuilder(prog)
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
      expected = "(b,a=Z)+(gAddAcc(v1,S(b)),a=S(v1))")
  }

  test(testName = "105 GCallGeneral") {
    drStep(prog = pAddAcc, e = "gAddAcc(gAddAcc(a,b),c)",
      expected = "(gAddAcc(b,c),a=Z)+(gAddAcc(gAddAcc(v1,S(b)),c),a=S(v1))")
  }

//  test(testName = "106 Let") {
//    drStep0(prog = Program(Nil),
//      term = Let(Ctr("C", List(Var("x"), Var("y"))),
//        List(("x", Var("a")), ("y", Var("b")))),
//      expected = "(C(x,y),*)+(a,*)+(b,*)")
//  }

  test(testName = "107 a a") {
    drStep(pAdd, "gAdd(a,a)",
      expected = "(Z,a=Z)+(S(gAdd(v1,S(v1))),a=S(v1))")
  }
}
