package spsc.tests

import org.scalatest.FunSuite

import spsc.Algebra._
import spsc._

class BasicSupercompilerTests extends FunSuite {

  val pAdd = "gAdd(S(x),y)=S(gAdd(x,y));gAdd(Z(),y)=y;"
  val pAddAcc = "gAddAcc(S(x),y)=gAddAcc(x,S(y));gAddAcc(Z(),y)=y;"

  def drStep(prog: String, e: String, expected: String): Unit =
    drStep0(SParsers.parseProg(prog), SParsers.parseTerm(e), expected)

  def drStep0(prog: Program, e: Term, expected: String): Unit = {
    resetVarGen()
    val scp = new BasicSupercompiler(prog)
    val branches = scp.driveExp(e)
    val branches_s = (branches map { case (exp, contr) =>
      "(" + exp.toString + "," +
        (if (contr == null) "" else contr.toString) + ")"
    }).mkString("")
    assert(branches_s == expected)
  }

  test(testName = "101 Ctr") {
    drStep(prog = "", e = "C(a,b)", expected = "(a,)(b,)")
  }

  test(testName = "102 FCall") {
    drStep(prog = "f(x)=x;", e = "f(A(z))", expected = "(A(z),)")
  }

  test(testName = "103 GCallCtr") {
    drStep(prog = pAddAcc, e = "gAddAcc(S(S(Z())),Z())",
      expected = "(gAddAcc(S(Z()),S(Z())),)")
  }

  test(testName = "104 GCallVar") {
    drStep(prog = pAddAcc, e = "gAddAcc(a,b)",
      expected = "(b,a=Z())(gAddAcc(v1,S(b)),a=S(v1))")
  }

  test(testName = "105 GCallGeneral") {
    drStep(prog = pAddAcc, e = "gAddAcc(gAddAcc(a,b),c)",
      expected = "(gAddAcc(b,c),a=Z())(gAddAcc(gAddAcc(v1,S(b)),c),a=S(v1))")
  }

  test(testName = "106 Let") {
    drStep0(prog = Program(List()),
      e = Let(Ctr("C", List(Var("x"), Var("y"))),
        List((Var("x"), Var("a")), (Var("y"), Var("b")))),
      expected = "(C(x,y),)(a,)(b,)")
  }
}
