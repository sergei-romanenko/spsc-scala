package spsc

import org.scalatest.FunSuite
import spsc.SLLParsers.{parseTask, parseTerm}

class BasicTreeBuilderTests extends FunSuite {

  def varAttackTrue(s: String): Unit = {
    val term = parseTerm(s)
    assert(Tree.aVarIsUnderAttack(term))
  }

  def varAttackFalse(s: String): Unit = {
    val term = parseTerm(s)
    assert(!Tree.aVarIsUnderAttack(term))
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

  def drStep(rules: String, t: String, expected: String): Unit = {
    val given = t + " where " + rules
    drStep0(parseTask(given), expected)
  }

  def drStep0(task: Task, expected: String): Unit = {
    val term = task.term
    val rules = task.rules
    val builder = new BasicTreeBuilder(Task(term, rules))
    val branches = builder.driveTerm(term)
    val branches_s = (branches map { case (exp, oc) =>
      "(" + exp.toString + "," +
        (if (oc.isEmpty) "*" else oc.get.toString) + ")"
    }).mkString("+")
    assert(branches_s == expected)
  }

  test(testName = "101 Ctr") {
    drStep(rules = "", t = "C(a,b)", expected = "(a,*)+(b,*)")
  }

  test(testName = "102 FCall") {
    drStep(rules = "f(x)=x;", t = "f(A(z))", expected = "(A(z),*)")
  }

  test(testName = "103 GCallCtr") {
    drStep(rules = pAddAcc, t = "gAddAcc(S(S(Z)),Z)",
      expected = "(gAddAcc(S(Z),S(Z)),*)")
  }

  test(testName = "104 GCallVar") {
    drStep(rules = pAddAcc, t = "gAddAcc(a,b)",
      expected = "(b,a=Z)+(gAddAcc(x1,S(b)),a=S(x1))")
  }

  test(testName = "105 GCallGeneral") {
    drStep(rules = pAddAcc, t = "gAddAcc(gAddAcc(a,b),c)",
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
