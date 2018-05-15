package spsc.tests

import org.scalatest.FunSuite
import spsc._


class SLLTests extends FunSuite {

  def assertToStr(t: Term, s: String): Unit = {
    assert(t.toString === s)
  }

  test(testName = "Term.toString 101 StrVarAndCall") {
    assertToStr(Var("x"), "x")
    assertToStr(Ctr("A", List(Var("x"), Var("y"))), "A(x,y)")
    assertToStr(Ctr("C", Nil), "C")
    assertToStr(FCall("fX", List(Var("x"), Var("y"))), "fX(x,y)")
    assertToStr(GCall("gX", List(Var("x"), Var("y"))), "gX(x,y)")
  }

  test(testName = "Term.toString 102 StrLet") {
    assertToStr(Let(Var("y"), List(("x", Var("y")))),
      "let x=y in y")
    assertToStr(Let(Var("x"), List(("x", Var("a")), ("y", Var("b")))),
      "let x=a,y=b in x")
  }

  test(testName = "Rule.toString 103 StrRule") {
    assert(FRule("f", List("x", "y"), Var("y")).toString
      === "f(x,y)=y;")
    assert(GRule("g", Pat("C", List("x")), List("y"), Var("y")).toString
      === "g(C(x),y)=y;")
    assert(GRule("g", Pat("C", Nil), List("y"), Var("y")).toString
      === "g(C,y)=y;")
    assert(GRule("g", Pat("C", Nil), Nil, Ctr("C", Nil)).toString
      === "g(C)=C;")
  }

  test(testName = "Program.toString 104 StrProgram") {
    assert(Program(List(
      FRule("f", Nil, Ctr("A", Nil)),
      FRule("f1", Nil, Ctr("A1", Nil)))).toString
      === "f()=A;f1()=A1;")
    assert(Program(List(
      GRule("g", Pat("C", Nil), Nil, Ctr("A", Nil)),
      GRule("g1", Pat("C", Nil), List("x"), Ctr("A", Nil)),
      GRule("g2", Pat("C", List("x")), Nil, Ctr("A", Nil)))).toString
      === "g(C)=A;g1(C,x)=A;g2(C(x))=A;")
  }

  test(testName = "Term.equals 201 Eq") {
    assert(Var("x") ==  Var("x"))
    assert(Var("x") !=  Var("y"))
    assert(Ctr("A", Nil) == Ctr("A", Nil))
    assert(Ctr("A", Nil) != Ctr("B", Nil))
    assert(Nil ==  Nil)
    assert(List(Var("x")) ==  List(Var("x")))
    assert(List(Var("x")) != List(Var("y")))
    assert(List(Var("x")) != List(Var("x"), Var("z")))
    assert(Ctr("A", List(Var("x"))) == Ctr("A", List(Var("x"))))
    assert(Ctr("A", List(Var("x"))) != Ctr("A", List(Var("y"))))
  }

}
