package spsc

import org.scalatest.FunSuite
import spsc.SLLCheck.checkTask
import spsc.SLLParsers.parseTask

class SLLCheckTests extends FunSuite {

  def chTaskOK(g: String): Unit = {
    val task = parseTask(g)
    assert(checkTask(task).isEmpty)
  }

  def chTask(g: String, e: String): Unit = {
    val task = SLLParsers.parseTask(g)
    assert(checkTask(task).get == e)
  }

  test(testName = "SepFG") {
    chTaskOK("a where f(x) = x; g(C1,y) = C; g(C2(x),y) = g(x,y);")
  }

  test(testName = "BothFG") {
    chTask("a where f(x)=x; f(C)=C;",
      "f is both f- and g-function")
  }

  test(testName = "BothFGV") {
    chTask("f where f(x)=x;",
      "f is both a function and a parameter")
  }

  test(testName = "RepFP") {
    chTask("a where f(x,x)=x;",
      "x is repeated in the parameters of f")
  }

  test(testName = "RepGP") {
    chTask("a where g(C(x),x)=x;",
      "x is repeated in the parameters of g")
  }

  test(testName = "RepGC") {
    chTask("a where g(C(x))=x;g(C(x))=x;",
      "In the definition of g, C appears twice in the patterns")
  }

  test(testName = "UFPV") {
    chTask("a where f(x)=y;",
      "In the definition of f, y is not a parameter")
  }

  test(testName = "UGPV") {
    chTask("a where g(C(x), y)=z;",
      "In the definition of g, z is not a parameter")
  }

  test(testName = "UFF") {
    chTask("a where f()=f1();",
      "In the definition of f, there is a call to an undefined function f1")
  }

  test(testName = "UGF") {
    chTask("a where g(C)=f1();",
      "In the definition of g, there is a call to an undefined function f1")
  }

  test(testName = "UTF") {
    chTask("f(a) where ",
      "In the initial term, there is a call to an undefined function f")
  }

  test(testName = "ArC1") {
    chTask("C(C) where ",
      "C has inconsistent arity: 0 and 1")
  }

  test(testName = "ArC2") {
    chTask("a where f()=C(C);",
      "C has inconsistent arity: 0 and 1")
  }

  test(testName = "ArC3") {
    chTask("a where g(C(x))=C;",
      "C has inconsistent arity: 0 and 1")
  }

  test(testName = "ArC4") {
    chTask("C where f(x)=C(x);",
      "C has inconsistent arity: 1 and 0")
  }

  test(testName = "ArF1") {
    chTask("f() where f(x)=x;",
      "f has inconsistent arity: 1 and 0")
  }

  test(testName = "ArF2") {
    chTask("a where f(x)=f(f());",
      "f has inconsistent arity: 0 and 1")
  }

  test(testName = "ArG1") {
    chTask("a where g(C, x)=g(C, g(C));",
      "g has inconsistent arity: 1 and 2")
  }

}
