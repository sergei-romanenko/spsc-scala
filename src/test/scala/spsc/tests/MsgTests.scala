package spsc.tests

import org.scalatest.FunSuite
import spsc.Algebra._
import spsc.MSG._
import spsc._

class MsgTests extends FunSuite {

  def msgOK(e1: String, e2: String, expected: String): Unit = {
    resetVarGen()
    val gen = msg(SLLParsers.parseTerm(e1), SLLParsers.parseTerm(e2))
    assert(gen.toString == expected)
  }

  test(testName = "101 msg common functor") {
    msgOK(e1 = "A(a1,C(a2,a3))", e2 = "A(b1,C(b2,b3))",
      expected = "Gen(A(v2,C(v4,v5))," +
        "Map(v2 -> a1, v4 -> a2, v5 -> a3),Map(v2 -> b1, v4 -> b2, v5 -> b3))")
  }

  test(testName = "102 msg merge subexp") {
    msgOK(
      e1 = "f(a1,a2,a1)",
      e2 = "f(b1,b2,b1)",
      expected = "Gen(f(v4,v3,v4)," +
        "Map(v3 -> a2, v4 -> a1),Map(v3 -> b2, v4 -> b1))")
  }

  test(testName = "103 msg merge subexp") {
    msgOK(e1 = "f(a,a)", e2 = "f(b,S(b))",
      expected = "Gen(f(v2,v3)," +
        "Map(v2 -> a, v3 -> a),Map(v2 -> b, v3 -> S(b)))")
  }
}
