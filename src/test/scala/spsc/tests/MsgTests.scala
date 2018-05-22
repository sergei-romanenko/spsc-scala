package spsc.tests

import org.scalatest.FunSuite
import spsc.SLLParsers.parseTerm
import spsc._

class MsgTests extends FunSuite {

  def msgOK(t1: String, t2: String, expected: String): Unit = {
    val ng = new NameGen(Seq())
    val msgen = new MSGen(ng)
    val gen = msgen.msg(parseTerm(t1), parseTerm(t2))
    assert(gen.toString == expected)
  }

  test(testName = "101 msg common functor") {
    msgOK(t1 = "A(a1,C(a2,a3))", t2 = "A(b1,C(b2,b3))",
      expected = "Gen(A(v2,C(v4,v5))," +
        "Map(v2 -> a1, v4 -> a2, v5 -> a3),Map(v2 -> b1, v4 -> b2, v5 -> b3))")
  }

  test(testName = "102 msg merge subexp") {
    msgOK(
      t1 = "f(a1,a2,a1)",
      t2 = "f(b1,b2,b1)",
      expected = "Gen(f(v4,v3,v4)," +
        "Map(v3 -> a2, v4 -> a1),Map(v3 -> b2, v4 -> b1))")
  }

  test(testName = "103 msg merge subexp") {
    msgOK(t1 = "f(a,a)", t2 = "f(b,S(b))",
      expected = "Gen(f(v2,v3)," +
        "Map(v2 -> a, v3 -> a),Map(v2 -> b, v3 -> S(b)))")
  }
}
