package spsc.tests

import org.scalatest.FunSuite
import spsc._

class ResTaskGenTests extends FunSuite {

  def runBScp(t: String, p: String): String = {
    val term = SLLParsers.parseTerm(t)
    val prog = SLLParsers.parseProg(p)
    val builder = new BasicTreeBuilder(Task(term, prog))
    val tree = builder.buildProcessTree()
    val rpg = new ResTaskGen(tree)
    rpg.buildResTask().toString
  }

  def runAScp(t: String, p: String): String = {
    val term = SLLParsers.parseTerm(t)
    val prog = SLLParsers.parseProg(p)
    val builder = new AdvancedTreeBuilder(Task(term, prog))
    val tree = builder.buildProcessTree()
    val rpg = new ResTaskGen(tree)
    rpg.buildResTask().toString
  }

  def testBScp(t: String, p: String, e: String): Unit = {
    assert(runBScp(t, p) == e)
  }

  def testAScp(t: String, p: String, e: String): Unit = {
    assert(runAScp(t, p) == e)
  }

  // Sample programs

  val pAdd = "gAdd(Z,y) = y;gAdd(S(x),y) = S(gAdd(x,y));"

  val pAddAcc = "gAddAcc(Z,y) = y;gAddAcc(S(x),y) = gAddAcc(x,S(y));"

  // Basic supercompiler

  test(testName = "Basic supercompiler") {

    testBScp("Z", "", "Z where ")

    testBScp("gAdd(a, b)", pAdd,
      "gAdd1(a,b) where gAdd1(Z,b)=b;gAdd1(S(x1),b)=S(gAdd1(x1,b));"
    )

    testBScp("gAdd(gAdd(a,b),c)", pAdd,
      "gAdd1(a,b,c) where gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(x1),b,c)=S(gAdd1(x1,b,c));gAdd2(Z,c)=c;gAdd2(S(x2),c)=S(gAdd2(x2,c));")

    testBScp("gAddAcc(a, b)", pAddAcc,
      "gAddAcc1(a,b) where gAddAcc1(Z,b)=b;gAddAcc1(S(x1),b)=gAddAcc1(x1,S(b));")
  }

  // Advanced supercompiler

  test(testName = "Advanced supercompiler") {

    testAScp("Z", "", "Z where ")

    testAScp("gAdd(a, b)", pAdd,
      "gAdd1(a,b) where gAdd1(Z,b)=b;gAdd1(S(x1),b)=S(gAdd1(x1,b));"
    )

    testAScp("gAdd(a, a)", pAdd,
      "gAdd1(a,a) where gAdd1(Z,v4)=v4;gAdd1(S(x5),v4)=S(gAdd1(x5,v4));"
    )

    testAScp("gAdd(gAdd(a,b),c)", pAdd,
      "gAdd1(a,b,c) where gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(x1),b,c)=S(gAdd1(x1,b,c));gAdd2(Z,c)=c;gAdd2(S(x2),c)=S(gAdd2(x2,c));")

    testAScp("gAddAcc(a, b)", pAddAcc,
      "gAddAcc1(a,b) where gAddAcc1(Z,b)=b;gAddAcc1(S(x1),b)=gAddAcc1(x1,S(b));")

    testAScp("gAddAcc(a, a)", pAddAcc,
      "gAddAcc1(a,a) where gAddAcc1(Z,v4)=v4;gAddAcc1(S(x5),v4)=gAddAcc1(x5,S(v4));")

    testAScp("gAddAcc(gAddAcc(a,b),c)", pAddAcc,
      "gAddAcc1(a,b,c) where gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(x1),b,c)=gAddAcc1(x1,S(b),c);gAddAcc2(Z,c)=c;gAddAcc2(S(x2),c)=gAddAcc2(x2,S(c));")

    // More general
    testAScp("f(a)", "f(x) = f(S(x));", "f1(a) where f1(a)=f1(S(a));")

    // Embedded
    testAScp("f(a)", "f(x) = g(f(x));g(A) = B;",
      "f1(a) where f1(a)=g2(f1(a));g2(A)=B;")

  }

}
