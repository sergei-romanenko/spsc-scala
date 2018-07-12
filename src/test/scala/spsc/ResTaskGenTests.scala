package spsc

import org.scalatest.FunSuite
import spsc.SLLParsers.parseTask

class ResTaskGenTests extends FunSuite {

  def runBScp(t: String, p: String): String = {
    val input = t + " where " + p
    val task = parseTask(input)
    val builder = new BasicTreeBuilder(task)
    val tree = builder.buildProcessTree()
    val rpg = new ResTaskGen(tree)
    rpg.buildResTask().toString
  }

  def runAScp(t: String, p: String): String = {
    val input = t + " where " + p
    val task = parseTask(input)
    val builder = new AdvancedTreeBuilder(task)
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

  val pAdd =
    """
add(Z,y) = y;
add(S(x),y) = S(add(x,y));
"""

  val pAddAcc =
    """
addAcc(Z,y) = y;
addAcc(S(x),y) = addAcc(x,S(y));
"""

  // Basic supercompiler

  test(testName = "Basic supercompiler") {

    testBScp("Z", "", "Z where ")

    testBScp("add(a, b)", pAdd,
      "add1(a,b) where " +
        "add1(Z,b)=b;" +
        "add1(S(x1),b)=S(add1(x1,b));"
    )

    testBScp("add(add(a,b),c)", pAdd,
      "add1(a,b,c) where " +
        "add1(Z,b,c)=add2(b,c);" +
        "add1(S(x1),b,c)=S(add1(x1,b,c));" +
        "add2(Z,c)=c;" +
        "add2(S(x2),c)=S(add2(x2,c));")

    testBScp("addAcc(a, b)", pAddAcc,
      "addAcc1(a,b) where " +
        "addAcc1(Z,b)=b;" +
        "addAcc1(S(x1),b)=addAcc1(x1,S(b));")
  }

  // Advanced supercompiler

  test(testName = "Advanced supercompiler") {

    testAScp("Z", "", "Z where ")

    testAScp("add(a, b)", pAdd,
      "add1(a,b) where " +
        "add1(Z,b)=b;" +
        "add1(S(x1),b)=S(add1(x1,b));"
    )

    testAScp("add(a, a)", pAdd,
      "add1(a,a) where " +
        "add1(Z,v4)=v4;" +
        "add1(S(x5),v4)=S(add1(x5,v4));"
    )

    testAScp("add(add(a,b),c)", pAdd,
      "add1(a,b,c) where " +
        "add1(Z,b,c)=add2(b,c);" +
        "add1(S(x1),b,c)=S(add1(x1,b,c));" +
        "add2(Z,c)=c;" +
        "add2(S(x2),c)=S(add2(x2,c));")

    testAScp("addAcc(a, b)", pAddAcc,
      "addAcc1(a,b) where " +
        "addAcc1(Z,b)=b;" +
        "addAcc1(S(x1),b)=addAcc1(x1,S(b));")

    testAScp("addAcc(a, a)", pAddAcc,
      "addAcc1(a,a) where " +
        "addAcc1(Z,v4)=v4;" +
        "addAcc1(S(x5),v4)=addAcc1(x5,S(v4));")

    testAScp("addAcc(addAcc(a,b),c)", pAddAcc,
      "addAcc1(a,b,c) where " +
        "addAcc1(Z,b,c)=addAcc2(b,c);" +
        "addAcc1(S(x1),b,c)=addAcc1(x1,S(b),c);" +
        "addAcc2(Z,c)=c;" +
        "addAcc2(S(x2),c)=addAcc2(x2,S(c));")

    // More general
    testAScp("f(a)", "f(x) = f(S(x));",
      "f1(a) where f1(a)=f1(S(a));")

    // Embedded
    testAScp("f(a)", "f(x) = g(f(x)); g(A) = B;",
      "f1(a) where " +
        "f1(a)=g2(f1(a));" +
        "g2(A)=B;")

  }

  // More samples

  test(testName = "global vs local control") {
    testAScp("g(x)",
      """
      g(True) = True;
      g(False) = g(False);
      """,
      "g1(x) where " +
        "g1(True)=True;" +
        "g1(False)=g2();" +
        "g2()=g2();")
  }

  val pAppend =
    """
append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));
"""

  test(testName = "append(append(xs, ys), zs))") {
    testAScp("append(append(xs, ys), zs)", pAppend,
      "append1(xs,ys,zs) where " +
        "append1(Nil,ys,zs)=append2(ys,zs);" +
        "append1(Cons(u1,us2),ys,zs)=Cons(u1,append1(us2,ys,zs));" +
        "append2(Nil,zs)=zs;" +
        "append2(Cons(u3,us4),zs)=Cons(u3,append2(us4,zs));")
  }

  val pRevAppend =
    """
rev(Nil) = Nil;
rev(Cons(x, xs))= append(rev(xs), Cons(x, Nil));
append(Nil, vs) = vs;
append(Cons(u, us), vs) = Cons(u, append(us, vs));
"""

  test(testName = "rev append") {
    testAScp("rev(xs)", pRevAppend,
      "rev1(xs) where " +
        "append2(Nil,v5)=v5;" +
        "append2(Cons(u6,us7),v5)=Cons(u6,append2(us7,v5));" +
        "rev1(Nil)=Nil;" +
        "rev1(Cons(x1,xs2))=append2(rev1(xs2),Cons(x1,Nil));")
  }

  test(testName = "f1 f2 f3 g") {
    testAScp("f1(z)",
      """
      f1(x) = f2(x);
      f2(x) = g(x);
      f3(x) = f1(x);
      g(A(a)) = f1(a);
      g(B(b)) = f1(b);
      """,
      "f11(z) where " +
        "f11(z)=g2(z);" +
        "g2(A(a1))=f11(a1);" +
        "g2(B(b2))=f11(b2);")
  }

  // eq(x, y)

  val pEq =
    """
eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);
"""

  test(testName = "eq(S(S(Z)), x)") {
    testAScp("eq(S(S(Z)), x)", pEq,
      "eqS1(x) where " +
        "eqS1(Z)=False;" +
        "eqS1(S(y1))=eqS2(y1);" +
        "eqS2(Z)=False;" +
        "eqS2(S(y2))=eqZ3(y2);" +
        "eqZ3(Z)=True;" +
        "eqZ3(S(x3))=False;")
  }

  test(testName = "eq(a, a)") {
    testAScp("eq(a, a)", pEq,
      "eq1(a) where " +
        "eq1(Z)=True;" +
        "eq1(S(x1))=eq1(x1);")
  }

  // S(x) -> S(S(x))

  test(testName = "S(x) -> S(S(x))") {
    testAScp("g(x)",
      """
      g(Z) = Z;
      g(S(x)) = g(S(S(x)));
      """,
      "g1(x) where " +
        "g1(Z)=Z;" +
        "g1(S(x1))=g2(x1);" +
        "g2(x1)=g2(S(x1));")
  }

  // Exponential time to linear

  test(testName = "Exp to linear") {
    testAScp("g(a,a)",
      """
      g(Z,y) = y;
      g(S(x),y)=g(g(x,x),g(x,x));
      """,
      "g1(a) where " +
        "g1(Z)=Z;" +
        "g1(S(x1))=g1(g1(x1));")
  }


  val pKMP =
    """
eqSymb(A, y) = eqA(y);
eqSymb(B, y) = eqB(y);
eqSymb(C, y) = eqC(y);

eqA(A) = True;  eqA(B) = False; eqA(C) = False;
eqB(A) = False; eqB(B) = True;  eqB(C) = False;
eqC(A) = False; eqC(B) = False; eqC(C) = True;

if(True, x, y) = x;
if(False, x, y) = y;

match(p, s) = m(p, s, p, s);

m(Nil, ss, op, os) = True;
m(Cons(p, pp), ss, op, os) = mx(ss, p, pp, op, os);

mx(Nil, p, pp,  op, os) = False;
mx(Cons(s, ss), p, pp,  op, os) = if(eqSymb(p, s), m(pp, ss, op, os), mn(os, op));

mn(Nil, op) = False;
mn(Cons(s, ss), op) = m(op, ss, op, ss);
"""

  test(testName = "KMP A") {
    testAScp("match(Cons(A, Nil), str)", pKMP,
      "m1(str) where " +
        "if3(A,ss2)=True;" +
        "if3(B,ss2)=m1(ss2);" +
        "if3(C,ss2)=m1(ss2);" +
        "m1(str)=mx2(str);" +
        "mx2(Nil)=False;" +
        "mx2(Cons(s1,ss2))=if3(s1,ss2);")
  }

  test(testName = "KMP A A") {
    testAScp("match(Cons(A, Cons(A, Nil)), str)", pKMP,
      "m1(str) where " +
        "if3(A,ss2)=mx4(ss2);" +
        "if3(B,ss2)=m1(ss2);" +
        "if3(C,ss2)=m1(ss2);" +
        "if5(A,ss4)=True;" +
        "if5(B,ss4)=m1(ss4);" +
        "if5(C,ss4)=m1(ss4);" +
        "m1(str)=mx2(str);" +
        "mx2(Nil)=False;" +
        "mx2(Cons(s1,ss2))=if3(s1,ss2);" +
        "mx4(Nil)=False;" +
        "mx4(Cons(s3,ss4))=if5(s3,ss4);")
  }

  test(testName = "KMP A B A") {
    testAScp("match(Cons(A, Cons(B, Cons (A, Nil))), str)", pKMP,
      "m1(str) where " +
        "if3(A,ss2)=if4(ss2);" +
        "if3(B,ss2)=m1(ss2);" +
        "if3(C,ss2)=m1(ss2);" +
        "if4(ss2)=mx5(ss2);" +
        "if6(A,ss4)=if4(ss4);" +
        "if6(B,ss4)=mx7(ss4);" +
        "if6(C,ss4)=m1(ss4);" +
        "if8(A,ss6)=True;" +
        "if8(B,ss6)=m1(ss6);" +
        "if8(C,ss6)=m1(ss6);" +
        "m1(str)=mx2(str);" +
        "mx2(Nil)=False;" +
        "mx2(Cons(s1,ss2))=if3(s1,ss2);" +
        "mx5(Nil)=False;" +
        "mx5(Cons(s3,ss4))=if6(s3,ss4);" +
        "mx7(Nil)=False;" +
        "mx7(Cons(s5,ss6))=if8(s5,ss6);")
  }

}
