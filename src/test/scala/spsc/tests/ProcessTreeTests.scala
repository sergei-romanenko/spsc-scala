package spsc.tests

import org.scalatest.FunSuite
import spsc._

class ProcessTreeTests extends FunSuite {

  def testTree: Tree = {
    val t0 = Tree.create(Var("r"))
    val t1 = t0.addChildren(t0(0),List((Var("m1"), None), (Var("m2"), None)))
    val m1 = t1(t1(0).children.head)
    val m2 = t1(t1(0).children.tail.head)
    val t2 = t1.addChildren(m1, List((Var("n"), None)))
    val t3 = t2.replaceSubtree(m2, Var("x"))._1
    t3
  }

  test(testName = "01 Tree.toString") {
    val t = testTree
    assert(t.toString
      == "{0:(r,,,[1,2]),1:(m1,,0,[3]),2:(x,,0,[]),3:(n,,1,[])}")
  }

  test(testName = "02 Tree.leaves") {
    val t = testTree
    assert(t.leaves.map(_.nodeId).toList
      == List(2, 3))
  }
}
