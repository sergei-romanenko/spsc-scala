package spsc.tests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import spsc._

class ProcessTreeTests extends FunSuite with BeforeAndAfter {

  var tree: Tree = _

  before {
    tree = Tree.create(Var("r"))
    tree = tree.addChildren(tree(0),List((Var("m1"), None), (Var("m2"), None)))
    val m1 = tree(tree(0).children.head)
    val m2 = tree(tree(0).children.tail.head)
    tree = tree.addChildren(m1, List((Var("n"), None)))
    tree = tree.replaceSubtree(m2, Var("x"))._1
  }

  test(testName = "01 Tree.toString") {
    assert(tree.toString
      == "{0:(r,,,[1,2]),1:(m1,,0,[3]),2:(x,,0,[]),3:(n,,1,[])}")
  }

  test(testName = "02 Tree.leaves") {
    assert(tree.leaves.map(_.nodeId).toList
      == List(3, 2))
  }
}
