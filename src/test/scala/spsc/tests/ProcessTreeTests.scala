package spsc.tests

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import spsc._

class ProcessTreeTests extends FunSuite with BeforeAndAfter {

  var tree: Tree = _

  before {
    tree = Tree.create(Var("r"))
    val r = tree.root
    tree = tree.addChildren(r,List((Var("m1"), null), (Var("m2"), null)))
    val m1 = tree.children(r).head
    val m2 = tree.children(r).tail.head
    tree = tree.addChildren(m1, List((Var("n"), null)))
    tree = tree.replace(m2, Var("x"))._1
  }

  test(testName = "01 Tree.toString") {
    assert(tree.toString
      == "{,0:(r,,,[1,4]),1:(m1,,0,[3]),3:(n,,1,[]),4:(x,,0,[])}")
  }

  test(testName = "02 Tree.leaves") {
    assert((tree.leaves map { n => n.nodeId })
      == List(3, 4))
  }
}
