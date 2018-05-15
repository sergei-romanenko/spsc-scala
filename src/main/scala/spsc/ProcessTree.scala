package spsc

import Algebra._

case class Contraction(n: String, pat: Pat) {
  override def toString: String =
    f"$n=${pat.toString}"
}

case class Node(nodeId: Int, term: Term, parent: Node, contr: Contraction) {

  def ancestors: List[Node] =
    if (parent == null) Nil else parent :: parent.ancestors

  def isProcessed: Boolean = term match {
    case Ctr(_, Nil) => true
    case v: Var => true
    case Let(_,_) => true
    case _ => funcAncestor != null
  }

  def funcAncestor: Node =
    ancestors.find { n => isFGCall(n.term) && equiv(term, n.term) }.orNull
}

case class Tree(freeId: Int, root: Node, children: Map[Node, List[Node]]) {

  def addChildren(n: Node, cs: List[(Term, Contraction)]): Tree = {
    var i = freeId - 1
    Tree(freeId + cs.length, root,
      children +
        (n -> (cs map { case (t, b) => i += 1; Node(i, t, n, b) })))
  }

  def replace(n: Node, term: Term): (Tree, Node) =
    if (n == root) {
      val n1 = Node(n.nodeId, term, parent = null, contr = null)
      (Tree(freeId, n1, Map().withDefaultValue(Nil)), n1)
    } else {
      val p = n.parent
      val n1 = Node(freeId, term, p, n.contr)
      val cs = children(p) map { m => if (m == n) n1 else m }
      (Tree(freeId + 1, root, children + (p -> cs)), n1)
    }

  // This function replaces the expression in a node with
  // a let-expression, and then adds child nodes.
  // Thus, a let-node cannot be a leaf.

  def decompose(n: Node, term: Term, bs: List[(String, Term)]): Tree = {
    val (tree1, n1) = replace(n, Let(term, bs))
    val cs = (term, null) :: bs.map({ case (_, t) => (t, null) })
    tree1.addChildren(n1, cs)
  }

  def leaves_(node: Node): List[Node] =
    if (children(node).isEmpty) List(node)
    else children(node).flatMap(leaves_)

  def leaves: List[Node] = leaves_(root)

  override def toString: String = {
    val acc = new StringBuilder()

    def walk(n: Node): Unit = {
      val parentId_s = if (n.parent == null) "" else n.parent.nodeId.toString
      val children_s =
        (for (child <- children(n))
          yield child.nodeId.toString).mkString(",")
      val contr_s = if (n.contr == null) "" else n.contr.toString
      val node_s = n.nodeId.toString + ":(" + n.term + "," + contr_s +
        "," + parentId_s + ",[" + children_s + "])"
      if (acc.nonEmpty) acc.append(",")
      acc.append(node_s)
      for (child <- children(n)) walk(child)
    }

    acc.append("{")
    walk(root)
    acc.append("}")
    acc.toString
  }
}

object Tree {

  def create(term: Term) =
    Tree(freeId = 1,
      root = Node(nodeId = 0, term = term, parent = null, contr = null),
      children = Map().withDefaultValue(Nil))
}

trait TreeBuilder {
  def buildProcessTree(term: Term): Tree
}
