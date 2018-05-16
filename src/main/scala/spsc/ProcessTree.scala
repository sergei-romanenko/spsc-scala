package spsc

import Algebra._
import spsc.Tree.Branch

case class Contraction(n: String, pat: Pat) {
  override def toString: String = f"$n=${pat.toString}"
}

case class Node(nodeId: Int, term: Term,
                parent: Option[Node], contr: Option[Contraction]) {

  def ancestors: List[Node] = parent match {
    case None => Nil
    case Some(q) => q :: q.ancestors
  }

  def isProcessed: Boolean = term match {
    case Ctr(_, Nil) => true
    case v: Var => true
    case Let(_, _) => true
    case _ => funcAncestor.isDefined
  }

  def funcAncestor: Option[Node] =
    ancestors.find { n => isFGCall(n.term) && equiv(term, n.term) }
}

case class Tree(freeId: Int, root: Node, children: Map[Node, List[Node]]) {

  def addChildren(n: Node, cs: List[Branch]): Tree = {
    var i = freeId - 1
    Tree(freeId + cs.length, root,
      children +
        (n -> (cs map { case (t, b) => i += 1; Node(i, t, Some(n), b) })))
  }

  def replace(n: Node, term: Term): (Tree, Node) =
    if (n == root) {
      val n1 = Node(n.nodeId, term, parent = None, contr = None)
      (Tree(freeId, n1, Map().withDefaultValue(Nil)), n1)
    } else {
      val q = n.parent.get
      val n1 = Node(freeId, term, n.parent, n.contr)
      val cs = children(q) map { m => if (m == n) n1 else m }
      (Tree(freeId + 1, root, children + (q -> cs)), n1)
    }

  // This function replaces the expression in a node with
  // a let-expression, and then adds child nodes.
  // Thus, a let-node cannot be a leaf.

  def decompose(n: Node, term: Term, bs: List[(String, Term)]): Tree = {
    val (tree1, n1) = replace(n, Let(term, bs))
    val cs = (term, None) :: bs.map({ case (_, t) => (t, None) })
    tree1.addChildren(n1, cs)
  }

  def leaves_(node: Node): List[Node] =
    if (children(node).isEmpty) List(node)
    else children(node).flatMap(leaves_)

  def leaves: List[Node] = leaves_(root)

  override def toString: String = {
    val acc = new StringBuilder()

    def walk(n: Node): Unit = {
      val parentId_s =
        if (n.parent == null) "null"
        else if (n.parent.isEmpty) ""
        else n.parent.get.nodeId.toString
      val children_s =
        (for (child <- children(n))
          yield child.nodeId.toString).mkString(",")
      val contr_s =
        if (n.contr == null) "null"
        else if (n.contr.isEmpty) ""
        else n.contr.get.toString
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

  type Branch = (Term, Option[Contraction])

  def create(term: Term) =
    Tree(freeId = 1,
      root = Node(nodeId = 0, term = term, parent = None, contr = None),
      children = Map().withDefaultValue(Nil))
}

trait TreeBuilder {
  def buildProcessTree(term: Term): Tree
}
