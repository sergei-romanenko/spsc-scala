package spsc

import Algebra._
import SLL._
import Tree._

import scala.collection.immutable.TreeMap

case class Contraction(n: Name, pat: Pat) {
  override def toString: String = s"$n=$pat"
}

case class Node(nodeId: NodeId, term: Term, contr: Option[Contraction],
                parent: Option[NodeId], children: List[NodeId],
                back: Option[NodeId]) {

  override def toString: String = {
    val contr_s = if (contr.isEmpty) "" else contr.get.toString
    val parentId_s = if (parent.isEmpty) "" else parent.get.toString
    val children_s = (for (chId <- children) yield chId.toString).mkString(",")
    val back_s = if (back.isEmpty) "" else s",${back.get.toString}"
    s"$nodeId:($term,$contr_s,$parentId_s,[$children_s]$back_s)"
  }
}

case class Tree(freeId: NodeId, getNode: NodeMap) {

  override def toString: String =
    getNode.values.map(_.toString).mkString("{", ",", "}")

  def apply(nodeId: NodeId): Node = getNode(nodeId)

  def getParent(node: Node): Option[Node] =
    node.parent.map(getNode)

  def setBack(b: Node, a: Node): Tree =
    this.copy(getNode = getNode + (b.nodeId -> b.copy(back = Some(a.nodeId))))

  // Enumerating nodes.

  def nodesAcc(node: Node, acc: Stream[Node]): Stream[Node] = {
    val children = node.children.map(getNode)
    node #:: children.foldRight(acc) (nodesAcc)
  }

  def nodes: Stream[Node] =
    nodesAcc(getNode(0), Stream.empty)

  def leaves: Stream[Node] =
  //nodes.filter(_.children.isEmpty)
    getNode.values.filter(_.children.isEmpty).toStream

  def funcNodes: Stream[Node] = leaves.flatMap(_.back match {
    case None => Nil
    case Some(bId) => List(getNode(bId))
  })

  // Finding unprocessed nodes.

  def isProcessed(n: Node): Boolean = n.term match {
    case _: Var => true
    case CFG(TKind.Ctr, _, args) => args.isEmpty
    case _: CFG => n.back.isDefined
    case _: Let => true
  }

  def findAnUnprocessedNode: Option[Node] =
    leaves.find(!isProcessed(_))

  // Finding ancestors.

  def ancestors(n: Node): Stream[Node] = getParent(n) match {
    case None => Stream.empty
    case Some(p) => p #:: ancestors(p)
  }

  def findFuncAncestor(n: Node): Option[Node] =
    ancestors(n).find(m => isFGCall(m.term) && equiv(n.term, m.term))

  def localAncestors(n: Node): Stream[Node] = getParent(n) match {
    case None => Stream.empty
    case Some(p) =>
      if (aVarIsUnderAttack(p.term))
        Stream.empty
      else
        p #:: localAncestors(p)
  }

  def globalAncestors(n: Node): Stream[Node] = getParent(n) match {
    case None => Stream.empty
    case Some(p) =>
      if (aVarIsUnderAttack(p.term))
        p #:: globalAncestors(p)
      else
        globalAncestors(p)
  }

  def findAMoreGeneralAncestor(b: Node): Option[Node] =
    if (aVarIsUnderAttack(b.term))
      globalAncestors(b).find(isMoreGeneral(b))
    else
      localAncestors(b).find(isMoreGeneral(b))

  def findAnEmbeddedAncestor(b: Node): Option[Node] =
    if (aVarIsUnderAttack(b.term))
      globalAncestors(b).find(isEmbeddedAncestor(b))
    else
      localAncestors(b).find(isEmbeddedAncestor(b))

  // -- Rewriting the tree.

  // Fiding the ids of the subnodes (not including the node's id)/

  def subnodeIds(n: Node): Set[NodeId] = {
    val chIdSet = n.children.toSet
    val subIdSets = n.children.map(getNode andThen subnodeIds)
    subIdSets.foldLeft(chIdSet) (_.union(_))
  }

  def removeSubnodes(m: NodeMap, n: Node): NodeMap =
    subnodeIds(n).foldLeft(m) (_ - _)

  def replaceSubtree(n: Node, term: Term): (Tree, Node) = {
    val n1 = n.copy(term = term, children = Nil)
    val tree1 = this.copy(getNode = removeSubnodes(getNode, n) + (n1.nodeId -> n1))
    (tree1, n1)
  }

  def addChildren(n: Node, cs: List[Branch]): Tree = {
    val freeId1 = freeId + cs.length
    val chIds = freeId until freeId1
    val n1 = n.copy(children = n.children ++ chIds)
    val chNodes =
      for ((nId1, (t1, c1)) <- chIds.zip(cs)) yield {
        (nId1, Node(nId1, t1, c1, Some(n.nodeId), Nil, None))
      }
    val getNode1 = getNode + (n1.nodeId -> n1) ++ chNodes
    this.copy(freeId = freeId1, getNode = getNode1)
  }

  // This function replaces the expression in a node with
  // a let-expression, and then adds child nodes.
  // Thus, a let-node cannot be a leaf.

  def decompose(n: Node, term: Term, bs: List[(Name, Term)]): Tree = {
    val (tree1, n1) = replaceSubtree(n, Let(term, bs))
    val cs = (term, None) :: bs.map({ case (_, t) => (t, None) })
    tree1.addChildren(n1, cs)
  }

}

object Tree {

  type NodeId = Int
  type Branch = (Term, Option[Contraction])
  type NodeMap = TreeMap[NodeId, Node]

  // We distinguish a specific category of expressions:
  // the ones that generate contractions in the process tree.

  // This is used to distiguish "global" and "local" contrlol:
  // expressions are compared only if they belong
  // to the same category (as defined by `aVarIsUnderAttack`).

  def aVarIsUnderAttack: Term => Boolean = {
    case GCall(_, arg :: args) =>
      aVarIsUnderAttack(arg)
    case Var(_) => true
    case _ => false
  }

  def isFGCall: Term => Boolean = {
    case FCall(_, _) => true
    case GCall(_, _) => true
    case _ => false
  }

  def isMoreGeneral(b: Node)(a: Node): Boolean =
    isFGCall(a.term) && instOf(b.term, a.term)

  def isEmbeddedAncestor(b: Node)(a: Node): Boolean =
    isFGCall(a.term) && HE.embeddedIn(a.term, b.term)

  // Names appearing in a tree.

  def nodeNames(n: Node): Set[Name] = {
    val tns = termNames(n.term)
    n.contr match {
      case None => tns
      case Some(Contraction(v, pat)) =>
        tns + v + pat.name ++ pat.params
    }
  }

  def treeNames(tree: Tree): Set[Name] =
    tree.nodes.map(nodeNames).foldLeft(Set[Name]()) (_ ++ _)

  // Initial tree.

  def create(term: Term): Tree = {
    val node0 = Node(nodeId = 0, term = term, contr = None,
      parent = None, children = Nil, back = None)
    val getNode0 = new NodeMap() + (0 -> node0)
    new Tree(freeId = 1, getNode = getNode0)
  }

}
