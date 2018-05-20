package spsc
import Algebra._
import Tree._

class ResTaskGen(val tree: Tree) {

  def initNameGen: NameGen =
    new NameGen(treeNames(tree).toSeq)

  private val ng: NameGen = initNameGen

  private val funcNodeIds : Set[NodeId] =
    tree.leaves.flatMap(n => n.back match {
      case None => Nil
      case Some(backId) => backId :: Nil
    }).toSet

  private val sigs =
    scala.collection.mutable.Map[NodeId, (String, List[Var])]()
  private val defs =
    new scala.collection.mutable.ListBuffer[Rule]

  private def walk(n: Node): Term = {
    val fa = tree.findFuncAncestor(n)
    if (fa.isEmpty) n.term match {
      case v: Var => v
      case Let(_, bs) =>
        val body = walk(tree(n.children.head))
        val ks = bs map { case (k, _) => k }
        val ts = n.children.tail.map(tree(_)).map(walk)
        applySubst(Map(ks zip ts: _*))(body)
      case Ctr(name, _) => Ctr(name, n.children.map(tree(_)).map(walk))
      case FCall(name, args) => walkCall(n, name, args)
      case GCall(name, args) => walkCall(n, name, args)
    } else {
      val q = fa.get
      val (name, args) = sigs(q.nodeId)
      if (tree(q.children.head).contr.isEmpty)
        applySubst(matchAgainst(q.term, n.term).get)(FCall(name, args))
      else
        applySubst(matchAgainst(q.term, n.term).get)(GCall(name, args))
    }
  }

  def walkCall(n: Node, name: String, args: List[Term]): Term = {
    val ns = vars(n.term)
    val vs = ns.map(Var)
    if (tree(n.children.head).contr.isDefined) {
      val (gname, _) =
        sigs.getOrElseUpdate(n.nodeId, (ng.freshName(name), vs))
      for (cn <- n.children.map(tree(_)))
          defs += GRule(gname, cn.contr.get.pat, ns.tail, walk(cn))
      GCall(gname, vs)
    } else if (funcNodeIds.contains(n.nodeId)) {
      val (fname, fargs) =
        sigs.getOrElseUpdate(n.nodeId, (ng.freshName(name), vs))
      defs += FRule(fname, fargs.map(_.name), walk(tree(n.children.head)))
      FCall(fname, vs)
    } else walk(tree(n.children.head))
  }

  def buildResTask(): Task =
    Task(walk(tree(0)), Program(defs.toList.sortWith(
      (r1, r2) => r1.name < r2.name)))
}
