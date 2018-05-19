package spsc

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._

object PrettyPrinter {

  // Pretty printing tasks.

  def docProgram(prog: Program): Doc =
    stack(prog.rules.map(Doc.str))

  def docTask(task: Task): Doc =
    Doc.str(task.term) / Doc.text(str = "where") + Doc.line + Doc.line +
      docProgram(task.prog) + Doc.line

  def ppTask(task: Task): String =
    docTask(task).render(width = 80)

  // Pretty printing trees.

  def docTree(tree: Tree): Doc =
    docNode(tree, tree(0)) + Doc.line

  def docNode(tree: Tree, node: Node): Doc = {
    docContr(node.contr) +
      (if (node.parent.isEmpty) Doc.empty else Doc.line) +
      Doc.str(node.nodeId) + Doc.text(str = " : ") + Doc.str(node.term) +
      docBack(tree, node) +
      docChildren(tree, node).nested(amount = 4)
  }

  def docBack(tree: Tree, node: Node): Doc = {
    node.back match {
      case None => Doc.empty
      case Some(bId) =>
        Doc.line + Doc.text(str = "--> ") + Doc.str(bId)
    }
  }

  def docContr(oc: Option[Contraction]): Doc = oc match {
    case None =>
      Doc.empty
    case Some(c) =>
      Doc.line + Doc.text(str = "{") +
        Doc.text(c.n) + Doc.text(str = " = ") + Doc.str(c.pat) +
        Doc.text(str = "}")
  }

  def docChildren(tree: Tree, node: Node): Doc = {
    val ns = node.children.map(tree(_))
    if (ns.isEmpty)
      Doc.empty
    else {
      Doc.line +
        Doc.intercalate(Doc.line, for (n <- ns) yield docNode(tree, n))
    }
  }

  def ppTree(tree: Tree): String =
    docTree(tree).render(width = 80)

}
