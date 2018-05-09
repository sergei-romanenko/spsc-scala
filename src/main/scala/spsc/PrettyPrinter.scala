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
    docNode(tree, tree.root) + Doc.line

  def docNode(tree: Tree, node: Node): Doc = {
    docContr(node.contr) +
      (if (node.nodeId == 0) Doc.empty else Doc.line) +
      Doc.str(node.nodeId) + Doc.text(str = " : ") + Doc.str(node.expr) +
      docBack(tree, node) +
      docChildren(tree, node).nested(amount = 4)
  }

  def docBack(tree: Tree, node: Node): Doc = {
    val fn = node.funcAncestor
    if (fn == null) Doc.empty else {
      Doc.line + Doc.text(str = "--> ") + Doc.str(fn.nodeId)
    }
  }

  def docContr(contr: Contraction): Doc =
    if (contr == null)
      Doc.empty
    else {
      Doc.line + Doc.text(str = "{") +
        Doc.str(contr.v) + Doc.text(str = " = ") + Doc.str(contr.pat) +
        Doc.text(str = "}")
    }

  def docChildren(tree: Tree, node: Node): Doc = {
    val ns = tree.children(node)
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
