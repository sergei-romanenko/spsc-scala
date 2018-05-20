package spsc

import java.io._
import scala.io.Source

import spsc.SLLParsers._
import spsc.SLLCheck._
import spsc.PrettyPrinter._

object Main extends App {

  def wrStr(path: String, str: String): Unit = {
    val pw = new PrintWriter(new File(path))
    try {
      pw.write(str)
    } finally {
      pw.close()
    }
  }

  try {
    if (args.length != 1) {
      println("Usage: spsc-scala taskname")
      System.exit(1)
    }
    val taskName = args.head
    val pathTask = taskName + ".task"
    val pathTree = taskName + ".tree"
    val pathRes = taskName + ".res"

    val strTask: String = Source.fromFile(pathTask).mkString
    println("* Task read from " + pathTask)
    val task: Task = parseTask(strTask)
    checkTask(task) match {
      case Some(msg) =>
        println(msg)
        System.exit(1)
      case None =>
    }
    val builder = new AdvancedTreeBuilder(task)
    val tree = builder.buildProcessTree()
    wrStr(pathTree, ppTree(tree))
    println("* Process tree written to " + pathTree)
    val rpg = new ResTaskGen(tree)
    val resTask = rpg.buildResTask()
    wrStr(pathRes, ppTask(resTask))
    println("* Output written to " + pathRes)
  } catch {
    case e: Exception =>
      println(e.getMessage)
      System.exit(1)
  }
}
