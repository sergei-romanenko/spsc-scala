package spsc

import java.io._
import scala.io.Source
import util.control.Breaks._

import spsc.SLLParsers._
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
    breakable {
      if (args.length != 1) {
        println("Usage: spsc-scala taskname")
        break
      }
      val taskName = args.head
      val pathTask = taskName + ".task"
      val pathTree = taskName + ".tree"
      val pathRes = taskName + ".res"

      var strTask: String = Source.fromFile(pathTask).mkString
      println("* Task read from " + pathTask)
      val task: Task = parseTask(strTask)
      val goal = task.term
      val prog = task.prog
      //Nothing = checkTask (MkTask e prog)
      //Just msg => putStrLn msg
      val builder = new AdvancedTreeBuilder(task.prog)
      val tree = builder.buildProcessTree(task.term)
      wrStr(pathTree, ppTree(tree))
      println("* Process tree written to " + pathTree)
      val rpg = new ResidualProgramGenerator(tree)
      val resTask = rpg.result
      wrStr(pathRes, ppTask(resTask))
      println("* Output written to " + pathRes)
    }
  } catch {
    case e: Exception =>
      println(e.getMessage)
      System.exit(1)
  }
}
