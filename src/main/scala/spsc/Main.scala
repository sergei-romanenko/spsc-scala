package spsc

import java.io._
import scala.io.Source
import util.control.Breaks._

import spsc.SParsers._
import spsc.PrettyPrinter._

object Main extends App {

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
      val scp = new AdvancedSupercompiler(task.prog)
      val tree = scp.buildProcessTree(task.term)
      //Right _ <- writeFile pathTree (ppTree $ tree)
      //Left ferr =>
      //  putStrLn ("Error writing file " ++ pathTree ++ ": " ++ show ferr)
      println("* Process tree written to " + pathTree)
      val rpg = new ResidualProgramGenerator(tree)
      val (resTerm, resProg) = rpg.result
      val resTask = Task(resTerm, resProg)
      val strResTask = ppTask(resTask)
      val pw = new PrintWriter(new File(pathRes))
      try {
        pw.write(strResTask)
      } finally {
        pw.close()
      }
      println("* Output written to " + pathRes)
    }
  } catch {
    case e: Exception =>
      println(e.getMessage)
      System.exit(1)
  }
}
