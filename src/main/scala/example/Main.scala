package example

import example.tree._
import scala.io.StdIn.readLine
import scala.io.Source

import java.io.BufferedReader
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object Main {
  def main(args: Array[String]): Unit = {
    val pt = PrefixTree[Morpheme](700000)
    doCommand(pt)
  }
  def doCommand(pt: PrefixTree[Morpheme]): Unit = {
    //val dict = "dictionary/dict.nw.500.txt"
    val dict = "dictionary/dict.txt"
    print("command > ")
    readLine.trim match {
      case "get" => {
        print("get > ")
        println(pt.get(readLine).mkString("\n"))
        doCommand(pt)
      }
      case "add" => {
        print("add(surface,token,left,right,cost) > ")
        val line = readLine.split(",")
        if (line.length != 5) {
          println("invalid value ...")
          doCommand(pt)
        } else {
          val surface = line(0).trim
          val token = line(1).trim
          val left  = try { line(2).trim.toInt } catch { case e:Exception => -1 }
          val right = try { line(3).trim.toInt } catch { case e:Exception => -1 }
          val cost  = try { line(4).trim.toInt } catch { case e:Exception => -1 }
          val npt = pt.add(Morpheme(surface, token, left, right, cost))
          doCommand(npt)
        }
      }
      case "delete" => {
        print("delete(surface,token,left,right,cost) > ")
        val line = readLine.split(",")
        if (line.length != 5) {
          println("invalid value ...")
          doCommand(pt)
        } else {
          val surface = line(0).trim
          val token = line(1).trim
          val left  = try { line(2).trim.toInt } catch { case e:Exception => -1 }
          val right = try { line(3).trim.toInt } catch { case e:Exception => -1 }
          val cost  = try { line(4).trim.toInt } catch { case e:Exception => -1 }
          val npt = pt.delete(Morpheme(surface, token, left, right, cost))
          doCommand(npt)
        }
      }
      case "build" => {
        val source = Source.fromFile(dict, "utf-8")
        val npt = source.getLines.foldLeft(pt) { (tree, i) =>
          if (i.startsWith("nw ")) {
            val line = i.substring(3).split(",")
            val surface = line(0).trim
            val token = line(1).trim
            val left  = try { line(2).trim.toInt } catch { case e:Exception => -1 }
            val right = try { line(3).trim.toInt } catch { case e:Exception => -1 }
            val cost  = try { line(4).trim.toInt } catch { case e:Exception => -1 }
            tree.add(Morpheme(surface, token.trim, left, right, cost))
          } else {
            tree
          }
        }
        doCommand(npt)
      }
      case "check" => {
        val source = Source.fromFile(dict, "utf-8")
        val notRegistered = source.getLines.foldLeft(Nil: List[Morpheme]) { (list, i) =>
          if (i.startsWith("nw ")) {
            val line = i.substring(3).split(",")
            val surface = line(0).trim
            val token = line(1).trim
            val left  = try { line(2).trim.toInt } catch { case e:Exception => -1 }
            val right = try { line(3).trim.toInt } catch { case e:Exception => -1 }
            val cost  = try { line(4).trim.toInt } catch { case e:Exception => -1 }
            val target = Morpheme(surface, token, left, right, cost)
            if (pt.get(surface).contains(target)) list else target :: list
          } else {
            list
          }
        }
        println("not registered : %d".format(notRegistered.length))
        doCommand(pt)
      }
      case "status" => {
        pt.status()
        doCommand(pt)
      }
      case "dump" => {
        pt.dump()
        doCommand(pt)
      }
      case "exit" => {
        println("bye...")
      }
      case _ => {
        println("command : get, add, delete, build, check, status, dump, exit")
        doCommand(pt)
      }
    }

  }
}