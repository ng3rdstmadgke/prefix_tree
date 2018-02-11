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
  def parseLine(input: String): Option[(String, String, Int, Int, Int)] = {
      val line = input.split(",")
      if (line.length != 5) {
        None
      } else {
        val surface = line(0).trim
        val token = line(1).trim
        val left  = try { line(2).trim.toInt } catch { case e:Exception => -1 }
        val right = try { line(3).trim.toInt } catch { case e:Exception => -1 }
        val cost  = try { line(4).trim.toInt } catch { case e:Exception => -1 }
        Some((surface, token, left, right, cost))
      }
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
      case "search" => {
        print("search > ")
        println(pt.search(readLine).mkString("\n"))
        doCommand(pt)
      }
      case "add" => {
        print("add(surface,token,left,right,cost) > ")
        parseLine(readLine) match {
          case None =>{
             println("invalid value ...")
             doCommand(pt)
          }
          case Some((s, t, l, r, c)) => {
            doCommand(pt.add(s, Morpheme(s, t, l, r, c)))
          }
        }
      }
      case "replace" => {
        print("replace(surface,token,left,right,cost) > ")
        parseLine(readLine) match {
          case None =>{
             println("invalid value ...")
             doCommand(pt)
          }
          case Some((s, t, l, r, c)) => {
            doCommand(pt.replace(s, Morpheme(s, t, l, r, c)) { m => if (m.left == l && m.right == r) true else false })
          }
        }
      }
      case "delete" => {
        print("delete(surface,token,left,right,cost) > ")
        parseLine(readLine) match {
          case None =>{
             println("invalid value ...")
             doCommand(pt)
          }
          case Some((s, t, l, r, c)) => {
            val npt = pt.delete(s) { m => if (m.left == l && m.right == r) true else false }
            doCommand(npt)
          }
        }
      }
      case "build" => {
        val source = Source.fromFile(dict, "utf-8")
        val npt = source.getLines.foldLeft(pt) { (tree, i) =>
          if (i.startsWith("nw ")) {
            parseLine(i.substring(3)) match {
              case None => tree
              case Some((s, t, l, r, c)) => tree.add(s, Morpheme(s, t, l, r, c))
            }
          } else {
            tree
          }
        }
        source.close
        doCommand(npt)
      }
      case "check" => {
        val source = Source.fromFile(dict, "utf-8")
        val notRegistered = source.getLines.foldLeft(Nil: List[Morpheme]) { (list, i) =>
          if (i.startsWith("nw ")) {
            parseLine(i.substring(3)) match {
              case None => list
              case Some((s, t, l, r, c)) => {
                val target = Morpheme(s, t, l, r, c)
                if (pt.get(s).contains(target)) list else target :: list
              }
            }
          } else {
            list
          }
        }
        println("not registered : %d".format(notRegistered.length))
        source.close
        doCommand(pt)
      }
      case "serialize" => {
        pt.serialize(dict + ".bin")
        doCommand(pt)
      }
      case "deserialize" => {
        val npt: PrefixTree[Morpheme] = try {
          PrefixTree.deserialize(dict + ".bin")
        } catch {
          case e:Exception => println("exec build & serialize"); pt
        }
        doCommand(npt)
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
        println("command : get, add, replace, delete, build, check, serialize, deserialize, status, dump, exit")
        doCommand(pt)
      }
    }

  }
}
