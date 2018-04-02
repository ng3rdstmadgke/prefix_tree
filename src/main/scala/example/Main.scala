package example

import example.tree._
import scala.io.StdIn.readLine
import scala.io.Source

import java.io.BufferedReader
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

class Timer {
  private var millis: Long = System.currentTimeMillis();
  def start(): Unit = {
    millis = System.currentTimeMillis();
  }
  def message(): String = {
    val end = System.currentTimeMillis();
    "Time: %d ms".format(end - millis);
  }
}

object Main {
  val timer = new Timer()
  def main(args: Array[String]): Unit = {
    val pt = DoubleArray[Morpheme](700000)
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
  def doCommand(pt: DoubleArray[Morpheme]): Unit = {
    //val dict = "dictionary/dict.nw.500.txt"
    val dict = "dictionary/dict.txt"
    print("command > ")
    readLine.trim match {
      case "get" => {
        print("get > ")
        println(pt.get(readLine).mkString("\n"))
        doCommand(pt)
      }
      case "search1" => {
        print("search > ")
        val line = readLine;
        (0 until line.length).foreach { j => printf(s"${j} : ");println(pt.search(line.substring(j)).toSeq) }
        doCommand(pt)
      }
      case "search2" => {
        print("search > ")
        val line = readLine;
        (0 until line.length).foreach { j => printf(s"${j} : ");println(pt.search2(line.substring(j)).toSeq) }
        doCommand(pt)
      }
      case "search1m" => {
        print("search > ")
        val line = readLine;
        timer.start();
        (1 to 499999).foreach { i =>
          (0 until line.length).foreach { j => pt.search(line.substring(j)).map(_._1).mkString("/") }
        }
        (0 until line.length).foreach { j => printf(s"${j} : ");println(pt.search(line.substring(j)).map(_._1).mkString("/")) }
        println(timer.message());
        doCommand(pt)
      }
      case "search2m" => {
        print("search > ")
        val line = readLine;
        timer.start();
        (1 to 499999).foreach { i =>
          (0 until line.length).foreach { j => pt.search2(line.substring(j)).map(_._1).mkString("/") }
        }
        (0 until line.length).foreach { j => printf(s"${j} : ");println(pt.search2(line.substring(j)).map(_._1).mkString("/")) }
        println(timer.message());
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
            val elem = Morpheme(s, t, l, r, c);
            val npt = pt.delete(s, elem) { (d1, d2) => d1.left == d2.left && d1.right == d2.right };
            doCommand(npt)
          }
        }
      }
      case "build" => {
        timer.start()
        val source = Source.fromFile(dict, "utf-8")
        val npt = source.getLines.
          filter{ line =>
            line.startsWith("nw ")
          }.map { line =>
            parseLine(line.substring(3))
          }.foldLeft(pt) { (tree, i) =>
            i match {
              case Some((s, t, l, r, c)) => tree.add(s, Morpheme(s, t, l, r, c))
              case _ => tree
            }
          }
        source.close
        println(timer.message)
        doCommand(npt)
      }
      case "check" => {
        timer.start()
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
        println(timer.message)
        doCommand(pt)
      }
      case "serialize" => {
        timer.start()
        pt.serialize(dict + ".bin")
        println(timer.message)
        doCommand(pt)
      }
      case "deserialize" => {
        timer.start()
        val npt: DoubleArray[Morpheme] = try {
          DoubleArray.deserialize(dict + ".bin")
        } catch {
          case e:Exception => println("exec build & serialize"); pt
        }
        println(timer.message)
        doCommand(npt)
      }
      case "isEmpty" => {
        println(pt.isEmpty())
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
        println("command : get, add, search, search_text, replace, delete, build, check, serialize, deserialize, status, dump, isEmpty, exit")
        doCommand(pt)
      }
    }

  }
}
