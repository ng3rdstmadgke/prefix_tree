package example.tree

import scala.annotation.tailrec
import scala.collection.mutable.ArraySeq
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.nio.file.Paths;
import java.nio.file.Files;

case class Morpheme(surface: String, token: String, left: Int, right: Int, cost: Int)
case class Node[A](index: Int, base: Int, check: Int, data: List[A], charCode: Int)
@SerialVersionUID(123L)
case class PrefixTree[A](private val base:  Array[Int],
                                         private val check: Array[Int],
                                         private val data:  ArraySeq[List[A]]) {
  val size = base.length

  // key にマッピングされているデータの List を取得する
  def get(key: String): List[A] = {
    PrefixTree._get(key.toList, 1, base, check) match {
      case -1 => Nil
      case idx => data(idx) match {
        case null => Nil
        case xs => xs
      }
    }
  }

  // subject と前方一致するデータを取得しListで返す
  def search(subject: String): List[(String, List[A])] = {
    @tailrec
    def _search(subjectList: List[Char], index: Int, key: String, ret: List[(String, List[A])]): List[(String, List[A])] = subjectList match {
      case Nil => ret
      case x :: xs => (base(index) + x.toInt) match {
        case nextIndex if check(nextIndex) != index => ret
        case nextIndex => data(nextIndex) match {
          case (null | Nil) => _search(xs, nextIndex, key + x, ret)
          case nextData     => {
            val k = key + x
            _search(xs, nextIndex, k, (k, nextData) :: ret)
          }
        }
      }
    }
    _search(subject.toList, 1, "", Nil).reverse
  }

  // 要素を削除する
  // f は引数が削除すべき要素かどうかを判定する関数
  def delete(key: String)(f: A => Boolean): PrefixTree[A] = {
    // target を削除した List を返す
    @tailrec
    def _delete(charList: List[A], rest: List[A]): List[A] = charList match {
      case Nil => Nil
      case x :: xs  => if (f(x)) {
        rest.reverse ::: xs
      } else {
        _delete(xs, x :: rest)
      }
    }
    PrefixTree._get(key.toList, 1, base, check) match {
      // 遷移に失敗した場合は変更を加えずにObjectを返す
      case -1  => this
      case idx => data(idx) match {
        // 削除対象要素が見つからなかった場合は変更を加えずにObjectを返す
        case (null | Nil) => this
        case xs           => {
          data(idx) = _delete(xs, Nil)
          new PrefixTree[A](base, check, data)
        }
      }
    }
  }

  // 要素を追加する(同じ要素が存在する場合は置換)
  // base, check, data は都度コピーを作らない(処理速度的な問題)
  // f は引数が置換対象要素かどうかを判定する関数
  def replace(key: String, elem: A)(f: A => Boolean): PrefixTree[A] = {
    @tailrec
    def _replace(charList: List[A], rest: List[A]): List[A] = charList match {
      case Nil => List(elem)
      case x :: xs  => if (f(x)) {
        rest.reverse ::: (elem :: xs)
      } else {
        _replace(xs, x :: rest)
      }
    }
    PrefixTree._get(key.toList, 1, base, check) match {
      case -1 => add(key, elem)
      case idx => {
        if (data(idx) == null || data(idx) == Nil) {
          data(idx) = List(elem)
        } else {
          data(idx) = _replace(data(idx), Nil)
        }
        new PrefixTree[A](base, check, data)
      }
    }
  }

  // 要素を追加する(同じ要素が存在する場合は登録しない)
  // base, check, data は都度コピーを作らない(処理速度的な問題)
  def add(key: String, elem: A): PrefixTree[A] = {
    getTransitionFailedStatus(key, elem) match {
      // 形態素がすでに登録されていた場合
      case None               => new PrefixTree[A](base, check, data)
      // 遷移が完了し後はListに追加するだけの場合
      case Some((index, Nil)) => {
        val newData = if (data(index) == null) List(elem) else elem :: data(index)
        data(index) = newData
        new PrefixTree[A](base, check, data)
      }
      // 途中で遷移に失敗した場合
      case Some((index, charList)) => PrefixTree._add[A](elem, index, charList, base, check, data) match {
        case (eBase, eCheck, eData) => new PrefixTree[A](eBase, eCheck, eData)
      }
    }
  }

  // key を探索し、遷移失敗時の index と残りの key を List[Char] で返す
  // 最後まで遷移に成功した場合はNoneを返す
  private def getTransitionFailedStatus(key: String, elem: A): Option[(Int, List[Char])] =  {
    @tailrec
    def go(index: Int, charList: List[Char]): Option[(Int, List[Char])] = charList match {
      case Nil     => if (data(index) == null || !data(index).contains(elem)) Some((index, Nil)) else None
      case x :: xs => (base(index) + x.toInt) match {
        case i if (i < base.length && check(i) == index) => go(i, xs)
        case _ => Some((index, charList))
      }
    }
    go(1, key.toList)
  }

  def dump(): Unit = {
    val outIdx = (1 until size).foldLeft(Nil: List[Int]) { (list, i) => if (base(i) != 0 || check(i) != 0 || data(i) != null) i :: list else list }
    println(outIdx.foldLeft("") { (out, i) => "%5d, ".format(i) + out})
    println(outIdx.foldLeft("") { (out, i) => "%5d, ".format(base(i)) + out})
    println(outIdx.foldLeft("") { (out, i) => "%5d, ".format(check(i)) + out})
    println(outIdx.foldLeft("") { (out, i) => if (data(i) != null) "    #, " + out else "     , " + out})
  }

  def status(): Unit = {
    println("array length : %d".format(size))
    println("data num : %d".format(data.count(_ != null)))
  }

  def serialize(file: String): Unit = {
    // ファイルが存在しない場合は作成、存在する場合は既存の内容を削除して上書き
    val oos = new ObjectOutputStream(Files.newOutputStream(Paths.get(file)))
    oos.writeObject(this)
    oos.close()
  }
}

object PrefixTree {
  val CharMax = 65535
  def apply[A](): PrefixTree[A] = PrefixTree[A](700000)
  def apply[A](size: Int): PrefixTree[A] = {
    val base = new Array[Int](size)
    base(1) = 1
    new PrefixTree[A](base, new Array[Int](size), new ArraySeq[List[A]](size))
  }

  def deserialize[A](file: String): PrefixTree[A] = {
    val ois = new ObjectInputStream(Files.newInputStream(Paths.get(file)))
    val ret = ois.readObject().asInstanceOf[PrefixTree[A]]
    ois.close()
    ret
  }

  // charList の文字で遷移し、遷移が終わった時点のindexを返す
  // 途中で遷移できなくなった場合 -1 を返す
  @tailrec
  def _get(charList: List[Char], index: Int, base: Array[Int], check: Array[Int]): Int = charList match {
    case Nil     => index
    case x :: xs => (base(index) + x.toInt) match {
      case i if (i < base.length && check(i) == index) => _get(xs, i, base, check)
      case _ => -1
    }
  }

  // PrefixTree にデータを追加する
  private def _add[A](elem: A, index: Int, charList: List[Char], base: Array[Int], check: Array[Int], data: ArraySeq[List[A]])
  : (Array[Int], Array[Int], ArraySeq[List[A]]) = {
    @tailrec
    def go(currIndex: Int, charList: List[Char], base: Array[Int], check: Array[Int], data: ArraySeq[List[A]])
    : (Int, Array[Int], Array[Int], ArraySeq[List[A]]) = charList match {
      case Nil => (currIndex, base, check, data)
      case x :: xs => {
        val currCharCode = x.toInt
        val nextIndex = base(currIndex) + currCharCode
        if (nextIndex >= base.length) {     // 遷移予定ノードの index が 配列のサイズよりも大きい場合
          // base, check, data を拡張した配列にコピー
          val (eBase, eCheck, eData) = extendArray(base, check, data)
          go(currIndex, charList, eBase, eCheck, eData)
        } else if (check(nextIndex) == 0) { // 遷移予定ノードが空の場合
          check(nextIndex) = currIndex
          base(nextIndex) = 1
          go(nextIndex, xs, base, check, data)
        } else {                            // 遷移予定ノードにすでに使用されている場合
          // 1. currIndexから遷移している全てのノードを取得する
          val nextNodes: List[Node[A]] = PrefixTree.getNextNodes(currIndex, base, check, data)
          // 2. 追加対象ノードと1.で求めたノードの全てが移動可能な base 値を求め、 currIndex の base 値を更新する
          // 計算途中で配列の拡張が必要になるため、計算後の配列も返す
          val (newBase, eBase, eCheck, eData) = PrefixTree.findNewBase(new Node[A](-1, -1, -1, null, currCharCode) :: nextNodes, 1, base, check, data)
          eBase(currIndex) = newBase
          // 3. 2.で求めた base 値で追加対象ノードの index を求め base 値と check 値を更新
          val nextIndex = newBase + currCharCode
          eCheck(nextIndex) = currIndex
          eBase(nextIndex)  = 1
          // 4. 1.で求めたノードを2.で求めた base 値で計算した遷移先にコピー
          nextNodes.foreach { node =>
            val dst = newBase + node.charCode
            eBase(dst)  = node.base
            eCheck(dst) = node.check
            eData(dst)  = node.data
          }
          // 5. 1.で求めたノードから更に遷移しているノードの check 値をコピー後の index に更新
          PrefixTree.relocateAfterNextNode(nextNodes, newBase, eBase, eCheck, eData)
          // 6. 1.で求めたノードのbase, check, dataをリセット
          nextNodes.foreach { node =>
            eBase(node.index)  = 0
            eCheck(node.index) = 0
            eData(node.index)  = null
          }
          go(nextIndex, xs, eBase, eCheck, eData)
        }
      }
    }
    val (dataIndex, eBase, eCheck, eData) = go(index, charList, base, check, data)
    val newData = if (eData(dataIndex) == null) List(elem) else elem :: eData(dataIndex)
    eData(dataIndex) = newData
    (eBase, eCheck, eData)
  }

  // currIndex の遷移先ノードを List で返す
  // check 値が currIndex と等しいノードが遷移先
  private def getNextNodes[A](currIndex: Int, base: Array[Int], check: Array[Int], data: ArraySeq[List[A]]): List[Node[A]] = {
    val end = if (base(currIndex) + PrefixTree.CharMax < base.length) base(currIndex) + PrefixTree.CharMax else base.length - 1
    @tailrec
    def _getNextNodes(ptr: Int, ret: List[Node[A]]): List[Node[A]] = {
      if (ptr > end) ret
      else check(ptr) match {
          case `currIndex` => _getNextNodes(ptr + 1, new Node[A](ptr, base(ptr), check(ptr), data(ptr), ptr - base(currIndex)) :: ret)
          case _           => _getNextNodes(ptr + 1, ret)
        }
    }
    _getNextNodes(base(currIndex), Nil)
  }

  // 遷移先ノードとcurrCharCodeが遷移可能なbase値を求める
  private def findNewBase[A](nextNodes: List[Node[A]], newBase: Int, base: Array[Int], check: Array[Int], data: ArraySeq[List[A]])
  : (Int, Array[Int], Array[Int], ArraySeq[List[A]]) = {
    @tailrec
    def go(tmpNodes: List[Node[A]], newBase: Int, base: Array[Int], check: Array[Int], data: ArraySeq[List[A]])
    : (Int, Array[Int], Array[Int], ArraySeq[List[A]]) = tmpNodes match {
      case Nil        => (newBase, base, check, data)
      case next :: xs => {
        if (newBase + next.charCode >= base.length) {
          // base, check, data を拡張した配列にコピー
          val (eBase, eCheck, eData) = extendArray(base, check, data)
          go(tmpNodes, newBase, eBase, eCheck, eData)
        } else if (check(newBase + next.charCode) == 0) {
          // 未使用のノードであれば配置可能。次のノードを調べる
          go(xs, newBase, base, check, data)
        } else {
          // 使用されているノードであれば base に 1 足して最初から調べ直し
          go(nextNodes, newBase + 1, base, check, data)
        }
      }
    }
    go(nextNodes, newBase, base, check, data)
  }

  // base, check, data を 1.25倍した配列にコピーしてそのコピーを返す
  private def extendArray[A](base: Array[Int], check: Array[Int], data: ArraySeq[List[A]])
  : (Array[Int], Array[Int], ArraySeq[List[A]]) = {
    val size = Math.floor(base.length * 1.25).toInt
    val baseCopy  = new Array[Int](size)
    val checkCopy = new Array[Int](size)
    val dataCopy  = data.clone
    base.copyToArray(baseCopy)
    check.copyToArray(checkCopy)
    (baseCopy, checkCopy, dataCopy)
  }

  // 遷移先ノードから更に遷移しているノードの check 値を更新された index で更新
  private def relocateAfterNextNode[A](nodes: List[Node[A]], newBase: Int, base: Array[Int], 
                                       check: Array[Int]   , data: ArraySeq[List[A]]): Unit = {
    nodes.foreach { next =>
      getNextNodes(next.index, base, check, data).foreach { afterNext => check(afterNext.index) = newBase + next.charCode }
    }
  }
}

