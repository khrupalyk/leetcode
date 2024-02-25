package problems

import org.scalatest.funsuite.AnyFunSuite


class RangeTree extends AnyFunSuite {

  test("s") {
    val a = Array(1, 2, 3, 4)
    //0,  1, 2, 3, 4, 5, 6, 7
    //   10, 3, 7, 1, 2, 3, 4

    //    10
    //  3     7
    // 1 2   3 4
    val X = Array.fill(2 * 4)(-1)

    def build(curr: Int, l: Int, r: Int): Unit = {
      if (l == r) {
        X(curr) = a(l)
      } else {
        val m = (l + r) / 2
        build(2 * curr, l, m)
        build(2 * curr + 1, m + 1, r)
        X(curr) = X(2 * curr) + X(2 * curr + 1)
      }
    }

    build(1, 0, a.length - 1)

    val (l, r) = (1, 2)

    def query(curr: Int, tl: Int, tr: Int): Int = {
      if (r < tl || tr < l) {
        println(s"end0 tl: $tl, tr: $tr, curr: $curr")
        0
      } else if (l <= tl && tr <= r) {
        println(s"end tl: $tl, tr: $tr, curr: $curr")
        X(curr)
      }
      else {
        val m = (tl + tr) / 2
        println(s"tl: $tl, tr: $tr, m: $m")
        query(2 * curr, tl, m) + query(2 * curr + 1, m + 1, tr)
      }

    }

    println(query(1, 0, a.length - 1))
  }
}





object awd extends App {

  val alpha = ('a' to 'z').toArray
  class TrieNode(var isWord: Boolean, var ref: Array[Int] = Array.fill(26)(-1)) {
    def char = ref.zipWithIndex.find(_._1 != -1).map(d => alpha(d._2))
  }
  val root = new TrieNode(false)
  val Trie = collection.mutable.ArrayBuffer[TrieNode](root)
  def insert(nodeId: Int, word: String, pos: Int): Unit = {
    if(pos == word.length) {
      return ;
    }
    var k = Trie(nodeId).ref(word(pos) - 'a')
    if(k == -1) {
      k = Trie.length
      Trie(nodeId).ref(word(pos) - 'a') = k
      Trie += new TrieNode(false)
    }

    if(pos == word.length - 1) {
      Trie(k).isWord = true
    }
    insert(k, word, pos + 1)
  }

//  insert(0, "apple", 0)
  insert(0, "appmle", 0)
//  insert(0, "blo", 0)
  insert(0, "bppmle", 0)
//  insert(0, "bab", 0)
  println(root)

}