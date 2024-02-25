package com.leetcode

object v19_RemoveNthNodeFromEndofList extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def removeNthFromEnd(head: ListNode, n2: Int): ListNode = {
    def sizeOfTheTree(node: ListNode): Int = node match {
      case _ if node.next == null => 1
      case _ => 1 + sizeOfTheTree(node.next)
    }

    val n = sizeOfTheTree(head) - n2

    def remove(node: ListNode, idx: Int): ListNode = node match {
      case _ if idx+1 == n && node.next != null=>
        new ListNode(node.x, node.next.next)
      case _ if idx+1 == n && node.next == null =>
        node.next = null
        node
      case _ if idx == n && node.next == null =>
        null
      case _ if idx == n && node.next != null =>
        node.next
      case e =>
        new ListNode(e.x, remove(e.next, idx + 1))
    }
    remove(head, 0 )

  }

  def print(node: ListNode): Unit = {
    def _print(node: ListNode): String = {
      if (node == null) {
        "[]"
      }else
      if(node.next == null) {
        node.x.toString
      } else {
        node.x.toString + ", " + _print(node.next)
      }
    }
    println(_print(node))
  }

  print(removeNthFromEnd(new ListNode(1, null), 1))
  print(removeNthFromEnd(new ListNode(1, new ListNode(2, null)), 1))
  print(removeNthFromEnd(new ListNode(1, new ListNode(2, null)), 2))
  print(removeNthFromEnd(new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, new ListNode(5, null))))), 2))

}
