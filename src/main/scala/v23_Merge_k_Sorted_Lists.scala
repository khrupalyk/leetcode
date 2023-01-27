object v23_Merge_k_Sorted_Lists extends App {

  def print(node: ListNode): Unit = {
    def _print(node: ListNode): String = {
      if (node == null) {
        "]"
      } else if (node.next == null) {
        node.x.toString + "]"
      } else {
        node.x.toString + ", " + _print(node.next)
      }
    }

    println("[" + _print(node))
  }

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def mergeLists(a: ListNode, b: ListNode): ListNode = (a, b) match {
    case (n1, n2) if n1 == null => n2
    case (n1, n2) if n2 == null => n1

    case (n1, n2) if n1.x >= n2.x => new ListNode(n2.x, mergeLists(n1, n2.next))
    case (n1, n2) if n2.x >= n1.x => new ListNode(n1.x, mergeLists(n1.next, n2))

  }

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    if(lists.length > 1) {
      lists.reduce((a, b) => mergeLists(a, b))
    } else {
      lists.headOption.orNull
    }
  }

  val l1 = new ListNode(1, new ListNode(4, new ListNode(5, null)))
  val l2 = new ListNode(1, new ListNode(3, new ListNode(4)))
  val l3 = new ListNode(2, new ListNode(6))


print(mergeLists(l1, l2))
print(mergeKLists(Array(l1, l2, l3)))
print(mergeKLists(Array()))
print(mergeKLists(Array(null)))

}
