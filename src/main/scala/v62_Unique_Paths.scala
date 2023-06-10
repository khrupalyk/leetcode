import org.scalatest.funsuite.AnyFunSuite
//https://leetcode.com/problems/unique-paths/

class v62_Unique_Paths extends AnyFunSuite {

  def uniquePaths(m: Int, n: Int): Int = {

    val table = new Array[Array[Int]](m)

    for (i <- table.indices) {
      table(i) = new Array[Int](n)
    }

    for (i <- table.indices.reverse) {
      for (j <- table(i).indices.reverse) {
        if(i == m - 1 && j == n - 1) {
          table(i)(j) = 1
        } else {
          val down = if(i + 1 >= m) 0 else table(i+1)(j)
          val right = if(j + 1 >= n) 0 else table(i)(j+1)
          table(i)(j) = down + right
        }
      }
    }
    table.head.head
  }

  test("Unique Paths") {
    assert(uniquePaths(3, 2) == 3)
    assert(uniquePaths(3, 7) == 28)
  }

}
