import org.scalatest.funsuite.AnyFunSuite

//https://leetcode.com/problems/n-queens/
class v51_N_Queens extends AnyFunSuite {

  def totalNQueens(n: Int): Int = {

    def getTable() = {
      val arr = (0 until n).map(_ => '.').toArray
      val boards = (0 until n).map { j =>
        arr(j) = 'Q'
        val board = arr.mkString
        arr(j) = '.'
        board
      }
      boards.permutations.toList
    }

    getTable().count(a => validate(n, a.toArray))
  }

  def validate(n: Int, board: Array[String]): Boolean = {
    for(i <- 0 until n) {
      val isRowValid = 1 >= board(i).count(_ == 'Q')
      if(!isRowValid) return false
      var colCount = 0
      for (j <- 0 until n) {
        if(board(j)(i) == 'Q')
          colCount = colCount + 1

        if(board(i)(j) == 'Q') {
          for(k <- i+1 until n) {
            val x = if(j + (k-i) >= n) '.' else board(k)(j + (k-i))
            if(x == 'Q') return false

            val x2 = if(j + (i - k) < 0) '.' else board(k)(j + (i - k))
            if (x2 == 'Q')
              return false
          }
        }
      }
      if(colCount > 1) return false
    }
    true
  }
  test("neg") {
    val list = Array(
      "Q...",
      "..Q.",
      "...Q",
      ".Q..")

    assert(!validate(4, list))
  }

  test("Queens") {

//        Q . . .,
    //    . . . Q,
    //    . Q . .,
    //    . . Q .

    //    .Q..
    //    ...Q
    //    Q...
    //    ..Q.

    //    ...Q
    //    Q...
    //    .Q..
    //    ..Q.

    println(solveNQueens(4))
////      [[".Q..","...Q","Q...","..Q."],["..Q.","Q...","...Q",".Q.."]]
//    println(solveNQueens(1))
//
    val list = Array(
        ".Q..",
        "...Q",
        "Q...",
        "..Q.")

    assert(validate(4, list))

    val listInvalid1 = Array(
      "Q...",
      "...Q",
      "Q...",
      "..Q.")
    assert(!validate(4, listInvalid1))

    val listInvalid2 = Array(
      ".Q.Q",
      "...Q",
      "Q...",
      "..Q.")
    assert(!validate(4, listInvalid2))

    val invalid3 = Array(
      ".Q..",
      "..Q.",
      "Q...",
      "..Q.")
    assert(!validate(4, invalid3))

    val invalid4 = Array(
      "Q...",
      "..Q.",
      "Q...",
      "...Q")
    assert(!validate(4, invalid4))

    val invalid5 = Array(
      "Q...",
      "..Q.",
      ".Q..",
      "....")
    assert(!validate(4, invalid5))


    ///    ...Q
    //    Q...
    //    .Q..
    //    ..Q.
    val invalid6 = Array(
      "...Q",
      "Q...",
      ".Q..",
      "..Q.")
    assert(!validate(4, invalid6))

    val invalid7 = Array(
      "Q...",
      "..Q.",
      "...Q",
      ".Q..")

    assert(!validate(4, invalid7))

  }

}
