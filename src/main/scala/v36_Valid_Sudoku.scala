object v36_Valid_Sudoku extends App {




  def validateRow(row: Array[Char]) = {
    !row.filter(c => c >= '1' && c <= '9')
      .groupBy(identity)
      .exists(_._2.length > 1)
  }

  def validateBlock(board: Array[Array[Char]], block: Int) = {
    val x = block % 3
    val y = block / 3
    val row = for {
      yi <- y*3  until (y *3 + 3)
      r <- board(yi).grouped(3).toIndexedSeq(x)
    } yield {
      r
    }

    validateRow(row.toArray)
  }


  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    val isNotValidRow = board.map(validateRow).exists(!_)
    val isNotValidColumn = (for {
      col <- 1 to 9
      column = board.map(_(col - 1))
    } yield validateRow(column)).exists(!_)

    val isNotValidBlock = (0 until 9 map { i =>
      validateBlock(board, i)
    }).exists(!_)

    !isNotValidRow && !isNotValidColumn && !isNotValidBlock
  }

  val board =
    Array(
      Array("5", "3", ".",   ".", "7", ".",    ".", ".", ".").map(_.toCharArray.head),
      Array("6", ".", ".",   "1", "9", "5",    ".", ".", ".").map(_.toCharArray.head),
      Array(".", "9", "8",   ".", ".", ".",    ".", "6", ".").map(_.toCharArray.head),

      Array("8", ".", ".",   ".", "6", ".",    ".", ".", "3").map(_.toCharArray.head),
      Array("4", ".", ".",   "8", ".", "3",    ".", ".", "1").map(_.toCharArray.head),
      Array("7", ".", ".",   ".", "2", ".",    ".", ".", "6").map(_.toCharArray.head),

      Array(".", "6", ".",   ".", ".", ".",    "2", "8", ".").map(_.toCharArray.head),
      Array(".", ".", ".",   "4", "1", "9",    ".", ".", "5").map(_.toCharArray.head),
      Array(".", ".", ".",   ".", "8", ".",    ".", "7", "9").map(_.toCharArray.head)
    )

      val b2 = Array(
            Array(".",".",".",  ".","5",".",  ".","1",".").map(_.toCharArray.head),
            Array(".","4",".",  "3",".",".",  ".",".",".").map(_.toCharArray.head),
            Array(".",".",".",  ".",".","3",  ".",".","1").map(_.toCharArray.head),

            Array("8",".",".",  ".",".",".",  ".","2",".").map(_.toCharArray.head),
            Array(".",".","2",  ".","7",".",  ".",".",".").map(_.toCharArray.head),
            Array(".","1","5",  ".",".",".",  ".",".",".").map(_.toCharArray.head),

            Array(".",".",".",  ".",".","2",  ".",".",".").map(_.toCharArray.head),
            Array(".","2",".",  "9",".",".",  ".",".",".").map(_.toCharArray.head),
            Array(".",".","4",  ".",".",".",  ".",".",".").map(_.toCharArray.head))

  validateBlock(b2, 2)
  validateBlock(b2, 2)



}
