package com.leetcode

import org.scalatest.funsuite.AnyFunSuite

//https://leetcode.com/problems/surrounded-regions/
class v130_Surrounded_Regions extends AnyFunSuite  {


  def solve(board: Array[Array[Char]]): Unit = {
    val n = board.length
    def check(i: Int, j: Int): Boolean = {
      if(i == 0 || j == board(i).length - 1 || j == 0 || i == n - 1) {
        false
      } else if(board(i)(j) == 'O') {
        val checkX = board(i).contains('X')
        val checkY = 0 until n exists  { k =>
          board(k)(j) == 'X'
        }
        checkX && checkY
      } else false
    }

    for (i <- 0 until n) {
      for (j <- board(i).indices) {
        if(check(i, j))
          board(i)(j) = 'X'
      }
    }
  }

  test("") {
    var array = Array(
      Array('X','X','X','X'),
      Array('X','O','O','X'),
      Array('X','X','O','X'),
      Array('X','O','X','X')
    )

    solve(array)

    println(array.map(_.toList).toList)


    array = Array(
      Array('X')
    )

    solve(array)

    println(array.map(_.toList).toList)


    array =  Array(
      Array('X','O','X','O','X','O'),
      Array('O','X','O','X','O','X'),
      Array('X','O','X','O','X','O'),
      Array('O','X','O','X','O','X')
    )


    //[["X","O","X","O","X","O"],
    // ["O","X","X","X","X","X"],
    // ["X","X","X","X","X","O"],
    // ["O","X","O","X","O","X"]]


//
//    println(check(2, 3))

    array = Array(
      Array('O','X','X','O','X'),
      Array('X','O','O','X','O'),
      Array('X','O','X','O','X'),
      Array('O','X','O','O','O'),
      Array('X','X','O','X','O')
    )
    //[["O","X","X","O","X"],
    // ["X","X","X","X","O"],
    // ["X","X","X","O","X"],
    // ["O","X","O","O","O"],
    // ["X","X","O","X","O"]]

    solve(array)

    println(array.map(_.mkString(" ")).mkString("\n"))
  }

}
