package com.leetcode

import org.scalatest.funsuite.AnyFunSuite

//https://leetcode.com/problems/set-matrix-zeroes/
class v73_Set_Matrix_Zeroes extends AnyFunSuite {

  def setZeroes(matrix: Array[Array[Int]]): Unit = {

    val colSet = scala.collection.mutable.Set[Int]()

    for(i <- matrix.indices) {
      var updateRow = false
      for(j <- matrix(i).indices) {
        val x = matrix(i)(j)
        if(x == 0) {
          updateRow = true
          colSet += j
        }
      }

      if(updateRow) {
        for (j <- matrix(i).indices) {
          matrix(i)(j) = 0
        }
      }
    }

    colSet.foreach { col =>
      for(i <- matrix.indices) {
        matrix(i)(col) = 0
      }
    }
  }

  def printMat(matrix: Array[Array[Int]]) = {

    println(matrix.map(_.mkString(" ")).mkString("\n"))
  }
  test("Set Matrix Zeroes") {
    var matrics = Array(Array(1,1,1), Array(1,0,1), Array(1,1,1))

    matrics = Array(Array(0,1,2,0),Array(3,4,5,2),Array(1,3,1,5))
    printMat(matrics)
    setZeroes(matrics)
    println("\n")
    printMat(matrics)

  }
}
