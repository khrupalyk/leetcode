package com.leetcode

object v48_Rotate_Image extends App {
  def rotate(matrix: Array[Array[Int]]): Array[Array[Int]] = {

    val n = matrix.length

    for(i <- 0 until n / 2) {
      for (j <- i until n - i - 1) {
        val x = matrix(i)(j)
        val x2 = matrix(n-j-1)(i)
        val x3 = matrix(n - i - 1)(n - j - 1)
        val x4 = matrix(j)(n - i - 1)
        matrix(i)(j) = matrix(n-j-1)(i)
        matrix(n-j-1)(i) = matrix(n - i - 1)(n - j - 1)
        matrix(n-i-1)(n-j-1) = matrix(j)(n - i - 1)
        matrix(j)(n - i - 1) = x
      }
    }
    matrix
  }

  val arr: Array[Array[Int]] = Array(Array(1,2,3),Array(4,5,60),Array(7,8,9))


  //[[7,4,1],[8,5,2],[9,6,3]]
//    println(rotate(arr).map(_.mkString("Array(", ", ", ")")).mkString("Array(", ", ", ")"))
    println(rotate(Array(
      Array(5 ,1 ,9 ,11),
      Array(2 ,4 ,8 ,10),
      Array(13,3 ,6 ,7),
      Array(15,14,12,16))).map(_.mkString("Array(", ", ", ")")).mkString("Array(", ", ", ")"))


}
