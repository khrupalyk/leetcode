package com.leetcode

import org.scalatest.funsuite.AnyFunSuite
//https://leetcode.com/problems/triangle/
class v120_Triangle extends AnyFunSuite {

  def minimumTotal(triangle: List[List[Int]]): Int = {

    val sums = new Array[Int](triangle.length)

    for (i <- sums.indices) {
      sums(i) = triangle(sums.length - 1)(i)
    }


    var i = triangle.length - 2

    while (i >= 0) {
      var j = 0
      while (j < triangle(i).length) {
        sums(j) = triangle(i)(j) + Math.min(sums(j), sums(j + 1))
        j = j + 1
      }
      i = i - 1
    }

    sums(0)
  }

  test("Triangle") {
    assert(minimumTotal(List(List(2), List(3, 4), List(6, 5, 7),List(4, 1, 8, 3))) == 11)
    assert(minimumTotal(List(List(-11))) == -11)
  }
}
