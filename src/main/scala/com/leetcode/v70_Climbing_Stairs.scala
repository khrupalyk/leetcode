package com.leetcode

import org.scalatest.funsuite.AnyFunSuite

class v70_Climbing_Stairs extends AnyFunSuite {

  def climbStairs(n: Int): Int = {
    val dp = new Array[Int](n+1)

    dp(n) = 1
    dp(n - 1) = 1

    for(i <- (0 until n-1).reverse) {
      dp(i) = dp(i+1) + dp(i+2)
    }

    dp(0)
  }


  test("Climbing Stairs") {
    assert(climbStairs(5) == 8)
    assert(climbStairs(2) == 2)
    assert(climbStairs(3) == 3)
  }

}
