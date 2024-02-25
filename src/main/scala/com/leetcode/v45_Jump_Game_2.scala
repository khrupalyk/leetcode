package com.leetcode

import org.scalatest.funsuite.AnyFunSuite

class v45_Jump_Game_2 extends AnyFunSuite {

  def jump(nums: Array[Int]): Int = {

    val dp = new Array[Int](nums.length)

    for (i <- dp.indices) {
      dp(i) = Int.MaxValue - 1
    }
    dp(0) = 0
    for (i <- nums.indices) {
      for (k <- 1 to nums(i)) {
        if (i + k < nums.length)
          dp(i + k) = Math.min(dp(i) + 1, dp(i + k))
      }
    }
    dp(dp.length - 1)
  }

  test("Jump") {
    assert(jump(Array(2,3,1,1,4)) == 2)
    assert(jump(Array(2,3,0,1,4)) == 2)
  }

}
