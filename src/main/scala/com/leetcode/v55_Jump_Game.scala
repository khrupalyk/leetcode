package com.leetcode

import scala.annotation.tailrec

// https://leetcode.com/problems/jump-game/
object v55_Jump_Game extends App {
  def canJump(nums: Array[Int]): Boolean = {
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
    dp(dp.length - 1) != Int.MaxValue - 1
  }

  println(canJump(Array(2,3,1,1,4)))
  println(canJump(Array(3,2,1,0,4)))
}
