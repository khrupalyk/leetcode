package com.leetcode

import org.scalatest.funsuite.AnyFunSuite

//https://leetcode.com/problems/longest-increasing-subsequence/
class v30_Longest_Increasing_Subsequence  extends AnyFunSuite {

  def lengthOfLIS(nums: Array[Int]): Int = {
    val sum = new Array[Int](nums.length)
    for(k <- nums.indices) {
      sum(k) = 1
      for (i <- 0 until k) {
        if(nums(i) < nums(k)) {
          sum(k) = Math.max(sum(k), sum(i)+1)
        }
      }
    }
    sum.max
  }

  test("Longest Increasing Subsequence") {
    assert(lengthOfLIS(Array(10,9,2,5,3,7,101,18)) == 4)
    assert(lengthOfLIS(Array(0,1,0,3,2,3)) == 4)
    assert(lengthOfLIS(Array(7,7,7,7,7,7,7)) == 1)
  }

}
