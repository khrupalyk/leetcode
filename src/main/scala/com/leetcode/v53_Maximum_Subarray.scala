package com.leetcode

import org.scalatest.funsuite.AnyFunSuite

//https://leetcode.com/problems/maximum-subarray/
class v53_Maximum_Subarray extends AnyFunSuite {

  def maxSubArray(nums: Array[Int]): Int = {
    var sum = nums.headOption.getOrElse(0)
    for(i <- nums.indices) {
      var j = i + 1
      var currentSum = nums(i)
      sum = if(currentSum > sum) currentSum else sum
      while(j < nums.length) {
        currentSum = currentSum + nums(j)
        sum = if(currentSum > sum) currentSum else sum
        j = j + 1
      }
    }
    sum
  }


  test("Maximum Subarray") {
    assert(maxSubArray(Array(-2,1,-3,4,-1,2,1,-5,4)) == 6)
    assert(maxSubArray(Array(5,4,-1,7,8)) == 23)
    assert(maxSubArray(Array(1)) == 1)
    assert(maxSubArray(Array(-1)) == -1)
    assert(maxSubArray(Array()) == 0)
    assert(maxSubArray(Array(-1,0,-2)) == 0)
  }

}
