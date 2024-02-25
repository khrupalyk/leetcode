package com.leetcode

//https://leetcode.com/problems/3sum/
object v15_3Sum extends App {

  def threeSumSlow(nums: Array[Int]): List[List[Int]] = {
    val size = nums.length

    val result = for {
      i <- 0 until size
      j <- 0 until size
      k <- 0 until size
      if (i != j && i != k && j != k) && nums(i) + nums(j) + nums(k) == 0
    } yield List(nums(i), nums(j), nums(k)).sorted

    result.toList.distinct
  }

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val size = nums.length
    var i = 0

    var acc = List.empty[List[Int]]
    while (i < size) {
      var j = i + 1
      val x1 = nums(i)
      while (j < size) {
        val x2 = nums(j)
        val x3 = nums(size - i - 1)

        if((i != j && i != size - i - 1 && j != size - i - 1) && nums(i) + nums(j) + nums(size - i - 1) == 0) {
          val l: List[Int] = List(nums(i), nums(j), nums(size - i - 1))
          acc = acc.+:(l)
        }

        j = j + 1
      }

      i = i + 1
    }

   acc
  }

  def threeSum2(nums: Array[Int]): List[List[Int]] = {
    val size = nums.length

    val result: Set[List[Int]] = for {
      i <- (0 until size).toSet[Int]
      j <- i until size
      k <- j until size
      if (i != j && i != k && j != k) && nums(i) + nums(j) + nums(k) == 0
    } yield List(nums(i), nums(j), nums(k)).sorted

    result.toSet.toList
  }


  println(threeSum(Array(-1,0,1,2,-1,-4)))
//  println(threeSum(Array(0,1,1)))
}
