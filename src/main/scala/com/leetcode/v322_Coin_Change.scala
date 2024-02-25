package com.leetcode

import org.scalatest.funsuite.AnyFunSuite

//https://leetcode.com/problems/coin-change/description/
class v322_Coin_Change extends AnyFunSuite {

  def coinChange(coins: Array[Int], n: Int): Int = {
    val arr = new Array[Int](n + 1)
    arr(0) = 0
    for (x <- 1 to n) {
      arr(x) = Int.MaxValue - 1
      for (c <- coins) {
        if (x - c >= 0 && (arr(x - c) + 1) < arr(x)) {
          arr(x) = arr(x - c) + 1
        }
      }
    }
    if(arr(n) == Int.MaxValue-1) -1 else arr(n)
  }

  test("Coin Change") {
    assert(coinChange(Array(1,2,5), 11) == 3)
    assert(coinChange(Array(2), 3) == -1)
    assert(coinChange(Array(1), 0) == 0)
  }

}
