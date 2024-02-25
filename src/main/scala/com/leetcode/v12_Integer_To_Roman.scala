package com.leetcode

object v12_Integer_To_Roman extends App {

  val symbols = List(
    1 -> "I",
    4 -> "IV",
    5 -> "V",
    9 -> "IX",
    10 -> "X",
    40 -> "XL",
    50 -> "L",
    90 -> "XC",
    100 -> "C",
    400 -> "CD",
    500 -> "D",
    900 -> "CM",
    1000 -> "M"
  ).sortBy(_._1)(Ordering.Int.reverse)

  def intToRoman(num: Int): String = {

    var n = num
    var s = ""


    symbols.foreach {
      case (value, symbol) =>

        while (n >= value) {
          s = s + symbol
          n = n - value
        }
    }
    s
  }

  println(intToRoman(3))
  println(intToRoman(58))
  println(intToRoman(1994))
}
