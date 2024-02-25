package com.leetcode

import java.lang.Math

//https://leetcode.com/problems/decode-ways/
object v91_Decode_Ways extends App {
  def numDecodings(s: String): Int = {
    ???
  }

  def plusOne(digits: Array[Int]): Array[Int] = {
    var i = digits.length - 1
    var nextAdd = 1
    var acc = Array.from(digits)
    while (i >= 0) {
      val el = acc(i) + nextAdd
      if(el < 10) {
        acc(i) = el
        i = -1
      } else if(i == 0 && nextAdd + digits(i) > 9) {
        acc(i) = 0
        acc = 1 +: acc
        i = -1
      } else {
        acc(i) = 0
        nextAdd = 1
        i = i - 1
      }
    }
    acc
  }


  def addBinary(a: String, b: String): String = {
    val delta = a.length - b.length
    val zeros = (0 until Math.abs(delta) map (_ => '0')).mkString("")
    val (an, bn) = if(delta > 0)
      (a, zeros + b)
    else if(delta <0)
      (zeros + a, b)
    else (a, b)

    val digits = an.zip(bn)

    var i = digits.length - 1
    var nextAdd = 0
    var acc = ""
    while (i >= 0) {
      val el = (digits(i)._1 - '0') + (digits(i)._2  - '0') + nextAdd
      if (el < 2) {
        acc = el.toString + acc
        nextAdd = 0
        i = i-1
      } else if (i == 0 && el > 1) {
        if(el == 3) {
          acc = "11" + acc
        } else {
          acc = "10" + acc
        }
        i = -1
      } else {

        if (nextAdd == 1) {
          acc = "0" + acc
        } else {
          acc = nextAdd + acc
        }
        nextAdd = 1
        i = i - 1
      }
    }
    acc
  }

println(addBinary(
  "001",
  "111"))
  //1000
  println(addBinary("1010",
                    "1011"))
                  //10101
  println(addBinary("11", "01")) //100
  println(addBinary("1111", "1111")) //11110
  println(addBinary("01", "00")) //01
  println(addBinary("11", "10")) //101
  println(addBinary("0010", "1011"))
  println(addBinary("010", "1011"))
  println(addBinary("0010", "011"))
//  println("111".zip("22"))

  "1203"
  //1,20,3
  //12,3
  //1,23
}
