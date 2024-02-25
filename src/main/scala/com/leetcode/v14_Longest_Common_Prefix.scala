package com.leetcode

object v14_Longest_Common_Prefix extends App {
  def longestCommonPrefix(strs: Array[String]): String = {
    val sorted = strs.sortBy(_.length)
    val shortestLine = sorted.headOption

    shortestLine match {
      case Some(short) =>
        short.zipWithIndex.takeWhile {  case (char, idx) =>
          sorted.tail.forall(s => s(idx) == char)
        }.map(_._1).mkString("")
      case None => ""
    }
  }

  println(longestCommonPrefix(Array("flower","flow","flight")))
  println(longestCommonPrefix(Array("dog","racecar","car")))
  println(longestCommonPrefix(Array("cir","car")))
}
