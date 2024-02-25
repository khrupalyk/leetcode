package com.leetcode

//https://leetcode.com/problems/find-the-index-of-the-first-occurrence-in-a-string/
object v28_Find_the_Index_of_the_First_Occurrence_in_a_String extends App {

  def strStr(haystack: String, needle: String): Int = {
    val notFound = -1
    if(needle.length > haystack.length)
      return notFound
    var i = 0
    while (i < haystack.length) {
      var found = 0
      var j = 0
      var k = i
      while (j < needle.length && k < haystack.length) {
        if(haystack(k) == needle(j)) {
          found = found + 1
        } else {
          return notFound
        }
        k = k + 1
        j = j + 1
      }
      if(found == needle.length) {
        return i
      }
      i = i + 1
    }
    notFound
  }

  println(strStr("sadbutsad", "sad"))
  println(strStr("leetcode", "leeto"))
  println(strStr("leetcode", "ode"))
  println(strStr("aaa", "aaaa"))
  println(strStr("mississippi", "issipi"))

}
