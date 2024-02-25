package com.leetcode

//https://leetcode.com/problems/group-anagrams/
object v49_Group_Anagrams extends App {
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    val anagramsMap = scala.collection.mutable.Map.empty[String, List[String]]

    strs.foreach { x =>
      anagramsMap.addOne(x.sorted -> (x :: anagramsMap.getOrElse(x.sorted, Nil)))
    }
    anagramsMap.values.toList
  }

  println(groupAnagrams(Array("eat","tea","tan","ate","nat","bat")))
  println(groupAnagrams(Array("a")))
  println(groupAnagrams(Array()))
  println(groupAnagrams(Array("")))
  println(groupAnagrams(Array("","")))
//  println(groupAnagrams(Array("eat","tea","tan","ate","nat","bat")))
}
