package com.leetcode

//https://leetcode.com/problems/letter-combinations-of-a-phone-number/
object v17_Letter_Combinations_of_a_Phone_Number extends App {


  def letterCombinations(digits: String): List[String] = {

    val dictionary = Map(
      '2'-> "abc",
      '3'-> "def",
      '4'-> "ghi",
      '5'-> "jkl",
      '6'-> "mno",
      '7'-> "pqrs",
      '8'-> "tuv",
      '9'-> "wxyz"
    )

    val originDig = digits.map(dictionary)

    def process(digits: List[String]): List[String] = digits match {
      case d if d.isEmpty => Nil
      case _ =>
        digits.head.flatMap { c =>
          val processed = process(digits.tail)
            if (processed.isEmpty) List(c.toString)
            else
              processed.map(c.toString + _)
        }.toList
    }

    process(originDig.toList)
  }

  println(letterCombinations("234"))
  println(letterCombinations(""))
  println(letterCombinations("23"))
}
