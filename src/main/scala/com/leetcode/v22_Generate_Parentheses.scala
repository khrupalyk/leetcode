package com.leetcode

//https://leetcode.com/problems/generate-parentheses/
object v22_Generate_Parentheses extends App {
  def generateParenthesis(n: Int): List[String] = {
    def isValid(str: String): Boolean = {
      var idx = 0
      str.forall { p =>
        if(p == '(')
           idx = idx + 1
        else if (p == ')')
          idx = idx - 1

        idx != -1
      } && idx == 0
    }

    val open = 1 to n map (_ => "(")
    val closed = 1 to n map (_ => ")")
    (open ++ closed).flatten.permutations.collect {
      case idx if isValid(idx.mkString("")) => idx.mkString("")
    }.toList
  }

  println(generateParenthesis(1))
  println(generateParenthesis(2))
  println(generateParenthesis(3))
}
