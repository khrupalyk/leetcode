package com.leetcode

//https://leetcode.com/problems/combination-sum/
object v39_Combination_Sum extends App {

  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {

    def permuteUnique(candidates: List[Int]): List[List[Int]] = candidates match {
      case n if n.isEmpty => List(List())
      case n if n.length == 1 => List(List(n.head))
      case n =>
        n.zipWithIndex.flatMap { case (i, idx) =>

          permuteUnique(n.take(idx) ++ n.drop(idx + 1))
            .map(i :: _)
        }
        n.zipWithIndex.flatMap { case (i, idx) =>
          permuteUnique(n.take(idx) ++ n.drop(idx + 1))
            .map(i :: _)
        }
    }

    permuteUnique(candidates.toList)
  }

//  Array().per

  println(combinationSum(Array(1,2,3), 2))
//  println(Array(2,3).toList.permutations.toList)

}
