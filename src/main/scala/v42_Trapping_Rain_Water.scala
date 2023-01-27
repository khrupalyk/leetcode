//https://leetcode.com/problems/trapping-rain-water/

import scala.annotation.tailrec

object v42_Trapping_Rain_Water extends App {
  def trap(height: Array[Int]): Int = {
    @tailrec
    def subCalc(line: Array[Int], acc: Int): Int = {
      line match {
        case _ if line.length <= 1 => acc
        case _ =>
          val x = line.head
          val xs = line.tail
          if (x == 1) {
            val after1 = xs.takeWhile(_ == 0)
            if (after1.length == xs.length)
              acc
            else subCalc(xs.drop(after1.length), acc + after1.length)
          } else {
            subCalc(xs, acc)
          }
      }
    }
    val max = height.max

    ((1 to max) map { i =>
      val arr = height.map { t =>
        if (t >= i) 1 else 0
      }
      subCalc(arr, 0)
    }).sum
  }

  def trap2(height: Array[Int]): Int = {
    def subCalcLoop(line: Array[Int]): Int = {
      var acc = 0
      var i = 0

      while (i < line.length) {
        val x = line(i)
        if (x == 1) {
          var count0 = 0
          i = i + 1
          while (i < line.length && line(i) == 0) {
            count0 = count0 + 1
            i = i + 1
          }

//          println(s"finish: ${i + count0 }")
          if (i >= line.length) {
            i = line.length
          } else {
            acc = acc + count0
          }
        } else {
          i = i + 1
        }

      }


//      println(s"sum = ${acc} ${line.toList}")
      acc
    }

//    val max = 1
    val max = height.max
    ((1 to max) map { i =>
      val arr = height.map { t =>
        if (t >= i) 1 else 0
      }
      subCalcLoop(arr)
    }).sum
  }

  def trap3(height: Array[Int]): Int = {

    def subCalcLoop(currentHeight: Int): Int = {
      var acc = 0
      var i = 0

      while (i < height.length) {
        val x = if (height(i) >= currentHeight) 1 else 0
        if (x == 1) {
          var count0 = 0
          i = i + 1
          while (i < height.length && (if (height(i) >= currentHeight) 1 else 0) == 0) {
            count0 = count0 + 1
            i = i + 1
          }
          if (i >= height.length) {
            i = height.length
          } else {
            acc = acc + count0
          }
        } else {
          i = i + 1
        }
      }
      acc
    }

    ((1 to height.max) map(subCalcLoop)).sum
  }

  def test(array: Array[Int]): Unit = {
    println(s"trap = ${trap(array)}, trap3 = ${trap3(array)}")
  }

  test(Array(4,2,0,3,2,5))
  test(Array(0,1,0,2,1,0,1,3,2,1,2,1))
//  test(Array(4,2,0,3,2,5))
}
