//https://leetcode.com/problems/insert-interval/

import org.scalatest.funsuite.AnyFunSuite

class v57_Insert_Interval extends AnyFunSuite {

  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    if(intervals.isEmpty) return Array(newInterval)

    var i = 0

    val newStart = newInterval.head
    val newEnd = newInterval.last

    var resArr = Array.empty[Array[Int]]

    while (i < intervals.length) {

      val (start, end) = (intervals(i).head, intervals(i).last)
      var startN = if( i < intervals.length - 1) {
        intervals(i + 1).head
      } else  {
        Int.MaxValue
      }

      if ((newStart >= start && newStart <= end) ||
        (newEnd >= start && newEnd <= end) ||
        (newStart <= start && newEnd >= end)
      ) {
        val startOverlap = if(newStart <= end && newStart >= start) start else newStart
        var endOverlap = if(newEnd <= end) end else newEnd

        var j = i
        while (j < intervals.length) {
          val (startO, endO) = (intervals(j).head, intervals(j).last)
          if(endOverlap < startO) {
            resArr = (intervals.take(i) :+ Array(startOverlap, endOverlap)) :++ intervals.drop(j)
            return resArr
          } else if(endOverlap == startO) {
            endOverlap = if(endO > endOverlap) endO else endOverlap
          } else if(endO > endOverlap) {
            endOverlap = endO
          }
          j = j + 1
        }

        resArr = (intervals.take(i) :+ Array(startOverlap, endOverlap)) :++ intervals.drop(j)
        i = intervals.length

      } else
      if(newEnd < start) {
        resArr = (intervals.take(i) :+ newInterval) :++ intervals.drop(i)
        i = intervals.length
      } else if (end < newStart && newEnd < startN) {
        if(i == intervals.length - 1) {
          resArr = intervals :+ newInterval
        } else {
          resArr = (intervals.take(i+1) :+ newInterval) :++ intervals.drop(i+1)
        }
        i = intervals.length
      }

      i = i + 1
    }

    resArr
  }

  test("57. Insert Interval") {
    def a(x: Int, y: Int) = Array(x,y)

    assert(insert(Array(a(3,4), a(8,9)), a(11,12)).map(_.toList).toList == Array(a(3,4), a(8,9), a(11,12)).map(_.toList).toList)

    assert(insert(Array(a(3,4), a(8,9), a(12,13)), a(5,6)).map(_.toList).toList == Array(a(3,4), a(5,6), a(8,9), a(12,13)).map(_.toList).toList)

    assert(insert(Array(a(3,4), a(8,9)), a(1,2)).map(_.toList).toList == Array(a(1,2), a(3,4), a(8,9)).map(_.toList).toList)

    assert(insert(Array(a(1,3), a(6,9)), a(2,5)).map(_.toList).toList == Array(a(1,5), a(6,9)).map(_.toList).toList)

    assert(insert(Array(a(1,3), a(6,9), a(10,12)), a(2,6)).map(_.toList).toList == Array(a(1,9), a(10,12)).map(_.toList).toList)

    assert(insert(Array(a(1,3), a(6,9), a(10,12)), a(2,12)).map(_.toList).toList == Array(a(1,12)).map(_.toList).toList)

    assert(insert(Array(a(1,3), a(6,9), a(10,12)), a(-1,11)).map(_.toList).toList == Array(a(-1,12)).map(_.toList).toList)

    assert(insert(Array(a(1,2), a(3,5), a(6,7), a(8, 10), a(12, 16)), a(4,8)).map(_.toList).toList == Array(a(1,2), a(3, 10), a(12, 16)).map(_.toList).toList)

    assert(insert(Array(), a(5,7)).map(_.toList).toList == Array(a(5, 7)).map(_.toList).toList)
  }

}
