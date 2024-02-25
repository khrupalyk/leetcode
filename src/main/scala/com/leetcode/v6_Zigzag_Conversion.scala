package com.leetcode

import org.scalatest.funsuite.AnyFunSuite

//https://leetcode.com/problems/zigzag-conversion/
class v6_Zigzag_Conversion extends AnyFunSuite {

  def convert(str: String, numRows: Int): String = {
    var result = ""
    if(numRows == 1) return str
    0 until numRows foreach { row =>
      var i = row
      var rowResult = ""
      while (i < str.length) {
        val nextIdx = (i - 1) + (numRows  - row) + Math.abs(row - (numRows - 1))

        val x_next = if (nextIdx >= str.length || row == 0 || row == numRows - 1) "" else str(nextIdx).toString
        rowResult = rowResult + str(i) + x_next
        i = i + numRows + numRows - 2
      }
      result = result + rowResult
    }
    result
  }

  test("Zigzag Conversion") {
    val str = "PAYPALISHIRING"
    val numRows = 3

    assert(convert(str, numRows) == "PAHNAPLSIIGYIR")
    assert(convert(str, numRows = 4) == "PINALSIGYAHRPI")
    assert(convert(str = "A", numRows = 1) == "A")
  }
}
