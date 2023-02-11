import org.scalatest.funsuite.AnyFunSuite

import java.util

class v74_Search_a_2D_Matrix extends AnyFunSuite {

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {

    def findRow(): Int = {
      val n = matrix.length
      var i = 0
      while (i < n - 1) {
        if (target >= matrix(i)(0) && target < matrix(i + 1)(0)) {
          return i
        }

        i = i + 1
      }
      matrix.length - 1
    }
    if(matrix.length > 0) {
      val row = findRow()
      java.util.Arrays.binarySearch(matrix(row), target) >= 0
    } else {
      false
    }
  }


  test("Search_a_2D_Matrix") {
    assert(!searchMatrix(Array(Array(1,3,5,7),Array(10,11,16,20),Array(23,30,34,60)), target = 13))

    assert(searchMatrix(Array(Array(1)), target = 1))

    assert(searchMatrix(Array(Array(1), Array(3)), target = 3))
  }

}
