//https://leetcode.com/problems/first-missing-positive/description/
object v41_First_Missing_Positive extends App {


  def firstMissingPositive(nums: Array[Int]): Int = {
    val sorted = nums.sorted
    var minValue = 1
    var i = 0

    while (i < nums.length) {
      val x = sorted(i)
      if(x < 1) {
        i = i + 1
      } else if(minValue == x) {
        minValue = minValue + 1
        i = i + 1
      } else {
        i = i + 1
      }
    }
    minValue
  }

  def test(arr: Array[Int], expected: Int) = {
    println(s"miValue = ${firstMissingPositive(arr.sorted)}, expected = $expected")
  }

  test(Array(2,1), expected = 3)
  test(Array(7,8,9,11,12), expected = 1)
  test(Array(3,4,-1,1), expected = 2)
  test(Array(1,2,0), expected = 3)
  test(Array(1,2,3), expected = 4)
  test(Array(3,4,5), expected = 1)
  test(Array(1,4,5), expected = 2)
  test(Array(-1, 1,4,5), expected = 2)
  test(Array(-1, 2,4,5), expected = 1)
}
