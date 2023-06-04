import org.scalatest.funsuite.AnyFunSuite

class v135_Candy extends AnyFunSuite {
  def candy(ratings: Array[Int]): Int = {

    val candies = new Array[Int](ratings.length)

    for(i <- candies.indices) {
      candies(i) = 1
    }

    var finished = false

    while (!finished) {
      finished = true
      for (i <- candies.indices) {
        if (i != ratings.length - 1 && ratings(i) > ratings(i + 1) && candies(i) <= candies(i + 1)) {
          candies(i) = candies(i + 1) + 1
          finished = false
        }

        if (i > 0 && ratings(i) > ratings(i - 1) && candies(i) <= candies(i - 1)) {
          candies(i) = candies(i - 1) + 1
          finished = false
        }
      }
    }

    candies.sum
  }

  test("1") {
    assert(candy(Array(1,0,2)) == 5)
    assert(candy(Array(1,2,2)) == 4)
  }
}
