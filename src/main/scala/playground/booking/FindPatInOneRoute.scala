package playground.booking

import org.scalatest.funsuite.AnyFunSuite

class FindPatInOneRoute extends AnyFunSuite {


  val route = Seq("a", "b", "c", "d", "e", "f", "j")
  val availability = Seq(
    ("ab", true),
    ("ac", false),
    ("ad", false),
    ("af", false),
    ("ae", false),
    ("aj", false),
    ("bc", false),
    ("bd", true),
    ("bf", false),
    ("bj", false),
    ("be", true),
    ("cd", false),

    ("de", true),
    ("df", true),
    ("dj", false),

    ("ef", false),
    ("ej", false),
    ("fj", true),
  ).toMap

  test("test") {

    assert(getPath("a", "d") == Seq("ab", "bd"))
    assert(getPath("d", "f") == Seq("df"))
    assert(getPath("c", "d") == Seq())
    assert(getPath("a", "j") == Seq("ab", "bd", "df", "fj"))

    assert(isAvailable("a", "b"))
    assert(!isAvailable("a", "c"))
    assert(isAvailable("a", "d"))

    assert(!isAvailable("b", "c"))
    assert(isAvailable("b", "d"))

    assert(!isAvailable("c", "d"))

    assert(isAvailable("d", "f"))
  }
  def isAvailable(a: String, b: String): Boolean = {

    val fromIndex = route.zipWithIndex.find(_._1 == a).get._2
    val toIndex = route.zipWithIndex.find(_._1 == b).get._2

    def check(start: Int, i: Int): Boolean = {
      val seg = route(start) + route(i)
      if (start == i) {
        false
      } else if (availability(seg)) {
        true
      } else {
        var isAvailable = false
        for (index <- start until i) {
          if (check(start, index) && check(index, i)) {
            isAvailable = true
          }
        }
        isAvailable
      }
    }

    check(fromIndex, toIndex)
  }

  def getPath(from: String, to: String): Seq[String] = {

    val fromIndex = route.zipWithIndex.find(_._1 == from).get._2
    val toIndex = route.zipWithIndex.find(_._1 == to).get._2

    def check(start: Int, i: Int): Seq[String] = {
      val seg = route(start) + route(i)
      if (start == i) {
        Nil
      } else if (availability(seg)) {
        Seq(seg)
      } else {
        var isAvailable = Seq.empty[String]
        for (index <- start until i) {
          val seg1 = check(start, index)
          val seg2 = if(seg1.nonEmpty) check(index, i) else Nil
          if (seg1.nonEmpty && seg2.nonEmpty) {
            isAvailable = seg1 ++ seg2
          }
        }
        isAvailable
      }
    }

    check(fromIndex, toIndex)
  }


}
