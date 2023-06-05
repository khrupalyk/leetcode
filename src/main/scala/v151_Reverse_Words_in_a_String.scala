import org.scalatest.funsuite.AnyFunSuite

//https://leetcode.com/problems/reverse-words-in-a-string/
class v151_Reverse_Words_in_a_String extends AnyFunSuite {
  def reverseWords(s: String): String = {
    s.split(" ")
      .reverse
      .filter(_.trim.nonEmpty)
      .mkString(" ")
  }

  test("reverse-words-in-a-string") {
    assert(reverseWords("the sky is blue") == "blue is sky the")
    assert(reverseWords("  hello world  ") == "world hello")
    assert(reverseWords("a good   example") == "example good a")
  }
}
