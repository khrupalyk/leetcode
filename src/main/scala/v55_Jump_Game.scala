import scala.annotation.tailrec

// https://leetcode.com/problems/jump-game/
object v55_Jump_Game extends App {


  // 1. Get first number and tail
  // 2. Increase by one
  // 3. Jump by one
  // 4. Do the same for other
  // 5. If not, get back, increase by two and again and again
//  def canJump(nums: Array[Int]): Boolean = {
//    //2,1,1
//    @tailrec
//    def process(currentStep: Int, max: Int, currentIndex: Int): Boolean = {
//      if(currentStep > max) {
//        false
//      } else
//      if(currentIndex + currentStep == nums.length - 1) {
//        true
//      } else {
//        val nextNum = nums(currentIndex + currentStep)
//          val success = process(1, nextNum, currentIndex +currentStep)
//            if (success) true
//          else
//            process(currentStep + 1, max, currentIndex)
//      }
//    }
//
//    nums.length match {
//      case 0 => false
//      case 1 => true
//      case _ => process(1, nums.head, 0)
//    }
//  }

  def canJump(nums: Array[Int]): Boolean = {
    //2,1,1
    def process(currentStep: Int, max: Int, currentIndex: Int): Boolean = {
      if(currentStep + currentIndex >= nums.length - 1) {
        true
      } else
      if (currentStep == 0 || currentStep > max) {
        false
      } else {
        val nextNum = nums(currentIndex + currentStep)
        val success = process(nextNum, nextNum, currentIndex + currentStep)
        if (success) true
        else
          process(currentStep - 1, max, currentIndex)
      }
    }

    nums.length match {
      case 0 => false
      case 1 => true
      case _ => process(nums.head, nums.head, 0)
    }
  }


//  println(canJump(Array(2,0,6,9,8,4,5,0,8,9,1,2,9,6,8,8,0,6,3,1,2,2,1,2,6,5,3,1,2,2,6,4,2,4,3,0,0,0,3,8,2,4,0,1,2,0,1,4,6,5,8,0,7,9,3,4,6,6,5,8,9,3,4,3,7,0,4,9,0,9,8,4,3,0,7,7,1,9,1,9,4,9,0,1,9,5,7,7,1,5,8,2,8,2,6,8,2,2,7,5,1,7,9,6)))
  println(canJump((1 to 9999).reverse.toArray)) //false
//  println(canJump(Array(0))) //true
//  println(canJump(Array(0,1))) //false
//  println(canJump(Array(2,0,3,0,1,0)))//true
//  println(canJump(Array(1,2,3)))//true
//  println(canJump(Array(3,0,8,2,0,0,1)))//true
//  println(canJump(Array(2)))//true
//  println(canJump(Array(2,3,1,1,4))) //true
//  println(canJump(Array(3,2,1,0,4))) //false
}
