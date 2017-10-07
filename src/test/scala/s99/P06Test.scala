package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class P06Test extends FunSuite with Checkers {
  import P06._
  test("whether a list is a palindrome") {
    check { (a: List[Int]) =>
      isPalindrome(a ::: a.reverse)
    }
    check { (b: List[Int], x: Int) =>
      isPalindrome(b ::: List(x) ::: b.reverse)
    }
    check { (b: List[Int], x: Int) =>
      if (b.isEmpty) {
        isPalindrome(x :: b ::: b.reverse)
      } else {
        isPalindrome(x :: b ::: b.reverse) == false
      }
    }

    check { (a: List[Int], b: List[Int]) =>
      if (a != b) {
        isPalindrome(a ::: b.reverse) == false
      } else {
        isPalindrome(a ::: b.reverse)
      }
    }
  }

}