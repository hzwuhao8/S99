package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class P06Test extends FunSuite with Checkers {
  import P06._
  type A = AnyVal
  test("whether a list is a palindrome") {
    check { (a: List[A]) =>
      isPalindrome(a ::: a.reverse)
    }
    check { (b: List[A], x: A) =>
      isPalindrome(b ::: List(x) ::: b.reverse)
    }
    check { (b: List[A], x: A) =>
      if (b.isEmpty) {
        isPalindrome(x :: b ::: b.reverse)
      } else {
        isPalindrome(x :: b ::: b.reverse) == false
      }
    }

    check { (a: List[A], b: List[A]) =>
      if (a != b) {
        isPalindrome(a ::: b.reverse) == false
      } else {
        isPalindrome(a ::: b.reverse)
      }
    }
  }

}