package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class P06Test extends FunSuite with Checkers {
  import P06._
  type A = Boolean
  test("whether a list is a palindrome") {
    check { (a: List[A]) =>
      isPalindrome(a ::: a.reverse)
    }
    check { (b: List[A], x: A) =>
      isPalindrome(b ::: List(x) ::: b.reverse)
    }
    /**
     * TODO
     * b = List(false )
     * x = false
     *
     * or
     *
     * b = List(true)
     * x = true
     *
     */
    check { (b: List[A], x: A) =>
      if (b.isEmpty) {
        isPalindrome(x :: b ::: b.reverse)
      } else {
        val l1 = x :: b ::: b.reverse
        isPalindrome(l1) == (l1 == l1.reverse)
      }
    }

    check { (a: List[A], b: List[A]) =>
      if (a != b) {
        val c = a ::: b.reverse
        if (c.size == 1) {
          isPalindrome(c)
        } else {
          isPalindrome(c) == (c == c.reverse)
        }
      } else {
        isPalindrome(a ::: b.reverse)
      }
    }
  }
}