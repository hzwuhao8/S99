package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class P07Test extends FunSuite with Checkers {
  import P07._
  type A = AnyVal
  test("list flatten") {
    check { (a: List[A], b: List[A], c: List[A]) =>
      flatten(List(a, b, c)) == a ::: b ::: c
    }
    check { (a: List[List[A]]) =>
      flatten(a) == a.flatten
    }
  }
}