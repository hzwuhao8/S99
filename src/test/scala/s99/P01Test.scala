package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import scala.util.Try

class P01Suit extends FunSuite with Checkers {
 import P01._
  test("list last check") {
    check { (a: List[Int]) =>
      if (a.isEmpty) {
        assertThrows[java.util.NoSuchElementException]{
          last(a)
        }
        true
      } else {
        last(a) == a.last
      }
    }
  }
}