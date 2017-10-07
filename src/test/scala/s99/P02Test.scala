package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import scala.util.Try

class P02Test extends FunSuite with Checkers {
  import P02._
  test("list penultimate check") {
    check { (a: List[Int]) =>
      if (a.isEmpty) {
        assertThrows[java.util.NoSuchElementException] {
          penultimate(a)
        }
        true
      } else if (a.size == 1) {
        assertThrows[java.util.NoSuchElementException] {
          penultimate(a)
        }
        true
      } else {
        penultimate(a) == a.reverse.tail.head
      }
    }
  }

}