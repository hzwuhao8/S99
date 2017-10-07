package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import scala.util.Try

/**
 *
 */
class P03Test extends FunSuite with Checkers {
  import P03._
  test("list nth check") {
    check { (a: List[Int], index: Int) =>
      if (a.isEmpty) {
        assertThrows[java.lang.IndexOutOfBoundsException] {
          nth(a, index)
        }
        true
      } else if (index < 0) {
        assertThrows[java.lang.IndexOutOfBoundsException] {
          nth(a, index)
        }
        true
      } else if (a.size < index) {
        assertThrows[java.lang.IndexOutOfBoundsException] {
          nth(a, index)
        }
        true
      } else {
        nth(a, index) == a(index)
      }
    }
  }

}