package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class P04Test extends FunSuite with Checkers {
  import P04._
  test("list length check") {
    check { (a: List[Int]) =>
      length(a) == a.size
      length2(a) == a.size
    }
  }

}