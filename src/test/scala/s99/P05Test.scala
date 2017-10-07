package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class P05Test extends FunSuite with Checkers {
  import P05._
  test("Reverse a list") {
    check { (a: List[Int]) =>
      reverse(a) == a.reverse
      reverse2(a) == a.reverse
    }
  }

}