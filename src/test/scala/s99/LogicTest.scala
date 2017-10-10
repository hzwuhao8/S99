package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

class LogicTest extends FunSuite with Checkers {
  import Logic._

  def f1(a: Boolean, b: Boolean): Boolean = and(a, or(a, b))
  def f2(a: Boolean, b: Boolean): Boolean = a and (a or not(b))

  test("P46 Truth tables for logical expressions.") {
    val res = table2(f1)
    debug(s"res=\n${res}")
  }

  test("P47") {
    val res = table2(f2)
    debug(s"res=\n${res}")
    assert(table2(f1) == table2(f2))
  }

  test("P48") {
    assert(gray(1) == List("0", "1"))
    assert(gray(2) == List("00", "01", "10", "11"))
    val res = gray(4)
    assert(res.size == Math.pow(2, 4))
    val g = Gen.choose(0, 1)
    val g4 = for {
      a <- g
      b <- g
      c <- g
      d <- g
    } yield {
      "" + a + b + c + d
    }
    check {
      Prop.forAll(g4) { s =>
        debug(s"s=${s}")
        res.contains(s)
      }
    }
    assert(res.contains("0100"))

  }
}