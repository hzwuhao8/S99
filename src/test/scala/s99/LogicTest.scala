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
}