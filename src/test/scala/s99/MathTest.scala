package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

/**
 * P31 - P41
 */

class MathTest extends FunSuite with Checkers {
  import MathOne._
  test("P31 isPrime") {
    assert(2.isPrime()._1)
    assert(3.isPrime()._1)
    val gen1 = Gen.choose(0, 1000)
    check {
      Prop.forAll(gen1) { (x: Int) =>
        val res = x.isPrime()
        if (res._1) {
          res._2 == Seq(1,x)
        } else {
          val p = res._2.tail.tail.head 
          p != 1 && p != x 
          x % p == 0
        }
      }
    }
  }

  test("P32  gcd") {
    val gen1 = Gen.choose(0, 1000)
    val gen2 = Gen.choose(0, 1000)
    val gen3 = for {
      a <- gen1
      b <- gen2
    } yield {
      (a, b)
    }

    check {
      Prop.forAll(gen3) { (p: (Int, Int)) =>
        val (a, b) = p
        debug(s"p={$p}")
        gcdA(a, b) == gcd(a, b)
      }
    }
  }
}