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

  def gen3(): Gen[(Int, Int)] = {
    val gen1 = Gen.choose(1, 1000)
    val gen2 = Gen.choose(1, 1000)
    val gen3 = for {
      a <- gen1
      b <- gen2
    } yield {
      (a, b)
    }
    gen3
  }
  import MathOne._
  test("P31 isPrime") {
    assert(2.isPrime()._1)
    assert(3.isPrime()._1)
    val gen1 = Gen.choose(0, 1000)
    check {
      Prop.forAll(gen1) { (x: Int) =>
        val res = x.isPrime()
        if (res._1) {
          res._2 == Seq(1, x)
        } else {
          val p = res._2.tail.tail.head
          p != 1 && p != x
          x % p == 0
        }
      }
    }
  }

  test("P32  gcd") {

    check {
      Prop.forAll(gen3) { (p: (Int, Int)) =>
        val (a, b) = p
        debug(s"p={$p}")
        gcdA(a, b) == gcd(a, b)
      }
    }
  }

  test("P33 isCoprimeTo") {
    assert(35.isCoprimeTo(64))
    assert(35.isCoprimeTo(25) == false)
    check {
      Prop.forAll(gen3) { p =>
        if (p._1.isPrime()._1) {
          p._1.isCoprimeTo(p._2)
        } else if (p._2.isPrime()._1) {
          p._1.isCoprimeTo(p._2)
        } else {
          true
        }

      }
    }
  }

  test("P34 Calculate Euler's totient function phi(m).") {
    assert(10.totient == 4)
  }

  test("P35") {
    assert(315.primeFactors == List(3, 3, 5, 7))
  }
  test("P36") {
    assert(315.primeFactorMultiplicity == List((3, 2), (5, 1), (7, 1)))
  }
  test("P37") {
    //assert(0.totient2 == 0)
    assert(10.totient2 == 4)
    check {
      Prop.forAll(Gen.choose(10, 100)) { x =>
        x.totient() == x.totient2()
      }
    }
  }

  test("P39") {
    assert(listPrimesinRange(7, 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  test("P40") {
    assert(4.goldbach contains (1, 3))
    assert(6.goldbach contains (1, 5))
    assert(28.goldbach contains (5, 23))
  }

  test("P41") {
    //pending
    val res: List[List[(Int, Int)]] = goldbachList(9, 20)
    assert(res.size == 6)
    assert(res.head contains (3, 7))
    assert(res.last contains (3, 17))
    //pending
    val res2 = goldbachListLimited(3, 2000, 50)

    assert(res2.filter(_ contains (73, 919)).size == 1)
    assert(res2.filter(_ contains (61, 1867)).size == 1)
  }
}