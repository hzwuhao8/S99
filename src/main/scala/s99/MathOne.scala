package s99

import scala.language.implicitConversions

object MathOne {
  class IntHelp(n: Int) {
    def isPrime(): (Boolean, Seq[Int]) = {
      val x = Math.abs(n)
      val max = Math.sqrt(x).toInt
      val res: Seq[(Int, Boolean)] = for (i <- 2 to max) yield {
        (i, (x % i) == 0)
      }
      val res1 = res.forall(_._2 == false)
      if (res1) {
        (true, Seq(n, 1))
      } else {
        (false, res.filter(_._2 != false).map { _._1 })
      }
    }
  }

  implicit def int2Help(n: Int): IntHelp = new IntHelp(n)

  def gcdA(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      0
    } else {
      val seq1 = a.isPrime()._2
      val seq2 = b.isPrime()._2
      (seq1.toSet).intersect(seq2.toSet).max
    }
  }

  def gcd(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) {
      0
    } else {
      if (a % b == 0) {
        b
      } else {
        gcd(b, a % b)
      }
    }
  }

}