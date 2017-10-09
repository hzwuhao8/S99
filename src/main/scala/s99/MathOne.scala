package s99

import scala.language.implicitConversions

object MathOne extends Log {
  class IntHelp(n: Int) {
    require(n >= 0)
    def isPrime(): (Boolean, Seq[Int]) = {

      val res: Seq[(Int, Boolean)] = for (i <- 1 to n) yield {
        (i, (n % i) == 0)
      }
      val res1 = res.filter(_._2 != false)
      if (res1.size == 2) {
        (true, res1.map { _._1 })
      } else {
        (false, res1.map { _._1 })
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