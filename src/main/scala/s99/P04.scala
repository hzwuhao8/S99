package s99

import scala.annotation.tailrec

/**
 *  Find the number of elements of a list.
 */
object P04 {

  def length[A](xs: List[A]): Int = xs match {
    case Nil     => 0
    case y :: ys => 1 + length(ys)
  }

  def length2[A](xs: List[A]): Int = lengthRec(xs, 0)

  @tailrec
  def lengthRec[A](xs: List[A], size: Int): Int = xs match {
    case Nil     => size
    case y :: ys => lengthRec(ys, size + 1)
  }
}