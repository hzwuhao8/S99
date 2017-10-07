package s99

import scala.annotation.tailrec

/**
 * Find the last but one element of a list.
 */
object P02 {
  @tailrec
  def penultimate[A](xs: List[A]): A = xs match {
    case Nil             => throw new java.util.NoSuchElementException()
   // case y :: Nil        => throw new java.util.NoSuchElementException()
    case y1 :: y2 :: Nil => y1
    case y1 :: ys  => penultimate(ys)
  }
}