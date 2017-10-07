package s99

import scala.annotation.tailrec

object P03 {
  @tailrec
  def nth[A](xs: List[A], index: Int): A = (xs, index) match {
    case (Nil, _)        => throw new java.lang.IndexOutOfBoundsException(index.toString)
    case (_, i) if i < 0 => throw new java.lang.IndexOutOfBoundsException(index.toString)
    case (y :: ys, 0)    => y
    case (y :: ys, i)    => nth(ys, i - 1)

  }
}