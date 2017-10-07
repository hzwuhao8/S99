package s99

/**
 * Find the last but one element of a list.
 */
object P02 {
  def penultimate[A](xs: List[A]): A = xs match {
    case Nil             => throw new java.util.NoSuchElementException()
    case y :: Nil        => throw new java.util.NoSuchElementException()
    case y1 :: y2 :: Nil => y1
    case y1 :: ys  => penultimate(ys)
  }
}