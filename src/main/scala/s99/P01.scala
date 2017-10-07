package s99

/**
 * P01 (*) Find the last element of a list.
 */
object P01 {
  def last[A](xs: List[A]): A = xs match {
    case Nil      => throw new java.util.NoSuchElementException()
    case y :: Nil => y 
    case y :: ys  => last(ys)
  }
}