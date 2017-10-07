package s99

/**
 * Flatten a nested list structure.
 */
object P07 {
  
  def flatten[A](xs: List[List[A]]): List[A] = xs match {
    case Nil => Nil
    case y :: ys => y ::: flatten(ys)
  }
  
}