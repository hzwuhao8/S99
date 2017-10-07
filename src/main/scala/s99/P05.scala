package s99

object P05 {
  def reverse[A](xs: List[A]): List[A] = xs match {
    case Nil     => Nil
    case y :: ys => reverse(ys) ::: List(y)
  }

  def reverse2[A](xs: List[A]): List[A] = {

    reverseRec(xs, Nil)
  }

  @scala.annotation.tailrec
  def reverseRec[A](xs: List[A], res: List[A]): List[A] = xs match {
    case Nil => res
    case y :: ys =>

      reverseRec(ys, y :: res)
  }
}