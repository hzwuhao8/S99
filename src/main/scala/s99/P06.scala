package s99

object P06 {
  def isPalindrome[A](xs: List[A]): Boolean = {
    xs == P05.reverse2(xs)
  }
}