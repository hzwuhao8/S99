package s99

object P0828 {

  def compress[A](xs: List[A]): List[A] = xs match {
    case Nil                          => Nil
    case y1 :: Nil                    => List(y1)

    case y1 :: y2 :: ys if (y1 == y2) => compress(y1 :: ys)
    case y1 :: y2 :: ys if (y1 != y2) => y1 :: compress(y2 :: ys)

  }

  def compress2[A](xs: List[A]): List[A] = compressRec(xs, Nil)

  @scala.annotation.tailrec
  def compressRec[A](xs: List[A], res: List[A]): List[A] = {
    xs match {
      case Nil                          => res
      case y1 :: Nil                    => res ::: List(y1)
      case y1 :: y2 :: ys if (y1 == y2) => compressRec(y1 :: ys, res)
      case y1 :: y2 :: ys               => compressRec(y2 :: ys, res ::: List(y1))
    }
  }
}