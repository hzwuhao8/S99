package s99

object P0828 {

  def compress[A](xs: List[A]): List[A] = xs match {
    case Nil                          => Nil
    case y1 :: Nil                    => List(y1)

    case y1 :: y2 :: ys if (y1 == y2) => compress(y1 :: ys)
    case y1 :: y2 :: ys if (y1 != y2) => y1 :: compress(y2 :: ys)

  }
  
  
}