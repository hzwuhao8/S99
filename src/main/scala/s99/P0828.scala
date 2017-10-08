package s99

import scala.annotation.tailrec
import com.typesafe.scalalogging.Logger

object P0828 {
  val logger = Logger("P0828")
  
  def compress[A](xs: List[A]): List[A] = xs match {
    case Nil                          => Nil
    case y1 :: Nil                    => List(y1)

    case y1 :: y2 :: ys if (y1 == y2) => compress(y1 :: ys)
    case y1 :: y2 :: ys if (y1 != y2) => y1 :: compress(y2 :: ys)

  }

  def compress2[A](xs: List[A]): List[A] = compressRec(xs, Nil)

  @tailrec
  def compressRec[A](xs: List[A], res: List[A]): List[A] = {
    xs match {
      case Nil                          => res
      case y1 :: Nil                    => res ::: List(y1)
      case y1 :: y2 :: ys if (y1 == y2) => compressRec(y1 :: ys, res)
      case y1 :: y2 :: ys               => compressRec(y2 :: ys, res ::: List(y1))
    }
  }

  def pack[A](xs: List[A]): List[List[A]] = {
    logger.debug("\n")
    logger.debug("*"*40)
    logger.debug(s"pack xs=${xs}")
    val res = packRec(xs, (Nil, Nil))
    logger.debug(s"res=${res}")
    logger.debug("*"*40)
    logger.debug("\n")
    res._2
  }

  @tailrec
  def packRec[A](xs: List[A], res: (List[A], List[List[A]])): (List[A], List[List[A]]) = {
    logger.debug(s"xs=${xs}\tres=${res}")
    xs match {
      case Nil => res
      case y1 :: Nil => {
        val p1: List[A] = res._1
        val res2 = p1 match {
          case Nil                  => (List(y1), res._2 ::: List(List(y1)))
          case z :: zs if (z == y1) => (y1 :: p1, res._2 ::: List(y1 :: p1))
          case z :: zs if (z != y1) => (List(y1), res._2 ::: List(List(y1)))

        }
        res2
      }
      case y1 :: y2 :: ys if (y1 == y2) => {
        val p1: List[A] = y1 :: res._1
        val p2: List[List[A]] = res._2

        packRec(y2 :: ys, (p1, p2))
      }
      case y1 :: y2 :: ys if (y1 != y2) => {
        val p1: List[A] = Nil

        val p2: List[List[A]] = res._2 ::: List(y1 :: res._1)
        packRec(y2 :: ys, (p1, p2))
      }
    }
  }
}