package s99

import scala.annotation.tailrec
import com.typesafe.scalalogging.Logger

object P0828 {
  val logger = Logger("P0828")
  def debug(arg: String)(implicit line: sourcecode.Line, name: sourcecode.FullName) = {
    logger.debug(s"${name.value}:${line.value} ${arg}")
  }

  def compress[A](xs: List[A]): List[A] = xs match {
    case Nil                          => Nil
    case y1 :: Nil                    => List(y1)

    case y1 :: y2 :: ys if (y1 == y2) => compress(y1 :: ys)
    case y1 :: y2 :: ys if (y1 != y2) => y1 :: compress(y2 :: ys)

  }

  def compress2[A](xs: List[A]): List[A] = compressRec(xs, Nil)

  @tailrec
  private def compressRec[A](xs: List[A], res: List[A]): List[A] = {
    xs match {
      case Nil                          => res
      case y1 :: Nil                    => res ::: List(y1)
      case y1 :: y2 :: ys if (y1 == y2) => compressRec(y1 :: ys, res)
      case y1 :: y2 :: ys               => compressRec(y2 :: ys, res ::: List(y1))
    }
  }

  def pack[A](xs: List[A]): List[List[A]] = {
    val size = 60
    //debug("*" * size)
    val res = packRec(xs, (Nil, Nil))
    //debug(s"res=${res}")
    //debug("*" * size + "\n")

    res._2
  }

  @tailrec
  private def packRec[A](xs: List[A], res: (List[A], List[List[A]])): (List[A], List[List[A]]) = {
    //debug(s"xs=${xs}\tres=${res}")
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

  def encode[A](xs: List[A]): List[(Int, A)] = {
    pack(xs).map { (x: List[A]) => (x.size, x.head) }
  }

  def encodeModified[A](xs: List[A]): List[Any] = {
    pack(xs).map { (x: List[A]) =>
      x match {
        case Nil      => Nil
        case y :: Nil => y
        case _        => (x.size, x.head)
      }
    }
  }

  def decode[A](xs: List[(Int, A)]): List[A] = {
    decodeRec(xs, Nil)
  }

  @tailrec
  private def decodeRec[A](xs: List[(Int, A)], res: List[A]): List[A] = {
    xs match {
      case Nil     => res

      case y :: ys => decodeRec(ys, res ::: (1 to y._1).map { x => y._2 }.toList)

    }
  }

  def encodeDirect[A](xs: List[A]): List[(Int, A)] = {
    val res = encodeDirectRec(xs, (Nil, Nil))
    res._2
  }

  @tailrec
  private def encodeDirectRec[A](xs: List[A], res: (List[A], List[(Int, A)])): (List[A], List[(Int, A)]) = {
    //debug(s"xs=${xs}\tres=${res}")
    xs match {
      case Nil => res
      case y1 :: Nil => {
        val p1: List[A] = res._1
        val res2 = p1 match {
          case Nil => (List(y1), res._2 ::: List((1, y1)))
          case z :: zs if (z == y1) => {
            res._2 match {
              case Nil     => (y1 :: p1, List((1, y1)))
              case List(a) => (y1 :: p1, List((a._1 + 1, a._2)))
              case _       => (y1 :: p1, res._2.init ::: List((res._2.last._1 + 1, res._2.last._2)))
            }

          }
          case z :: zs if (z != y1) => (List(y1), res._2 ::: List((1, y1)))
        }
        res2
      }
      case y1 :: y2 :: ys if (y1 == y2) => {
        val p1: List[A] = y1 :: res._1
        val p2: List[(Int, A)] = res._2 match {
          case Nil     => List((1, y1))
          case List(a) => if (a._2 == y1) List((a._1 + 1, a._2)) else res._2 ::: List((1, y1))
          case _       => if (res._2.last._2 == y1) res._2.init ::: List((res._2.last._1 + 1, res._2.last._2)) else res._2 ::: List((1, y1))
        }
        encodeDirectRec(y2 :: ys, (p1, p2))
      }
      case y1 :: y2 :: ys if (y1 != y2) => {
        val p1: List[A] = res._1 match {
          case Nil     => List(y1)
          case a :: as => if (a == y1) y1 :: res._1 else List(y1)
        }
        val p2: List[(Int, A)] = res._2 match {
          case Nil     => List((1, y1))
          case List(a) => if (a._2 == y1) List((a._1 + 1, a._2)) else res._2 ::: List((1, y1))
          case _       => if (res._2.last._2 == y1) res._2.init ::: List((res._2.last._1 + 1, res._2.last._2)) else res._2 ::: List((1, y1))
        }

        encodeDirectRec(y2 :: ys, (p1, p2))
      }
    }
  }

  def duplicate[A](xs: List[A]): List[A] = {
    xs match {
      case Nil     => Nil
      case a :: as => List(a, a) ::: duplicate(as)
    }
  }

  @tailrec
  def duplicateRec[A](xs: List[A], res: List[A]): List[A] = {
    xs match {
      case Nil     => res.reverse
      case a :: as => duplicateRec(as, a :: a :: res)
    }
  }

  def duplicateN[A](n: Int, xs: List[A]): List[A] = {
    if (n <= 1) {
      xs
    } else {
      xs match {
        case Nil     => Nil
        case a :: as => List.fill(n)(a) ::: duplicateN(n, as)
      }
    }
  }

  @tailrec
  def duplicateNRec[A](n: Int, xs: List[A], res: List[A]): List[A] = {
    if (n <= 1) {
      xs
    } else {
      xs match {
        case Nil     => res.reverse
        case a :: as => duplicateNRec(n, as, List.fill(n)(a) ::: res)
      }
    }
  }

  def drop[A](n: Int, xs: List[A]): List[A] = {
    def dropR(c: Int, xs: List[A]): List[A] = {
      //debug(s"n=${n}\txs=${xs}")
      (c, xs) match {

        case (_, Nil)     => Nil
        case (1, a :: as) => dropR(n, as)
        case (c, a :: as) => a :: dropR(c - 1, as)
      }
    }

    dropR(n, xs)
  }

  def dropRec[A](n: Int, xs: List[A]): List[A] = {
    @tailrec
    def dropR(c: Int, xs: List[A], res: List[A]): List[A] = {
      //debug(s"n=${n}\txs=${xs}")
      (c, xs) match {

        case (_, Nil)     => res.reverse
        case (1, a :: as) => dropR(n, as, res)
        case (c, a :: as) => dropR(c - 1, as, a :: res)
      }
    }

    dropR(n, xs, Nil)
  }

  def split[A](n: Int, xs: List[A]): (List[A], List[A]) = {
    (n, xs) match {
      case (_, Nil)           => (Nil, Nil)
      case (x, _) if (x <= 0) => (Nil, xs)
      case (x, a :: as) =>
        val r = split(x - 1, as)
        (a :: r._1, r._2)
    }
  }

  @tailrec
  def splitRec[A](n: Int, xs: List[A], res: (List[A], List[A])): (List[A], List[A]) = {
    (n, xs) match {
      case (_, Nil)           => (res._1.reverse, res._2.reverse)
      case (x, _) if (x <= 0) => (res._1, xs ::: res._2)
      case (x, a :: as)       => splitRec(x - 1, as, (a :: res._1, res._2))
    }
  }

  def slice[A](from: Int, until: Int, xs: List[A]): List[A] = {
    val from2 = if(from < 0) 0 else from
    if (from2 >= until) {
      Nil
    } else {
      xs match {
        case Nil                  => Nil
        case a :: as if from2 <= 0 => a :: slice(0, until - 1, as)
        case a :: as              => slice(from2 - 1, until - 1, as)
      }

    }
  }
  
  @tailrec
  def sliceRec[A](from: Int, until: Int, xs: List[A] , res: List[A]): List[A] = {
    val from2 = if(from < 0) 0 else from
    if (from2 >= until) {
      res.reverse
    } else {
      xs match {
        case Nil                  => res.reverse
        case a :: as if from2 <=0 => sliceRec(0, until - 1, as,a :: res)
        case a :: as              => sliceRec(from2 - 1, until-1, as,res)
      }

    }
  }
  
  def rotate[A](n: Int, xs :List[A]): List[A] = {
      val bound = if(xs.isEmpty) 0  else n % xs.size
      if( bound <0) 
        rotate(bound + xs.size , xs)
      else {
        val tmp = split(bound , xs)
        tmp._2 ::: tmp._1
      }  
  }
}