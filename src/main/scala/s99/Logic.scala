package s99

import scala.language.implicitConversions

object Logic extends Log {

  def not(a: Boolean): Boolean = !a
  def and(a: Boolean, b: Boolean): Boolean = a && b
  def or(a: Boolean, b: Boolean): Boolean = a || b
  def nand(a: Boolean, b: Boolean): Boolean = !(and(a, b))
  def nor(a: Boolean, b: Boolean): Boolean = !(nor(a, b))
  def xor(a: Boolean, b: Boolean): Boolean = !(a == b)
  def equ(a: Boolean, b: Boolean): Boolean = a == b
  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)
  implicit def b2l(a: Boolean) = new Logic(a)

  def table2(f: (Boolean, Boolean) => Boolean): String = {
    val fmt = s"%-5s %-5s %-5s\n"
    val head = fmt.format("A", "B", "Result")
    val res = for {
      a <- List(true, false)
      b <- List(true, false)
    } yield {
      fmt.format(a, b, f(a, b))
    }

    head + res.mkString
  }

  def gray(n: Int): List[String] = {
    n match {
      case 1 => List("0", "1")
      case n =>
        val tmp = gray(n - 1)
        val tmp1 = tmp.map { x => "0" + x }
        val tmp2 = tmp.map { x => "1" + x }
        tmp1 ++ tmp2
    }
  }

  def huffman(xs: List[(String, Int)]): List[(String, String)] = {
    val ys = xs.sortBy(_._2)
    val zs = P0828.combinations(2, ys)

    Nil
  }

}
class Logic(a: Boolean) {
  import Logic.not
  import Logic.b2l
  def and(b: Boolean) = a && b

  def or(b: Boolean): Boolean = a || b
  def nand(b: Boolean): Boolean = not(and(b))
  def nor(b: Boolean): Boolean = not(nor(b))
  def xor(b: Boolean): Boolean = not(a == b)
  def equ(b: Boolean): Boolean = a == b
  def impl(b: Boolean): Boolean = not(a) or (b)

}

/**
 * http://rosettacode.org/wiki/Huffman_coding#Scala
 */
object Huffman extends Log {
  sealed trait Tree[+A]
  case class Leaf[A](v: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  //case object End extends Tree[Nothing]

  def contains[A](tree: Tree[A], ch: A): Boolean = {
    tree match {
      //case End                 => false
      case Leaf(a)             => ch == a
      case Branch(left, right) => contains(left, ch) || contains(right, ch)
    }
  }

  def encode[A](tree: Tree[A], v: A): String = {
    @scala.annotation.tailrec
    def go[A](tree: Tree[A], v: A, code: String): String = tree match {
      //case End          => ""
      case Leaf(_)      => code
      case Branch(l, r) => if (contains(l, v)) go(l, v, code + "0") else go(r, v, code + "1")
    }
    go(tree, v, "")
  }

  @scala.annotation.tailrec
  def merge[A](xs: List[(Tree[A], Int)]): List[(Tree[A], Int)] = {
    debug(s"xs=${xs}")
    xs match {
      case List(a) => xs
      case l :: r :: as =>
        val m = (Branch(l._1, r._1), l._2 + r._2)
        merge( (m :: as).sortBy(_._2))
    }
  }

  def codify[A](xs: Tree[A]): List[(A, String)] = {
    def recurese(xs: Tree[A], prefix: String): List[(A, String)] = xs match {
     
      case Leaf(c)      => (c, prefix) :: Nil
      case Branch(l, r) => recurese(l, prefix + "0") ::: recurese(r, prefix + "1")
    }
    recurese(xs, "")
  }

}