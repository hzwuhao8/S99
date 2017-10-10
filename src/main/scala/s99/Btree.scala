package s99

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree extends Log {

  def merger[A](value: A, tree: Tree[A]): List[Tree[A]] = {
    debug(s"t={$tree}")
    tree match {
      case End => Nil
      case Node(v1, End, End) =>
        List(Node(v1, Node(value), End), Node(v1, End, Node(value)))
      case Node(v1, left, right) =>
        val lsize = size(left)
        val rsize = size(right)
        val (leftList, rightList):(List[Tree [A]],List[Tree[A]]) = if (lsize == rsize) {
          val l1 = merger(value, left)
          val r1 = merger(value, right)
          (l1, r1)
        }else if(lsize > rsize){
          val l1 = List(left)
          val r1 = merger(value, right)
          (l1,r1)
        }else{
          val l1 = merger(value, left)
          val r1 = List(right)
          (l1,r1)
        }
        val l1 = leftList.map { newleft => Node(v1, newleft, right) }
        val l2 = rightList.map { newright => Node(v1, left, newright) }
        l1 ::: l2
    }
  }

  def size[A](t: Tree[A]): Int = t match {
    case End               => 0
    case Node(_, End, End) => 1
    case Node(_, l, r)     => size(l) + size(r)
  }

  def cBalanced[A](n: Int, value: A): List[Tree[A]] = {
    n match {
      case 1 => List(Node(value))
     // case 2 => List(Node(value, End, Node(value)), Node(value, Node(value), End))
      case n =>
        val tmp = cBalanced(n - 1, value)
        tmp.flatMap { t => merger(value, t) }
        
    }
  }
}