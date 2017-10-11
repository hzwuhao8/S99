package s99

sealed abstract class Tree[+T] {

}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree extends Log {

  def merger[A](value: A, tree: Tree[A]): List[Tree[A]] = {
    trace(s"t={$tree}")
    val res = tree match {
      case End => List(Node(value, End, End))
      //      case Node(v1, End, End) =>
      //        List(Node(v1, Node(value), End), Node(v1, End, Node(value)))
      case Node(v1, left, right) =>
        val lsize = size(left)
        val rsize = size(right)
        val (leftList, rightList): (List[Tree[A]], List[Tree[A]]) = if (lsize == rsize) {
          val l1 = merger(value, left)
          val r1 = merger(value, right)
          (l1, r1)
        } else if (lsize > rsize) {
          val l1 = Nil
          val r1 = merger(value, right)
          (l1, r1)
        } else {
          val l1 = merger(value, left)
          val r1 = Nil
          (l1, r1)
        }
        val l1 = leftList.map { newleft => Node(v1, newleft, right) }
        val l2 = rightList.map { newright => Node(v1, left, newright) }
        l1 ::: l2
    }
    trace(s"res=\n${res.mkString("\n")}")
    res
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

  def isMirrorOf[A](left: Tree[A], right: Tree[A]): Boolean = {
    (left, right) match {
      case (End, End) => true
      case (Node(_, End, End), Node(_, End, End)) => true
      case (Node(_, Node(_, End, End), End), Node(_, End, Node(_, End, End))) => true
      case (Node(_, l1, r1), Node(_, l2, r2)) => isMirrorOf(l1, r2) && isMirrorOf(r1, l2)
      case _ => false
    }
  }

  def isSymmetric[A](t: Tree[A]): Boolean = {
    t match {
      case End               => true
      case Node(_, End, End) => true
      case Node(_, l, r)     => isMirrorOf(l, r)

    }
  }

  def addValue[U <% Ordered[U]](t: Tree[U], v: U): Tree[U] = {
    debug(s"T=${t}\tv=${v}")
    t match {
      case End               => Node(v)
      case Node(x, End, End) => if (x > v) Node(x, Node(v), End) else Node(x, End, Node(v))
      case Node(x, l, r)     => if (x > v) Node(x, addValue(l, v), r) else Node(x, l, addValue(r, v))
    }
  }

  def fromList[A <% Ordered[A]](xs: List[A]): Tree[A] = {
    debug(s"xs={$xs}")
    xs match {
      case Nil     => End
      case a :: as => addValue(fromList(as), a)
    }
  }

  def symmetricBalancedTrees[A](n: Int, x: A): List[Tree[A]] = {
    cBalanced(n, x).filter(isSymmetric(_))
  }

  def leafCount[A](x: Tree[A]): Int = {
    x match {
      case End               => 0
      case Node(_, End, End) => 1
      case Node(_, l, r)     => leafCount(l) + leafCount(r)
    }
  }

  def leafList[A](t: Tree[A]): List[A] = t match {
    case End               => Nil
    case Node(x, End, End) => List(x)
    case Node(_, l, r)     => leafList(l) ::: leafList(r)
  }

  def internalList[A](t: Tree[A]): List[A] = t match {
    case End => Nil
    case Node(x, End, End) => Nil
    case Node(x, Node(_, End, End), End) => List(x)
    case Node(x, End, Node(_, End, End)) => List(x)
    case Node(x, Node(y, End, End), Node(z, End, End)) => List(x)
    case Node(x, l, r) => x :: internalList(l) ::: internalList(r)
  }

  def atLevel[A](t: Tree[A], level: Int): List[A] = {
    val res = countLevel(t, 1)
    res.filter(_._2 == level).map { _._1 }
  }

  def countLevel[A](t: Tree[A], level: Int): List[(A, Int)] = {
    t match {
      case End               => Nil
      case Node(x, End, End) => List((x, level))
      case Node(x, l, r)     => (x, level) :: countLevel(l, level + 1) ::: countLevel(r, level + 1)
    }
  }

  def hight[A](t: Tree[A]): Int = t match {
    case End               => 0
    case Node(_, End, End) => 1
    case Node(_, l, r)     => Math.max(hight(l), hight(r)) + 1
  }

  def isHbalance[A](t: Tree[A]): Boolean = t match {
    case End               => true
    case Node(_, End, End) => true
    case Node(_, l, r)     => Math.abs(hight(l) - hight(r)) <= 1
  }

  def minHbalNodes(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case 2 => 2
    case n =>
      1 + minHbalNodes(n - 1) + minHbalNodes(n - 2)
  }
}