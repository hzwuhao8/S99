package s99

import scala.util.Success
import scala.util.Failure

sealed abstract class Tree[+T] {
  def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1
  def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)
  def toDotstring: String
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
    val (leftTree, myX) = left.layoutBinaryTreeInternal(x, depth + 1)
    val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1)
    (PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
  }
  def toDotstring: String = value.toString + left.toDotstring + right.toDotstring
}

case class PositionedNode[+T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) extends Tree[T] {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
    throw new RuntimeException("Not Exec")
  }
  def toDotstring: String = value.toString + left.toDotstring + right.toDotstring
}

case object End extends Tree[Nothing] {
  override def toString = "."
  def toDotstring: String = "."
  def layoutBinaryTreeInternal(x: Int, depth: Int) = (End, x)
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
      case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
    }
    trace(s"res=\n${res.mkString("\n")}")
    res
  }

  def size[A](t: Tree[A]): Int = t match {
    case End                  => 0
    case Node(_, End, End)    => 1
    case Node(_, l, r)        => size(l) + size(r)
    case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
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
      case End                  => true
      case Node(_, End, End)    => true
      case Node(_, l, r)        => isMirrorOf(l, r)
      case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
    }
  }

  def addValue[U <% Ordered[U]](t: Tree[U], v: U): Tree[U] = {
    debug(s"T=${t}\tv=${v}")
    t match {
      case End                  => Node(v)
      case Node(x, End, End)    => if (x > v) Node(x, Node(v), End) else Node(x, End, Node(v))
      case Node(x, l, r)        => if (x > v) Node(x, addValue(l, v), r) else Node(x, l, addValue(r, v))
      case x: PositionedNode[U] => throw new RuntimeException("Must not reach here!")
    }
  }

  def fromList[A <% Ordered[A]](xs: List[A]): Tree[A] = {
    trace(s"xs={$xs}")
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
      case End                  => 0
      case Node(_, End, End)    => 1
      case Node(_, l, r)        => leafCount(l) + leafCount(r)
      case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
    }
  }

  def leafList[A](t: Tree[A]): List[A] = t match {
    case End                  => Nil
    case Node(x, End, End)    => List(x)
    case Node(_, l, r)        => leafList(l) ::: leafList(r)
    case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
  }

  def internalList[A](t: Tree[A]): List[A] = t match {
    case End => Nil
    case Node(x, End, End) => Nil
    case Node(x, Node(_, End, End), End) => List(x)
    case Node(x, End, Node(_, End, End)) => List(x)
    case Node(x, Node(y, End, End), Node(z, End, End)) => List(x)
    case Node(x, l, r) => x :: internalList(l) ::: internalList(r)
    case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
  }

  def atLevel[A](t: Tree[A], level: Int): List[A] = {
    val res = countLevel(t, 1)
    res.filter(_._2 == level).map { _._1 }
  }

  def countLevel[A](t: Tree[A], level: Int): List[(A, Int)] = {
    t match {
      case End                  => Nil
      case Node(x, End, End)    => List((x, level))
      case Node(x, l, r)        => (x, level) :: countLevel(l, level + 1) ::: countLevel(r, level + 1)
      case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
    }
  }

  def hight[A](t: Tree[A]): Int = t match {
    case End                  => 0
    case Node(_, End, End)    => 1
    case Node(_, l, r)        => Math.max(hight(l), hight(r)) + 1
    case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
  }

  def isHbalance[A](t: Tree[A]): Boolean = t match {
    case End                  => true
    case Node(_, End, End)    => true
    case Node(_, l, r)        => Math.abs(hight(l) - hight(r)) <= 1
    case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
  }

  def maxHbalNodes(height: Int): Int = 2 * height - 1

  def minHbalNodes(height: Int): Int = height match {
    case 0 => 0
    case 1 => 1
    case 2 => 2
    case h =>
      1 + minHbalNodes(h - 1) + minHbalNodes(h - 2)
  }

  def minHbalHeight(nodes: Int): Int =
    if (nodes == 0) 0
    else minHbalHeight(nodes / 2) + 1

  def maxHbalHeight(nodes: Int): Int = Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last

  def toString[A](t: Tree[A]): String = t match {
    case End                  => ","
    case Node(x, End, End)    => x.toString
    case Node(x, left, End)   => s"${x}(${toString(left)},)"
    case Node(x, End, right)  => s"${x}(,${toString(right)})"
    case Node(x, left, right) => s"${x}(${toString(left)},${toString(right)})"
    case x: PositionedNode[A] => throw new RuntimeException("Must not reach here!")
  }
  def fromString(str: String): Tree[Char] = {
    trace(s"str=${str}")
    if (str.startsWith("(") && str.endsWith(")")) {
      fromString(str.substring(1, str.length() - 1))
    } else {
      val res = str.toList match {
        case Nil                          => End
        case List(a)                      => Node(a)
        case List(a, '(', b, ',', c, ')') => Node(a, Node(b), Node(c))
        case List(f, '(', g, ',', ')')    => Node(f, Node(g), End)
        case List(f, '(', ',', g, ')')    => Node(f, End, Node(g))
        case a :: as if (a >= 'a' && a <= 'z') =>
          val (myleft, myright): (String, String) = as match {
            case '(' :: ',' :: aas =>
              val left = ""
              val right = aas.init
              (left, right.mkString)
            case '(' :: x :: ',' :: aas =>
              val left = x.toString
              val right = aas.init
              (left, right.mkString)
            case _ =>
              val bs = as.map { a =>
                if (a == '(') {
                  ("(", 1)
                } else if (a == ')') {
                  (")", -1)
                } else {
                  (a.toString, 0)
                }
              }
              trace(s"bs=${bs}")

              val cs = bs.scanLeft(("", 0)) { (z, b) =>
                val res = (z._1 + b._1, z._2 + b._2)
                trace(s"foldres=${res}")
                res
              }

              trace(s"cs=\n${cs.mkString("\n")}")
              val xa = cs.indexWhere(_._2 > 1)
              trace(s"xa=${xa}")
              val xb = cs.indexWhere(_._2 == 1, xa)
              trace(s"xb=${xb}")
              val left = as.drop(1).take(xb).init.mkString
              trace(s"left=${left}")
              val right = as.drop(xb + 1).init.mkString
              trace(s"right=${right}")
              (left, right)
          }

          Node(a, fromString(myleft), fromString(myright))
        case _ => End
      }

      res
    }
  }

  /**
   * use fastParse
   */
  def fromStringParse(str: String): Tree[Char] = {
    String2Tree.ttWithEnd.parse(str).fold({
      (_, _, _) => s99.End
    }, {
      case (x: Tree[Char], i: Int) => x
    })

  }

  def fromStringUseParboiled(str: String): Tree[Char] = {
    if (str.isEmpty()) {
      End
    } else {
      val res = (new String2TreeUseParboiled2(str)).InputLine.run()
      res match {
        case Success(x) => x
        case Failure(_) => End
      }
    }
  }

  def preorder[A](t: Tree[A]): List[A] = t match {
    case End                  => Nil
    case Node(x, left, right) => List(x) ::: preorder(left) ::: preorder(right)
  }

  def inOrder[A](t: Tree[A]): List[A] = t match {
    case End                  => Nil
    case Node(x, left, right) => inOrder(left) ::: List(x) ::: inOrder(right)
  }

  def preInTree[A](pre: List[A], in: List[A]): Tree[A] = {
    trace(s"pre=$pre\tin=${in}")
    pre match {
      case Nil => End
      case a :: as =>
        val (leftIn, rightIn) = in.span(_ != a)
        Node(a, preInTree(as.take(leftIn.size), leftIn), preInTree(as.drop(leftIn.size), rightIn.drop(1)))
    }
  }

  def fromDotstring(ds: String): Tree[Char] = {
    def fromDotstringR(pos: Int): (Tree[Char], Int) = {
      debug(s"pos=${pos}\tc=${ds(pos)}")
      val res = ds(pos) match {
        case '.' => (End, pos + 1)
        case c => {
          val (lTree, lPos) = fromDotstringR(pos + 1)
          val (rTree, rPos) = fromDotstringR(lPos)
          (Node(c, lTree, rTree), rPos)
        }
      }
      debug(s"res=${res}")
      res
    }
    fromDotstringR(0)._1
  }

  def fromDotstringUsefastParse(ds: String): Tree[Char] = {
    DotString2Tree.run.parse(ds) fold ({
      (_, _, _) => s99.End
    }, {
      case (x: Tree[Char], i: Int) => x
    })

  }

}

object String2Tree {
  import fastparse.all._
  val cc: P[Char] = P(CharIn('a' to 'z').!.map(_.head))
  //val parens: P[Tree[Char]] = P(cc ~ "(" ~/ tt ~ "," ~ tt ~ ")")

  val n0: P[Tree[Char]] = P(cc).map { c =>
    Node(c)
  }

  val n1: P[Tree[Char]] = P(cc ~ "(" ~ "," ~ tt ~ ")").map { p =>
    Node(p._1, s99.End, p._2)
  }
  val n2: P[Tree[Char]] = P(cc ~ "(" ~ tt ~ "," ~ ")").map { p =>
    Node(p._1, p._2, s99.End)
  }
  val n3: P[Tree[Char]] = P(cc ~ "(" ~ tt ~ "," ~ tt ~ ")").map { p =>
    Node(p._1, p._2, p._3)
  }

  val n4 = P(cc ~ "(" ~ n0 ~ "," ~ n0 ~ ")")

  val tt: P[Tree[Char]] = P(n1 | n2 | n3 | n0)

  val ttWithEnd: P[Tree[Char]] = P(tt ~ fastparse.all.End)

}

class String2TreeUseParboiled2(val input: org.parboiled2.ParserInput) extends org.parboiled2.Parser {
  import org.parboiled2._
  def Alpha = rule { CharPredicate.LowerAlpha }
  def n0: Rule1[s99.Node[Char]] = rule { capture(Alpha) ~> { (x: String) => s99.Node(x.head) } }
  def n1: Rule1[s99.Node[Char]] = rule { n0 ~ "(," ~ nn ~> { (y: s99.Node[Char], x: s99.Node[Char]) => y.copy(right = x) } ~ ")" }
  def n2: Rule1[s99.Node[Char]] = rule { n0 ~ "(" ~ nn ~> { (y: s99.Node[Char], x: s99.Node[Char]) => y.copy(left = x) } ~ ",)" }
  def n3: Rule1[s99.Node[Char]] = rule { n0 ~ "(" ~ nn ~> { (y: s99.Node[Char], x: s99.Node[Char]) => y.copy(left = x) } ~ "," ~ nn ~> { (y: s99.Node[Char], x: s99.Node[Char]) => y.copy(right = x) } ~ ")" }

  def nn: Rule1[s99.Node[Char]] = rule { n1 | n2 | n3 | n0 }

  def InputLine: Rule1[s99.Node[Char]] = rule { nn ~ EOI }
}

object DotString2Tree {
  import fastparse.all._
  val cc: P[Char] = P(CharIn('a' to 'z').!.map(_.head))
  //val parens: P[Tree[Char]] = P(cc ~ "(" ~/ tt ~ "," ~ tt ~ ")")

  /**
   * e..
   */
  val n1: P[Node[Char]] = P { cc ~ ".." }.map { c => Node(c) }

  /**
   * a.b..
   */
  val n2: P[Node[Char]] = P { cc ~ "." ~ cc ~ ".." }.map { p => Node(p._1, s99.End, Node(p._2)) }

  /**
   * ab...
   */
  val n3: P[Node[Char]] = P { cc ~ cc ~ "..." }.map { p => Node(p._1, Node(p._2), s99.End) }

  val n: P[Node[Char]] = P { n1 | n2 | n3 }

  val na: P[Node[Char]] = P { cc ~ "." ~ nn }.map { p => (Node(p._1)).copy(right = p._2) }
  val nb: P[Node[Char]] = P { cc ~ nn ~ "." }.map { p => (Node(p._1)).copy(left = p._2) }
  val nc: P[Node[Char]] = P { cc ~ nn ~ nn }.map { p => (Node(p._1)).copy(left = p._2, right = p._3) }

  val nn: P[Node[Char]] = P { n1 | n2 | n3 | na | nb | nc  }
  val run: P[Node[Char]] = P { nn ~ fastparse.all.End }
}
 
