package s99

case class MTree[T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  def toString2 = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
  def nodeCount(): Int = {
    1 + children.map { _.nodeCount }.sum
  }
  override def toString: String = {
    val s1 = children.map { _.toString + "^" }.mkString

    s"${value}${s1}"
  }

  def addChild(a: MTree[T]): MTree[T] = MTree(value, a :: children)
}

object MTree extends Log {
  import scala.language.implicitConversions

  def apply[T](value: T) = new MTree(value, List())
  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

  implicit def string2MTree(s: String): MTree[Char] = {
    debug(s"s=${s}")
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    val strList = splitChildStrings(1)
    debug(s"strList=${strList}")
    MTree(s(0), strList.map(string2MTree(_)))

  }
}

object MTreeParse {
  import fastparse.all._
  val cc: P[Char] = P(CharIn('a' to 'z').!.map(_.head))
  val baser1: P[MTree[Char]] = P { cc }.map { c => MTree(c) }

  val baser2: P[MTree[Char]] = P { baser1 ~ (baser1 ~ "^").rep(1) }.map { p => MTree(p._1.value, p._2.toList) }

  val baser3: P[MTree[Char]] = P { baser1 ~ (baser1 ~ "^").rep ~ baser1 ~ baser1 ~ "^^" }.map { p =>
    val parent = p._1
    val child = p._2.toList
    val lastchild = p._3.copy(children = List(p._4))
    parent.copy(children = (child ::: List(lastchild)))
  }

  //val r1: P[MTree[Char]] = P { rr ~ rr ~ "^" }.map { p => MTree(p._1.value, List(p._2)) }

  val r2a: P[Seq[MTree[Char]]] = P { (rr ~ "^").rep(1) }

  val r2: P[MTree[Char]] = P { rr ~ r2a }.map { p => p._1.copy(children = p._2.toList) }

  val r3: P[MTree[Char]] = P { rr ~ (rr ~ "^").rep(0) ~ rr ~ rr ~ "^^" }.map { p =>
    val parent = p._1
    val child = p._2.toList
    val lastchild = p._3.copy(children = List(p._4))
    parent.copy(children = (child ::: List(lastchild)))
  }

  val rr: P[MTree[Char]] = P { baser3 | baser2 | baser1 | r3 | r2 }

  val rrr = P { rr ~ fastparse.all.End }

}