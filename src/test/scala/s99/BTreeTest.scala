package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

class BTreeTest extends FunSuite with Checkers with Log {

  test("P55 cBalanced") {
    //    val res1 = Tree.cBalanced(1, "a")
    //    debug(s"res=${res1}")
    //    
    //    val res2 = Tree.cBalanced(2, "a")
    //    debug(s"res=${res2}")

    val res3 = Tree.cBalanced(3, 'a')
    info(s"res=\n${res3.mkString("\n")}")

    val res4 = Tree.cBalanced(4, 'a')
    info(s"res=\n${res4.mkString("\n")}")
  }

  test("P56 isSymmetric") {
    val tree = Node('a, Node('b), Node('c))
    assert(Tree.isSymmetric(tree))

    {
      val t1 = Node(2, Node(3), Node(4))
      val t2 = Node(2, Node(4), Node(3))
      val tree2 = Node(1, t1, t2)
      assert(Tree.isSymmetric(tree2))
    }

    {
      val t1 = Node(2, End, Node(4))
      val t2 = Node(2, End, Node(3))
      val tree2 = Node(1, t1, t2)
      assert(Tree.isSymmetric(tree2) == false ) 
    }
  }
}