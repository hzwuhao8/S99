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
      assert(Tree.isSymmetric(tree2) == false)
    }
  }

  test("P58 ") {
    val res = Tree.symmetricBalancedTrees(5, 'x)
    debug(s"res=\n${res.mkString("\n")}")
  }
  
  test("P60"){
    assert( Tree.minHbalNodes(3) == 4 )
    assert( Tree.minHbalNodes(4) == 7 )
  }

  test("P61") {
    assert(Tree.leafCount(End) == 0)
    assert(Tree.leafCount(Node(1)) == 1)
    assert(Tree.leafCount(Node(1, Node(1), Node(1))) == 2)
  }

  test("P61A") {
    check { (xs: List[Byte]) =>
      val t = Tree.fromList(xs)
      val c1 = Tree.leafCount(t)
      val l1 = Tree.leafList(t)
      c1 == l1.size
    }
  }
  
  test("p62"){
    val t = Node('a', Node('b'), Node('c', Node('d'), Node('e')))
    assert( Tree.internalList(t) == List('a','c') )
    
  }
  
  test("p62B"){
    val t = Node('a', Node('b'), Node('c', Node('d'), Node('e')))
    assert( Tree.atLevel(t,2) == List('b','c') )
  }
  
  test("P64"){
    val res = Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
    debug(s"res=$res")
  }
  
  test("P67 A string representation of binary trees."){
    val t = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))
    val res = Tree.toString(t)
    debug(s"res=${res}")
    
    val t2 = Tree.fromString(res)
  }
  
}